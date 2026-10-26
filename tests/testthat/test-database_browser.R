box::use(
  jsonlite[fromJSON],
  shiny[reactive, testServer],
  withr[local_tempdir],
)
box::use(
  app / logic / custom_fields[custom_col],
  app / logic / database_functions[read_metadata_table],
  app / view / database_browser,
  dev / mock_database,
)

impl <- attr(database_browser, "namespace")

# paste0() recycles a zero-length vector to "", so the prefix is applied only
# when there is something to prefix - otherwise a zero-isolate frame would come
# back with one row.
.frame <- function(isolates, prefix) {
  isolates <- as.character(isolates)
  data.frame(
    isolate = isolates,
    host = if (length(isolates)) paste0(prefix, isolates) else character(0),
    stringsAsFactors = FALSE
  )
}

# The grid's snapshot: what the user has been editing.
edited_frame <- function(isolates) .frame(isolates, "edited-")

# The metadata table as it stands in the database right now.
current_frame <- function(isolates) .frame(isolates, "db-")

test_that("an unchanged pool keeps every edit", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_identical(got$host, c("edited-a", "edited-b"))
})

test_that("an isolate added since the grid was filled survives the save", {
  # save_metadata_table() itself now writes only the cells a tidy edit diff
  # names, so it no longer depends on this reconciliation for safety - but the
  # save handler still calls it to report "N added, M removed elsewhere" to
  # the user, and that report has to be right: an isolate typed while the grid
  # sat with pending edits must show up as added, not silently vanish from it.
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b", "typed_meanwhile"))
  )
  expect_identical(got$isolate, c("a", "b", "typed_meanwhile"))
  expect_identical(got$host, c("edited-a", "edited-b", "db-typed_meanwhile"))
})

test_that("an isolate removed since the grid was filled is not resurrected", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "removed", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_false("removed" %in% got$isolate)
})

test_that("edits apply to the right rows when the pool changed order", {
  # match() on isolate, not row position: the fresh read may order rows
  # differently, and aligning by position would write A's host onto B.
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b", "c")),
    current_frame(c("c", "a", "b"))
  )
  expect_identical(got$isolate, c("c", "a", "b"))
  expect_identical(got$host, c("edited-c", "edited-a", "edited-b"))
})

test_that("simultaneous addition and removal are both handled", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "gone")),
    current_frame(c("a", "fresh"))
  )
  expect_identical(got$isolate, c("a", "fresh"))
  expect_identical(got$host, c("edited-a", "db-fresh"))
})

test_that("the database's column set wins for columns the grid never had", {
  edited <- edited_frame("a")
  current <- current_frame("a")
  current$country <- "DE"
  got <- impl$.reconcile_metadata(edited, current)
  expect_true("country" %in% names(got))
  expect_identical(got$country, "DE")
  expect_identical(got$host, "edited-a")
})

test_that("a column the grid holds but the database lacks is not written back", {
  # Derived columns (MLST / AMR / custom) are stripped before this point, but
  # the guard matters: they must never reach the metadata table.
  edited <- edited_frame("a")
  edited$mlst_ST <- "7"
  got <- impl$.reconcile_metadata(edited, current_frame("a"))
  expect_false("mlst_ST" %in% names(got))
})

test_that("the isolate key itself is never overwritten from the snapshot", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("a", "b")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
})

test_that("no overlap at all leaves the database untouched", {
  got <- impl$.reconcile_metadata(
    edited_frame(c("x", "y")),
    current_frame(c("a", "b"))
  )
  expect_identical(got$isolate, c("a", "b"))
  expect_identical(got$host, c("db-a", "db-b"))
})

test_that("an empty or missing current table falls back to the snapshot", {
  # A database with no metadata rows yet: there is nothing to reconcile
  # against, so the grid is the only source.
  expect_identical(
    impl$.reconcile_metadata(edited_frame("a"), NULL)$isolate,
    "a"
  )
  expect_identical(
    impl$.reconcile_metadata(edited_frame("a"), current_frame(character(0)))$isolate,
    "a"
  )
})

test_that("an empty snapshot yields the current table unchanged", {
  got <- impl$.reconcile_metadata(edited_frame(character(0)), current_frame("a"))
  expect_identical(got$isolate, "a")
  expect_identical(got$host, "db-a")
})

# ---------------------------------------------------------------------------
# The cell-edit -> State$metadata_dirty -> save_metadata_table() flow
# ---------------------------------------------------------------------------

# 0-based, matching what DT's cell_edit input and editData() both expect.
.col_idx <- function(state, column) match(column, names(state$data)) - 1L

test_that("editing a cell buffers a tidy diff entry and marks the grid pending", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "France")
    )
    session$flushReact()

    expect_identical(State$metadata_dirty$isolate, "A")
    expect_identical(State$metadata_dirty$column, "geo_loc_name_country")
    expect_identical(State$metadata_dirty$value, "France")
    expect_true(State$pending)
  })
})

test_that("re-editing the same cell replaces its pending value, not queues a second one", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "France")
    )
    session$flushReact()
    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "Spain")
    )
    session$flushReact()

    expect_identical(nrow(State$metadata_dirty), 1L)
    expect_identical(State$metadata_dirty$value, "Spain")
  })
})

test_that("editing back to the original value is a no-op, not a dirty entry", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "Germany")
    )
    session$flushReact()

    expect_null(State$metadata_dirty)
    expect_false(isTRUE(State$pending))
  })
})

test_that("Save writes the buffered cell and clears the dirty state", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "France")
    )
    session$flushReact()

    session$setInputs(save = 1)
    session$flushReact()

    expect_null(State$metadata_dirty)
    expect_false(State$pending)
  })

  after <- read_metadata_table(path)
  expect_identical(after$geo_loc_name_country[after$isolate == "A"], "France")
  # B, which the user never touched, keeps its original value.
  expect_identical(after$geo_loc_name_country[after$isolate == "B"], "Germany")
})

test_that("Discard drops the buffered cell edit without writing it", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "France")
    )
    session$flushReact()

    session$setInputs(discard = 1)
    session$flushReact()

    expect_null(State$metadata_dirty)
    expect_false(State$pending)
  })

  expect_identical(read_metadata_table(path)$geo_loc_name_country, "Germany")
})

# ---------------------------------------------------------------------------
# Spreadsheet batches (app/js/dt-sheet.js -> metadata_table_sheet)
# ---------------------------------------------------------------------------

# One paste/cut/delete batch as the client sends it: columnar, keyed by isolate.
.sheet_batch <- function(isolate, col, value, skipped = 0L) {
  list(op = "edit", isolate = isolate, col = col, value = value, skipped = skipped)
}

test_that("a pasted block buffers one diff entry per cell", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B", "C")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_sheet = .sheet_batch(
        c("C", "A"),
        c(col, col),
        c("Spain", "France")
      )
    )
    session$flushReact()

    dirty <- State$metadata_dirty[order(State$metadata_dirty$isolate), ]
    expect_identical(dirty$isolate, c("A", "C"))
    expect_identical(dirty$value, c("France", "Spain"))
    expect_identical(State$data$geo_loc_name_country, c("France", "Germany", "Spain"))
    expect_true(State$pending)
  })
})

test_that("read-only columns are never written, whatever the batch says", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()

    session$setInputs(
      metadata_table_sheet = .sheet_batch(
        c("A", "A"),
        c(.col_idx(State, "isolate"), .col_idx(State, "organism")),
        c("Z", "Other species")
      )
    )
    session$flushReact()

    expect_null(State$metadata_dirty)
    expect_identical(State$data$isolate, "A")
    expect_false(isTRUE(State$pending))
  })
})

test_that("a pasted date must be YYYY-MM-DD; the valid ones still go through", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "sample_collection_date")

    session$setInputs(
      metadata_table_sheet = .sheet_batch(
        c("A", "B"),
        c(col, col),
        c("2025-03-04", "04.03.2025")
      )
    )
    session$flushReact()

    expect_identical(State$metadata_dirty$isolate, "A")
    expect_identical(State$metadata_dirty$value, "2025-03-04")
    expect_identical(State$data$sample_collection_date[[2]], "2024-01-02")
  })
})

test_that("custom-variable cells are validated and normalised by their type", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B", "C")))
  seed_custom(path, list(
    age = list(type = "integer"),
    risk = list(type = "category", levels = '["High","Low"]')
  ))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    age <- .col_idx(State, custom_col("age"))
    risk <- .col_idx(State, custom_col("risk"))

    session$setInputs(
      metadata_table_sheet = .sheet_batch(
        c("A", "B", "A", "B", "C"),
        c(age, age, risk, risk, risk),
        c("42", "forty", "high", "Medium", "")
      )
    )
    session$flushReact()

    dirty <- State$custom_dirty
    expect_identical(nrow(dirty), 2L)
    expect_identical(dirty$value[dirty$isolate == "A"], c("42", "High"))
    expect_identical(State$data[[custom_col("age")]], c(42, NA, NA))
    expect_identical(State$data[[custom_col("risk")]], c("High", NA, NA))
  })
})

test_that("clearing cells stores empty values and drops no-op clears", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_custom(path, list(note = list(type = "text", values = c(A = "x"))))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    country <- .col_idx(State, "geo_loc_name_country")
    note <- .col_idx(State, custom_col("note"))

    session$setInputs(
      metadata_table_sheet = .sheet_batch(
        c("A", "A", "B"),
        c(country, note, note),
        c("", "", "")
      )
    )
    session$flushReact()

    expect_identical(State$metadata_dirty$value, "")
    expect_identical(State$custom_dirty$isolate, "A")
    expect_true(is.na(State$custom_dirty$value))
  })
})

test_that("a later batch replaces cells still pending from an earlier one", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_sheet = .sheet_batch(c("A", "B"), c(col, col), c("France", "Spain"))
    )
    session$flushReact()
    session$setInputs(
      metadata_table_sheet = .sheet_batch("B", col, "Italy")
    )
    session$flushReact()

    dirty <- State$metadata_dirty[order(State$metadata_dirty$isolate), ]
    expect_identical(dirty$value, c("France", "Italy"))
  })
})

test_that("undo restores a pasted block and redo re-applies it", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")

    session$setInputs(
      metadata_table_sheet = .sheet_batch(c("A", "B"), c(col, col), c("France", "Spain"))
    )
    session$flushReact()
    session$setInputs(metadata_table_sheet = list(op = "undo"))
    session$flushReact()
    expect_identical(State$data$geo_loc_name_country, c("Germany", "Germany"))

    session$setInputs(metadata_table_sheet = list(op = "redo"))
    session$flushReact()
    expect_identical(State$data$geo_loc_name_country, c("France", "Spain"))
  })
})

test_that("undo covers single-cell edits and restores values validation would reject", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  meta <- meta_df("A")
  meta$sample_collection_date <- "04.03.2020"
  build_db(path, default_local(), metadata = meta)

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "sample_collection_date")

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = col, value = "2020-03-04")
    )
    session$flushReact()
    expect_identical(State$data$sample_collection_date, "2020-03-04")

    session$setInputs(metadata_table_sheet = list(op = "undo"))
    session$flushReact()
    expect_identical(State$data$sample_collection_date, "04.03.2020")
    expect_null(State$metadata_dirty)
  })
})

test_that("a new edit drops the redo steps, and discard drops the history", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df("A"))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    col <- .col_idx(State, "geo_loc_name_country")
    edit <- function(value) {
      session$setInputs(metadata_table_sheet = .sheet_batch("A", col, value))
      session$flushReact()
    }

    edit("France")
    session$setInputs(metadata_table_sheet = list(op = "undo"))
    session$flushReact()
    edit("Spain")
    session$setInputs(metadata_table_sheet = list(op = "redo"))
    session$flushReact()
    expect_identical(State$data$geo_loc_name_country, "Spain")

    session$setInputs(discard = 1)
    session$flushReact()
    session$setInputs(metadata_table_sheet = list(op = "undo"))
    session$flushReact()
    expect_identical(State$data$geo_loc_name_country, "Germany")
    expect_false(State$pending)
  })
})

test_that("undoing back to the saved values leaves nothing pending", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  seed_custom(path, list(age = list(type = "integer", values = c(A = "3"))))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    country <- .col_idx(State, "geo_loc_name_country")
    age <- .col_idx(State, custom_col("age"))

    session$setInputs(
      metadata_table_sheet = .sheet_batch(c("A", "A"), c(country, age), c("test", "9"))
    )
    session$flushReact()
    expect_true(State$pending)

    session$setInputs(metadata_table_sheet = list(op = "undo"))
    session$flushReact()
    expect_null(State$metadata_dirty)
    expect_null(State$custom_dirty)
    expect_false(State$pending)
  })
})

test_that("a row drag in Selection mode is applied without error", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    session$setInputs(selection_mode = "on", metadata_table_rows_selected = 1L)
    session$setInputs(
      metadata_table_row_drag = list(isolate = list("A", "B"), select = TRUE)
    )
    expect_silent(session$flushReact())
  })
})

# ---------------------------------------------------------------------------
# AMR gene columns join the grid only once their group is picked
# ---------------------------------------------------------------------------

# A mock database with AMR screens, plus a custom variable: the one column
# that sits after the gene columns in the full frame, so its grid index moves
# when genes join.
amr_db <- function(dir) {
  path <- file.path(dir, "mock.db")
  suppressMessages(mock_database$mock_database(path, n_isolates = 12L))
  seed_custom(path, list(age = list(type = "integer")))
  path
}

# The grid widget's options as rendered.
rendered_options <- function(output) fromJSON(output$metadata_table, simplifyVector = FALSE)$x$options

group_token <- function(group) paste0(impl$AMR_GROUP_TOKEN_PREFIX, group)

test_that("gene columns stay out of the grid until their group is picked", {
  path <- amr_db(local_tempdir())

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    groups <- amr_gene_groups()
    expect_gt(length(groups), 0)
    expect_length(intersect(names(groups), grid_cols()), 0)
    expect_identical(grid_cols(), setdiff(names(State$data), names(groups)))

    group <- unname(groups[1])
    session$setInputs(col_picker = group_token(group))
    expect_setequal(intersect(names(groups), grid_cols()), names(groups)[groups == group])

    # Hiding a loaded group is a visibility toggle: its genes stay in the grid.
    session$setInputs(col_picker = character(0))
    expect_setequal(intersect(names(groups), grid_cols()), names(groups)[groups == group])
  })
})

test_that("an edit's column index is a position in the grid, not in the frame", {
  path <- amr_db(local_tempdir())

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    session$setInputs(col_picker = group_token(unname(amr_gene_groups()[1])))
    age <- custom_col("age")
    expect_false(identical(match(age, grid_cols()), match(age, names(State$data))))

    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = match(age, grid_cols()) - 1L, value = "42")
    )
    session$flushReact()

    expect_equal(State$data[[age]][1], 42)
    expect_identical(nrow(State$custom_dirty), 1L)
    expect_null(State$metadata_dirty)
  })
})

test_that("loading a group keeps unsaved edits and carries sort and filters by name", {
  path <- amr_db(local_tempdir())

  testServer(database_browser$server, args = list(db_path = reactive(path)), {
    session$flushReact()
    age <- custom_col("age")
    before <- grid_cols()
    session$setInputs(
      metadata_table_cell_edit = list(row = 1, col = match(age, before) - 1L, value = "7"),
      metadata_table_state = list(order = list(list(match(age, before) - 1L, "asc"))),
      metadata_table_search_columns = ifelse(before == age, "1 ... 9", "")
    )
    session$flushReact()
    invisible(output$metadata_table)

    session$setInputs(col_picker = group_token(unname(amr_gene_groups()[1])))
    after <- grid_cols()
    expect_gt(length(after), length(before))
    opts <- rendered_options(output)

    # The re-render is not a fresh read: the pending edit survives it.
    expect_true(State$pending)
    expect_equal(State$data[[age]][1], 7)
    # Sort and filter follow the column to its new index.
    expect_identical(opts$order[[1]][[1]], match(age, after) - 1L)
    expect_identical(opts$order[[1]][[2]], "asc")
    expect_identical(opts$searchCols[[match(age, after)]]$search, "1 ... 9")
    expect_identical(opts$searchCols[[1]]$search, "")
  })
})
