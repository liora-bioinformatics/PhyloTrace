box::use(
  jsonlite[toJSON],
)
box::use(
  app / logic / sheet_grid,
)

impl <- attr(sheet_grid, "namespace")

.edits <- function(row, column, value) {
  data.frame(row = row, column = column, value = value, stringsAsFactors = FALSE)
}

.grid <- function() {
  data.frame(
    isolate = c("A", "B"),
    host = c("cow", ""),
    custom_n = c(1.5, NA),
    custom_d = c(NA, "2024-01-01"),
    stringsAsFactors = FALSE
  )
}

types <- c(custom_n = "numeric", custom_d = "date")
na_cols <- c("custom_n", "custom_d")

test_that("accepted values are written with each column's R type", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 2, 1), c("host", "custom_n", "custom_d"), c("pig", "12", "2025-02-03")),
    types,
    na_cols = na_cols
  )
  expect_identical(res$data$host, c("pig", ""))
  expect_identical(res$data$custom_n, c(1.5, 12))
  expect_identical(res$data$custom_d, c("2025-02-03", "2024-01-01"))
  expect_identical(nrow(res$changed), 3L)
  expect_identical(nrow(res$rejected), 0L)
})

test_that("invalid values are rejected with a reason and leave the cell alone", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 2), c("custom_n", "custom_d"), c("lots", "01/02/2025")),
    types,
    na_cols = na_cols
  )
  expect_identical(res$data, .grid())
  expect_identical(res$rejected$column, c("custom_n", "custom_d"))
  expect_true(all(nzchar(res$rejected$reason)))
})

test_that("unchanged and empty-to-empty cells are not changes", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 2, 2), c("host", "host", "custom_n"), c("cow", "", "")),
    types,
    na_cols = na_cols
  )
  expect_identical(nrow(res$changed), 0L)
})

test_that("clearing stores NA in NA columns and \"\" elsewhere", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 1), c("host", "custom_n"), c("", "")),
    types,
    na_cols = na_cols
  )
  expect_identical(res$data$host[[1]], "")
  expect_true(is.na(res$data$custom_n[[1]]))
  expect_identical(res$changed$old, c("cow", "1.5"))
})

test_that("later cells win, and unknown rows or columns are ignored", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(
      c(1, 1, 9, NA, 1),
      c("host", "host", "host", "host", "nope"),
      c("a", "b", "c", "d", "e")
    ),
    types,
    na_cols = na_cols
  )
  expect_identical(res$data$host, c("b", ""))
  expect_identical(nrow(res$changed), 1L)
})

test_that("trusted edits skip validation", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(1, "custom_d", "03.02.2020"),
    types,
    na_cols = na_cols,
    trusted = TRUE
  )
  expect_identical(res$data$custom_d[[1]], "03.02.2020")
})

test_that("old numbers are recorded in full, never in scientific notation", {
  expect_identical(impl$.as_text(c(1e6, 12.5, NA)), c("1000000", "12.5", NA))
})

test_that("decode_batch() finds rows by isolate and columns by 0-based index", {
  got <- sheet_grid$decode_batch(
    list(isolate = list("B", "Z"), col = list(1, 2), value = list("x", "y")),
    .grid()
  )
  expect_identical(got$row, c(2L, NA))
  expect_identical(got$column, c("host", "custom_n"))
})

test_that("merge_dirty() keeps one row per cell, newest winning", {
  buffer <- data.frame(isolate = c("A", "B"), column = "x", value = c("1", "2"))
  entries <- data.frame(isolate = c("B", "C"), column = "x", value = c("3", "4"))
  got <- sheet_grid$merge_dirty(buffer, entries, c("isolate", "column"))
  expect_identical(got$isolate, c("A", "B", "C"))
  expect_identical(got$value, c("1", "3", "4"))
  expect_identical(sheet_grid$merge_dirty(NULL, entries, "isolate"), entries)
  expect_identical(sheet_grid$merge_dirty(buffer, NULL, "isolate"), buffer)
})

test_that("edit_report() summarises rejections and read-only skips", {
  none <- data.frame(
    column = character(0),
    value = character(0),
    type = character(0),
    reason = character(0)
  )
  expect_null(sheet_grid$edit_report(none, 0L))
  expect_match(sheet_grid$edit_report(none, 2L), "2 read-only cells were left unchanged")

  one <- data.frame(column = "x", value = "abc", type = "integer", reason = "Nope.")
  expect_identical(
    sheet_grid$edit_report(one, 0L),
    "\"abc\" is not a valid whole number value. Nope."
  )

  html <- as.character(sheet_grid$edit_report(one[rep(1, 5), ], 1L))
  expect_match(html, "5 values were not accepted")
  expect_match(html, "and 2 more")
  expect_match(html, "1 read-only cell was left unchanged")
})

test_that("the history undoes newest first and a new edit drops redo", {
  h <- sheet_grid$new_history()
  step <- function(v) data.frame(isolate = "A", column = "x", value = v)
  sheet_grid$record_step(h, step("1"))
  sheet_grid$record_step(h, step("2"))
  expect_identical(sheet_grid$take_step(h, "undo")$value, "2")

  sheet_grid$record_step(h, step("redo-me"), "undo")
  expect_identical(length(h$redo), 1L)
  sheet_grid$record_step(h, step("3"))
  expect_identical(length(h$redo), 0L)

  sheet_grid$clear_history(h)
  expect_null(sheet_grid$take_step(h, "undo"))
})

test_that("the history keeps at most HISTORY_LIMIT steps", {
  h <- sheet_grid$new_history()
  for (i in seq_len(impl$HISTORY_LIMIT + 5)) {
    sheet_grid$record_step(h, data.frame(isolate = "A", column = "x", value = as.character(i)))
  }
  expect_identical(length(h$undo), impl$HISTORY_LIMIT)
  expect_identical(h$undo[[1]]$value, "6")
})

test_that("sheet_lists() maps column positions to dropdown choices", {
  defs <- data.frame(
    name = c("risk", "ok", "note"),
    type = c("category", "boolean", "text"),
    levels = c('["High","Low"]', NA, NA),
    stringsAsFactors = FALSE
  )
  got <- sheet_grid$sheet_lists(c("isolate", "custom_note", "custom_risk", "custom_ok"), defs)
  expect_identical(got, list(`2` = c("High", "Low"), `3` = c("yes", "no")))
})

test_that("merge_dirty() drops cells put back to their saved value", {
  buffer <- data.frame(isolate = c("A", "B"), column = "x", value = c("1", "2"))
  entries <- data.frame(isolate = c("A", "C"), column = "x", value = c("0", "3"))
  got <- sheet_grid$merge_dirty(buffer, entries, c("isolate", "column"), c(TRUE, FALSE))
  expect_identical(got$isolate, c("B", "C"))
  expect_null(sheet_grid$merge_dirty(buffer[1, ], entries[1, ], "isolate", TRUE))
})

test_that("matches_base() compares edited text with the saved values", {
  got <- sheet_grid$matches_base(
    .grid(),
    c("A", "B", "A", "B", "Z"),
    c("host", "host", "custom_n", "custom_n", "host"),
    c("cow", "x", "1.5", NA, "cow")
  )
  expect_identical(got, c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(sheet_grid$matches_base(NULL, "A", "host", "cow"), FALSE)
})

test_that("pasted text is cleaned silently: breaks, invisibles, padding, NFC", {
  got <- impl$.clean_text(c(
    "Line one\r\nline\ttwo",
    "﻿Ger​many ",
    "  padded  ",
    "Zürich",
    "bell\u0007",
    NA
  ))
  expect_identical(got[1:3], c("Line one line two", "Germany", "padded"))
  expect_identical(got[[4]], "Zürich")
  expect_identical(got[[5]], "bell")
  expect_true(is.na(got[[6]]))
})

test_that("cleaned values are what gets stored and compared", {
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 2), c("host", "host"), c(" cow​ ", "pig\n")),
    types,
    na_cols = na_cols
  )
  # "cow" is unchanged once cleaned; only row 2 changes.
  expect_identical(res$changed$value, "pig")
  expect_identical(res$data$host, c("cow", "pig"))
})

test_that("free text over MAX_CELL_CHARS is rejected, typed values are not capped", {
  long <- strrep("x", impl$MAX_CELL_CHARS + 1L)
  ok <- strrep("x", impl$MAX_CELL_CHARS)
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(c(1, 2), c("host", "host"), c(long, ok)),
    types,
    na_cols = na_cols
  )
  expect_identical(res$data$host, c("cow", ok))
  expect_identical(res$rejected$column, "host")
  expect_true(is.na(res$rejected$type))

  report <- sheet_grid$edit_report(res$rejected)
  expect_match(report, "is too long")
  expect_match(report, "…", fixed = TRUE)
})

test_that("undo/redo restore long or uncleaned legacy values exactly", {
  legacy <- paste0(strrep("x", impl$MAX_CELL_CHARS + 5L), "\t")
  res <- sheet_grid$apply_edits(
    .grid(),
    .edits(1, "host", legacy),
    types,
    na_cols = na_cols,
    trusted = TRUE
  )
  expect_identical(res$data$host[[1]], legacy)
})

# A session standing in for Shiny's, keeping the custom messages it is sent.
.recording_session <- function() {
  sent <- new.env()
  sent$messages <- list()
  list(
    sent = sent,
    sendCustomMessage = function(type, message) {
      sent$messages[[length(sent$messages) + 1L]] <- list(type = type, message = message)
    }
  )
}

test_that("window_options() pages without page controls and counts every row", {
  opts <- sheet_grid$window_options()
  expect_true(opts$paging)
  expect_true(opts$deferRender)
  expect_false(opts$lengthChange)
  expect_identical(opts$pageLength, impl$GRID_WINDOW)
  expect_false(grepl("_START_|_END_", opts$language$info))
})

test_that("patch_cells() sends each cell's stored value at 0-based grid positions", {
  session <- .recording_session()
  df <- .grid()
  df$host[2] <- "a<b & c"
  sheet_grid$patch_cells(session, "grid", df, c(2L, 1L, 2L), c("host", "custom_n", "custom_n"))

  expect_length(session$sent$messages, 1L)
  sent <- session$sent$messages[[1]]
  expect_identical(sent$type, "pt-dt-patch")
  expect_identical(sent$message$id, "grid")
  expect_identical(unlist(sent$message$rows), c(1L, 0L, 1L))
  expect_identical(unlist(sent$message$cols), c(1L, 2L, 2L))
  # Escaped as datatable() escapes the text it sends; numbers stay numbers.
  expect_identical(sent$message$values[[1]], "a&lt;b &amp; c")
  expect_identical(sent$message$values[[2]], 1.5)
  expect_true(is.na(sent$message$values[[3]]))
})

test_that("patch_cells() skips cells off the grid and sends nothing for none", {
  session <- .recording_session()
  sheet_grid$patch_cells(session, "grid", .grid(), c(NA, 9L, 1L), c("host", "host", "gone"))
  expect_length(session$sent$messages, 0L)
})

test_that("replace_rows() sends every column as an array, a single row too", {
  session <- .recording_session()
  sheet_grid$replace_rows(session, "grid", .grid()[1, , drop = FALSE])
  data <- session$sent$messages[[1]]$message$data
  expect_length(data, 4L)
  expect_null(names(data))
  # As Shiny serialises a custom message.
  json <- toJSON(data, auto_unbox = TRUE, na = "null")
  expect_identical(as.character(json), '[["A"],["cow"],[1.5],[null]]')
})
