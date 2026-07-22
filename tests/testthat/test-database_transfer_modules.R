# Drives the Import and Export module servers through their real reactive
# graphs, rather than calling the logic layer directly.

box::use(
  shiny[testServer, reactive, reactiveVal],
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / view / database_export,
  app / view / database_import,
  app / logic / db_import[isolate_profile_hashes],
)

# What shinyFiles' JS sends back for a chosen file / save target.
file_selection <- function(path) {
  list(root = "Root", files = list(`0` = as.list(strsplit(path, "/")[[1]])))
}

save_selection <- function(path) {
  list(
    root = "Root",
    path = as.list(head(strsplit(path, "/")[[1]], -1L)),
    name = basename(path)
  )
}

test_that("export module previews and writes the chosen subset", {
  dir <- local_tempdir()
  src <- file.path(dir, "source.db")
  dest <- file.path(dir, "shared.db")
  build_db(
    src,
    default_local(),
    metadata = meta_df(
      c("A", "B"),
      extra = list(sample_collected_by = c("lab-x", "lab-y"))
    )
  )

  testServer(
    database_export$server,
    args = list(db_path = reactive(src), session_reset = reactive(0L)),
    {
      session$setInputs(
        isolates = "A",
        meta_cols = "sample_collection_date"
      )

      expect_equal(preview()$n_isolates, 1L)
      expect_identical(
        preview()$columns,
        c("isolate", "organism", "sample_collection_date")
      )
      expect_true(nchar(output$summary$html) > 0L)

      # No destination yet, so nothing can be written.
      expect_true(is.null(dest_path()))

      session$setInputs(db = save_selection(dest))
      expect_identical(as.character(dest_path()), dest) # picker path is normalised

      session$setInputs(export_btn = 1)
    }
  )

  expect_true(file.exists(dest))
  expect_setequal(q1(dest, "SELECT DISTINCT souche FROM mlst"), c("ref", "A"))
  expect_identical(
    names(qdf(dest, "SELECT * FROM metadata LIMIT 0")),
    c("isolate", "organism", "sample_collection_date")
  )
  # The withheld column never made it into the shared file.
  expect_false(
    "sample_collected_by" %in%
      names(qdf(dest, "SELECT * FROM metadata LIMIT 0"))
  )
})

test_that("one save button per output extension, each mounted exactly once", {
  html <- as.character(database_export$ui("x"))

  # shinyFiles binds a file dialog to the button *element*. A button re-rendered
  # through renderUI swaps that element out and leaves the old dialog orphaned in
  # the DOM — an empty window behind the real one. A static button per extension
  # keeps the tailored dialog without ever rebinding.
  expect_false(grepl("dest_button_ui", html, fixed = TRUE))

  for (ext in c("db", "xlsx", "tsv", "csv", "zip")) {
    hits <- gregexpr(paste0('id="x-', ext, '"'), html, fixed = TRUE)[[1]]
    expect_equal(length(hits[hits > 0]), 1L, info = ext)
  }

  expect_true(grepl("Export database as", html, fixed = TRUE))
  expect_true(grepl("Export typing results as", html, fixed = TRUE))
})

test_that("the staged controls start hidden and never use conditionalPanel", {
  for (html in list(
    as.character(database_export$ui("x")),
    as.character(database_import$ui("y"))
  )) {
    # These panels are injected into a navset_hidden after the database loads,
    # where Shiny's conditional-panel binding never takes — the panels would just
    # stay visible. Visibility is driven from the server with shinyjs instead.
    expect_false(grepl("data-display-if", html, fixed = TRUE))
  }

  ex <- as.character(database_export$ui("x"))
  # Everything downstream of the export type starts hidden; the server reveals
  # the steps that apply.
  for (id in c(
    "content_group",
    "format_group",
    paste0("slot_", c("db", "xlsx", "tsv", "csv", "zip"))
  )) {
    expect_true(
      grepl(sprintf('shinyjs-hide" id="x-%s"', id), ex, fixed = TRUE),
      info = id
    )
  }

  im <- as.character(database_import$ui("y"))
  for (id in c("typing_group", "set_name_group")) {
    expect_true(
      grepl(sprintf('shinyjs-hide" id="y-%s"', id), im, fixed = TRUE),
      info = id
    )
  }
})

test_that("the deliverable follows what the user asked for", {
  dir <- local_tempdir()
  src <- file.path(dir, "source.db")
  build_db(src, default_local(), metadata = meta_df(c("A", "B")))

  testServer(
    database_export$server,
    args = list(db_path = reactive(src), session_reset = reactive(0L)),
    {
      session$setInputs(
        isolates = c("A", "B"),
        meta_cols = character(0),
        value_kind = "hash",
        export_type = "database",
        file_format = "xlsx",
        sequences = "none"
      )
      expect_identical(target_ext(), "db")

      session$setInputs(export_type = "typing")
      # A workbook holds both tables as sheets, so it stays a single file…
      expect_identical(target_ext(), "xlsx")

      # …but sequences cannot live in a workbook, so they force a bundle.
      session$setInputs(sequences = "fasta")
      expect_identical(target_ext(), "zip")
      expect_true(grepl("alleles.fasta", deliverable_text(), fixed = TRUE))

      # A delimited export needs one file per table, and the metadata table
      # always travels (`isolate` and `organism` are never withheld), so a
      # delimited profile export is always a bundle.
      session$setInputs(sequences = "none", file_format = "tsv")
      expect_identical(target_ext(), "zip")
      expect_identical(
        deliverable_text(),
        "a bundle containing profile.tsv + metadata.tsv"
      )
    }
  )
})

test_that("the typing save button drives a profile export", {
  dir <- local_tempdir()
  src <- file.path(dir, "source.db")
  dest <- file.path(dir, "results.xlsx")
  build_db(src, default_local(), metadata = meta_df(c("A", "B")))

  testServer(
    database_export$server,
    args = list(db_path = reactive(src), session_reset = reactive(0L)),
    {
      session$setInputs(
        export_type = "typing",
        isolates = c("A", "B"),
        meta_cols = "sample_collection_date",
        file_format = "xlsx",
        value_kind = "hash",
        sequences = "none",
        # The workbook button, not the database one.
        xlsx = save_selection(dest)
      )

      expect_identical(as.character(dest_path()), dest)
      expect_false(resolved_target()$bundled)

      session$setInputs(export_btn = 1)
    }
  )

  expect_true(file.exists(dest))
  expect_setequal(openxlsx::getSheetNames(dest), c("profile", "metadata"))
})

test_that("the export tiles describe the isolates, not the scheme reference", {
  dir <- local_tempdir()
  src <- file.path(dir, "source.db")
  build_db(src, default_local(), metadata = meta_df(c("A", "B")))

  testServer(
    database_export$server,
    args = list(db_path = reactive(src), session_reset = reactive(0L)),
    {
      session$setInputs(
        isolates = c("A", "B"),
        meta_cols = character(0),
        file_format = "xlsx",
        value_kind = "hash",
        sequences = "none"
      )

      session$setInputs(export_type = "database")
      db <- preview()
      session$setInputs(export_type = "typing")
      tbl <- preview()

      # The headline figures must not jump when the export type changes: both
      # describe the same isolates. A `.db` additionally carries the scheme
      # reference, which is reported separately rather than inflating the tiles.
      expect_equal(db$n_isolates, tbl$n_isolates)
      expect_equal(db$n_alleles, tbl$n_alleles)
      expect_equal(db$n_calls, tbl$n_calls)

      # The reference contributes one call per locus, plus alleles of its own.
      expect_equal(db$ref_calls, 3L)
      expect_true(db$ref_alleles > 0L)
      expect_null(tbl$ref_calls)
    }
  )
})

test_that("export module writes no metadata table when the source has none", {
  dir <- local_tempdir()
  src <- file.path(dir, "source.db")
  dest <- file.path(dir, "shared.db")
  build_db(src, default_local())

  testServer(
    database_export$server,
    args = list(db_path = reactive(src), session_reset = reactive(0L)),
    {
      session$setInputs(
        isolates = c("A", "B"),
        meta_cols = character(0),
        db = save_selection(dest)
      )
      expect_identical(preview()$columns, character(0))
      session$setInputs(export_btn = 1)
    }
  )

  tables <- qdf(dest, "SELECT name FROM sqlite_master WHERE type='table'")$name
  expect_false("metadata" %in% tables)
})

test_that("import module gates on compatibility and merges on confirm", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(
    peer,
    default_peer(),
    metadata = meta_df(c("B", "C"), extra = list(ward_id = c("W1", "W2")))
  )

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      # Nothing selected: the panel shows its empty state.
      expect_true(grepl("Select a PhyloTrace database", output$body$html))

      session$setInputs(ext_db = file_selection(peer))

      expect_false(blocked())
      expect_true(all(compat()$status == "pass"))

      # B is an identical duplicate (skipped), C is new (added).
      expect_identical(
        classification()$status[classification()$isolate == "B"],
        "identical_duplicate"
      )
      expect_equal(accepted(), 1L)
      expect_identical(clashes(), character(0))
      expect_setequal(
        ext_meta_cols(),
        c("sample_collection_date", "geo_loc_name_country", "ward_id")
      )

      session$setInputs(meta_cols = "ward_id")
      session$setInputs(confirm_merge = 1)

      expect_equal(session$returned$imported(), 1L)
    }
  )

  after <- isolate_profile_hashes(local)
  expect_setequal(names(after), c("A", "B", "C"))
  expect_identical(
    q1(local, "SELECT ward_id FROM metadata WHERE isolate = 'C'"),
    "W2"
  )
  expect_true(is.na(q1(
    local,
    "SELECT ward_id FROM metadata WHERE isolate = 'A'"
  )))
  expect_equal(length(list.files(dir, pattern = "\\.bak-")), 1L)
})

test_that("import module blocks an incompatible database", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), species = "Escherichia coli")

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      session$setInputs(ext_db = file_selection(peer))

      expect_true(blocked())
      expect_true(grepl("cannot be merged", output$body$html))
      expect_equal(session$returned$imported(), 0L)
    }
  )
})

test_that("import module surfaces name clashes and honours rename", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  peer_alleles <- default_peer()
  peer_alleles$B[["g2"]] <- seqv("DIVERGED")
  build_db(peer, peer_alleles, metadata = meta_df(c("B", "C")))

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      session$setInputs(ext_db = file_selection(peer))

      expect_identical(clashes(), "B")
      # A clash defaults to skip, so only the new isolate C is accepted.
      expect_equal(accepted(), 1L)

      # Row order follows classification(); B is row 1, C is row 2.
      idx <- which(classification()$isolate == "B")
      session$setInputs(ext_db = file_selection(peer))
      do.call(
        session$setInputs,
        stats::setNames(list("rename"), paste0("action_", idx))
      )
      do.call(
        session$setInputs,
        stats::setNames(list("B_imp"), paste0("rename_", idx))
      )

      expect_equal(accepted(), 2L)
      expect_identical(resolutions()$final_isolate[idx], "B_imp")

      session$setInputs(confirm_merge = 1)
      expect_equal(session$returned$imported(), 1L)
    }
  )

  expect_setequal(
    names(isolate_profile_hashes(local)),
    c("A", "B", "B_imp", "C")
  )
})

test_that("import module restores a backup", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(peer, default_peer(), metadata = meta_df(c("B", "C")))
  before <- isolate_profile_hashes(local)

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      session$setInputs(ext_db = file_selection(peer))
      session$setInputs(confirm_merge = 1)
      expect_true("C" %in% names(isolate_profile_hashes(local)))

      backup <- backups()
      expect_equal(length(backup), 1L)

      session$setInputs(backup_pick = backup[[1]], confirm_restore = 1)
      expect_equal(session$returned$imported(), 2L)
    }
  )

  expect_identical(isolate_profile_hashes(local), before)
})
