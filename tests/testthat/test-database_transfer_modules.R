# Drives the Import and Export module servers through their real reactive
# graphs, rather than calling the logic layer directly.

box::use(
  shiny[testServer, reactive, reactiveVal],
  stats[setNames],
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
  app / logic / mlst_scheme[mlst_scheme_info, store_mlst_scheme],
)

# Picks a file in the app's file dialog `id`, as typing its path does.
pick_file <- function(session, id, path) {
  do.call(session$setInputs, setNames(list(path), paste0(id, "-path")))
}

# Saves through the file dialog `id`: opens it, goes to the folder, names the
# file (the dialog adds the extension) and confirms.
save_to <- function(session, id, path) {
  set <- function(name, value) {
    do.call(session$setInputs, setNames(list(value), paste0(id, "-", name)))
  }
  set("open", as.numeric(Sys.time()))
  set("path", dirname(path))
  set("name", sub("\\.[^.]+$", "", basename(path)))
  set("confirm", as.numeric(Sys.time()))
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
      selected_isolates("A")
      session$setInputs(
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

      save_to(session, "dest", dest)
      expect_identical(as.character(dest_path()), dest)

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

test_that("the export has one destination dialog and no shinyFiles markup", {
  html <- as.character(database_export$ui("x"))
  hits <- gregexpr('id="x-dest-open"', html, fixed = TRUE)[[1]]
  expect_equal(length(hits[hits > 0]), 1L)
  expect_false(grepl("shinyFiles", html, fixed = TRUE))
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
    "format_group"
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
        sequences = "none"
      )
      # The dialog's extension follows the format: a workbook.
      save_to(session, "dest", dest)

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
        meta_cols = character(0)
      )
      save_to(session, "dest", dest)
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

      pick_file(session, "ext_db", peer)

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
      pick_file(session, "ext_db", peer)

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
      pick_file(session, "ext_db", peer)

      expect_identical(clashes(), "B")
      # A clash defaults to skip, so only the new isolate C is accepted.
      expect_equal(accepted(), 1L)

      # Row order follows classification(); B is row 1, C is row 2.
      idx <- which(classification()$isolate == "B")
      pick_file(session, "ext_db", peer)
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
      pick_file(session, "ext_db", peer)
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

test_that("import module refuses merge and restore while typing runs", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(peer, default_peer(), metadata = meta_df(c("B", "C")))
  busy <- reactiveVal(TRUE)

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L), typing_active = busy),
    {
      pick_file(session, "ext_db", peer)
      expect_true(grepl("Typing is writing", output$typing_busy_note$html))

      session$setInputs(confirm_merge = 1)
      expect_equal(session$returned$imported(), 0L)
      expect_false("C" %in% names(isolate_profile_hashes(local)))

      busy(FALSE)
      session$setInputs(confirm_merge = 2)
      expect_equal(session$returned$imported(), 1L)

      busy(TRUE)
      session$flushReact()
      expect_true(grepl("Not available while typing", output$restore_ui$html))
      session$setInputs(backup_pick = backups()[[1]], confirm_restore = 1)
      expect_equal(session$returned$imported(), 1L)
    }
  )
  expect_true("C" %in% names(isolate_profile_hashes(local)))
})

test_that("import module offers the peer's classical MLST scheme and adopts it", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(peer, default_peer(), metadata = meta_df(c("B", "C")))
  store_mlst_scheme(
    peer,
    list(
      loci = c("adk", "gyr"),
      profiles = data.frame(st = 1L, locus = c("adk", "gyr"), allele = 1L),
      alleles = data.frame(locus = c("adk", "gyr"), allele = 1L, sequence = c("ACGT", "TTGA"))
    ),
    list(name = "Testus MLST", version = "1")
  )

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      pick_file(session, "ext_db", peer)
      expect_true(scheme_relation()$adopt_offer)
      expect_true(grepl("Adopt the imported database", output$results_picker_ui$html))

      session$setInputs(adopt_mlst_scheme = TRUE, confirm_merge = 1)
      expect_equal(session$returned$imported(), 1L)
    }
  )
  expect_identical(mlst_scheme_info(local)$name, "Testus MLST")
})

test_that("the loaded database is hashed into a copy once per selection, not per render", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")), with_hashes = FALSE)
  peer_alleles <- default_peer()
  peer_alleles$B[["g2"]] <- seqv("DIVERGED")
  build_db(peer, peer_alleles, metadata = meta_df(c("B", "C")))

  testServer(
    database_import$server,
    args = list(db_path = reactive(local), session_reset = reactive(0L)),
    {
      pick_file(session, "ext_db", peer)
      first <- local_prep()$path
      expect_false(identical(first, local))
      expect_true(nchar(output$body$html) > 0L)

      idx <- which(classification()$isolate == "B")
      do.call(session$setInputs, stats::setNames(list("rename"), paste0("action_", idx)))
      expect_true(nchar(output$body$html) > 0L)
      expect_identical(local_prep()$path, first)

      # A new selection releases the old copy.
      pick_file(session, "ext_db", peer)
      expect_false(file.exists(first))
    }
  )
})
