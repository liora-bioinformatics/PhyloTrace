box::use(
  testthat[expect_error, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / amr_overview[load_amr_overview],
  app / logic / cgmlst_overview[load_cgmlst_overview],
  app / logic / db_compat[check_db_loadable],
  app / logic / isolate_overview[load_isolate_overview],
  app / logic / mlst_overview[load_mlst_overview],
  dev / mock_database,
)

SLICES <- setdiff(
  names(mock_database$MOCK_SCENARIOS),
  c("orphan_records", "skipped", "no_classical", "amr_skipped", "no_amr")
)

quiet <- function(expr) suppressMessages(expr)

test_that("a mock database opens like a typed one", {
  path <- file.path(local_tempdir(), "mock.db")
  quiet(mock_database$mock_database(path, n_isolates = 20, n_loci = 15))

  expect_true(check_db_loadable(path)$ok)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(
    c(
      "alembic_version", "mlst_type", "mlst", "sequences", "hashes", "targets",
      "scheme_overview", "metadata", "classical_mlst", "typing_provenance",
      "genome_hashes", "amr_results", "amr_summary"
    ) %in% DBI::dbListTables(con)
  ))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM metadata")$n,
    20L
  )
  expect_identical(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM mlst WHERE souche = 'ref'")$n,
    15L
  )
})

test_that("the same seed writes the same database", {
  dir <- local_tempdir()
  calls <- function(name) {
    path <- file.path(dir, name)
    quiet(mock_database$mock_database(path, n_isolates = 12, n_loci = 10, seed = 7))
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(con, "SELECT isolate, gene, allele, st FROM classical_mlst ORDER BY id")
  }

  expect_identical(calls("a.db"), calls("b.db"))
})

test_that("every slice scenario surfaces in the overviews", {
  path <- file.path(local_tempdir(), "mock.db")
  plan <- quiet(mock_database$mock_database(path, n_isolates = 120, scenarios = SLICES))
  ov <- load_mlst_overview(path)
  checks <- ov$checks$check

  expect_true(all(SLICES %in% c(plan$scenario, plan$record)))
  expect_true(all(
    c(
      "Mixed schemes", "Outdated unresolved calls", "Profile releases",
      "Mixed thresholds", "Mixed pyMLST versions", "Calls without alleles",
      "Failed calls", "No classical record", "Provenance mismatch"
    ) %in% checks
  ))
  expect_true("ambiguous" %in% ov$status$status)

  cg <- load_cgmlst_overview(path)$checks$check
  expect_true(all(
    c(
      "Low completeness", "Duplicate assemblies", "Scheme version changed",
      "Unusual assembly size", "Mixed thresholds", "Fragmented assemblies"
    ) %in% cg
  ))

  amr <- load_amr_overview(path)$checks$check
  expect_true(all(
    c(
      "Results missing", "Failed screens", "Mixed AMRFinderPlus databases",
      "Point mutations partly screened", "No screening record"
    ) %in% amr
  ))

  isolates <- load_isolate_overview(path)$checks$check
  expect_true(all(
    c(
      "Assembly changed since typing", "Duplicate assemblies", "Mixed assembly types",
      "Unusual assembly size", "Fragmented assemblies", "Mixed PhyloTrace versions",
      "No assembly record"
    ) %in% isolates
  ))
})

test_that("whole-database scenarios set the overview state", {
  dir <- local_tempdir()
  skipped <- file.path(dir, "skipped.db")
  none <- file.path(dir, "none.db")
  quiet(mock_database$mock_database(skipped, n_isolates = 10, scenarios = "skipped"))
  quiet(mock_database$mock_database(none, n_isolates = 10, scenarios = "no_classical"))

  expect_identical(load_mlst_overview(skipped)$state, "skipped")
  expect_identical(load_mlst_overview(none)$state, "none")

  amr_off <- file.path(dir, "amr_off.db")
  amr_none <- file.path(dir, "amr_none.db")
  quiet(mock_database$mock_database(amr_off, n_isolates = 10, scenarios = "amr_skipped"))
  quiet(mock_database$mock_database(amr_none, n_isolates = 10, scenarios = "no_amr"))
  expect_identical(load_amr_overview(amr_off)$state, "skipped")
  expect_identical(load_amr_overview(amr_none)$state, "none")
})

test_that("unknown scenarios and existing files are refused", {
  path <- file.path(local_tempdir(), "mock.db")
  expect_error(mock_database$mock_database(path, scenarios = "nope"), "Unknown scenario")

  quiet(mock_database$mock_database(path, n_isolates = 5, n_loci = 5))
  expect_error(mock_database$mock_database(path, n_isolates = 5), "File exists")
  quiet(mock_database$mock_database(path, n_isolates = 6, n_loci = 5, overwrite = TRUE))
  expect_identical(nrow(load_mlst_overview(path)$isolates), 6L)
})
