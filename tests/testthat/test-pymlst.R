box::use(
  testthat[expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / pymlst,
)

# The exact shape `claMLST search` prints for a genome whose gapA never got
# called: the ST column holds every ST compatible with the six remaining loci.
AMBIGUOUS_STS <- "1953;3684;5029;5510;6853;1384;11;2193;5809;690;3348;1460;7123;7252;6843"

test_that("clamlst_status reports a single ST with a complete profile as known", {
  expect_identical(
    pymlst$clamlst_status("11", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "known"
  )
})

test_that("clamlst_status treats an uncalled locus as partial, not a call", {
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=,infB=3,mdh=1"),
    "partial"
  )
  # An ST candidate list next to a missing locus must never read as "known".
  expect_identical(
    pymlst$clamlst_status(AMBIGUOUS_STS, "gapA=,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "partial"
  )
})

test_that("clamlst_status flags several STs over a complete profile as ambiguous", {
  expect_identical(
    pymlst$clamlst_status("11;12", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "ambiguous"
  )
  # Two alleles at one locus leave the profile undecided as well.
  expect_identical(
    pymlst$clamlst_status("", "gapA=3;5,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "ambiguous"
  )
})

test_that("clamlst_status reports a complete but unregistered profile as novel", {
  expect_identical(
    pymlst$clamlst_status("", "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"),
    "novel"
  )
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=new,infB=3,mdh=1"),
    "novel"
  )
})

test_that("clamlst_status returns NA when nothing was called", {
  expect_identical(
    pymlst$clamlst_status(NA_character_, "gapA=,infB=,mdh="),
    NA_character_
  )
  expect_identical(pymlst$clamlst_status(NA_character_, NA_character_), NA_character_)
})

test_that("store_clamlst_results persists an ST only for an unambiguous call", {
  db <- file.path(local_tempdir(), "db.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE mlst_type (name TEXT)")
  DBI::dbDisconnect(con)

  results <- data.frame(
    strain = c("clean", "gapA_missing"),
    st = c("11", AMBIGUOUS_STS),
    alleles = c(
      "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4",
      "gapA=,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"
    ),
    stringsAsFactors = FALSE
  )
  expect_true(pymlst$store_clamlst_results(db, results))

  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  stored <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT isolate, st, status FROM classical_mlst ORDER BY isolate"
  )

  expect_identical(stored$isolate, c("clean", "gapA_missing"))
  expect_identical(stored$st, c("11", NA_character_))
  expect_identical(stored$status, c("known", "partial"))

  # The uncalled locus is dropped, the six that worked are still recorded.
  n_loci <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM classical_mlst WHERE isolate = 'gapA_missing'"
  )
  expect_identical(n_loci$n, 6L)
  expect_false(any(grepl(";", stored$st, fixed = TRUE), na.rm = TRUE))
})
