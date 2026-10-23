box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / mlst_overview,
)

SCHEME <- "Testus spp. (MLST)"

call_rows <- function(
  isolate,
  alleles,
  st = NA_character_,
  status = "known",
  scheme = SCHEME,
  version = "2026-01-01",
  repository = "pubmlst",
  identity = 0.9,
  coverage = 0.9
) {
  data.frame(
    isolate = isolate,
    gene = names(alleles),
    allele = unname(alleles),
    st = st,
    status = status,
    scheme = scheme,
    scheme_version = version,
    repository = repository,
    identity = identity,
    coverage = coverage,
    pymlst_version = "2.2.2",
    called_at = "2026-01-02 10:00:00",
    stringsAsFactors = FALSE
  )
}

prov_rows <- function(isolate, status, version = "2026-01-01", scheme = SCHEME) {
  data.frame(
    isolate = isolate,
    cla_status = status,
    cla_scheme = scheme,
    cla_scheme_version = version,
    cla_repository = "pubmlst",
    stringsAsFactors = FALSE
  )
}

# A database's worth of records exercising every provenance state:
#   A, B  known ST 1 on two releases of the primary scheme
#   C     novel profile on the older release
#   D     partial profile (locus b missing)
#   E     known ST 9, but on another scheme
#   F     ambiguous in provenance only - no alleles stored
#   G     failed in provenance
#   H     no classical record at all
#   A's provenance names a release its stored calls do not.
scenario <- function() {
  calls <- rbind(
    call_rows("A", c(a = "1", b = "1"), st = "1"),
    call_rows("B", c(a = "1", b = "1"), st = "1", version = "2026-02-01"),
    call_rows("C", c(a = "1", b = "2"), status = "novel"),
    call_rows("D", c(a = "3"), status = "partial", version = "2026-02-01"),
    call_rows("E", c(x = "4", y = "5"), st = "9", scheme = "Testus spp. (MLST Oxford)")
  )
  prov <- rbind(
    prov_rows("A", "known", version = "2026-03-01"),
    prov_rows("B", "known", version = "2026-02-01"),
    prov_rows("F", "ambiguous"),
    prov_rows("G", "failed")
  )
  mlst_overview$build_overview(LETTERS[1:8], calls, prov)
}

test_that("isolates get one merged status, calls taking precedence", {
  ov <- scenario()
  status <- setNames(ov$isolates$status, ov$isolates$isolate)

  expect_identical(
    unname(status[LETTERS[1:8]]),
    c("known", "known", "novel", "partial", "known", "ambiguous", "failed", "untyped")
  )
  expect_identical(ov$state, "ok")
  expect_equal(
    setNames(ov$status$n, ov$status$status)[c("known", "untyped")],
    c(known = 3L, untyped = 1L)
  )
})

test_that("the primary scheme is the most used one, its releases pooled", {
  ov <- scenario()

  expect_identical(ov$primary$scheme, SCHEME)
  expect_identical(ov$primary$versions, c("2026-01-01", "2026-02-01"))
  expect_identical(ov$primary$latest, "2026-02-01")
  # F and G have no calls, but their provenance places them on the scheme too.
  expect_setequal(
    ov$isolates$isolate[ov$isolates$on_primary],
    c("A", "B", "C", "D", "F", "G")
  )
  expect_setequal(ov$isolates$isolate[ov$isolates$older_release], c("A", "C", "F", "G"))
})

test_that("loci report calls and coverage per locus", {
  loci <- scenario()$loci

  expect_identical(loci$Locus, c("a", "b"))
  expect_identical(loci$`Observed Alleles`, c(2L, 2L))
  expect_identical(loci$`Isolates Called`, c(4L, 3L))
  expect_identical(loci$Missing, c(0L, 1L))
  expect_identical(loci$.gene, c("a", "b"))
})

test_that("STs are counted on the primary scheme only, with their profile", {
  ov <- scenario()

  expect_identical(ov$sts$ST, 1L)
  expect_identical(ov$sts$Isolates, 2L)
  expect_identical(ov$sts$a, "1")
  expect_identical(ov$sts$b, "1")
})

test_that("provenance checks flag every inconsistency, most severe first", {
  checks <- scenario()$checks
  n <- setNames(checks$n, checks$check)

  expect_identical(checks$check[1], "Mixed schemes")
  expect_identical(checks$tone[1], "danger")
  expect_equal(n[["Mixed schemes"]], 1L)
  expect_equal(n[["Outdated unresolved calls"]], 2L) # C (novel), F (ambiguous)
  expect_equal(n[["Calls without alleles"]], 1L)
  expect_equal(n[["Provenance mismatch"]], 1L)
  expect_equal(n[["Failed calls"]], 1L)
  expect_equal(n[["No classical record"]], 1L)
  severity <- c(danger = 1L, warning = 2L, info = 3L, success = 4L)
  expect_false(is.unsorted(severity[checks$tone]))
})

test_that("a consistent database reports a single passing scheme check", {
  calls <- rbind(
    call_rows("A", c(a = "1", b = "1"), st = "1"),
    call_rows("B", c(a = "2", b = "1"), st = "2")
  )
  ov <- mlst_overview$build_overview(c("A", "B"), calls)

  expect_identical(ov$checks$check, "Scheme consistency")
  expect_identical(ov$checks$tone, "success")
})

test_that("calls searched at minority thresholds are flagged", {
  calls <- rbind(
    call_rows("A", c(a = "1", b = "1"), st = "1"),
    call_rows("B", c(a = "1", b = "1"), st = "1"),
    call_rows("C", c(a = "1", b = "1"), st = "1", identity = 0.8)
  )
  ov <- mlst_overview$build_overview(c("A", "B", "C"), calls)

  expect_identical(ov$isolates$isolate[ov$isolates$other_thresholds], "C")
  check <- ov$checks[ov$checks$check == "Mixed thresholds", ]
  expect_identical(check$n, 1L)
  expect_true(grepl("identity 0.8 / coverage 0.9 (1)", check$detail, fixed = TRUE))
})

test_that("a database without classical records reports why", {
  skipped <- mlst_overview$build_overview(
    c("A", "B"),
    NULL,
    prov_rows(c("A", "B"), "skipped", scheme = NA_character_)
  )
  expect_identical(skipped$state, "skipped")
  expect_null(skipped$primary)
  expect_identical(nrow(skipped$loci), 0L)

  empty <- mlst_overview$build_overview(c("A", "B"))
  expect_identical(empty$state, "none")
  expect_identical(empty$status$status, "untyped")
})

test_that("simpson_diversity is the Hunter-Gaston index", {
  expect_equal(mlst_overview$simpson_diversity(c(1, 1)), 1)
  expect_equal(mlst_overview$simpson_diversity(5), 0)
  expect_equal(mlst_overview$simpson_diversity(c(2, 2)), 1 - 4 / 12)
  expect_identical(mlst_overview$simpson_diversity(1), NA_real_)
})

test_that("locus_alleles counts allele usage at one locus", {
  ov <- scenario()
  alleles <- mlst_overview$locus_alleles(ov$calls, "a")

  expect_identical(alleles$allele, c("1", "3"))
  expect_identical(alleles$count, c(3L, 1L))
  expect_equal(alleles$share, c(75, 25))
})

test_that("load_mlst_overview reads a database end to end", {
  db <- file.path(local_tempdir(), "db.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbWriteTable(con, "metadata", data.frame(isolate = c("A", "B", "H")))
  calls <- rbind(
    call_rows("A", c(a = "1", b = "1"), st = "1"),
    call_rows("B", c(a = "2", b = "1"), status = "novel")
  )
  calls$sequence <- c("ACGT", "GGCC", "TTAA", "GGCC")
  DBI::dbWriteTable(con, "classical_mlst", calls)
  DBI::dbDisconnect(con)

  ov <- mlst_overview$load_mlst_overview(db)

  expect_identical(ov$state, "ok")
  expect_setequal(ov$isolates$isolate, c("A", "B", "H"))
  expect_identical(ov$isolates$status[ov$isolates$isolate == "H"], "untyped")
  expect_identical(ov$loci$`Observed Alleles`, c(2L, 1L))
  expect_identical(
    mlst_overview$load_cla_allele_sequence(db, "a", "2", SCHEME, "pubmlst"),
    "TTAA"
  )
  expect_null(
    mlst_overview$load_cla_allele_sequence(db, "a", "2", "Other scheme", "pubmlst")
  )
  expect_identical(
    mlst_overview$cla_locus_fasta(db, "a", SCHEME, "pubmlst"),
    c(">a_1\nACGT", ">a_2\nTTAA")
  )
})
