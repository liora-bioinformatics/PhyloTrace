box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / cgmlst_overview,
  dev / mock_database,
)

# Ten loci; the reference holds all of them.
LOCI <- sprintf("L%02d", 1:10)

# A database's worth of counts and records exercising every check:
#   A-F   complete, consistent
#   G     7 of 10 loci called (low completeness)
#   H     same assembly as A (duplicate)
#   I     typed against scheme version 0.9
#   J     a 50% larger assembly in 400 contigs (unusual size, fragmented)
#   K     other allele-calling thresholds, no provenance for L
scenario <- function() {
  iso <- LETTERS[1:12]
  per_isolate <- data.frame(
    souche = c("ref", iso),
    loci = c(10L, rep(10L, 6), 7L, rep(10L, 5))
  )
  prov <- data.frame(
    isolate = iso[1:11],
    typed_at = "2026-09-01 10:00:00",
    cg_scheme_version = c(rep("1.0", 8), "0.9", "1.0", "1.0"),
    cg_identity = c(rep(0.95, 10), 0.9),
    cg_coverage = 0.9,
    cg_alleles_added = 1L,
    cg_partial_genes = 2L,
    cg_filled_genes = 1L,
    cg_removed_genes = 1L,
    pymlst_version = "2.2.2",
    blat_version = "35",
    mafft_version = "v7.525",
    n_contigs = NA,
    total_length = NA,
    genome_digest = NA,
    stringsAsFactors = FALSE
  )
  hashes <- data.frame(
    isolate = iso,
    genome_digest = c("d1", paste0("d", 2:7), "d1", paste0("d", 9:12)),
    n_contigs = c(rep(40L, 9), 400L, 40L, 40L),
    total_length = c(rep(3e6, 9), 4.5e6, 3e6, 3e6)
  )
  # H shares A's digest; per-isolate figures follow the table above.
  hashes$genome_digest[8] <- "d1"
  cgmlst_overview$build_cg_overview(
    isolates = iso,
    scheme = data.frame(key = c("Version", "Locus Count"), value = c("1.0", "10")),
    targets = data.frame(
      Locus = LOCI,
      Gene = "",
      Start = "1",
      Length = "300",
      Product = "protein",
      Alleles = ""
    ),
    locus_counts = data.frame(
      gene = LOCI,
      alleles = c(5L, rep(2L, 9)),
      calls = c(rep(13L, 3), rep(12L, 7))
    ),
    per_isolate = per_isolate,
    prov = prov,
    hashes = hashes
  )
}

test_that("isolates carry completeness and assembly statistics", {
  iso <- scenario()$isolates
  at <- function(id) iso[iso$isolate == id, ]

  expect_identical(nrow(iso), 12L)
  expect_identical(at("A")$completeness, 100)
  expect_identical(at("G")$completeness, 70)
  expect_identical(at("G")$missing, 3L)
  expect_identical(at("J")$contigs, 400L)
  expect_true(at("G")$low_completeness)
  expect_true(all(iso$duplicate[iso$isolate %in% c("A", "H")]))
  expect_true(at("I")$other_version)
  expect_true(at("J")$odd_size)
  expect_true(at("J")$fragmented)
  expect_true(at("K")$other_thresholds)
  expect_false(at("L")$in_prov)
})

test_that("loci report calls without the reference row", {
  loci <- scenario()$loci

  expect_identical(loci$Locus, LOCI)
  expect_identical(loci$`Isolates Called`, c(rep(12L, 3), rep(11L, 7)))
  expect_identical(loci$Missing, c(rep(0L, 3), rep(1L, 7)))
  expect_identical(loci$`Allele Count`, c(5L, rep(2L, 9)))
  expect_identical(loci$Start, rep(1, 10))
})

test_that("completeness bands and bins count typed isolates", {
  ov <- scenario()

  expect_identical(ov$locus_count, 10L)
  expect_identical(ov$completeness$n, c(11L, 0L, 0L, 1L))
  expect_identical(ov$missing_bins$n[ov$missing_bins$label == "None"], 11L)
  expect_identical(ov$missing_bins$n[ov$missing_bins$label == "1-5"], 1L)
  expect_identical(ov$allele_bins$n[ov$allele_bins$label == "2-5"], 10L)
})

test_that("every inconsistency surfaces as a check, most severe first", {
  checks <- scenario()$checks
  n <- setNames(checks$n, checks$check)

  expect_equal(n[["Low completeness"]], 1L)
  expect_equal(n[["Duplicate assemblies"]], 2L)
  expect_equal(n[["Scheme version changed"]], 1L)
  expect_equal(n[["Unusual assembly size"]], 1L)
  expect_equal(n[["Mixed thresholds"]], 1L)
  expect_equal(n[["Fragmented assemblies"]], 1L)
  expect_equal(n[["No provenance"]], 1L)
  expect_false("Typing consistency" %in% checks$check)
  severity <- c(danger = 1L, warning = 2L, info = 3L, success = 4L)
  expect_false(is.unsorted(severity[checks$tone]))
})

test_that("typing settings group isolates by how they were typed", {
  settings <- scenario()$settings

  expect_identical(sum(settings$isolates), 12L)
  expect_identical(settings$isolates[1], 9L)
  expect_identical(sum(!settings$recorded), 1L)
})

test_that("a consistent database reports a single passing check", {
  ov <- cgmlst_overview$build_cg_overview(
    isolates = c("A", "B"),
    per_isolate = data.frame(souche = c("ref", "A", "B"), loci = c(5L, 5L, 5L)),
    prov = data.frame(isolate = c("A", "B"), cg_identity = 0.95, cg_coverage = 0.9)
  )

  expect_identical(ov$checks$check, "Typing consistency")
  expect_identical(ov$checks$tone, "success")
})

test_that("an empty database yields empty tables, not errors", {
  ov <- cgmlst_overview$build_cg_overview()

  expect_identical(nrow(ov$isolates), 0L)
  expect_identical(nrow(ov$loci), 0L)
  expect_identical(nrow(ov$checks), 0L)
  expect_identical(nrow(ov$settings), 0L)
})

test_that("load_cgmlst_overview reads a mock database end to end", {
  path <- file.path(local_tempdir(), "mock.db")
  suppressMessages(mock_database$mock_database(
    path,
    n_isolates = 30,
    n_loci = 40,
    scenarios = c("low_completeness", "duplicate_assemblies")
  ))
  ov <- cgmlst_overview$load_cgmlst_overview(path)

  expect_identical(ov$locus_count, 40L)
  expect_identical(nrow(ov$loci), 40L)
  expect_identical(sum(ov$isolates$typed), 30L)
  expect_true(all(c("Low completeness", "Duplicate assemblies") %in% ov$checks$check))
})
