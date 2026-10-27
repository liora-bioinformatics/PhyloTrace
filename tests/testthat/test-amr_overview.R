box::use(
  testthat[expect_equal, expect_false, expect_identical, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / amr_overview,
  dev / mock_database,
)

hit_rows <- function(
  isolate,
  gene,
  type = "AMR",
  subtype = "AMR",
  class = "BETA-LACTAM",
  method = "EXACTX",
  db = "2024-07-22.1",
  pm = 1L
) {
  data.frame(
    isolate = isolate,
    gene_symbol = gene,
    sequence_name = paste("protein", gene),
    element_type = type,
    element_subtype = subtype,
    class = class,
    subclass = class,
    method = method,
    pct_coverage = 100,
    pct_identity = 99.5,
    contig = "c1",
    start = 10L,
    stop = 900L,
    strand = "+",
    ref_accession = "WP_1.1",
    ref_name = paste("ref", gene),
    organism = if (pm == 1L) "Testus" else NA,
    point_mutations = pm,
    abritamr_version = "1.0.19",
    amrfinder_version = "3.12.8",
    amrfinder_db_version = db,
    called_at = "2026-09-30 10:00:00",
    stringsAsFactors = FALSE
  )
}

prov_rows <- function(isolate, status, elements = NA_integer_) {
  data.frame(
    isolate = isolate,
    amr_status = status,
    amr_organism = "Testus",
    amr_point_mutations = 1L,
    amr_elements = elements,
    amr_abritamr_version = "1.0.19",
    amr_amrfinder_version = "3.12.8",
    amr_amrfinder_db_version = "2024-07-22.1",
    stringsAsFactors = FALSE
  )
}

# A database's worth of screens exercising every outcome and check:
#   A, B  resistance in two classes plus virulence and stress
#   C     one partial resistance gene, screened against an older database
#   D     screened clean (done, no elements)
#   E     screen failed
#   F     no screening record at all
#   G     screened without point mutations
#   H     provenance reports 3 elements, 1 stored
scenario <- function() {
  results <- rbind(
    hit_rows("A", "blaTEM-1"),
    hit_rows("A", "gyrA_S83L", subtype = "POINT", class = "QUINOLONE", method = "POINTX"),
    hit_rows("A", "fimH", type = "VIRULENCE", subtype = "VIRULENCE", class = NA),
    hit_rows("A", "merA", type = "STRESS", subtype = "METAL", class = "MERCURY"),
    hit_rows("B", "blaTEM-1", method = "BLASTX"),
    hit_rows("B", "tet(A)", class = "TETRACYCLINE"),
    hit_rows("B", "clpK", type = "STRESS", subtype = "HEAT", class = NA),
    hit_rows("C", "blaTEM-1", method = "PARTIAL_CONTIG_ENDX", db = "2023-11-15.1"),
    hit_rows("G", "tet(A)", class = "TETRACYCLINE", pm = 0L),
    hit_rows("H", "blaTEM-1")
  )
  prov <- rbind(
    prov_rows(c("A", "B", "C", "G"), "done"),
    prov_rows("D", "done", 0L),
    prov_rows("E", "failed"),
    prov_rows("H", "done", 3L)
  )
  amr_overview$build_amr_overview(LETTERS[1:8], results, prov)
}

test_that("isolates get one screening outcome and counts per element type", {
  iso <- scenario()$isolates
  at <- function(id) iso[iso$isolate == id, ]

  expect_identical(
    iso$status[match(LETTERS[1:8], iso$isolate)],
    c("found", "found", "found", "clean", "failed", "unscreened", "found", "found")
  )
  expect_identical(at("A")$acquired, 1L)
  expect_identical(at("A")$point, 1L)
  expect_identical(at("A")$virulence, 1L)
  expect_identical(at("A")$stress, 1L)
  expect_identical(at("A")$classes, 2L)
  expect_identical(at("C")$partial, 1L)
  expect_true(at("C")$other_db)
  expect_false(at("G")$point_mutations)
  expect_true(at("H")$lost)
})

test_that("elements count each isolate once, at its best call", {
  el <- scenario()$elements
  tem <- el[el$Element == "blaTEM-1", ]

  expect_identical(tem$Isolates, 4L)
  expect_identical(tem$Perfect, 2L)
  expect_identical(tem$Strong, 1L)
  expect_identical(tem$Partial, 1L)
  expect_identical(tem$Class, "Beta-lactam")
  expect_equal(tem$`Prevalence (%)`, round(100 * 4 / 6, 1))
  expect_identical(el$Type[1], "Resistance")
})

test_that("types group by drug class, stress target and virulence factor", {
  ov <- scenario()

  expect_identical(ov$groups$Resistance$group[1], "Beta-lactam")
  expect_true(all(c("Mercury", "Heat") %in% ov$groups$Stress$group))
  expect_identical(ov$groups$Virulence$group, "fimH")
})

test_that("profiles cover every screened isolate, none included", {
  ov <- scenario()
  screened <- ov$isolates[ov$isolates$screened, ]
  prof <- amr_overview$profile_table("Resistance", ov$hits, screened)

  expect_identical(sum(prof$Isolates), 6L)
  expect_true("(none)" %in% prof$Profile)
  expect_identical(prof$Isolates[prof$Profile == "blaTEM-1"], 2L)
  expect_identical(
    sum(scenario()$counts$Virulence$n),
    6L
  )
})

test_that("every screening problem surfaces as a check, most severe first", {
  checks <- scenario()$checks
  n <- setNames(checks$n, checks$check)

  expect_identical(checks$check[1], "Results missing")
  expect_equal(n[["Results missing"]], 1L)
  expect_equal(n[["Failed screens"]], 1L)
  expect_equal(n[["Mixed AMRFinderPlus databases"]], 1L)
  expect_equal(n[["Point mutations partly screened"]], 1L)
  expect_equal(n[["Partial elements"]], 1L)
  expect_equal(n[["No screening record"]], 1L)
  severity <- c(danger = 1L, warning = 2L, info = 3L, success = 4L)
  expect_false(is.unsorted(severity[checks$tone]))
})

test_that("a database without screens reports why", {
  skipped <- amr_overview$build_amr_overview(c("A", "B"), NULL, prov_rows(c("A", "B"), "skipped"))
  expect_identical(skipped$state, "skipped")
  expect_identical(nrow(skipped$elements), 0L)

  none <- amr_overview$build_amr_overview(c("A", "B"))
  expect_identical(none$state, "none")
  expect_identical(none$status$status, "unscreened")
})

test_that("element_carriers lists each carrier once, best call first", {
  ov <- scenario()
  carriers <- amr_overview$element_carriers(ov$hits, "blaTEM-1", "Resistance")

  expect_identical(nrow(carriers), 4L)
  expect_identical(carriers$Call, c("Perfect", "Perfect", "Strong", "Partial"))
  expect_identical(carriers$Isolate[4], "C")
  expect_identical(carriers$Hits, rep(1L, 4))
})

test_that("classes spelled with underscores group with their spaced twin", {
  results <- rbind(
    hit_rows("A", "fusB", class = "FUSIDIC_ACID"),
    hit_rows("B", "fusC", class = "FUSIDIC ACID")
  )
  ov <- amr_overview$build_amr_overview(c("A", "B"), results)

  expect_identical(ov$groups$Resistance$group, "Fusidic acid")
  expect_identical(ov$groups$Resistance$isolates, 2L)
})

test_that("load_amr_overview reads a mock database end to end", {
  path <- file.path(local_tempdir(), "mock.db")
  suppressMessages(mock_database$mock_database(
    path,
    n_isolates = 40,
    n_loci = 20,
    scenarios = c("amr_failed", "amr_mixed_db")
  ))
  ov <- amr_overview$load_amr_overview(path)

  expect_identical(ov$state, "ok")
  expect_true(nrow(ov$elements) > 0)
  expect_true(all(c("Failed screens", "Mixed AMRFinderPlus databases") %in% ov$checks$check))

  # Where a call sits is read on demand, for one carrier at a time.
  top <- ov$elements[1, ]
  carrier <- amr_overview$element_carriers(ov$hits, top$Element, top$Type)$Isolate[1]
  calls <- amr_overview$load_element_calls(path, carrier, top$Element, top$Type)
  expect_true(nrow(calls) >= 1)
  expect_true(all(c("contig", "start", "stop", "strand", "ref_accession") %in% names(calls)))
  expect_identical(unique(calls$isolate), carrier)

  # The abritamr source reads the same screen's summary, with the same outcome.
  rollup <- amr_overview$load_amr_overview(path, "abritamr")
  expect_true(nrow(rollup$elements) > 0)
  expect_true(all(c("Match", "Inexact", "Partial") %in% names(rollup$elements)))
  expect_identical(rollup$isolates$status, ov$isolates$status)
  expect_identical(rollup$checks, ov$checks)
})

test_that("the abritamr source reads elements, classes and calls off its summary", {
  results <- rbind(
    hit_rows("A", "blaOXA", method = "BLASTX"),
    hit_rows("A", "aac(3)-Ia", method = "PARTIALX", class = "AMINOGLYCOSIDE"),
    hit_rows("B", "blaOXA-23", method = "EXACTX"),
    hit_rows("B", "merA", type = "STRESS", subtype = "METAL", class = "MERCURY"),
    hit_rows("B", "fimH", type = "VIRULENCE", subtype = "VIRULENCE", class = NA)
  )
  # A's family hit is named after its closest allele and flagged inexact.
  summary <- data.frame(
    isolate = c("A", "A", "B", "B", "B"),
    section = c("matches", "partials", "matches", "virulence", "virulence"),
    drug_class = c("Carbapenemase", "Gentamicin", "Carbapenemase", "Metal", "Virulence"),
    genes = c("blaOXA-23*", "aac(3)-Ia^", "blaOXA-23", "merA", "fimH"),
    stringsAsFactors = FALSE
  )
  ov <- amr_overview$build_amr_overview(
    c("A", "B", "C"),
    results,
    prov_rows(c("A", "B", "C"), "done"),
    summary = summary,
    source = "abritamr"
  )

  el <- ov$elements
  oxa <- el[el$Element == "blaOXA-23", ]
  expect_identical(oxa$Isolates, 2L)
  expect_identical(c(oxa$Match, oxa$Inexact, oxa$Partial), c(1L, 1L, 0L))
  expect_identical(oxa$Class, "Carbapenemase")
  expect_false(any(c("Identity (%)", "Perfect", "Subclass") %in% names(el)))

  expect_identical(ov$groups$Resistance$group, c("Carbapenemase", "Gentamicin"))
  expect_identical(ov$groups$Stress$group, "Metal")
  expect_identical(ov$groups$Virulence$group, "fimH")

  iso <- ov$isolates[match(c("A", "B", "C"), ov$isolates$isolate), ]
  expect_identical(iso$status, c("found", "found", "clean"))
  expect_identical(iso$inexact, c(1L, 0L, 0L))
  expect_identical(iso$partial, c(1L, 0L, 0L))
  expect_identical(iso$classes, c(2L, 1L, 0L))

  # The same screen read from AMRFinderPlus keeps the family symbol.
  afp <- amr_overview$build_amr_overview(c("A", "B", "C"), results, summary = summary)
  expect_true("blaOXA" %in% afp$elements$Element)
  expect_identical(afp$groups$Resistance$group[1], "Beta-lactam")
})

test_that("group_carriers counts overlapping groups' isolates once", {
  results <- rbind(
    hit_rows("A", "blaTEM-1"),
    hit_rows("A", "tet(A)", class = "TETRACYCLINE"),
    hit_rows("B", "tet(A)", class = "TETRACYCLINE")
  )
  ov <- amr_overview$build_amr_overview(c("A", "B"), results)

  expect_identical(
    amr_overview$group_carriers(ov$hits, "Resistance", c("Beta-lactam", "Tetracycline")),
    2L
  )
  expect_identical(amr_overview$group_carriers(ov$hits, "Stress", "Mercury"), 0L)
})
