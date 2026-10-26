box::use(
  DBI[dbConnect, dbDisconnect, dbExecute, dbWriteTable],
  RSQLite[SQLite],
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_match,
    expect_setequal,
    expect_true,
    test_that,
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / isolate_overview,
  dev / mock_database,
)

impl <- attr(isolate_overview, "namespace")

quiet <- function(expr) suppressMessages(expr)

# Assembly records of a small collection:
#   A-F   ordinary short-read drafts around 3 Mb
#   G     shares F's assembly (duplicate)
#   H     a 50% larger assembly in 400 contigs (unusual size, fragmented)
#   I     a closed long-read assembly in 2 contigs
#   J     its assembly record replaced after typing
#   K     hashed with another algorithm
#   L     no assembly record at all
hash_rows <- function() {
  data.frame(
    isolate = c(LETTERS[1:11], "GONE"),
    genome_digest = c("dA", "dB", "dC", "dD", "dE", "dF", "dF", "dH", "dI", "dJ-new", "dK", "dX"),
    file_sha256 = paste0("sha", 1:12),
    algorithm = c(rep("ga4gh-sorted-sequences-v1", 10), "legacy", "ga4gh-sorted-sequences-v1"),
    n_contigs = c(50, 55, 60, 45, 52, 58, 58, 400, 2, 50, 51, 40),
    total_length = c(3e6, 3.02e6, 2.98e6, 3.01e6, 2.99e6, 3e6, 3e6, 4.5e6, 3.05e6, 3e6, 3e6, 3e6),
    file_bytes = 3.05e6,
    hashed_at = "2026-09-30 10:00:00",
    stringsAsFactors = FALSE
  )
}

prov_rows <- function() {
  data.frame(
    isolate = LETTERS[1:12],
    run_id = c(rep("run_1.log", 6), rep("run_2.log", 5), NA),
    typed_at = c(rep("2026-09-01 10:00:00", 6), rep("2026-09-30 10:00:00", 5), NA),
    elapsed_seconds = c(40, 42, 45, 50, 38, 41, 44, 120, 30, 43, 47, NA),
    phylotrace_version = c(rep("1.6.1", 10), "1.5.0", NA),
    genome_digest = c("dA", "dB", "dC", "dD", "dE", "dF", "dF", "dH", "dI", "dJ-old", "dK", NA),
    algorithm = c(rep("ga4gh-sorted-sequences-v1", 10), "legacy", NA),
    n_contigs = NA,
    total_length = NA,
    stringsAsFactors = FALSE
  )
}

meta_rows <- function() {
  data.frame(
    isolate = LETTERS[1:12],
    source = c(rep("local", 9), "peer-lab", NA, ""),
    called_at = "2026-10-01 10:00:00",
    stringsAsFactors = FALSE
  )
}

fixture <- function() {
  isolate_overview$build_isolate_overview(
    isolates = LETTERS[1:12],
    meta = meta_rows(),
    hashes = hash_rows(),
    prov = prov_rows()
  )
}

test_that("an empty database yields empty summaries and no checks", {
  ov <- isolate_overview$build_isolate_overview()

  expect_identical(ov$state, "none")
  expect_identical(nrow(ov$isolates), 0L)
  expect_identical(nrow(ov$runs), 0L)
  expect_identical(nrow(ov$environments), 0L)
  expect_identical(nrow(ov$contiguity), 0L)
  expect_identical(nrow(ov$origin), 0L)
  expect_identical(nrow(ov$timeline), 0L)
  expect_identical(nrow(ov$checks), 0L)
  expect_identical(sum(ov$contig_bins$n), 0L)
  expect_true(is.na(ov$median_length))
})

test_that("isolates without any record are counted and flagged as such", {
  ov <- isolate_overview$build_isolate_overview(isolates = c("A", "B", NA, "A"))
  iso <- ov$isolates

  expect_identical(iso$isolate, c("A", "B"))
  expect_true(all(iso$no_assembly))
  expect_identical(iso$source, c("local", "local"))
  expect_identical(ov$runs$Run, "Not recorded")
  expect_identical(ov$contiguity$key, "unknown")
  expect_identical(ov$checks$check, "No assembly record")
  expect_identical(isolate_overview$isolate_flags(iso), rep("no assembly record", 2))
})

test_that("assembly records merge per isolate, the hash table first", {
  iso <- fixture()$isolates
  at <- function(x) iso[iso$isolate == x, , drop = FALSE]

  expect_identical(at("A")$contigs, 50)
  expect_identical(at("A")$mean_contig, 60000)
  expect_identical(at("I")$contiguity, "complete")
  expect_identical(at("H")$contiguity, "fragmented")
  expect_true(at("L")$no_assembly)
  expect_true(is.na(at("L")$contiguity))
  expect_true(at("J")$imported)
  expect_identical(at("J")$source, "peer-lab")
  expect_false(at("K")$imported)
  expect_false(at("L")$imported)
  # No provenance timestamp: the hash stamp, then the metadata one stand in.
  expect_identical(at("L")$typed_at, "2026-10-01 10:00:00")
})

test_that("record problems are flagged on the isolates they concern", {
  iso <- fixture()$isolates
  flagged <- function(col) iso$isolate[iso[[col]]]

  expect_identical(flagged("changed"), "J")
  expect_identical(flagged("duplicate"), c("F", "G"))
  expect_identical(iso$twins[iso$isolate == "F"], "G")
  expect_identical(flagged("odd_size"), "H")
  expect_identical(flagged("fragmented"), "H")
  expect_identical(flagged("other_algorithm"), "K")
  expect_identical(flagged("no_assembly"), "L")
  expect_match(isolate_overview$isolate_flags(iso)[iso$isolate == "H"], "unusual size; fragmented")
})

test_that("digests of different algorithms are neither duplicates nor changes", {
  hashes <- hash_rows()
  hashes$genome_digest[hashes$isolate == "K"] <- "dA"
  prov <- prov_rows()
  prov$genome_digest[prov$isolate == "K"] <- "other"
  prov$algorithm[prov$isolate == "K"] <- "ga4gh-sorted-sequences-v1"
  iso <- isolate_overview$build_isolate_overview(LETTERS[1:12], NULL, hashes, prov)$isolates

  expect_false(iso$duplicate[iso$isolate == "K"])
  expect_false(iso$duplicate[iso$isolate == "A"])
  expect_false(iso$changed[iso$isolate == "K"])
})

test_that("checks cover every record problem, most severe first", {
  checks <- fixture()$checks

  expect_identical(checks$check[1], "Assembly changed since typing")
  expect_identical(checks$tone[1], "danger")
  expect_true(all(
    c(
      "Duplicate assemblies", "Mixed assembly types", "Unusual assembly size",
      "Mixed digest algorithms", "Fragmented assemblies", "Mixed PhyloTrace versions",
      "No assembly record", "Orphaned records"
    ) %in% checks$check
  ))
  expect_false("Record consistency" %in% checks$check)
  expect_identical(checks$n[checks$check == "Mixed assembly types"], 1L)
  expect_identical(checks$n[checks$check == "Orphaned records"], 1L)
})

test_that("a clean collection gets the consistency check alone", {
  hashes <- hash_rows()[1:6, ]
  ov <- isolate_overview$build_isolate_overview(LETTERS[1:6], NULL, hashes, prov_rows()[1:6, ])

  expect_identical(ov$checks$check, "Record consistency")
  expect_identical(ov$checks$tone, "success")
})

test_that("records of removed isolates are reported, not shown", {
  ov <- fixture()

  expect_identical(ov$orphans$hashes, "GONE")
  expect_identical(ov$orphans$provenance, character(0))
  expect_false("GONE" %in% ov$isolates$isolate)
})

test_that("runs are listed chronologically with the unrecorded last", {
  runs <- fixture()$runs

  expect_identical(runs$Run, c("run_1.log", "run_2.log", "Not recorded"))
  expect_identical(runs$Isolates, c(6L, 5L, 1L))
  expect_identical(runs$Typed[1], "2026-09-01")
  expect_identical(runs$Flagged, c(1L, 4L, 1L))
  expect_identical(runs$PhyloTrace[2], "1.5.0, 1.6.1")
  expect_match(runs$Source[2], "peer-lab", fixed = TRUE)
})

test_that("environments group by PhyloTrace version and digest algorithm", {
  env <- fixture()$environments

  expect_identical(env$isolates[1], 10L)
  expect_identical(env$phylotrace[1], "1.6.1")
  expect_identical(sum(env$isolates), 12L)
  expect_true(any(is.na(env$phylotrace)))
})

test_that("distributions count every recorded assembly", {
  ov <- fixture()

  expect_identical(sum(ov$contig_bins$n), 11L)
  expect_identical(ov$contig_bins$n[ov$contig_bins$label == "2-10"], 1L)
  expect_identical(sum(ov$size_bins$n), 11L)
  expect_identical(ov$size_bins$n[ov$size_bins$label == "Over 20% larger"], 1L)
  expect_identical(ov$origin$label, c("Typed here", "peer-lab"))
  expect_identical(ov$contiguity$label, c("Near-complete", "Draft", "Fragmented", "Not recorded"))
})

test_that("unparsable or impossible figures read as unknown", {
  hashes <- hash_rows()[1:3, ]
  hashes$n_contigs <- c("abc", "0", "-4")
  hashes$total_length <- c("", "0", "3e6")
  hashes$hashed_at <- c("garbage", NA, "2026-13-45")
  prov <- prov_rows()[1:3, ]
  prov$typed_at <- NA
  prov$elapsed_seconds <- c("x", "-3", "12")
  iso <- isolate_overview$build_isolate_overview(LETTERS[1:3], NULL, hashes, prov)$isolates

  expect_true(all(is.na(iso$contigs)))
  expect_identical(iso$length, c(NA, NA, 3e6))
  expect_true(all(is.na(iso$mean_contig)))
  expect_true(all(is.na(iso$typed_date)))
  expect_identical(iso$elapsed, c(NA, NA, 12))
})

test_that("contiguity classes have inclusive upper bounds", {
  expect_identical(
    isolate_overview$contiguity_class(c(1, 10, 11, 200, 201, 500, 501, 0, NA)),
    c(
      "complete", "complete", "draft", "draft", "fragmented", "fragmented",
      "highly_fragmented", NA, NA
    )
  )
})

test_that("the timeline keeps gaps and coarsens over long spans", {
  monthly <- impl$timeline_counts(as.Date(c("2026-01-15", "2026-03-02", "2026-03-20")))
  expect_identical(monthly$label, c("2026-01", "2026-02", "2026-03"))
  expect_identical(monthly$n, c(1L, 0L, 2L))

  quarterly <- impl$timeline_counts(as.Date(c("2025-06-11", "2026-09-30")))
  expect_identical(quarterly$label[c(1, 6)], c("2025 Q2", "2026 Q3"))
  expect_identical(sum(quarterly$n), 2L)

  yearly <- impl$timeline_counts(as.Date(c("2019-02-01", "2026-09-30", NA)))
  expect_identical(yearly$label, as.character(2019:2026))
})

test_that("one isolate's checks explain its flags", {
  ov <- fixture()
  iso <- ov$isolates
  checks <- isolate_overview$one_isolate_checks(iso, which(iso$isolate == "H"))

  expect_identical(checks$check, c("Unusual assembly size", "Fragmented assembly"))
  expect_match(checks$detail[1], "+50.0%", fixed = TRUE)
  expect_identical(nrow(isolate_overview$one_isolate_checks(iso, which(iso$isolate == "A"))), 0L)
  expect_identical(
    isolate_overview$one_isolate_checks(iso, which(iso$isolate == "J"))$tone,
    "danger"
  )
})

test_that("durations and lengths format for reading", {
  expect_identical(
    isolate_overview$format_duration(c(42, 192, 3840, NA)),
    c("42 s", "3 min 12 s", "1 h 4 min", NA)
  )
  expect_identical(
    isolate_overview$format_bp(c(3.1e6, 52300, 640, NA)),
    c("3.10 Mb", "52.3 kb", "640 bp", NA)
  )
})

test_that("a database without record tables or their newer columns still loads", {
  path <- file.path(local_tempdir(), "old.db")
  con <- dbConnect(SQLite(), path)
  dbWriteTable(con, "metadata", data.frame(isolate = c("A", "B", "ref")))
  dbExecute(con, "CREATE TABLE genome_hashes (isolate TEXT, genome_digest TEXT)")
  dbExecute(con, "INSERT INTO genome_hashes VALUES ('A', 'dA')")
  dbDisconnect(con)

  ov <- isolate_overview$load_isolate_overview(path)

  expect_identical(ov$isolates$isolate, c("A", "B"))
  expect_identical(ov$isolates$no_assembly, c(FALSE, TRUE))
  expect_identical(ov$isolates$source, c("local", "local"))
  expect_true(is.na(ov$isolates$contigs[1]))
})

test_that("every isolate scenario of the mock database surfaces", {
  path <- file.path(local_tempdir(), "mock.db")
  scenarios <- c(
    "assembly_changed", "long_read_assemblies", "no_assembly_record", "imported",
    "orphan_records", "duplicate_assemblies", "unusual_assemblies"
  )
  plan <- quiet(mock_database$mock_database(path, n_isolates = 80, scenarios = scenarios))
  ov <- isolate_overview$load_isolate_overview(path)
  iso <- ov$isolates

  expect_identical(nrow(iso), 80L)
  expect_true(all(
    c(
      "Assembly changed since typing", "Duplicate assemblies", "Mixed assembly types",
      "Unusual assembly size", "Fragmented assemblies", "Mixed PhyloTrace versions",
      "No assembly record", "Orphaned records"
    ) %in% ov$checks$check
  ))
  expect_setequal(iso$isolate[iso$changed], plan$isolate[plan$record %in% "assembly_changed"])
  expect_setequal(iso$isolate[iso$no_assembly], plan$isolate[plan$record %in% "no_assembly_record"])
  expect_setequal(iso$isolate[iso$imported], plan$isolate[plan$record %in% "imported"])
  long_read <- plan$isolate[plan$record %in% "long_read_assemblies"]
  expect_true(all(iso$contiguity[match(long_read, iso$isolate)] == "complete"))
  expect_setequal(ov$orphans$provenance, c("MOCK_GONE_1", "MOCK_GONE_2"))
  expect_true("typing_20250611_090000_partner.log" %in% ov$runs$Run)
})

test_that("a clean mock database passes every record check", {
  path <- file.path(local_tempdir(), "mock.db")
  quiet(mock_database$mock_database(path, n_isolates = 12, n_loci = 10))
  ov <- isolate_overview$load_isolate_overview(path)

  expect_identical(ov$checks$check, "Record consistency")
  expect_false(any(nzchar(isolate_overview$isolate_flags(ov$isolates))))
  expect_equal(sum(ov$contiguity$n), 12L)
})

test_that("analysis outcomes merge stored results with recorded steps", {
  prov <- data.frame(
    isolate = c("A", "B", "C", "D", "E"),
    cla_status = c("known", "failed", "known", "skipped", NA),
    amr_status = c("done", "incomplete", "done", "skipped", NA),
    stringsAsFactors = FALSE
  )
  iso <- isolate_overview$build_isolate_overview(
    isolates = c("A", "B", "C", "D", "E", "F"),
    prov = prov,
    profiled = c("A", "B", "C", "D", "F"),
    classical = c("A", "F"),
    screened = "B"
  )$isolates

  # C's call was recorded but none stored; its clean AMR screen stores nothing.
  expect_identical(iso$mlst, c("done", "failed", "failed", "skipped", "missing", "done"))
  expect_identical(iso$amr, c("done", "done", "done", "skipped", "missing", "missing"))
  # E was typed (provenance) but lost its profile; F never had a provenance row.
  expect_identical(iso$cgmlst, c("done", "done", "done", "done", "failed", "done"))
  expect_identical(iso$mlst_gap, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(iso$amr_gap, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
})

test_that("analysis gaps and failures are flagged and checked", {
  ov <- isolate_overview$build_isolate_overview(
    isolates = c("A", "B", "C"),
    prov = data.frame(isolate = c("A", "B"), amr_status = c("done", "failed")),
    profiled = c("A", "B"),
    classical = "A"
  )
  flags <- isolate_overview$isolate_flags(ov$isolates)

  expect_identical(
    flags,
    c(
      "no assembly record",
      "no assembly record; no MLST; AMR failed",
      "no assembly record; no allele profile; no MLST; no AMR"
    )
  )
  expect_true(all(
    c("No allele profile", "Failed analyses", "Analyses not run") %in% ov$checks$check
  ))
  expect_identical(ov$checks$n[ov$checks$check == "Analyses not run"], 2L)
  expect_identical(ov$analyses$done, c(2L, 1L, 1L))
  expect_identical(ov$analyses$failed, c(0L, 0L, 1L))
  expect_identical(
    isolate_overview$one_isolate_checks(ov$isolates, 2L)$check,
    c("AMR screening failed", "No assembly record", "No Classical MLST")
  )
})

test_that("an analysis no isolate went through is no gap", {
  iso <- isolate_overview$build_isolate_overview(
    isolates = c("A", "B"),
    prov = data.frame(isolate = c("A", "B"), cla_status = "skipped")
  )$isolates

  expect_false(any(iso$mlst_gap | iso$amr_gap))
  expect_identical(iso$mlst, c("skipped", "skipped"))
})

test_that("result rows of removed isolates count as orphans", {
  ov <- isolate_overview$build_isolate_overview(
    isolates = "A",
    classical = c("A", "GONE"),
    screened = "GONE2"
  )

  expect_identical(ov$orphans$classical, "GONE")
  expect_identical(ov$orphans$amr, "GONE2")
  expect_identical(ov$checks$n[ov$checks$check == "Orphaned records"], 2L)
})

test_that("failed and missing analyses of the mock database surface", {
  path <- file.path(local_tempdir(), "mock.db")
  scenarios <- c("failed", "untyped", "calls_without_alleles", "amr_failed", "amr_unscreened")
  plan <- quiet(mock_database$mock_database(path, n_isolates = 80, scenarios = scenarios))
  iso <- isolate_overview$load_isolate_overview(path)$isolates
  at <- function(sc) match(plan$isolate[plan$scenario %in% sc], iso$isolate)

  expect_true(all(iso$cgmlst == "done"))
  expect_true(all(iso$mlst[at(c("failed", "calls_without_alleles"))] == "failed"))
  expect_true(all(iso$mlst_gap[at("untyped")]))
  expect_true(all(iso$amr[at("amr_failed")] == "failed"))
  expect_true(all(iso$amr_gap[at("amr_unscreened")]))
})
