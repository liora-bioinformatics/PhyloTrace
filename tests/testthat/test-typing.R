# Drives the typing module's per-isolate persistence through its real reactive
# graph and a real database, rather than calling the logic layer directly.

box::use(
  shiny[reactive, testServer],
  testthat[expect_identical, expect_setequal, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / provenance[scheme_provenance],
  app / logic / pymlst[cg_outcome, parse_typing_log],
  app / view / typing,
)

ALLELES <- "gapA=3,infB=3,mdh=1,pgi=1,phoE=1,rpoB=1,tonB=4"

# The provenance the script reports once per run, before the first genome.
RUN_PREAMBLE <- c(
  "Classical MLST repository: pasteur",
  "Classical MLST scheme: Klebsiella (MLST)",
  "Classical MLST scheme version: 2026-07-28",
  "pyMLST version: 2.2.2",
  "BLAT version: 35",
  "MAFFT version: v7.525",
  "AMR abritamr version: 1.0.19",
  "AMR finder version: 4.0.19",
  "AMR database version: 2026-01-01.1"
)

# scheme_overview as the cgMLST download leaves it.
OVERVIEW <- data.frame(
  key = c("Database", "Version", "Seed Genome", "Genus", "Species",
          "Locus Count", "Complex Type Distance", "Complex Type Count"),
  value = c("cgMLST.org Nomenclature Server (h25)", "1.0", "NTUH-K2044",
            "Klebsiella", "pneumoniae/variicola/quasipneumoniae", "2,358",
            "15", "23,933"),
  stringsAsFactors = FALSE
)

# One strain's section of a run log: allele calling through to the AMR screen.
strain_section <- function(name, cla = TRUE, amr = TRUE, closed = TRUE) {
  c(
    paste("Processing Strain:", name),
    "[INFO: 2026-08-05 09:00:00,000] found 2350 genes",
    "[INFO: 2026-08-05 09:00:00,000] Added 3 new MLST genes to database",
    "[INFO: 2026-08-05 09:00:00,000] DONE",
    if (cla) c(
      "Classical MLST ST: 11",
      paste("Classical MLST alleles:", ALLELES)
    ),
    if (amr) paste0("AMR: done ", name, " (1 elements)"),
    if (closed) c("Strain elapsed: 10", "Strain finished: 09:00:10")
  )
}

# A minimal amrfinder.out, the file store_amr_results() reads a screen back from.
seed_amr_dir <- function(root, strain) {
  dir.create(file.path(root, strain), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      paste(
        c(
          "Protein identifier", "Contig id", "Start", "Stop", "Strand",
          "Gene symbol", "Sequence name", "Scope", "Element type",
          "Element subtype", "Class", "Subclass", "Method", "Target length",
          "Reference sequence length", "% Coverage of reference sequence",
          "% Identity to reference sequence", "Alignment length",
          "Accession of closest sequence", "Name of closest sequence"
        ),
        collapse = "\t"
      ),
      paste(
        c(
          "NA", "contig1", "100", "900", "+", "blaTEST", "beta-lactamase TEST",
          "core", "AMR", "AMR", "BETA-LACTAM", "CEPHALOSPORIN", "BLASTX",
          "266", "266", "100.00", "99.62", "266", "WP_000027057.1",
          "beta-lactamase TEST"
        ),
        collapse = "\t"
      )
    ),
    file.path(root, strain, "amrfinder.out")
  )
}

isolates_in <- function(path, table) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!table %in% DBI::dbListTables(con)) {
    return(character(0))
  }
  DBI::dbGetQuery(con, sprintf("SELECT DISTINCT isolate FROM %s", table))$isolate
}

test_that("a finished isolate is written while the rest of the run is still going", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genomes <- vapply(
    c("A", "B"),
    function(s) {
      f <- file.path(dir, paste0(s, ".fna"))
      writeLines(c(paste0(">", s, "_contig1"), "ATGCATGCATGCATGCATGC"), f)
      f
    },
    character(1)
  )
  amr_out <- file.path(dir, "amr")
  seed_amr_dir(amr_out, "A")

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- c("A", "B")
    Typing$queued_files <- unname(genomes)
    Typing$cla_enabled <- TRUE
    Typing$amr_enabled <- TRUE
    Typing$amr_out <- amr_out

    # A is through its whole pipeline; B has only just started.
    lines <- c(
      strain_section("A"),
      "Processing Strain: B",
      "[INFO: 2026-08-05 09:00:11,000] Search coregene with BLAT"
    )
    persist_results(parse_typing_log(lines, Typing$queued_strains), lines)

    # Everything A produced is in the database before the run is over, and
    # nothing of B's is - it has no results yet.
    expect_identical(isolates_in(path, "classical_mlst"), "A")
    expect_identical(isolates_in(path, "amr_results"), "A")
    expect_identical(isolates_in(path, "genome_hashes"), "A")
    expect_setequal(persisted$done$cla, "A")
    expect_setequal(persisted$done$amr, "A")
    expect_setequal(persisted$done$hash, "A")
  })
})

test_that("the closing sweep picks up the last isolate without rewriting the rest", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genomes <- vapply(
    c("A", "B"),
    function(s) {
      f <- file.path(dir, paste0(s, ".fna"))
      writeLines(c(paste0(">", s, "_contig1"), "ATGCATGCATGCATGCATGC"), f)
      f
    },
    character(1)
  )

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- c("A", "B")
    Typing$queued_files <- unname(genomes)
    Typing$cla_enabled <- TRUE

    mid_run <- c(
      strain_section("A"),
      "Processing Strain: B",
      "[INFO: 2026-08-05 09:00:11,000] Search coregene with BLAT"
    )
    persist_results(parse_typing_log(mid_run, Typing$queued_strains), mid_run)
    expect_identical(isolates_in(path, "classical_mlst"), "A")

    # The run ends: B's steps finished after the last poll, and the sweep runs
    # over both strains.
    final <- c(strain_section("A"), strain_section("B"), "Done!")
    results <- parse_typing_log(final, Typing$queued_strains)
    persist_results(results, final, retry = TRUE)

    expect_setequal(isolates_in(path, "classical_mlst"), c("A", "B"))
    # Seven loci each, so A was not written a second time.
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    rows <- DBI::dbGetQuery(
      con,
      "SELECT isolate, COUNT(*) AS n FROM classical_mlst GROUP BY isolate"
    )
    expect_identical(rows$n, c(7L, 7L))
  })
})

test_that("nothing is written for an isolate the database did not accept", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genome <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGC"), genome)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- "A"
    Typing$queued_files <- genome
    Typing$cla_enabled <- TRUE

    # pyMLST refused the assembly, so no isolate exists to key rows to - even
    # though the classical MLST search that followed did return a call.
    lines <- c(
      "Processing Strain: A",
      "Error: No path was found for the core genome",
      "Classical MLST ST: 11",
      paste("Classical MLST alleles:", ALLELES),
      "Strain elapsed: 3",
      "Strain finished: 09:00:03",
      "Done!"
    )
    results <- parse_typing_log(lines, Typing$queued_strains)
    expect_identical(cg_outcome(results), "Incompatible")

    persist_results(results, lines, retry = TRUE)
    expect_identical(isolates_in(path, "classical_mlst"), character(0))
    expect_identical(isolates_in(path, "genome_hashes"), character(0))
  })
})

test_that("every isolate gets a provenance row as it is typed", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "scheme_overview", OVERVIEW, overwrite = TRUE)
  DBI::dbDisconnect(con)

  genome <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGC"), genome)
  amr_out <- file.path(dir, "amr")
  seed_amr_dir(amr_out, "A")

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- "A"
    Typing$queued_files <- genome
    Typing$cla_enabled <- TRUE
    Typing$amr_enabled <- TRUE
    Typing$amr_out <- amr_out
    Typing$log_file <- file.path(dir, "typing_20260805_090000_ab.log")
    Typing$scheme_context <- scheme_provenance(path)

    lines <- c(RUN_PREAMBLE, strain_section("A"), "Done!")
    results <- parse_typing_log(lines, Typing$queued_strains)
    persist_results(results, lines, retry = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    row <- DBI::dbGetQuery(con, "SELECT * FROM typing_provenance")

    expect_identical(nrow(row), 1L)
    expect_identical(row$isolate, "A")
    expect_identical(row$run_id, "typing_20260805_090000_ab.log")

    # The scheme the isolate was typed against, snapshotted from the database.
    expect_identical(row$cg_scheme_database, "cgMLST.org Nomenclature Server (h25)")
    expect_identical(row$cg_locus_count, 2358L)
    expect_identical(row$cg_complex_type_count, 23933L)

    # What allele calling made of this assembly, and the assembly it read.
    expect_identical(row$cg_loci_found, 2350L)
    expect_identical(row$cg_alleles_added, 3L)
    expect_identical(row$cg_completeness, 99.7)
    expect_true(nzchar(row$genome_digest))
    expect_identical(row$n_contigs, 1L)

    # Every tool that touched it, and the reference data they used.
    expect_identical(row$pymlst_version, "2.2.2")
    expect_identical(row$blat_version, "35")
    expect_identical(row$mafft_version, "v7.525")
    expect_identical(row$cla_scheme, "Klebsiella (MLST)")
    expect_identical(row$cla_repository, "pasteur")
    expect_identical(row$amr_abritamr_version, "1.0.19")
    expect_identical(row$amr_elements, 1L)
    expect_identical(row$elapsed_seconds, 10)
  })
})
