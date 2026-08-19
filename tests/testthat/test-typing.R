# Drives the typing module's per-isolate persistence through its real reactive
# graph and a real database, rather than calling the logic layer directly.

box::use(
  shiny[reactive, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / genome_hash[store_genome_hash],
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

test_that("the two searches record their own thresholds, not one shared pair", {
  # The classical search runs at its own cutoffs (pyMLST defaults it looser than
  # allele calling), so provenance has to keep the two pairs apart - reading
  # cg_identity off a classical row would misreport how the ST was called.
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genome <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGC"), genome)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- "A"
    Typing$queued_files <- genome
    Typing$cla_enabled <- TRUE
    Typing$scheme_context <- scheme_provenance(path)
    session$setInputs(
      identity = 0.99,
      coverage = 0.95,
      cla_identity = 0.9,
      cla_coverage = 0.8
    )

    lines <- c(RUN_PREAMBLE, strain_section("A", amr = FALSE), "Done!")
    results <- parse_typing_log(lines, Typing$queued_strains)
    persist_results(results, lines, retry = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    row <- DBI::dbGetQuery(con, "SELECT * FROM typing_provenance")

    expect_identical(row$cg_identity, 0.99)
    expect_identical(row$cg_coverage, 0.95)
    expect_identical(row$cla_identity, 0.9)
    expect_identical(row$cla_coverage, 0.8)

    # The classical_mlst rows carry the classical pair too - they describe that
    # search, not allele calling.
    cla <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT identity, coverage FROM classical_mlst"
    )
    expect_identical(cla$identity, 0.9)
    expect_identical(cla$coverage, 0.8)
  })
})

test_that("a provenance row says which steps failed, ran, or never ran", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genome <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGC"), genome)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- "A"
    Typing$queued_files <- genome
    Typing$cla_enabled <- TRUE
    Typing$amr_enabled <- TRUE
    Typing$amr_out <- file.path(dir, "amr")

    # Allele calling worked; the ST search returned nothing usable and the AMR
    # screen fell over. Both leave their result tables empty, so without a
    # status the run-level scheme and version fields would be all the row shows.
    lines <- c(
      RUN_PREAMBLE,
      "Processing Strain: A",
      "[INFO: 2026-08-05 09:00:00,000] found 2350 genes",
      "[INFO: 2026-08-05 09:00:00,000] DONE",
      "Classical MLST ST: NA",
      "AMR: failed A",
      "Strain elapsed: 10",
      "Strain finished: 09:00:10",
      "Done!"
    )
    results <- parse_typing_log(lines, Typing$queued_strains)
    persist_results(results, lines, retry = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    row <- DBI::dbGetQuery(con, "SELECT * FROM typing_provenance")

    expect_identical(nrow(row), 1L)
    expect_identical(row$cla_status, "failed")
    expect_identical(row$amr_status, "failed")
    # The scheme searched against is still recorded - it is what the failed
    # search ran on - but no result was produced by either step.
    expect_identical(row$cla_scheme, "Klebsiella (MLST)")
    expect_identical(row$amr_elements, NA_integer_)
    expect_identical(isolates_in(path, "classical_mlst"), character(0))
    expect_identical(isolates_in(path, "amr_results"), character(0))
  })
})

test_that("steps switched off for a run are recorded as skipped", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  genome <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGC"), genome)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- "A"
    Typing$queued_files <- genome
    # No classical scheme resolved, AMR screening opted out.
    Typing$cla_enabled <- FALSE
    Typing$amr_enabled <- FALSE

    lines <- c(
      "pyMLST version: 2.2.2",
      "BLAT version: 35",
      "Processing Strain: A",
      "[INFO: 2026-08-05 09:00:00,000] found 2350 genes",
      "[INFO: 2026-08-05 09:00:00,000] DONE",
      "Strain elapsed: 10",
      "Strain finished: 09:00:10",
      "Done!"
    )
    results <- parse_typing_log(lines, Typing$queued_strains)
    persist_results(results, lines, retry = TRUE)

    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    row <- DBI::dbGetQuery(con, "SELECT * FROM typing_provenance")

    expect_identical(row$cla_status, "skipped")
    expect_identical(row$amr_status, "skipped")
    # Allele calling still recorded everything it knows.
    expect_identical(row$cg_loci_found, 2350L)
    expect_identical(row$pymlst_version, "2.2.2")
  })
})

test_that("a large skip list pauses for confirmation before anything is hashed", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")

  # Six isolates already in the database, so six of the selected files below
  # are skipped on their name alone - one over name_conflict_gate_min.
  taken <- paste0("S", 1:6)
  alleles <- lapply(taken, function(s) {
    c(g1 = seqv(paste0(s, "1")), g2 = seqv(paste0(s, "2")), g3 = seqv(paste0(s, "3")))
  })
  names(alleles) <- taken
  build_db(path, c(list(ref = ref_alleles()), alleles))

  all_strains <- c(taken, "NEW1", "NEW2")
  genomes <- vapply(
    all_strains,
    function(s) {
      f <- file.path(dir, paste0(s, ".fna"))
      writeLines(c(paste0(">", s, "_contig1"), paste0("ATGCATGCATGC", s)), f)
      f
    },
    character(1)
  )

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$files <- unname(genomes)
    Typing$strains <- all_strains
    session$setInputs(start = 1)

    # The gate is raised straight out of the Start click: the skip verdict is a
    # name lookup, so it needs no digests and nothing has been hashed yet.
    expect_identical(Typing$status, "idle")
    expect_identical(chk$n, NULL)

    # The queue is settled before the gate opens, so confirming can only ever
    # send the typeable files on - never a name pyMLST would reject.
    expect_setequal(Typing$queued_strains, c("NEW1", "NEW2"))
    expect_setequal(Typing$skipped_strains, taken)
    expect_identical(nrow(Typing$dup_report), 6L)

    # begin_typing_run() was never reached - it is the first thing that would
    # set Typing$log_file.
    expect_identical(Typing$log_file, NULL)

    # Cancelling backs out without ever hashing or starting a process.
    session$setInputs(cancel_typing_gate = 1)
    expect_identical(Typing$status, "idle")
    expect_identical(Typing$log_file, NULL)
    expect_identical(chk$n, NULL)
  })
})

test_that("confirming the gate hashes only the typeable files", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")

  taken <- paste0("S", 1:6)
  alleles <- lapply(taken, function(s) {
    c(g1 = seqv(paste0(s, "1")), g2 = seqv(paste0(s, "2")), g3 = seqv(paste0(s, "3")))
  })
  names(alleles) <- taken
  build_db(path, c(list(ref = ref_alleles()), alleles))

  all_strains <- c(taken, "NEW1", "NEW2")
  genomes <- vapply(
    all_strains,
    function(s) {
      f <- file.path(dir, paste0(s, ".fna"))
      writeLines(c(paste0(">", s, "_contig1"), paste0("ATGCATGCATGC", s)), f)
      f
    },
    character(1)
  )

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$files <- unname(genomes)
    Typing$strains <- all_strains
    session$setInputs(start = 1)
    session$setInputs(confirm_typing_gate = 1)

    # Only the two typeable genomes are hashed - the six skipped ones are never
    # read, which is the whole point of deciding typeability by name first.
    expect_identical(chk$n, 2L)
    expect_setequal(chk$out$strain, c("NEW1", "NEW2"))
  })
})

test_that("an unresolvable scheme is announced in the run's own transcript", {
  # This is the actual failure mode reported against a real database
  # (Acinetobacter baumannii, whose PubMLST database hosts two equally
  # classical 7-locus schemes with nothing to break the tie): before this,
  # resolve_mlst_scheme() failing left no trace anywhere in the log a user
  # inspects, only in the app's internal event log. No species is configured
  # here, which is the same code path with no network dependency.
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local(), species = NA_character_)
  repo_root <- normalizePath(file.path(getwd(), "..", ".."))

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$queued_strains <- character(0)
    Typing$queued_files <- character(0)

    withr::with_dir(repo_root, begin_typing_run())
    on.exit(
      if (!is.null(Typing$proc)) Typing$proc$wait(timeout = 5000),
      add = TRUE
    )

    expect_false(is.null(Typing$log_file))
    lines <- readLines(Typing$log_file)
    expect_true(any(grepl(
      "no resolvable species for this scheme",
      lines,
      fixed = TRUE
    )))
    # It leads the transcript, ahead of anything the subprocess itself writes -
    # append mode preserves it rather than racing the child's own first write.
    expect_identical(
      lines[2],
      paste(
        "Classical MLST: no resolvable species for this scheme",
        "(skipping ST calls for this run)."
      )
    )
  })
})

test_that("a name already in the database is skipped without being hashed", {
  # The isolate name alone decides typeability - pyMLST keys on it and rejects
  # a name the database already holds - so a file it rules out is never read.
  # Reaching pyMLST with such a file cost a full cgMLST + classical MLST + AMR
  # pass per file before it was rejected; hashing it would cost less but still
  # buy nothing the run acts on.
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())   # isolates A, B already fully in `mlst`

  # Named after an existing isolate, and not even readable: if the run tried to
  # hash it, genome_digest() would have to touch a file that is not there.
  a_conflict <- file.path(dir, "A.fna")
  writeLines(c(">A_contig1", "TTTTGGGGCCCCAAAATTTTGGGGCCCC"), a_conflict)

  fresh <- file.path(dir, "FRESH.fna")
  writeLines(c(">F_contig1", "GGGGCCCCAAAATTTTGGGGCCCC"), fresh)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$files <- c(a_conflict, fresh)
    Typing$strains <- c("A", "FRESH")
    session$setInputs(start = 1)
    for (i in 1:5) session$elapse(50)

    # Only the free name is queued, and only it is hashed.
    expect_identical(Typing$queued_strains, "FRESH")
    expect_identical(Typing$skipped_strains, "A")
    expect_identical(chk$out$strain, "FRESH")

    # The skip still reaches the panel the user reads afterwards.
    expect_true("A" %in% Typing$dup_report$strain)
  })
})

test_that("nothing is typed or hashed when every selected name is taken", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())   # isolates A, B already in `mlst`

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
    Typing$files <- unname(genomes)
    Typing$strains <- c("A", "B")
    session$setInputs(start = 1)

    # Bails out at the Start click: no queue, no hashing phase, no run.
    expect_identical(Typing$queued_strains, character(0))
    expect_identical(Typing$status, "idle")
    expect_identical(chk$n, NULL)
    expect_identical(Typing$log_file, NULL)
    # The panel still explains why, rather than leaving a silent no-op.
    expect_setequal(Typing$dup_report$strain, c("A", "B"))
  })
})

test_that("a typed genome whose assembly is already stored is reported, not blocked", {
  # The content axis: a free name whose assembly matches an isolate already on
  # record. Legitimate (a re-deposit or a rename), so it is typed - but the
  # duplication is surfaced, since only the metadata can settle whether the two
  # records are the same epidemiological isolate.
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  a_asm <- file.path(dir, "A_asm.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGCAAAA"), a_asm)
  store_genome_hash(path, "A", a_asm)

  # A free name holding A's exact assembly.
  renamed <- file.path(dir, "A_RENAMED.fna")
  writeLines(c(">A_contig1", "ATGCATGCATGCATGCATGCAAAA"), renamed)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$files <- renamed
    Typing$strains <- "A_RENAMED"
    session$setInputs(start = 1)
    for (i in 1:5) session$elapse(50)

    # Typed, because the name is free.
    expect_identical(Typing$queued_strains, "A_RENAMED")
    expect_identical(chk$out$other, "A")

    # And reported, so the duplication is not silent.
    row <- Typing$dup_report[Typing$dup_report$strain == "A_RENAMED", ]
    expect_identical(nrow(row), 1L)
    expect_identical(row$other, "A")
  })
})

# ---------------------------------------------------------------------------
# typing_active: the navbar spinner's signal (see app/main.R)
# ---------------------------------------------------------------------------

test_that("typing_active is FALSE before any run starts", {
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  testServer(typing$server, args = list(db_path = reactive(path)), {
    expect_identical(Typing$status, "idle")
    expect_false(typing_active())
  })
})

test_that("typing_active turns true while the run is checking genomes", {
  # "checking" is reached by the app's own path (Start), not by poking
  # Typing$status directly - setting that field to values the run's own
  # observers watch for (running/done/...) fires the real poller and finalize
  # logic, which expect a fully populated run rather than this fixture's bare
  # queue. Checking is reachable safely because it is driven by input$start
  # alone; the run proper (which would need a live process) is not exercised
  # here - see the persistence tests above for that.
  dir <- local_tempdir()
  path <- file.path(dir, "db.db")
  build_db(path, default_local())

  # A name the database does not hold: anything already named is skipped at the
  # Start click and never reaches the checking phase this test is about.
  genome <- file.path(dir, "NEW.fna")
  writeLines(c(">NEW_contig1", "ATGCATGCATGCATGCATGC"), genome)

  testServer(typing$server, args = list(db_path = reactive(path)), {
    Typing$files <- genome
    Typing$strains <- "NEW"
    session$setInputs(start = 1)

    expect_identical(Typing$status, "checking")
    expect_true(typing_active())
  })
})
