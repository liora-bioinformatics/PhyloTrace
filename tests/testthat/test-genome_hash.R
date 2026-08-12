box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / genome_hash[
    GENOME_HASH_ALGORITHM,
    check_genomes,
    genome_digest,
    genome_hash_map,
    sha512t24u,
    store_genome_hash
  ],
  app / logic / database_functions[remove_isolates],
  app / logic / db_export[export_database],
  app / logic / db_import[
    classify_isolate_collisions,
    default_resolutions,
    merge_databases
  ],
)

# Two records whose content is fixed across every fixture below; only their
# presentation (headers, wrapping, case, order) varies.
C1 <- "ACGTACGTNNNNGGGGCCCCAAAATTTT"
C2 <- "TTTTGGGGCCCCAAAACGTACGTAGGCA"

fasta <- function(dir, name, lines) {
  path <- file.path(dir, name)
  writeLines(lines, path)
  path
}

# An empty database file, so store_genome_hash() has something to attach to.
scratch_db <- function(dir) {
  path <- file.path(dir, "scratch.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE placeholder (a)")
  DBI::dbDisconnect(con)
  path
}

test_that("sha512t24u reproduces the GA4GH specification test vector", {
  # ga4gh:SQ.aKF498dAxcJAqme6QYQ7EZ07-fiw8Kw2 — if this ever changes, our
  # per-contig digests have stopped being refget identifiers.
  expect_identical(sha512t24u(charToRaw("ACGT")), "aKF498dAxcJAqme6QYQ7EZ07-fiw8Kw2")
  expect_identical(sha512t24u("ACGT"), "aKF498dAxcJAqme6QYQ7EZ07-fiw8Kw2")
  expect_identical(nchar(sha512t24u(charToRaw("ACGT"))), 32L)
})

test_that("the digest ignores headers, wrapping, case and contig order", {
  dir <- local_tempdir()

  plain <- fasta(dir, "plain.fa", c(">c1", C1, ">c2", C2))

  # Same sequences: different headers, reversed order, lowercase, re-wrapped,
  # and a blank line thrown in.
  dressed <- fasta(dir, "dressed.fa", c(
    ">NODE_2_length_28_cov_11.7 whatever",
    tolower(substr(C2, 1, 10)),
    tolower(substr(C2, 11, 28)),
    "",
    ">NODE_1_length_28_cov_9.3",
    tolower(C1)
  ))

  a <- genome_digest(plain)
  b <- genome_digest(dressed)

  expect_identical(a$genome_digest, b$genome_digest)
  expect_identical(a$n_contigs, 2L)
  expect_identical(b$n_contigs, 2L)
  expect_equal(a$total_length, nchar(C1) + nchar(C2))

  # ...but the raw file bytes did change, which is the point of keeping both.
  expect_false(identical(a$file_sha256, b$file_sha256))
  expect_identical(a$algorithm, GENOME_HASH_ALGORITHM)
})

test_that("a single changed base changes the digest", {
  dir <- local_tempdir()
  mutated <- paste0(substr(C1, 1, 27), "A")

  a <- genome_digest(fasta(dir, "a.fa", c(">c1", C1, ">c2", C2)))
  b <- genome_digest(fasta(dir, "b.fa", c(">c1", mutated, ">c2", C2)))

  expect_false(identical(a$genome_digest, b$genome_digest))
})

test_that("empty records are dropped rather than changing identity", {
  dir <- local_tempdir()

  a <- genome_digest(fasta(dir, "a.fa", c(">c1", C1, ">c2", C2)))
  b <- genome_digest(fasta(dir, "b.fa", c(">c1", C1, ">empty", "", ">c2", C2)))

  expect_identical(a$genome_digest, b$genome_digest)
  expect_identical(b$n_contigs, 2L)
})

test_that("duplicate contigs are part of the identity", {
  # `sorted_sequences` keeps duplicates, so an assembly carrying the same contig
  # twice must not collapse onto the one-copy assembly.
  dir <- local_tempdir()

  once <- genome_digest(fasta(dir, "once.fa", c(">c1", C1)))
  twice <- genome_digest(fasta(dir, "twice.fa", c(">c1", C1, ">c1_dup", C1)))

  expect_false(identical(once$genome_digest, twice$genome_digest))
  expect_identical(twice$n_contigs, 2L)
})

test_that("unreadable or non-FASTA input yields NULL, not an error", {
  dir <- local_tempdir()

  expect_null(genome_digest(file.path(dir, "absent.fa")))
  expect_null(genome_digest(fasta(dir, "notes.txt", c("just", "text"))))
  expect_null(genome_digest(fasta(dir, "headers.fa", c(">c1", ">c2"))))
  expect_null(genome_digest(NULL))
  expect_null(genome_digest(NA_character_))
})

test_that("store_genome_hash creates the table and re-typing replaces the row", {
  dir <- local_tempdir()
  db <- scratch_db(dir)
  file <- fasta(dir, "S1.fa", c(">c1", C1, ">c2", C2))

  expect_true(store_genome_hash(db, "S1", file))

  row <- qdf(db, "SELECT * FROM genome_hashes")
  expect_identical(nrow(row), 1L)
  expect_identical(row$isolate, "S1")
  expect_identical(row$algorithm, GENOME_HASH_ALGORITHM)
  expect_identical(row$n_contigs, 2L)
  expect_identical(nchar(row$genome_digest), 32L)
  expect_identical(nchar(row$file_sha256), 64L)

  # `isolate` is the primary key: a second run must replace, not accumulate.
  expect_true(store_genome_hash(db, "S1", file))
  expect_identical(q1(db, "SELECT COUNT(*) FROM genome_hashes"), 1L)
})

test_that("store_genome_hash is best-effort on bad input", {
  dir <- local_tempdir()
  db <- scratch_db(dir)
  file <- fasta(dir, "S1.fa", c(">c1", C1))

  expect_false(store_genome_hash(file.path(dir, "no.db"), "S1", file))
  expect_false(store_genome_hash(db, NA_character_, file))
  expect_false(store_genome_hash(db, "S1", file.path(dir, "absent.fa")))
  expect_identical(
    q1(db, "SELECT COUNT(*) FROM sqlite_master WHERE name = 'genome_hashes'"),
    0L
  )
})

test_that("store_genome_hash accepts a precomputed digest without re-reading", {
  dir <- local_tempdir()
  db <- scratch_db(dir)
  file <- fasta(dir, "S1.fa", c(">c1", C1))
  digest <- genome_digest(file)

  # File removed first: if it were re-read, this could not succeed.
  unlink(file)
  expect_true(store_genome_hash(db, "S1", file, digest = digest))
  expect_identical(
    q1(db, "SELECT genome_digest FROM genome_hashes"),
    digest$genome_digest
  )
})

test_that("genome_hash_map is empty for databases without the table", {
  dir <- local_tempdir()
  expect_identical(length(genome_hash_map(scratch_db(dir))), 0L)
  expect_identical(length(genome_hash_map(file.path(dir, "absent.db"))), 0L)
})

test_that("check_genomes separates the four interesting outcomes", {
  dir <- local_tempdir()
  db <- scratch_db(dir)

  s1 <- fasta(dir, "S1.fa", c(">c1", C1, ">c2", C2))
  renamed <- file.path(dir, "S1_copy.fa")
  file.copy(s1, renamed)
  s2 <- fasta(dir, "S2.fa", c(">c1", paste0(substr(C1, 1, 27), "A")))

  store_genome_hash(db, "S1", s1)

  out <- check_genomes(
    db,
    strains = c("S1", "S1_copy", "S2", "S1"),
    files = c(s1, renamed, s2, s2),
    known_strains = c("S1")
  )

  # Same name, same assembly - a genuine re-run.
  expect_identical(out$status[1], "retype")
  # Free name, but the assembly is already stored under S1. The name is what
  # decides typeability, so this stays "new"; the twin is reported in `other`.
  expect_identical(out$status[2], "new")
  expect_identical(out$other[2], "S1")
  # Neither the name nor the assembly is on record.
  expect_identical(out$status[3], "new")
  expect_true(is.na(out$other[3]))
  # The name exists but was typed from a different assembly - pyMLST would
  # reject this file and its data would vanish silently.
  expect_identical(out$status[4], "name_conflict")
})

test_that("a taken name is never masked by a content match elsewhere", {
  # Regression test: the content check used to run first and win, so a file
  # that was BOTH a name conflict and a copy of some third isolate came back as
  # a content duplicate - which reads as typeable. Those files were queued and
  # then rejected one by one by `wgMLST add`, burning a full cgMLST + classical
  # MLST + AMR pass each. The name axis has to be decided on its own.
  dir <- local_tempdir()
  db <- scratch_db(dir)

  taken <- fasta(dir, "TAKEN.fa", c(">c1", C1))
  third <- fasta(dir, "THIRD.fa", c(">c1", C2))
  store_genome_hash(db, "TAKEN", taken)
  store_genome_hash(db, "THIRD", third)

  # Name "TAKEN" is in use, and the file offered for it holds THIRD's assembly.
  out <- check_genomes(
    db,
    strains = "TAKEN",
    files = third,
    known_strains = c("TAKEN", "THIRD")
  )

  expect_identical(out$status, "name_conflict")
  # The content match is still reported - it just does not override the verdict.
  expect_identical(out$other, "THIRD")
})

test_that("check_genomes reports 'name_untracked' for isolates predating digests", {
  # The majority state for existing databases: the isolate is on record but its
  # assembly never was. This must never be reported as a mismatch.
  dir <- local_tempdir()
  db <- scratch_db(dir)
  file <- fasta(dir, "OLD.fa", c(">c1", C1))

  out <- check_genomes(db, "OLD", file, known_strains = c("OLD"))
  expect_identical(out$status, "name_untracked")
  expect_true(is.na(out$other))
})

test_that("digests survive an export / import round trip", {
  # The whole point of recording a digest is that it travels: a receiving lab
  # must be able to check an isolate came from the assembly it thinks it did.
  dir <- local_tempdir()

  src <- file.path(dir, "src.db")
  build_db(src, default_local(), metadata = meta_df(c("A", "B")))
  store_genome_hash(src, "A", fasta(dir, "A.fa", c(">c1", C1, ">c2", C2)))
  store_genome_hash(src, "B", fasta(dir, "B.fa", c(">c1", C2)))
  before <- genome_hash_map(src)

  # Export carries genome_hashes unconditionally, filtered to the selection.
  shared <- file.path(dir, "shared.db")
  export_database(src, shared, "A", "sample_collection_date")

  expect_identical(genome_hash_map(shared)[["A"]], before[["A"]])
  expect_identical(length(genome_hash_map(shared)), 1L)

  # ...and import merges it into a database that has never seen the table.
  local <- file.path(dir, "local.db")
  build_db(local, list(ref = ref_alleles()), metadata = NULL)

  suppressMessages(merge_databases(
    local,
    shared,
    default_resolutions(classify_isolate_collisions(local, shared)),
    metadata_cols = "sample_collection_date",
    backup = FALSE
  ))

  expect_identical(genome_hash_map(local)[["A"]], before[["A"]])
})

test_that("importing the same isolate twice leaves one digest row", {
  # `isolate` is the primary key, so an overwritten isolate must end up with the
  # peer's digest on a single row rather than two rows or a PK error.
  dir <- local_tempdir()

  peer <- file.path(dir, "peer.db")
  build_db(peer, default_local(), metadata = meta_df(c("A", "B")))
  store_genome_hash(peer, "A", fasta(dir, "A.fa", c(">c1", C1)))

  local <- file.path(dir, "local.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  store_genome_hash(local, "A", fasta(dir, "A_old.fa", c(">c1", C2)))

  suppressMessages(merge_databases(
    local,
    peer,
    data.frame(
      ext_isolate = "A",
      action = "overwrite",
      final_isolate = "A",
      stringsAsFactors = FALSE
    ),
    metadata_cols = "sample_collection_date",
    backup = FALSE
  ))

  expect_identical(q1(local, "SELECT COUNT(*) FROM genome_hashes"), 1L)
  expect_identical(genome_hash_map(local)[["A"]], genome_hash_map(peer)[["A"]])
})

test_that("removing an isolate removes its digest", {
  # A stale row would make a later isolate of the same name read as a re-type of
  # the deleted one, which is exactly the confusion this feature exists to end.
  dir <- local_tempdir()
  db <- file.path(dir, "db.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))

  store_genome_hash(db, "A", fasta(dir, "A.fa", c(">c1", C1)))
  store_genome_hash(db, "B", fasta(dir, "B.fa", c(">c1", C2)))

  remove_isolates(db, "A")

  expect_identical(names(genome_hash_map(db)), "B")
})

test_that("check_genomes tolerates unreadable files and empty input", {
  dir <- local_tempdir()
  db <- scratch_db(dir)

  out <- check_genomes(db, "GONE", file.path(dir, "absent.fa"))
  expect_identical(nrow(out), 1L)
  expect_true(is.na(out$digest))

  expect_identical(nrow(check_genomes(db, character(0), character(0))), 0L)
})
