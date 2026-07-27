box::use(
  testthat[
    expect_error,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / pymlst[hash_database, hashes_pending],
)

# hash_database() backfills the pyMLST allele `hashes` table (id, sha256(sequence))
# when a database is loaded. The regression guarded here: on an already-hashed
# database it must NOT rewrite the table, because that touched the .db file's
# mtime on every load even though nothing changed.

# sha256(sequence) as the app stores it: a lowercase hex string.
sha_hex <- function(x) as.character(openssl::sha256(x))

test_that("an already-hashed database is left untouched on disk", {
  dir <- local_tempdir()
  db <- file.path(dir, "hashed.db")
  build_db(db, default_local(), with_hashes = TRUE)

  # Pin the mtime to a fixed point in the past. A real write would move it to
  # "now"; eliding the write leaves it exactly here, regardless of filesystem
  # timestamp resolution.
  stamp <- as.POSIXct("2001-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(db, stamp)
  before <- file.info(db)$mtime

  before_rows <- qdf(db, "SELECT id, hash FROM hashes")
  suppressMessages(hash_database(db))

  expect_identical(file.info(db)$mtime, before)
  # And the table is unchanged.
  expect_identical(qdf(db, "SELECT id, hash FROM hashes"), before_rows)
})

test_that("a database with no hashes table gets one covering every sequence", {
  dir <- local_tempdir()
  db <- file.path(dir, "unhashed.db")
  build_db(db, default_local(), with_hashes = FALSE)

  expect_identical(
    q1(db, "SELECT COUNT(*) FROM sqlite_master WHERE name = 'hashes'"),
    0L
  )

  suppressMessages(hash_database(db))

  seqs <- qdf(db, "SELECT id, sequence FROM sequences")
  hashes <- qdf(db, "SELECT id, hash FROM hashes")

  # One hash per sequence, and each is sha256 of that sequence.
  expect_setequal(hashes$id, seqs$id)
  expect_identical(
    hashes$hash[match(seqs$id, hashes$id)],
    sha_hex(seqs$sequence)
  )
})

test_that("only sequences missing from the hashes table are hashed and appended", {
  dir <- local_tempdir()
  db <- file.path(dir, "partial.db")
  build_db(db, default_local(), with_hashes = TRUE)

  # Introduce a sequence that has no hash yet, so counts diverge and the
  # append branch runs.
  new_id <- q1(db, "SELECT MAX(id) + 1 FROM sequences")
  new_seq <- "ATGCNEWALLELETAA"
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(
    con,
    "INSERT INTO sequences (id, sequence) VALUES (?, ?)",
    list(new_id, new_seq)
  )
  DBI::dbDisconnect(con)

  before <- qdf(db, "SELECT id, hash FROM hashes")
  suppressMessages(hash_database(db))
  after <- qdf(db, "SELECT id, hash FROM hashes")

  # Exactly one row added: the new id, hashed correctly.
  expect_setequal(after$id, c(before$id, new_id))
  expect_identical(after$hash[match(new_id, after$id)], sha_hex(new_seq))
  # Pre-existing hashes are preserved verbatim.
  expect_identical(
    after$hash[match(before$id, after$id)],
    before$hash
  )
})

test_that("hashes_pending tracks exactly when hash_database would write", {
  dir <- local_tempdir()

  # Fully hashed: nothing to do.
  done <- file.path(dir, "done.db")
  build_db(done, default_local(), with_hashes = TRUE)
  expect_false(hashes_pending(done))

  # No hashes table yet: pending.
  fresh <- file.path(dir, "fresh.db")
  build_db(fresh, default_local(), with_hashes = FALSE)
  expect_true(hashes_pending(fresh))
  # ...and once hash_database has run, no longer pending.
  suppressMessages(hash_database(fresh))
  expect_false(hashes_pending(fresh))

  # A new, unhashed sequence makes it pending again.
  con <- DBI::dbConnect(RSQLite::SQLite(), fresh)
  DBI::dbExecute(
    con,
    "INSERT INTO sequences (id, sequence)
       VALUES ((SELECT MAX(id) + 1 FROM sequences), 'ATGCNEWTAA')"
  )
  DBI::dbDisconnect(con)
  expect_true(hashes_pending(fresh))

  # No sequences table at all: FALSE, so callers never invoke hash_database()
  # where it would error.
  empty <- file.path(dir, "empty.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), empty)
  DBI::dbExecute(con, "CREATE TABLE placeholder (a)")
  DBI::dbDisconnect(con)
  expect_false(hashes_pending(empty))
})

test_that("a database without a sequences table is a hard error", {
  # Documents the caller's contract: hash_database() assumes a valid pyMLST
  # schema and does not defend against a missing `sequences` table.
  dir <- local_tempdir()
  db <- file.path(dir, "empty.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE placeholder (a)")
  DBI::dbDisconnect(con)

  expect_error(suppressMessages(hash_database(db)))
})
