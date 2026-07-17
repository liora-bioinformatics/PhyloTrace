box::use(
  testthat[
    describe,
    it,
    expect_equal,
    expect_true,
    expect_false,
    expect_identical,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / db_compat[read_db_signature, check_import_compatibility, attach_ro],
)

status_of <- function(checks, name) checks$status[checks$check == name]

test_that("attach_ro survives the paths a file picker actually produces", {
  dir <- local_tempdir()

  # shinyFiles' "Root" volume yields a leading double slash. Pasted after
  # "file:" that would parse as a URI authority and SQLite would refuse it.
  db <- file.path(dir, "peer.db")
  build_db(db, default_local())
  doubled <- paste0("/", db)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  attach_ro(con, doubled, "ext")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ext.mlst")$n, 9L)
  DBI::dbExecute(con, "DETACH DATABASE ext")
})

test_that("attach_ro keeps a peer database read-only, even when its name has a '?'", {
  dir <- local_tempdir()

  # SQLite splits a URI at the first '?', so an unencoded '?' in the filename
  # would truncate the path and drop mode=ro — attaching read-write.
  db <- file.path(dir, "odd?name.db")
  build_db(db, default_local())

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  attach_ro(con, db, "ext")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ext.mlst")$n, 9L)
  expect_error(
    DBI::dbExecute(con, "DELETE FROM ext.mlst"),
    "readonly|read-only"
  )
  # No stray file was conjured by a mis-parsed URI.
  expect_setequal(list.files(dir), "odd?name.db")
})

test_that("read_db_signature summarises a well-formed database", {
  dir <- local_tempdir()
  db <- file.path(dir, "local.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))

  sig <- read_db_signature(db)

  expect_true(sig$ok)
  expect_identical(sig$species, "Testus organismus")
  expect_identical(sig$genes, c("g1", "g2", "g3"))
  expect_identical(sig$isolates, c("A", "B"))
  expect_equal(length(sig$ref_alleles), 3L)
  expect_identical(sig$alembic, "a793f8f3fd83")
})

test_that("read_db_signature hashes the reference when `hashes` is absent", {
  dir <- local_tempdir()
  with_h <- file.path(dir, "with.db")
  without_h <- file.path(dir, "without.db")
  build_db(with_h, default_local())
  build_db(without_h, default_local(), with_hashes = FALSE)

  # Same reference, so the same content-addressed reference alleles, whether
  # they were read from `hashes` or computed on the fly.
  expect_identical(
    read_db_signature(with_h)$ref_alleles,
    read_db_signature(without_h)$ref_alleles
  )
})

test_that("read_db_signature reports a missing or unreadable file", {
  dir <- local_tempdir()
  expect_false(read_db_signature(file.path(dir, "nope.db"))$ok)
  expect_false(read_db_signature(NULL)$ok)

  not_a_db <- file.path(dir, "junk.db")
  writeLines("definitely not sqlite", not_a_db)
  expect_false(read_db_signature(not_a_db)$ok)
})

test_that("a database is compatible with itself", {
  dir <- local_tempdir()
  db <- file.path(dir, "local.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))

  checks <- check_import_compatibility(db, db)

  expect_false(attr(checks, "blocked"))
  expect_true(all(checks$status == "pass"))
})

test_that("a different organism blocks the import", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), species = "Escherichia coli")

  checks <- check_import_compatibility(local, peer)

  expect_true(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Organism"), "fail")
})

test_that("organism comparison ignores case and padding", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), species = "Testus organismus")
  build_db(peer, default_peer(), species = "  testus ORGANISMUS ")

  checks <- check_import_compatibility(local, peer)
  expect_identical(status_of(checks, "Organism"), "pass")
})

test_that("a differing locus set blocks the import", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())

  # Peer carries a fourth locus the local scheme has never seen.
  peer_alleles <- default_peer()
  peer_alleles$ref <- c(peer_alleles$ref, g4 = seqv("R4"))
  peer_alleles$B <- c(peer_alleles$B, g4 = seqv("B4"))
  peer_alleles$C <- c(peer_alleles$C, g4 = seqv("C4"))
  build_db(peer, peer_alleles)

  checks <- check_import_compatibility(local, peer)

  expect_true(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Locus set"), "fail")
})

test_that("same loci but a different reference build blocks the import", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())

  peer_alleles <- default_peer()
  peer_alleles$ref[["g2"]] <- seqv("DIFFERENT")
  build_db(peer, peer_alleles)

  checks <- check_import_compatibility(local, peer)

  expect_true(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Locus set"), "pass")
  expect_identical(status_of(checks, "Reference alleles"), "fail")
})

test_that("a differing scheme identity blocks the import", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), scheme_source = "somewhere.else")

  checks <- check_import_compatibility(local, peer)

  expect_true(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Scheme identity"), "fail")
})

test_that("a differing alembic revision only warns", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), alembic = "deadbeef1234")

  checks <- check_import_compatibility(local, peer)

  # Alembic is pyMLST's schema-migration stamp; PhyloTrace never reads it, so a
  # mismatch is advisory and must not block a merge.
  expect_false(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Schema revision"), "warn")
})

test_that("missing core tables fail fast and skip the rest", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer())

  con <- DBI::dbConnect(RSQLite::SQLite(), peer)
  DBI::dbExecute(con, "DROP TABLE mlst_type")
  DBI::dbDisconnect(con)

  checks <- check_import_compatibility(local, peer)

  expect_true(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Core tables"), "fail")
  expect_true(all(checks$status[checks$check != "Core tables"] == "skipped"))
})
