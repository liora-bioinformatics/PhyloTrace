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
  app /
    logic /
    db_compat[
      attach_ro,
      check_db_loadable,
      check_import_compatibility,
      read_db_signature
    ],
  app / logic / db_compat,
  app / logic / mlst_scheme[MLST_SCHEME_TABLES, store_mlst_scheme],
)

impl <- attr(db_compat, "namespace")

status_of <- function(checks, name) checks$status[checks$check == name]

test_that("attach_ro survives the paths a file picker actually produces", {
  dir <- local_tempdir()

  # A path with a leading double slash (as some pickers produce). Pasted after
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

test_that("attach_ro opens a file whose name holds a literal percent sequence", {
  dir <- local_tempdir()
  db <- file.path(dir, "partner%20lab #1.db")
  build_db(db, default_local())

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  attach_ro(con, db, "ext")
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM ext.mlst")$n, 9L)
  DBI::dbExecute(con, "DETACH DATABASE ext")
  expect_setequal(list.files(dir), "partner%20lab #1.db")
})

test_that("check_db_loadable accepts a real database and rejects the rest", {
  dir <- local_tempdir()
  good <- file.path(dir, "good.db")
  build_db(good, default_local())

  expect_true(check_db_loadable(good)$ok)

  # Nothing selected.
  expect_false(check_db_loadable(NULL)$ok)
  expect_false(check_db_loadable(NA_character_)$ok)

  # Selected but gone from disk (deleted/moved mid-session) — and the read-only
  # probe must not resurrect it as an empty file.
  gone <- file.path(dir, "gone.db")
  res <- check_db_loadable(gone)
  expect_false(res$ok)
  expect_true(grepl("no longer", res$reason))
  expect_false(file.exists(gone))

  # A file that exists but carries no pyMLST schema.
  empty <- file.path(dir, "empty.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), empty)
  DBI::dbExecute(con, "CREATE TABLE placeholder (a)")
  DBI::dbDisconnect(con)
  res <- check_db_loadable(empty)
  expect_false(res$ok)
  expect_true(grepl("not a valid PhyloTrace database", res$reason))

  # Not an SQLite file at all.
  junk <- file.path(dir, "notes.txt")
  writeLines("hello", junk)
  expect_false(check_db_loadable(junk)$ok)
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

test_that("a differing scheme label only warns", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer(), scheme_source = "somewhere.else")

  checks <- check_import_compatibility(local, peer)

  # Source and version are labels a user typed in at setup; the locus set and
  # the reference alleles, checked exactly, are what make profiles comparable.
  expect_false(attr(checks, "blocked"))
  expect_identical(status_of(checks, "Scheme identity"), "warn")
  expect_identical(status_of(checks, "Reference alleles"), "pass")
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

scheme_fixture <- function(n_st = 2L, name = "Testus MLST", loci = c("adk", "gyr"),
                           seq_tag = "M", version = paste0("v", n_st)) {
  alleles <- do.call(rbind, lapply(loci, function(l) {
    data.frame(locus = l, allele = 1:2, sequence = paste0(seqv(paste0(seq_tag, l)), c("A", "C")))
  }))
  profiles <- do.call(rbind, lapply(seq_len(n_st), function(st) {
    data.frame(st = st, locus = loci, allele = ((st - 1L) %% 2L) + 1L)
  }))
  list(
    scheme = list(loci = loci, profiles = profiles, alleles = alleles),
    info = list(name = name, version = version, source = "test")
  )
}

scheme_pair <- function(local = NULL, peer = NULL) {
  dir <- local_tempdir(.local_envir = parent.frame())
  paths <- c(local = file.path(dir, "local.db"), peer = file.path(dir, "peer.db"))
  build_db(paths[["local"]], default_local())
  build_db(paths[["peer"]], default_peer())
  if (!is.null(local)) store_mlst_scheme(paths[["local"]], local$scheme, local$info)
  if (!is.null(peer)) store_mlst_scheme(paths[["peer"]], peer$scheme, peer$info)
  suppressMessages(check_import_compatibility(paths[["local"]], paths[["peer"]]))
}

test_that("the scheme table list matches the one mlst_scheme.R writes", {
  expect_identical(impl$MLST_TABLES, MLST_SCHEME_TABLES)
})

test_that("no scheme on either side, or the same one, passes", {
  none <- scheme_pair()
  expect_identical(status_of(none, "Classical MLST scheme"), "pass")
  expect_identical(attr(none, "mlst_scheme")$relation, "none")

  same <- scheme_pair(scheme_fixture(), scheme_fixture())
  expect_identical(status_of(same, "Classical MLST scheme"), "pass")
  expect_identical(attr(same, "mlst_scheme")$relation, "identical")
  expect_false(attr(same, "mlst_scheme")$adopt_offer)
})

test_that("a scheme only the peer has is offered for adoption, ticked", {
  checks <- scheme_pair(NULL, scheme_fixture())
  rel <- attr(checks, "mlst_scheme")
  expect_identical(rel$relation, "ext_only")
  expect_true(rel$adopt_offer)
  expect_true(rel$adopt_default)
  expect_true(rel$classical_default)
  expect_false(attr(checks, "blocked"))
})

test_that("a scheme only the local database has is simply kept", {
  rel <- attr(scheme_pair(scheme_fixture(), NULL), "mlst_scheme")
  expect_identical(rel$relation, "local_only")
  expect_false(rel$adopt_offer)
})

test_that("a newer copy of the same scheme warns and is offered", {
  checks <- scheme_pair(scheme_fixture(2L), scheme_fixture(3L))
  rel <- attr(checks, "mlst_scheme")
  expect_identical(status_of(checks, "Classical MLST scheme"), "warn")
  expect_identical(rel$relation, "newer")
  expect_true(rel$adopt_offer)
  expect_true(rel$classical_default)
  expect_false(attr(checks, "blocked"))

  older <- attr(scheme_pair(scheme_fixture(3L), scheme_fixture(2L)), "mlst_scheme")
  expect_identical(older$relation, "older")
  expect_false(older$adopt_offer)
})

test_that("a different scheme warns, unticks classical results and never blocks", {
  renamed <- scheme_pair(scheme_fixture(), scheme_fixture(name = "Other MLST"))
  other_loci <- scheme_pair(scheme_fixture(), scheme_fixture(loci = c("adk", "rec")))
  resequenced <- scheme_pair(scheme_fixture(), scheme_fixture(seq_tag = "Z"))

  for (checks in list(renamed, other_loci, resequenced)) {
    rel <- attr(checks, "mlst_scheme")
    expect_identical(status_of(checks, "Classical MLST scheme"), "warn")
    expect_identical(rel$relation, "different")
    expect_false(rel$classical_default)
    expect_false(rel$adopt_offer)
    expect_false(attr(checks, "blocked"))
  }
})

test_that("a different AMR organism is an advisory row and only with AMR results", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local())
  build_db(peer, default_peer())
  for (x in list(c(local, "Escherichia"), c(peer, ""))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), x[1])
    DBI::dbExecute(con, "CREATE TABLE phylotrace_meta (key TEXT PRIMARY KEY, value TEXT)")
    DBI::dbExecute(con, "INSERT INTO phylotrace_meta VALUES ('amr_organism', ?)", list(x[2]))
    DBI::dbDisconnect(con)
  }

  expect_identical(status_of(check_import_compatibility(local, peer), "AMR organism"), "pass")

  seed_results(peer, "C", classical = FALSE, amr = TRUE)
  checks <- check_import_compatibility(local, peer)
  expect_identical(status_of(checks, "AMR organism"), "warn")
  expect_true(grepl("local setting is kept", checks$detail[checks$check == "AMR organism"]))
  expect_false(attr(checks, "blocked"))
})
