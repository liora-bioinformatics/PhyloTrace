box::use(
  testthat[
    expect_equal,
    expect_false,
    expect_identical,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app /
    logic /
    db_sources[
      SOURCE_COL,
      SOURCE_LOCAL,
      db_uuid,
      list_sources,
      register_source,
      unique_source_label
    ],
  app / logic / db_import[METADATA_RESERVED, merge_databases],
  app / logic / database_functions[sync_metadata_table],
)

# `merge_databases()` and `hash_database()` are chatty; the messages are useful
# in the app but only noise here.
quiet <- function(expr) suppressMessages(expr)

con_to <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  con
}

# A database file with nothing but the registry in it: enough to exercise
# label allocation without building a whole scheme.
blank_db <- function(dir, name = "reg.db") {
  path <- file.path(dir, name)
  con <- con_to(path)
  DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
  DBI::dbDisconnect(con)
  path
}

with_uuid <- function(path, uuid) {
  con <- con_to(path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE phylotrace_meta (key TEXT, value TEXT)")
  DBI::dbExecute(
    con,
    "INSERT INTO phylotrace_meta (key, value) VALUES ('uuid', ?)",
    params = list(uuid)
  )
}

meta_source <- function(path) {
  con <- con_to(path)
  on.exit(DBI::dbDisconnect(con))
  df <- DBI::dbReadTable(con, "metadata")
  stats::setNames(df[[SOURCE_COL]], df$isolate)
}

# The columns `sync_metadata_table()` appends for a newly typed isolate. A
# metadata table missing any of them cannot take the append at all (an
# app-written table always has them), so the local fixture carries the full set.
FULL_META_EXTRA <- stats::setNames(
  rep(list(NA_character_), 7),
  c(
    "primary_laboratory_sample_id",
    "specimen_source_id",
    "geo_loc_name_state_province",
    "sample_collected_by",
    "sequence_submitted_by",
    "purpose_of_sampling",
    "purpose_of_sequencing"
  )
)

pair <- function(dir, peer_name = "peer.db") {
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, peer_name)
  build_db(
    local,
    default_local(),
    metadata = meta_df(c("A", "B"), extra = FULL_META_EXTRA)
  )
  build_db(peer, default_peer(), metadata = meta_df(c("B", "C")))
  list(local = local, peer = peer)
}

resolve <- function(isolate, action, final = isolate) {
  data.frame(
    ext_isolate = isolate,
    action = action,
    final_isolate = final,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Label allocation
# ---------------------------------------------------------------------------

test_that("a label is the file name without its extension", {
  expect_identical(unique_source_label("/data/partner-lab.db"), "partner-lab")
  expect_identical(unique_source_label("partner-lab.DB"), "partner-lab")
})

test_that("labels never collide, case included", {
  expect_identical(
    unique_source_label("peer.db", taken = "peer"),
    "peer (2)"
  )
  expect_identical(
    unique_source_label("peer.db", taken = c("peer", "peer (2)")),
    "peer (3)"
  )
  # A reader cannot tell "Peer" from "peer"; treat them as one.
  expect_identical(unique_source_label("Peer.db", taken = "peer"), "Peer (2)")
})

test_that("'local' is reserved for this database's own isolates", {
  expect_identical(unique_source_label("local.db"), "local (2)")
})

test_that("a nameless source still gets a label", {
  expect_identical(unique_source_label(NA_character_), "external database")
  expect_identical(unique_source_label(""), "external database")
})

# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

test_that("the same peer keeps one label, even after it is renamed on disk", {
  dir <- local_tempdir()
  con <- con_to(blank_db(dir))
  on.exit(DBI::dbDisconnect(con))

  first <- register_source(con, uuid = "uuid-1", file_name = "partner.db")
  again <- register_source(con, uuid = "uuid-1", file_name = "partner-2026.db")

  expect_identical(first, "partner")
  expect_identical(again, first)
  expect_identical(nrow(list_sources(con)), 1L)
})

test_that("two different databases sharing a file name get distinct labels", {
  dir <- local_tempdir()
  con <- con_to(blank_db(dir))
  on.exit(DBI::dbDisconnect(con))

  a <- register_source(con, uuid = "uuid-a", file_name = "peer.db")
  b <- register_source(con, uuid = "uuid-b", file_name = "peer.db")

  expect_identical(a, "peer")
  expect_identical(b, "peer (2)")
  expect_identical(nrow(list_sources(con)), 2L)
})

test_that("without a uuid the file name identifies the peer", {
  dir <- local_tempdir()
  con <- con_to(blank_db(dir))
  on.exit(DBI::dbDisconnect(con))

  a <- register_source(con, file_name = "/one/peer.db")
  b <- register_source(con, file_name = "/two/PEER.db")

  expect_identical(a, "peer")
  expect_identical(b, a)
})

test_that("labels already used by metadata rows are not handed out again", {
  dir <- local_tempdir()
  path <- blank_db(dir)
  con <- con_to(path)
  on.exit(DBI::dbDisconnect(con))

  # A database whose rows carry a label its registry never recorded - what a
  # restored export looks like.
  DBI::dbExecute(con, "CREATE TABLE metadata (isolate TEXT, source TEXT)")
  DBI::dbExecute(con, "INSERT INTO metadata VALUES ('A', 'peer')")

  expect_identical(
    register_source(con, uuid = "uuid-x", file_name = "peer.db"),
    "peer (2)"
  )
})

test_that("db_uuid reads phylotrace_meta and tolerates its absence", {
  dir <- local_tempdir()
  plain <- blank_db(dir, "plain.db")
  stamped <- blank_db(dir, "stamped.db")
  with_uuid(stamped, "abc123")

  con <- con_to(plain)
  on.exit(DBI::dbDisconnect(con))
  expect_true(is.na(db_uuid(con)))

  con2 <- con_to(stamped)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_identical(db_uuid(con2), "abc123")
})

# ---------------------------------------------------------------------------
# Provenance written through the real paths
# ---------------------------------------------------------------------------

test_that("sync_metadata_table stamps isolates it appends as local", {
  dir <- local_tempdir()
  path <- file.path(dir, "typed.db")
  # No metadata table at all: everything present was typed here.
  build_db(path, default_local(), metadata = NULL)

  md <- quiet(sync_metadata_table(path))

  expect_true(SOURCE_COL %in% names(md))
  expect_setequal(md[[SOURCE_COL]], SOURCE_LOCAL)
})

test_that("an isolate typed after a merge is stamped local, not the peer", {
  dir <- local_tempdir()
  p <- pair(dir)

  quiet(merge_databases(
    local_path = p$local,
    ext_path = p$peer,
    resolutions = resolve("C", "add"),
    backup = FALSE
  ))

  # Typing writes allele calls; the metadata row is backfilled on the next load.
  con <- con_to(p$local)
  DBI::dbExecute(
    con,
    "INSERT INTO mlst (souche, gene, seqid)
       SELECT 'D', gene, seqid FROM mlst WHERE souche = 'A'"
  )
  DBI::dbDisconnect(con)

  md <- quiet(sync_metadata_table(p$local))
  src <- stats::setNames(md[[SOURCE_COL]], md$isolate)

  expect_identical(unname(src[["D"]]), SOURCE_LOCAL)
  expect_identical(unname(src[["C"]]), "peer")
})

test_that("rows predating the column stay unknown rather than claiming local", {
  dir <- local_tempdir()
  path <- file.path(dir, "legacy.db")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))

  md <- quiet(sync_metadata_table(path))

  expect_true(SOURCE_COL %in% names(md))
  expect_true(all(is.na(md[[SOURCE_COL]])))
})

test_that("a merge stamps incoming isolates with the peer's label", {
  dir <- local_tempdir()
  p <- pair(dir)

  quiet(merge_databases(
    local_path = p$local,
    ext_path = p$peer,
    resolutions = resolve("C", "add"),
    backup = FALSE
  ))

  src <- meta_source(p$local)
  expect_identical(unname(src[["C"]]), "peer")
  # The local rows are none of the import's business.
  expect_true(all(is.na(src[c("A", "B")])))
})

test_that("the label names the file the user chose, not a staged copy", {
  dir <- local_tempdir()
  p <- pair(dir)
  staged <- file.path(dir, "phylotrace_import_deadbeef.db")
  file.copy(p$peer, staged)

  res <- quiet(merge_databases(
    local_path = p$local,
    ext_path = staged,
    resolutions = resolve("C", "add"),
    backup = FALSE,
    source_file = p$peer
  ))

  expect_identical(res$source, "peer")
  expect_identical(unname(meta_source(p$local)[["C"]]), "peer")
})

test_that("an overwritten isolate takes on the peer's label", {
  dir <- local_tempdir()
  p <- pair(dir)

  quiet(merge_databases(
    local_path = p$local,
    ext_path = p$peer,
    resolutions = resolve("B", "overwrite"),
    backup = FALSE
  ))

  src <- meta_source(p$local)
  expect_identical(unname(src[["B"]]), "peer")
  expect_true(is.na(src[["A"]]))
})

test_that("merging the same peer twice reuses its label", {
  dir <- local_tempdir()
  p <- pair(dir)

  first <- quiet(merge_databases(
    local_path = p$local,
    ext_path = p$peer,
    resolutions = resolve("C", "add"),
    backup = FALSE
  ))
  second <- quiet(merge_databases(
    local_path = p$local,
    ext_path = p$peer,
    resolutions = resolve("B", "overwrite"),
    backup = FALSE
  ))

  expect_identical(second$source, first$source)

  con <- con_to(p$local)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(nrow(list_sources(con)), 1L)
})

test_that("a peer's own source values are never carried over", {
  dir <- local_tempdir()
  local <- file.path(dir, "local.db")
  peer <- file.path(dir, "peer.db")
  build_db(local, default_local(), metadata = meta_df(c("A", "B")))
  build_db(
    peer,
    default_peer(),
    # The peer imported C from somewhere else once; that is its history.
    metadata = meta_df(c("B", "C"), extra = list(source = "far-away-lab"))
  )

  expect_false(SOURCE_COL %in% setdiff(c("source"), METADATA_RESERVED))

  quiet(merge_databases(
    local_path = local,
    ext_path = peer,
    resolutions = resolve("C", "add"),
    # Even asked for explicitly, the column is reserved.
    metadata_cols = c("sample_collection_date", SOURCE_COL),
    backup = FALSE
  ))

  expect_identical(unname(meta_source(local)[["C"]]), "peer")
})
