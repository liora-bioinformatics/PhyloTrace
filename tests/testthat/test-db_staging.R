box::use(
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_identical,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app /
    logic /
    db_staging[
      delete_imported_set,
      imported_metadata_wide,
      imported_profile_long,
      list_imported_sets,
      local_allele_map,
      resolve_profile,
      stage_profile_set,
      taken_isolate_names
    ],
  app /
    logic /
    profile_io[
      build_profile_table,
      format_profile,
      parse_profile_file,
      scheme_loci,
      write_delim_table
    ],
)

status_of <- function(checks, name) checks$status[checks$check == name]

# The local database knows isolates A and B; the "peer" ships C and D, built
# from the same scheme so their alleles are comparable.
peer_alleles <- function() {
  list(
    ref = ref_alleles(),
    C = c(g1 = seqv("A1"), g2 = seqv("C2"), g3 = seqv("R3")), # g1 shared with A
    D = c(g1 = seqv("D1"), g2 = seqv("C2"), g3 = seqv("D3"))
  )
}

# Write `alleles` out as a profile table and read it back the way an import would.
peer_profile <- function(dir, db, value_kind = "hash", preset = "phylotrace") {
  peer_db <- file.path(dir, "peer.db")
  if (!file.exists(peer_db)) build_db(peer_db, peer_alleles())

  f <- file.path(dir, paste0("peer_", value_kind, ".tsv"))
  write_delim_table(
    format_profile(build_profile_table(peer_db, c("C", "D"), value_kind), preset),
    f
  )
  parse_profile_file(f, scheme_loci(db))
}

fixture <- function(dir) {
  db <- file.path(dir, "local.db")
  build_db(db, default_local(), metadata = meta_df(c("A", "B")))
  db
}

test_that("local_allele_map lists every (gene, hash, seqid) the database knows", {
  dir <- local_tempdir()
  db <- fixture(dir)

  map <- local_allele_map(db)

  expect_setequal(names(map), c("gene", "hash", "seqid"))
  expect_true(all(nchar(map$hash) == 64L))
  # Each seqid belongs to exactly one gene — the property the whole design rests on.
  per_seqid <- tapply(map$gene, map$seqid, function(g) length(unique(g)))
  expect_true(all(per_seqid == 1L))
})

test_that("a hash profile is linkable with no sequences at all", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))

  expect_true(r$linkable)
  expect_false(attr(r$checks, "blocked"))
  expect_identical(status_of(r$checks, "Allele values"), "pass")
  expect_setequal(r$isolates, c("C", "D"))
  expect_setequal(names(r$long), c("isolate", "gene", "hash"))
})

test_that("the shared-allele rate measures how much of the peer we already know", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))

  # Of C and D's six calls, three are alleles the local database already holds
  # (C's g1 = A's g1, C's g3 = the reference's g3, D's g2 = C's g2 is novel).
  expect_true(r$shared_allele_rate > 0)
  expect_true(r$shared_allele_rate < 1)
})

test_that("a hash profile sharing nothing with us warns loudly", {
  dir <- local_tempdir()
  db <- fixture(dir)

  # A peer that hashes sequences differently than we do produces hashes we have
  # never seen — the distances would be meaningless, so this must not pass quietly.
  parsed <- peer_profile(dir, db, "hash")
  parsed$long$value <- vapply(
    parsed$long$value,
    function(h) as.character(openssl::sha256(paste0("different-convention-", h))),
    character(1)
  )

  r <- resolve_profile(db, parsed)

  expect_identical(status_of(r$checks, "Shared alleles"), "warn")
  expect_equal(r$shared_allele_rate, 0)
})

test_that("an integer profile from this database's lineage links", {
  dir <- local_tempdir()
  db <- fixture(dir)

  # Our own export of our OWN isolates: the integers are this database's seqids.
  f <- file.path(dir, "own.tsv")
  write_delim_table(
    format_profile(build_profile_table(db, c("A", "B"), "index"), "grapetree"),
    f
  )

  r <- resolve_profile(db, parse_profile_file(f, scheme_loci(db)))

  expect_true(r$linkable)
  expect_identical(status_of(r$checks, "Allele values"), "pass")
  expect_equal(r$shared_allele_rate, 1)
})

test_that("a foreign per-locus numbering is refused, not silently mis-linked", {
  dir <- local_tempdir()
  db <- fixture(dir)

  # This is what SeqSphere / chewBBACA / cgMLST.org emit: allele numbers that
  # restart at 1 for every locus. They say nothing about WHICH sequence they
  # mean, so they cannot be linked to our alleles.
  parsed <- peer_profile(dir, db, "hash")
  parsed$long$value <- as.character(
    ave(parsed$long$value, parsed$long$gene, FUN = function(x) match(x, unique(x)))
  )
  parsed$value_kind <- "index"

  r <- resolve_profile(db, parsed)

  expect_false(r$linkable)
  expect_true(attr(r$checks, "blocked"))
  expect_identical(status_of(r$checks, "Allele values"), "fail")
  expect_null(r$long)
})

test_that("a foreign profile becomes linkable once its sequences are supplied", {
  dir <- local_tempdir()
  db <- fixture(dir)

  parsed <- peer_profile(dir, db, "hash")
  hashes <- parsed$long$value
  # Renumber per locus (foreign convention) and ship the matching sequences.
  parsed$long$value <- as.character(
    ave(hashes, parsed$long$gene, FUN = function(x) match(x, unique(x)))
  )
  parsed$value_kind <- "index"

  peer <- file.path(dir, "peer.db")
  seqs <- qdf(
    peer,
    "SELECT m.gene AS gene, s.sequence AS sequence, h.hash AS hash
       FROM mlst m JOIN sequences s ON s.id = m.seqid
       JOIN hashes h ON h.id = m.seqid"
  )
  supplied <- data.frame(
    gene = parsed$long$gene,
    allele = parsed$long$value,
    sequence = seqs$sequence[match(hashes, seqs$hash)],
    stringsAsFactors = FALSE
  )
  supplied <- unique(supplied)

  r <- resolve_profile(db, parsed, supplied)

  expect_true(r$linkable)
  expect_identical(status_of(r$checks, "Sequences"), "pass")
  # The identities recovered from the sequences are the peer's real hashes.
  expect_setequal(unique(r$long$hash), unique(hashes))
})

test_that("a hash we cannot recompute is refused without sequences", {
  dir <- local_tempdir()
  db <- fixture(dir)

  parsed <- peer_profile(dir, db, "hash")
  parsed$long$value <- substr(parsed$long$value, 1, 8) # crc32-width
  parsed$value_kind <- "hash_other"

  r <- resolve_profile(db, parsed)

  expect_false(r$linkable)
  expect_identical(status_of(r$checks, "Allele values"), "fail")
})

test_that("staging writes the set and leaves the typing tables untouched", {
  dir <- local_tempdir()
  db <- fixture(dir)

  before <- lapply(
    c("mlst", "sequences", "hashes", "metadata"),
    function(t) qdf(db, sprintf("SELECT * FROM %s", t))
  )

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))
  sid <- stage_profile_set(db, "peer-lab", r, source_file = "peer.tsv")

  sets <- list_imported_sets(db)
  expect_equal(nrow(sets), 1L)
  expect_identical(sets$name, "peer-lab")
  expect_equal(sets$n_isolates, 2L)

  long <- imported_profile_long(db, sid)
  expect_setequal(unique(long$isolate), c("C", "D"))

  after <- lapply(
    c("mlst", "sequences", "hashes", "metadata"),
    function(t) qdf(db, sprintf("SELECT * FROM %s", t))
  )
  expect_identical(after, before)
})

test_that("a staged isolate may not collide with a local one", {
  dir <- local_tempdir()
  db <- fixture(dir)

  parsed <- peer_profile(dir, db, "hash")
  parsed$long$isolate[parsed$long$isolate == "C"] <- "A" # already local
  r <- resolve_profile(db, parsed)

  expect_error(stage_profile_set(db, "clash", r), "already in use")

  # …but a rename resolves it.
  sid <- stage_profile_set(db, "clash", r, renames = c(A = "A_ext"))
  expect_setequal(
    unique(imported_profile_long(db, sid)$isolate),
    c("A_ext", "D")
  )
})

test_that("taken_isolate_names covers local souches and already-staged sets", {
  dir <- local_tempdir()
  db <- fixture(dir)

  expect_setequal(taken_isolate_names(db), c("A", "B"))

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))
  stage_profile_set(db, "peer-lab", r)

  expect_setequal(taken_isolate_names(db), c("A", "B", "C", "D"))
  expect_false("ref" %in% taken_isolate_names(db))
})

test_that("set names must be unique and non-empty", {
  dir <- local_tempdir()
  db <- fixture(dir)
  r <- resolve_profile(db, peer_profile(dir, db, "hash"))

  stage_profile_set(db, "peer-lab", r)
  expect_error(stage_profile_set(db, "peer-lab", r), "already exists")
  expect_error(stage_profile_set(db, "   ", r), "name")
})

test_that("an unlinkable profile cannot be staged", {
  dir <- local_tempdir()
  db <- fixture(dir)

  parsed <- peer_profile(dir, db, "hash")
  parsed$value_kind <- "mixed"
  r <- resolve_profile(db, parsed)

  expect_error(stage_profile_set(db, "bad", r), "cannot be linked")
})

test_that("staged metadata comes back wide, tagged with its source", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))
  sid <- stage_profile_set(
    db,
    "peer-lab",
    r,
    metadata = data.frame(
      isolate = c("C", "D"),
      ward = c("W1", "W2"),
      stringsAsFactors = FALSE
    )
  )

  md <- imported_metadata_wide(db, sid)

  expect_setequal(md$isolate, c("C", "D"))
  expect_identical(unique(md$source), "peer-lab")
  expect_identical(md$ward[md$isolate == "C"], "W1")
})

test_that("deleting a set removes its rows and its orphaned sequences", {
  dir <- local_tempdir()
  db <- fixture(dir)

  r <- resolve_profile(db, peer_profile(dir, db, "hash"))
  sid <- stage_profile_set(db, "peer-lab", r)

  delete_imported_set(db, sid)

  expect_equal(nrow(list_imported_sets(db)), 0L)
  expect_equal(nrow(imported_profile_long(db, sid)), 0L)
  expect_equal(q1(db, "SELECT COUNT(*) FROM imported_sequences"), 0L)
  expect_setequal(taken_isolate_names(db), c("A", "B"))
})

test_that("nothing is staged when no set is asked for", {
  dir <- local_tempdir()
  db <- fixture(dir)

  expect_equal(nrow(list_imported_sets(db)), 0L)
  expect_equal(nrow(imported_profile_long(db, integer(0))), 0L)
  expect_null(imported_metadata_wide(db, integer(0)))
})
