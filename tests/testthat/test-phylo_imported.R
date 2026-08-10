# Folding staged peer profiles into the distance matrix.
#
# The load-bearing claim is that this changes NOTHING for a database with no
# staged sets, and that a staged isolate lands at exactly the distance its
# alleles imply.

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
    phylo[
      compute_dist_matrix,
      compute_mst,
      compute_phylo_tree,
      hamming_dist_ignore,
      load_allele_profile
    ],
  app / logic / db_staging[resolve_profile, stage_profile_set],
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

# Six isolates, enough to build a tree and an MST over.
many_alleles <- function() {
  list(
    ref = ref_alleles(),
    A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
    B = c(g1 = seqv("B1"), g2 = seqv("A2"), g3 = seqv("A3")),
    C = c(g1 = seqv("C1"), g2 = seqv("C2"), g3 = seqv("A3")),
    D = c(g1 = seqv("D1"), g2 = seqv("C2"), g3 = seqv("D3"))
  )
}

fixture <- function(dir) {
  db <- file.path(dir, "local.db")
  build_db(db, many_alleles(), metadata = meta_df(c("A", "B", "C", "D")))
  db
}

# Export `isolates` as a hash profile and stage it back under `_ext` names.
stage_clone <- function(dir, db, isolates, name = "peer") {
  f <- file.path(dir, paste0(name, ".tsv"))
  write_delim_table(
    format_profile(build_profile_table(db, isolates, "hash"), "phylotrace"),
    f
  )
  r <- resolve_profile(db, parse_profile_file(f, scheme_loci(db)))
  stage_profile_set(
    db,
    name,
    r,
    renames = stats::setNames(paste0(isolates, "_ext"), isolates)
  )
}

test_that("with no staged sets the profile is byte-for-byte what it always was", {
  dir <- local_tempdir()
  db <- fixture(dir)

  base <- load_allele_profile(db)

  expect_identical(load_allele_profile(db, imported_sets = NULL), base)
  expect_identical(load_allele_profile(db, imported_sets = integer(0)), base)
  # Still the integer seqid matrix the distance kernels expect.
  expect_identical(storage.mode(base), "integer")
  expect_setequal(rownames(base), c("A", "B", "C", "D"))
  expect_false("ref" %in% rownames(base))
})

test_that("a NULL isolate list resolves against the database, not the caller", {
  # Documents the contract that made the app's tree leak. NULL is legitimate
  # here - "every isolate in this database" is a reasonable library default -
  # but it is resolved from `mlst` at call time, so it is only ever as current
  # as the file. An isolate written by another process a moment ago is in it.
  #
  # That is why app/view/visualization_plot.R resolves "all isolates" to a
  # concrete vector before calling in (see its `engine_isolates`): during a
  # typing run the two answers differ, and the UI's is the one the plot's
  # labels come from.
  dir <- local_tempdir()
  db <- fixture(dir)

  before <- rownames(load_allele_profile(db))

  # Another writer adds an isolate to `mlst` behind our back.
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(
    con,
    "INSERT INTO mlst (souche, gene, seqid)
       SELECT 'MID_RUN', gene, seqid FROM mlst WHERE souche = 'A'"
  )
  DBI::dbDisconnect(con)

  # NULL picks it up immediately...
  expect_true("MID_RUN" %in% rownames(load_allele_profile(db)))
  # ...while an explicit list is bounded by what the caller asked for.
  expect_setequal(rownames(load_allele_profile(db, isolates = before)), before)
})

test_that("staging a set does not disturb the local profile or its distances", {
  dir <- local_tempdir()
  db <- fixture(dir)

  before <- load_allele_profile(db)
  d_before <- compute_dist_matrix(before, hamming_dist_ignore)

  stage_clone(dir, db, c("A", "B"))

  # A staged set is inert until it is asked for.
  expect_identical(load_allele_profile(db), before)

  after <- load_allele_profile(db, imported_sets = 1L)
  locals <- rownames(before)
  expect_identical(after[locals, , drop = FALSE], before)
  expect_identical(
    compute_dist_matrix(after[locals, , drop = FALSE], hamming_dist_ignore),
    d_before
  )
})

test_that("a staged clone sits at distance zero from the isolate it copies", {
  dir <- local_tempdir()
  db <- fixture(dir)
  stage_clone(dir, db, c("A", "B", "C", "D"))

  p <- load_allele_profile(db, imported_sets = 1L)
  expect_equal(nrow(p), 8L)

  d <- compute_dist_matrix(p, hamming_dist_ignore)
  dimnames(d) <- list(rownames(p), rownames(p))

  for (iso in c("A", "B", "C", "D")) {
    expect_equal(d[iso, paste0(iso, "_ext")], 0, info = iso)
  }
  # …and its distance to every OTHER isolate is the original's distance.
  expect_equal(d["A_ext", "C"], d["A", "C"])
  expect_equal(d["B_ext", "D_ext"], d["B", "D"])
})

test_that("an allele we have never seen is novel, not missing", {
  dir <- local_tempdir()
  db <- fixture(dir)

  # A peer isolate carrying an allele absent from our database. It must differ
  # from every local allele at that locus — Hamming compares identity, so a
  # brand-new hash is simply a code we have not used before.
  peer <- file.path(dir, "peer.db")
  build_db(
    peer,
    list(
      ref = ref_alleles(),
      X = c(g1 = seqv("NOVEL"), g2 = seqv("A2"), g3 = seqv("A3"))
    )
  )
  f <- file.path(dir, "peer.tsv")
  write_delim_table(
    format_profile(build_profile_table(peer, "X", "hash"), "phylotrace"),
    f
  )
  r <- resolve_profile(db, parse_profile_file(f, scheme_loci(db)))
  stage_profile_set(db, "peer", r)

  p <- load_allele_profile(db, imported_sets = 1L)
  d <- compute_dist_matrix(p, hamming_dist_ignore)
  dimnames(d) <- list(rownames(p), rownames(p))

  # X shares g2 and g3 with A, and its g1 is novel -> exactly one difference.
  expect_equal(d["X", "A"], 1)
  # The novel allele got a code of its own, above the local id space.
  expect_true(p["X", "g1"] > max(p[c("A", "B", "C", "D"), "g1"]))
})

test_that("two staged isolates sharing a novel allele share its code", {
  dir <- local_tempdir()
  db <- fixture(dir)

  peer <- file.path(dir, "peer.db")
  novel <- seqv("NOVEL")
  build_db(
    peer,
    list(
      ref = ref_alleles(),
      X = c(g1 = novel, g2 = seqv("A2"), g3 = seqv("A3")),
      Y = c(g1 = novel, g2 = seqv("A2"), g3 = seqv("A3"))
    )
  )
  f <- file.path(dir, "peer.tsv")
  write_delim_table(
    format_profile(build_profile_table(peer, c("X", "Y"), "hash"), "phylotrace"),
    f
  )
  r <- resolve_profile(db, parse_profile_file(f, scheme_loci(db)))
  stage_profile_set(db, "peer", r)

  p <- load_allele_profile(db, imported_sets = 1L)
  expect_identical(p["X", "g1"], p["Y", "g1"])

  d <- compute_dist_matrix(p, hamming_dist_ignore)
  dimnames(d) <- list(rownames(p), rownames(p))
  expect_equal(d["X", "Y"], 0)
})

test_that("the MST folds a staged clone into its original's node", {
  dir <- local_tempdir()
  db <- fixture(dir)

  before <- compute_mst(db, "ignore_na")
  n_before <- igraph::vcount(before)

  stage_clone(dir, db, c("A", "B", "C", "D"))
  after <- compute_mst(db, "ignore_na", imported_sets = 1L)

  # Identical profiles collapse into one node, so adding exact copies of every
  # isolate must not add a single node.
  expect_equal(igraph::vcount(after), n_before)
  # Each node now represents twice as many samples.
  expect_identical(igraph::V(after)$n, igraph::V(before)$n * 2L)
})

test_that("the Tree gains the staged isolates as tips", {
  dir <- local_tempdir()
  db <- fixture(dir)
  stage_clone(dir, db, c("A", "B", "C"))

  tree <- compute_phylo_tree(db, "ignore_na", "Neighbour-Joining", imported_sets = 1L)

  expect_setequal(
    tree$tip.label,
    c("A", "B", "C", "D", "A_ext", "B_ext", "C_ext")
  )
})

test_that("the isolate selection still subsets a profile with staged sets", {
  dir <- local_tempdir()
  db <- fixture(dir)
  stage_clone(dir, db, c("A", "B"))

  p <- load_allele_profile(
    db,
    isolates = c("A", "A_ext"),
    imported_sets = 1L
  )

  expect_setequal(rownames(p), c("A", "A_ext"))
})
