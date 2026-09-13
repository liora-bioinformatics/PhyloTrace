# Allele profile extraction from the `mlst` table.
#
# The load-bearing claim is that a genome carrying two alleles at one locus -
# which pyMLST's keyless `mlst` table permits and real assemblies produce -
# scores that one locus missing for that one isolate, and leaves every other
# cell of the matrix and every other isolate untouched.

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
  app /
    logic /
    phylo[
      compute_dist_matrix,
      compute_mst,
      hamming_dist,
      hamming_dist_category,
      hamming_dist_ignore,
      load_allele_profile,
      mst_from_distance,
      tree_from_distance,
    ],
)

# A/B/C are clean; D carries a second, different allele at g2.
paralog_alleles <- function() {
  list(
    ref = ref_alleles(),
    A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
    B = c(g1 = seqv("B1"), g2 = seqv("A2"), g3 = seqv("A3")),
    C = c(g1 = seqv("C1"), g2 = seqv("A2"), g3 = seqv("A3")),
    D = c(g1 = seqv("A1"), g2 = seqv("A2"), g2 = seqv("D2"), g3 = seqv("A3"))
  )
}

test_that("a duplicated locus does not break the whole profile matrix", {
  db <- file.path(local_tempdir(), "paralog.db")
  build_db(db, paralog_alleles())

  mat <- load_allele_profile(db)

  expect_identical(storage.mode(mat), "integer")
  expect_equal(dim(mat), c(4L, 3L))
  expect_true(all(c("A", "B", "C", "D") %in% rownames(mat)))
})

test_that("only the ambiguous cell goes missing", {
  db <- file.path(local_tempdir(), "paralog.db")
  build_db(db, paralog_alleles())

  mat <- load_allele_profile(db)

  expect_true(is.na(mat["D", "g2"]))
  expect_equal(sum(is.na(mat)), 1L)
  # D still carries A's g1 and g3, so the surviving loci are intact.
  expect_equal(unname(mat["D", "g1"]), unname(mat["A", "g1"]))
  expect_equal(unname(mat["D", "g3"]), unname(mat["A", "g3"]))
})

test_that("the ambiguous locus is ignored rather than counted as a difference", {
  db <- file.path(local_tempdir(), "paralog.db")
  build_db(db, paralog_alleles())

  mat <- load_allele_profile(db)
  d <- compute_dist_matrix(mat, "ignore_na")

  # A and D differ only at the locus that was scored missing.
  expect_equal(d["A", "D"], 0)
  expect_equal(d["A", "B"], 1)
})

# --- The vectorised distance matrix ------------------------------------------

# The per-pair reference kernel applied the slow, obvious way.
pairwise_reference <- function(mat, kernel) {
  n <- nrow(mat)
  d <- matrix(0L, n, n, dimnames = list(rownames(mat), rownames(mat)))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d[i, j] <- as.integer(kernel(mat[i, ], mat[j, ]))
    }
  }
  d
}

# Small allele codes reused across loci, plenty of missing calls, a column
# missing for everyone and a pair of identical profiles.
awkward_profile <- function() {
  set.seed(42)
  mat <- matrix(sample(1:4, 25 * 40, replace = TRUE), 25, 40)
  mat[sample(length(mat), 150)] <- NA
  mat[, 7] <- NA
  mat[2, ] <- mat[1, ]
  storage.mode(mat) <- "integer"
  rownames(mat) <- sprintf("iso%02d", seq_len(nrow(mat)))
  mat
}

test_that("ignore_na matches the pairwise kernel exactly", {
  mat <- awkward_profile()
  expect_identical(
    compute_dist_matrix(mat, "ignore_na"),
    pairwise_reference(mat, hamming_dist_ignore)
  )
})

test_that("category matches the pairwise kernel exactly", {
  mat <- awkward_profile()
  expect_identical(
    compute_dist_matrix(mat, "category"),
    pairwise_reference(mat, hamming_dist_category)
  )
})

test_that("omit matches the plain kernel over the loci called everywhere", {
  mat <- awkward_profile()
  mat[, 1:10][is.na(mat[, 1:10])] <- 1L
  complete <- mat[, colSums(is.na(mat)) == 0, drop = FALSE]
  expect_true(ncol(complete) > 0)
  expect_identical(
    compute_dist_matrix(mat, "omit"),
    pairwise_reference(complete, hamming_dist)
  )
})

test_that("an allele code shared by two loci is not a shared allele", {
  mat <- matrix(c(1L, 2L, 2L, 1L), 2, 2, byrow = TRUE, dimnames = list(c("A", "B"), NULL))
  expect_equal(compute_dist_matrix(mat)["A", "B"], 2)
})

test_that("degenerate profiles give an all-zero matrix of the right shape", {
  one <- matrix(1:3, 1, 3, dimnames = list("A", NULL))
  expect_identical(compute_dist_matrix(one), matrix(0L, 1, 1, dimnames = list("A", "A")))

  all_missing <- matrix(NA_integer_, 3, 2, dimnames = list(c("A", "B", "C"), NULL))
  expect_true(all(compute_dist_matrix(all_missing, "ignore_na") == 0L))
  expect_true(all(compute_dist_matrix(all_missing, "omit") == 0L))
  expect_true(all(compute_dist_matrix(all_missing, "category") == 0L))
})

# --- Building from a distance matrix -----------------------------------------

test_that("tree_from_distance needs three isolates and labels tips by row", {
  mat <- awkward_profile()[1:5, ]
  d <- compute_dist_matrix(mat)

  expect_null(tree_from_distance(d[1:2, 1:2]))
  expect_null(tree_from_distance(NULL))
  tree <- tree_from_distance(d)
  expect_identical(sort(tree$tip.label), sort(rownames(mat)))
})

test_that("mst_from_distance merges identical profiles and reads weights off the matrix", {
  mat <- awkward_profile()[1:6, ]
  d <- compute_dist_matrix(mat)
  graph <- mst_from_distance(d)

  # iso01 and iso02 are identical, so six isolates make five nodes.
  expect_equal(igraph::vcount(graph), 5L)
  expect_true("iso01\niso02" %in% igraph::V(graph)$name)
  expect_equal(sum(igraph::V(graph)$n), 6L)
  expect_null(mst_from_distance(d[1, 1, drop = FALSE]))

  # Every MST edge carries the distance between the nodes' first members.
  ends <- igraph::ends(graph, igraph::E(graph))
  first <- function(name) sub("\n.*", "", name)
  expect_equal(
    igraph::E(graph)$weight,
    as.numeric(d[cbind(first(ends[, 1]), first(ends[, 2]))])
  )
})

test_that("compute_mst still works end to end from the database", {
  db <- file.path(local_tempdir(), "paralog.db")
  build_db(db, paralog_alleles())

  # A and D sit at distance zero once the ambiguous locus is scored missing.
  expect_equal(igraph::vcount(compute_mst(db, "ignore_na")), 3L)
})

test_that("a clean database is untouched by the ambiguity check", {
  db <- file.path(local_tempdir(), "clean.db")
  build_db(db, default_local())

  mat <- load_allele_profile(db)

  expect_false(any(is.na(mat)))
  expect_equal(dim(mat), c(2L, 3L))
})
