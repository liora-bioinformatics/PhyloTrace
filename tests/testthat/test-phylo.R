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
    expect_true,
    test_that
  ],
  withr[local_tempdir],
)
box::use(
  app / logic / phylo[compute_dist_matrix, hamming_dist_ignore, load_allele_profile],
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
  d <- compute_dist_matrix(mat, hamming_dist_ignore)
  dimnames(d) <- list(rownames(mat), rownames(mat))

  # A and D differ only at the locus that was scored missing.
  expect_equal(d["A", "D"], 0)
  expect_equal(d["A", "B"], 1)
})

test_that("a clean database is untouched by the ambiguity check", {
  db <- file.path(local_tempdir(), "clean.db")
  build_db(db, default_local())

  mat <- load_allele_profile(db)

  expect_false(any(is.na(mat)))
  expect_equal(dim(mat), c(2L, 3L))
})
