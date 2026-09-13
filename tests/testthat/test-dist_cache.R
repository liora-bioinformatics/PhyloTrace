# The session distance cache.
#
# The load-bearing claims: whatever the cache hands back is identical to a fresh
# computation over the same request, and it stops handing anything back the
# moment the revision bus announces a write that could change a profile.

box::use(
  testthat[expect_equal, expect_identical, expect_null, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / db_events[bump, new_bus],
  app / logic / dist_cache[new_dist_cache],
  app /
    logic /
    phylo[
      compute_distance,
      compute_mst,
      compute_phylo_tree,
      mst_from_distance,
      tree_from_distance,
    ],
)

# D has no call at g3, so `omit` over a set containing D drops that locus.
fixture <- function() {
  db <- file.path(local_tempdir(.local_envir = parent.frame()), "four.db")
  build_db(
    db,
    list(
      ref = ref_alleles(),
      A = c(g1 = seqv("A1"), g2 = seqv("A2"), g3 = seqv("A3")),
      B = c(g1 = seqv("B1"), g2 = seqv("A2"), g3 = seqv("A3")),
      C = c(g1 = seqv("C1"), g2 = seqv("C2"), g3 = seqv("C3")),
      D = c(g1 = seqv("D1"), g2 = seqv("C2"))
    )
  )
  db
}

# A cache over the real computation that counts how often it had to compute.
counted_cache <- function(bus = new_bus(), ...) {
  calls <- 0L
  cache <- new_dist_cache(
    bus,
    compute = function(...) {
      calls <<- calls + 1L
      compute_distance(...)
    },
    ...
  )
  list(cache = cache, calls = function() calls)
}

all_iso <- c("A", "B", "C", "D")

test_that("a repeated request is served from memory and equals a fresh computation", {
  db <- fixture()
  cc <- counted_cache()

  first <- cc$cache$get(db, "ignore_na", all_iso)
  second <- cc$cache$get(db, "ignore_na", all_iso)

  expect_identical(cc$calls(), 1L)
  expect_identical(second, first)
  expect_identical(first, compute_distance(db, "ignore_na", all_iso))
})

test_that("a subset is sliced from the matrix already held", {
  db <- fixture()
  cc <- counted_cache()

  cc$cache$get(db, "category", all_iso)
  sub <- cc$cache$get(db, "category", c("C", "A"))

  expect_identical(cc$calls(), 1L)
  expect_identical(sub, compute_distance(db, "category", c("C", "A")))
})

test_that("isolates beyond the held matrix grow it to the union", {
  db <- fixture()
  cc <- counted_cache()

  cc$cache$get(db, "ignore_na", c("A", "B"))
  cc$cache$get(db, "ignore_na", c("C", "D"))
  all <- cc$cache$get(db, "ignore_na", all_iso)

  expect_identical(cc$calls(), 2L)
  expect_identical(all, compute_distance(db, "ignore_na", all_iso))
})

test_that("omit is held per exact selection, never sliced", {
  db <- fixture()
  cc <- counted_cache()

  with_d <- cc$cache$get(db, "omit", all_iso)
  without_d <- cc$cache$get(db, "omit", c("A", "B", "C"))

  expect_identical(cc$calls(), 2L)
  # Without D, g3 is back in and separates A from C, so the subset's distances
  # are not a slice of the four-isolate matrix.
  expect_identical(without_d, compute_distance(db, "omit", c("A", "B", "C")))
  expect_equal(with_d["A", "C"], 2)
  expect_equal(without_d["A", "C"], 3)
})

test_that("a tree and MST over a sliced subselection equal ones computed for it alone", {
  # Few alleles over three loci make many tied distances, which is where the
  # input order a tree is built from shows up in its topology. The selection
  # comes in scrambled, as engine_isolates hands it over.
  set.seed(3)
  isolates <- sprintf("I%02d", 1:14)
  alleles <- lapply(isolates, function(i) {
    c(
      g1 = seqv(sample(c("A", "B"), 1)),
      g2 = seqv(sample(c("A", "B", "C"), 1)),
      g3 = seqv(sample(c("A", "B"), 1))
    )
  })
  db <- file.path(local_tempdir(), "tied.db")
  build_db(db, c(list(ref = ref_alleles()), stats::setNames(alleles, isolates)))
  subset <- sample(isolates, 9)

  for (na in c("ignore_na", "category")) {
    cc <- counted_cache()
    cc$cache$get(db, na, isolates)
    sliced <- cc$cache$get(db, na, subset)
    expect_identical(cc$calls(), 1L)

    for (algo in c("Neighbour-Joining", "UPGMA")) {
      expect_identical(
        tree_from_distance(sliced, algo),
        compute_phylo_tree(db, na, algo, subset),
        info = paste(na, algo)
      )
    }
    direct <- compute_mst(db, na, subset)
    cached <- mst_from_distance(sliced)
    expect_identical(igraph::V(cached)$name, igraph::V(direct)$name, info = na)
    expect_identical(igraph::as_edgelist(cached), igraph::as_edgelist(direct), info = na)
    expect_identical(igraph::E(cached)$weight, igraph::E(direct)$weight, info = na)
  }
})

test_that("each missing-value policy keeps its own matrix", {
  db <- fixture()
  cc <- counted_cache()

  cc$cache$get(db, "ignore_na", all_iso)
  cc$cache$get(db, "category", all_iso)
  cc$cache$get(db, "ignore_na", all_iso)
  cc$cache$get(db, "category", all_iso)

  expect_identical(cc$calls(), 2L)
})

test_that("an announced profile write invalidates, a metadata edit does not", {
  db <- fixture()
  bus <- new_bus()
  cc <- counted_cache(bus)

  cc$cache$get(db, "ignore_na", all_iso)
  bump(bus, "metadata")
  cc$cache$get(db, "ignore_na", all_iso)
  expect_identical(cc$calls(), 1L)

  for (domain in c("isolates", "schema", "staged")) {
    bump(bus, domain)
    cc$cache$get(db, "ignore_na", all_iso)
  }
  expect_identical(cc$calls(), 4L)
})

test_that("a NULL selection is resolved live, never cached", {
  db <- fixture()
  cc <- counted_cache()

  cc$cache$get(db, "ignore_na", NULL)
  cc$cache$get(db, "ignore_na", NULL)

  expect_identical(cc$calls(), 2L)
})

test_that("the least recently used matrix is evicted past max_entries", {
  db <- fixture()
  cc <- counted_cache(max_entries = 2L)

  cc$cache$get(db, "ignore_na", all_iso)
  cc$cache$get(db, "category", all_iso)
  cc$cache$get(db, "ignore_na", all_iso)
  cc$cache$get(db, "omit", all_iso)
  expect_identical(cc$calls(), 3L)

  # ignore_na was touched after category, so category is the one evicted.
  cc$cache$get(db, "ignore_na", all_iso)
  expect_identical(cc$calls(), 3L)
  cc$cache$get(db, "category", all_iso)
  expect_identical(cc$calls(), 4L)
})

test_that("clear drops everything, and an empty database yields NULL", {
  db <- fixture()
  cc <- counted_cache()

  cc$cache$get(db, "ignore_na", all_iso)
  cc$cache$clear()
  cc$cache$get(db, "ignore_na", all_iso)
  expect_identical(cc$calls(), 2L)

  expect_null(cc$cache$get(db, "ignore_na", "NOT_THERE"))
})
