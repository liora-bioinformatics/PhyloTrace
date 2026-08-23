box::use(
  igraph[graph_from_data_frame, set_edge_attr],
  rlang[`%||%`],
  testthat[
    expect_equal,
    expect_false,
    expect_gt,
    expect_identical,
    expect_lt,
    expect_named,
    expect_true,
    test_that,
  ],
)
box::use(
  app / logic / mst_plot,
)

impl <- attr(mst_plot, "namespace")

# --- fixtures ----------------------------------------------------------------

# An MST as compute_mst() hands it over: a weighted undirected igraph whose
# vertex names are newline-joined isolate lists (that is how zero-distance
# isolates are merged, and it is also where the per-node isolate count comes
# from).
mst_graph <- function(from, to, weight, ids = NULL) {
  ids <- ids %||% unique(c(from, to))
  g <- graph_from_data_frame(
    data.frame(from = from, to = to, stringsAsFactors = FALSE),
    directed = FALSE,
    vertices = data.frame(name = ids, stringsAsFactors = FALSE)
  )
  set_edge_attr(g, "weight", value = weight)
}

# A star with one merged centre, one long branch and two short ones.
demo_graph <- function() {
  mst_graph(
    from = c("a", "a", "a", "d"),
    to = c("b\nc", "d", "e", "f"),
    weight = c(1, 3, 400, 2),
    ids = c("a", "b\nc", "d", "e", "f")
  )
}

demo_meta <- function() {
  data.frame(
    isolate = c("a", "b", "c", "d", "e", "f"),
    country = c("Kenya", "Kenya", "Peru", "Peru", NA, "Chile"),
    year = c(2019, 2020, 2021, 2021, 2022, NA),
    stringsAsFactors = FALSE
  )
}

# Randomly shaped trees, including the two shapes an equal-angle layout is most
# likely to fold onto itself: a path (every node has one child) and a star.
random_tree <- function(n, shape) {
  from <- switch(
    shape,
    star = rep(1L, n - 1L),
    chain = seq_len(n - 1L),
    binary = (2:n) %/% 2L,
    random = vapply(2:n, function(i) sample.int(i - 1L, 1L), integer(1))
  )
  data.frame(from = paste0("n", from), to = paste0("n", 2:n),
             stringsAsFactors = FALSE)
}

# --- 1. Edge length model ----------------------------------------------------

test_that("the median edge is drawn at the base length in every transform", {
  w <- c(1, 1, 2, 3, 5, 40, 3145)
  for (mode in c("log", "real", "uniform")) {
    len <- mst_plot$mst_edge_lengths(w, mode, spread = 15, shorten = FALSE)$length
    # Whatever the transform, the middle of the distribution lands in the same
    # place, so switching transform re-proportions the drawing without resizing
    # it.
    expect_equal(stats::median(len), impl$MST_BASE_EDGE_PX, tolerance = 1e-8)
  }
})

# The defect this model exists to fix: log(1) is 0, so every single-allele
# difference used to be drawn at length zero and its two isolates landed on top
# of each other.
test_that("no edge is ever drawn at zero length", {
  expect_true(all(mst_plot$mst_edge_lengths(c(1, 1, 1, 2), "log")$length > 0))
  # A one-allele edge among mostly distant ones is where the transform would
  # otherwise vanish it: log1p(1) is a twentieth of log1p(600), and before the
  # floor that was a zero-length edge with two isolates stacked on it.
  len <- mst_plot$mst_edge_lengths(c(1, 500, 600, 700), "log")$length
  expect_equal(min(len), impl$MST_BASE_EDGE_PX * impl$MST_MIN_EDGE_FRAC)
})

test_that("a branch too long to draw to scale is capped and reported", {
  out <- mst_plot$mst_edge_lengths(c(1, 2, 2, 3, 5000), "real", shorten = TRUE)
  expect_identical(out$shortened, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(max(out$length), impl$MST_BASE_EDGE_PX * impl$MST_MAX_EDGE_MULT)
  # Nothing is capped when the caller says not to, and then the drawing really
  # is to scale — at whatever size that takes.
  loose <- mst_plot$mst_edge_lengths(c(1, 2, 2, 3, 5000), "real", shorten = FALSE)
  expect_false(any(loose$shortened))
  expect_gt(max(loose$length), impl$MST_BASE_EDGE_PX * impl$MST_MAX_EDGE_MULT)
})

test_that("the cap multiplier is configurable, not fixed at MST_MAX_EDGE_MULT", {
  weights <- c(1, 2, 2, 3, 5000)
  tight <- mst_plot$mst_edge_lengths(weights, "real", shorten = TRUE, cap_mult = 3)
  expect_equal(max(tight$length), impl$MST_BASE_EDGE_PX * 3)
  expect_identical(tight$shortened, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # A weight now capped at 3x that was not capped at the default 7x.
  loose <- mst_plot$mst_edge_lengths(weights, "real", shorten = TRUE)
  expect_lt(sum(loose$shortened), sum(tight$shortened) + 1L)
  expect_gt(max(loose$length), max(tight$length))
  # A cap below 1 is nonsensical (nothing would ever draw to scale), so it is
  # floored rather than honoured.
  floored <- mst_plot$mst_edge_lengths(weights, "real", shorten = TRUE, cap_mult = 0)
  expect_equal(max(floored$length), impl$MST_BASE_EDGE_PX)
})

test_that("uniform lengths ignore the weights entirely", {
  len <- mst_plot$mst_edge_lengths(c(1, 17, 900), "uniform")$length
  expect_equal(length(unique(len)), 1L)
})

test_that("spread scales the whole drawing and is bounded", {
  base <- mst_plot$mst_edge_lengths(c(1, 2, 4), "log", spread = 15)$length
  wide <- mst_plot$mst_edge_lengths(c(1, 2, 4), "log", spread = 30)$length
  expect_equal(wide, base * 2)
  # Past the clamp the numbers stop growing, so no control value can produce a
  # drawing no canvas can hold.
  huge <- mst_plot$mst_edge_lengths(c(1, 2, 4), "log", spread = 1e6)$length
  expect_equal(huge, base * 4)
})

test_that("the transform is chosen from the spread of the weights", {
  expect_identical(mst_plot$mst_length_mode(c(1, 2, 3, 4)), "real")
  expect_identical(mst_plot$mst_length_mode(c(1, 2, 3, 3145)), "log")
  expect_identical(mst_plot$mst_length_mode(numeric(0)), "uniform")
})

# --- 2. Layout ---------------------------------------------------------------

# The crossing counter is what every planarity claim below rests on, so it is
# checked against a drawing that certainly does cross before it is trusted to
# report that others do not.
test_that("the crossing counter finds a crossing that is really there", {
  coords <- data.frame(
    id = c("a", "b", "c", "d"),
    x = c(-1, 1, 0, 0),
    y = c(0, 0, -1, 1),
    stringsAsFactors = FALSE
  )
  expect_identical(
    mst_plot$mst_count_crossings(coords, c("a", "c"), c("b", "d")),
    1L
  )
  # Two segments that share an endpoint meet by construction and are not a
  # crossing.
  expect_identical(
    mst_plot$mst_count_crossings(coords, c("a", "a"), c("b", "c")),
    0L
  )
})

test_that("the layout never crosses an edge, whatever the shape or transform", {
  set.seed(11)
  for (shape in c("star", "chain", "binary", "random")) {
    for (n in c(2L, 5L, 25L, 80L)) {
      edges <- random_tree(n, shape)
      w <- sample(c(1, 1, 2, 3, 8, 60, 400, 3000), nrow(edges), replace = TRUE)
      ids <- paste0("n", seq_len(n))
      # Both fans: the plain proportional one, and the one that re-allocates
      # angle to the branches leaving a cluster. Reordering a wedge and
      # re-sharing it out keeps the wedges contiguous and disjoint, so it must
      # not cost the planarity everything here rests on — but "must not" is
      # worth what the test is worth.
      cl <- mst_plot$mst_clusters(ids, edges$from, edges$to, w, 5)$node
      for (mode in c("log", "real", "uniform")) {
        len <- mst_plot$mst_edge_lengths(w, mode)$length
        for (fan in list(NULL, cl)) {
          coords <- mst_plot$mst_layout(
            edges$from, edges$to, len, ids, cluster = fan
          )
          expect_identical(
            mst_plot$mst_count_crossings(coords, edges$from, edges$to),
            0L,
            info = paste(shape, n, mode, is.null(fan))
          )
          expect_true(all(is.finite(c(coords$x, coords$y))))
        }
      }
    }
  }
})

test_that("every edge is drawn at exactly the length it was given", {
  edges <- random_tree(30L, "random")
  set.seed(3)
  len <- runif(nrow(edges), 20, 400)
  ids <- paste0("n", seq_len(30))
  coords <- mst_plot$mst_layout(edges$from, edges$to, len, ids)
  ix <- stats::setNames(seq_len(nrow(coords)), coords$id)
  drawn <- sqrt(
    (coords$x[ix[edges$from]] - coords$x[ix[edges$to]])^2 +
      (coords$y[ix[edges$from]] - coords$y[ix[edges$to]])^2
  )
  expect_equal(drawn, len, tolerance = 1e-8)
})

test_that("the same graph always lays out identically", {
  edges <- random_tree(40L, "random")
  ids <- paste0("n", seq_len(40))
  len <- rep(50, nrow(edges))
  first <- mst_plot$mst_layout(edges$from, edges$to, len, ids)
  # A saved analysis has to reopen as the same picture, so nothing here may
  # depend on hash order or on chance.
  expect_identical(first, mst_plot$mst_layout(edges$from, edges$to, len, ids))
})

test_that("the root is the graph centre, and the busiest node breaks a tie", {
  # A path of five: the middle vertex is the only one of minimum eccentricity.
  edges <- random_tree(5L, "chain")
  ids <- paste0("n", 1:5)
  coords <- mst_plot$mst_layout(edges$from, edges$to, rep(40, 4), ids)
  expect_identical(coords$id[coords$root], "n3")
  expect_identical(coords$depth[coords$root], 0L)
  # A path of four has two equally central vertices; the one holding more
  # isolates wins.
  edges4 <- random_tree(4L, "chain")
  ids4 <- paste0("n", 1:4)
  rooted <- mst_plot$mst_layout(
    edges4$from, edges4$to, rep(40, 3), ids4,
    weight = c(1, 1, 9, 1)
  )
  expect_identical(rooted$id[rooted$root], "n3")
})

test_that("a graph too small to lay out still returns one row per node", {
  none <- character(0)
  expect_identical(
    nrow(mst_plot$mst_layout(none, none, numeric(0), c("a", "b"))),
    2L
  )
  expect_identical(
    nrow(mst_plot$mst_layout(none, none, numeric(0), none)),
    0L
  )
})

# --- 3. The fit --------------------------------------------------------------

test_that("the fit reproduces the shipped defaults at the anchor", {
  fit <- mst_plot$mst_auto_layout(15, c(1, 1, 2, 2, 3, 4, 12), label_chars = 12)
  expect_equal(fit$node_size, mst_plot$MST_FIT_DEFAULTS$node_size)
  expect_equal(fit$node_size_min, mst_plot$MST_FIT_DEFAULTS$node_size_min)
  expect_equal(fit$node_font_size, mst_plot$MST_FIT_DEFAULTS$node_font_size)
  expect_equal(fit$edge_font_size, mst_plot$MST_FIT_DEFAULTS$edge_font_size)
  expect_true(fit$show_label)
})

test_that("a node is never wider than the gap to its nearest neighbour", {
  for (spread in c(3, 15, 60)) {
    for (w in list(c(1, 1, 2), c(1, 4, 900), c(30, 31, 32))) {
      fit <- mst_plot$mst_auto_layout(20, w, spread = spread)
      shortest <- min(mst_plot$mst_edge_lengths(w, fit$length_mode, spread)$length)
      # Two radii have to fit inside the shortest edge, or the two nodes it
      # joins merge into the single blob the drawing exists to separate. The one
      # exception is a radius already at its legibility floor: below about six
      # pixels a node is not a node, so at the smallest spreads the floor wins
      # and the reader zooms instead.
      expect_true(
        2 * fit$node_size <= shortest * 1.01 || fit$node_size <= 6,
        info = paste(spread, paste(w, collapse = "/"))
      )
    }
  }
})

test_that("labels are switched off once there are too many nodes to read them", {
  small <- mst_plot$mst_auto_layout(20, c(1, 2, 3), label_chars = 10)
  large <- mst_plot$mst_auto_layout(400, c(1, 2, 3), label_chars = 10)
  expect_true(small$show_label)
  expect_false(large$show_label)
  expect_false(large$labels_legible)
  # And a merged node stops listing its members, which is the other half of the
  # same problem.
  expect_identical(large$label_lines, 1L)
})

test_that("the label source decides legibility as much as the node count", {
  w <- c(1, 1, 2, 2, 3, 4, 12)
  # Twelve characters of host species fit beside twenty nodes; thirty-five
  # characters of assembly accession do not.
  expect_true(mst_plot$mst_auto_layout(20, w, label_chars = 12)$labels_legible)
  expect_false(mst_plot$mst_auto_layout(20, w, label_chars = 35)$labels_legible)
  # And a wider spread makes room for them again, which is what makes that
  # control the answer to the notification rather than a dead end.
  expect_true(
    mst_plot$mst_auto_layout(20, w, label_chars = 35, spread = 45)$labels_legible
  )
})

# --- 4. Clustering -----------------------------------------------------------

test_that("the threshold defaults to the scheme's own complex-type distance", {
  overview <- data.frame(
    key = c("Name", "Locus Count", "Complex Type Distance"),
    value = c("P. aeruginosa cgMLST", "3,867", "12"),
    stringsAsFactors = FALSE
  )
  expect_identical(mst_plot$mst_threshold_default(overview), 12L)
})

test_that("a scheme that does not publish a distance keeps the fallback", {
  expect_identical(mst_plot$mst_threshold_default(NULL), 10L)
  expect_identical(
    mst_plot$mst_threshold_default(
      data.frame(key = "Name", value = "x", stringsAsFactors = FALSE)
    ),
    10L
  )
  # Present but unusable is the same case as absent.
  expect_identical(
    mst_plot$mst_threshold_default(
      data.frame(key = "Complex Type Distance", value = "n/a",
                 stringsAsFactors = FALSE)
    ),
    10L
  )
})

test_that("clusters are the single-linkage components at the threshold", {
  ids <- c("a", "b", "c", "d", "e")
  from <- c("a", "b", "c", "d")
  to <- c("b", "c", "d", "e")
  # Two links at 2 and 3 alleles, then a 90-allele gap, then another link.
  out <- mst_plot$mst_clusters(ids, from, to, c(2, 3, 90, 1), threshold = 10)
  expect_identical(
    out$node,
    c("Cluster 1", "Cluster 1", "Cluster 1", "Cluster 2", "Cluster 2")
  )
  # The edge that spans the gap belongs to neither cluster: the threshold
  # rejected it, and a halo drawn along it would claim the opposite.
  expect_identical(is.na(out$edge), c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(out$table$cluster, c("Cluster 1", "Cluster 2"))
  expect_identical(out$table$nodes, c(3L, 2L))
})

test_that("clusters are numbered from the largest down", {
  ids <- c("a", "b", "c", "d", "e", "f")
  out <- mst_plot$mst_clusters(
    ids,
    from = c("a", "c", "d", "e"),
    to = c("b", "d", "e", "f"),
    weight = c(1, 1, 1, 1),
    threshold = 5
  )
  expect_identical(out$table$cluster, c("Cluster 1", "Cluster 2"))
  expect_identical(out$table$nodes, c(4L, 2L))
  expect_identical(out$node[[1]], "Cluster 2")
})

test_that("a node's isolate count, not its node count, sizes a cluster", {
  ids <- c("a\nb\nc", "d", "e")
  out <- mst_plot$mst_clusters(
    ids, from = "a\nb\nc", to = "d", weight = 1, threshold = 5,
    sizes = mst_plot$mst_node_sizes(ids)
  )
  expect_identical(out$table$nodes, 2L)
  expect_identical(out$table$isolates, 4L)
})

test_that("nothing clusters below the threshold, and singletons are not clusters", {
  ids <- c("a", "b", "c")
  out <- mst_plot$mst_clusters(ids, c("a", "b"), c("b", "c"), c(30, 40), 10)
  expect_true(all(is.na(out$node)))
  expect_identical(nrow(out$table), 0L)
})

test_that("a cluster's region follows its own subtree", {
  coords <- data.frame(
    id = c("a", "b", "c", "d"),
    x = c(0, 100, 400, 500),
    y = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  cl <- mst_plot$mst_clusters(
    coords$id, c("a", "b", "c"), c("b", "c", "d"), c(1, 90, 1), 10
  )
  blobs <- mst_plot$mst_cluster_blobs(
    coords, cl$node, cl$edge, c("a", "b", "c"), c("b", "c", "d"),
    radius = 12, pad = 6
  )
  expect_named(blobs, c("Cluster 1", "Cluster 2"))
  # Two members and the one edge between them — never the edge that leaves the
  # cluster, which is what let a region swallow its neighbour.
  expect_identical(length(blobs[["Cluster 1"]]$x), 2L)
  expect_identical(nrow(blobs[["Cluster 1"]]$seg), 1L)
  expect_equal(blobs[["Cluster 1"]]$radius, 18)
})

# How much daylight there is between one node and a cluster's region, measured
# the way the region is actually drawn: every disc at its own half-width, every
# band at its own two. A negative answer means the node is inside the shape.
region_gap <- function(b, x, y, r) {
  d <- sqrt((x - b$x)^2 + (y - b$y)^2) - b$r
  if (nrow(b$seg)) {
    d <- c(
      d,
      impl$.point_seg_dist(
        x, y, b$seg[, 1], b$seg[, 2], b$seg[, 3], b$seg[, 4]
      ) - pmax(b$seg_r[, 1], b$seg_r[, 2])
    )
  }
  min(d) - r
}

test_that("point-to-segment distance answers for every point it is given", {
  # Vectorised over either argument, and the second direction is the one that
  # matters: the region cap asks one branch against every node outside the
  # cluster at once. Written with ifelse() it returned only the first node's
  # answer — the cap then ignored all the rest, which is how an unclustered node
  # ended up inside a region that was supposed to have made room for it.
  seg <- c(0, 0, 100, 0)
  px <- c(50, 50, 50)
  py <- c(400, 5, 900)
  d <- impl$.point_seg_dist(px, py, seg[[1]], seg[[2]], seg[[3]], seg[[4]])
  expect_equal(d, c(400, 5, 900))
  # And the other way round: one point against many segments.
  expect_equal(
    impl$.point_seg_dist(50, 5, c(0, 0), c(0, 100), c(100, 100), c(0, 100)),
    c(5, 95)
  )
  # Past an end, the nearer endpoint rather than the infinite line.
  expect_equal(impl$.point_seg_dist(-30, 0, 0, 0, 100, 0), 30)
  # A branch of zero length is its own endpoint.
  expect_equal(impl$.point_seg_dist(3, 4, 0, 0, 0, 0), 5)
})

test_that("a region never reaches another cluster's node", {
  # Membership is a claim the threshold made. A region wide enough to cover a
  # node the threshold put in a *different* cluster makes the drawing say the
  # opposite, and neither region can give way to the other by being cut.
  coords <- data.frame(
    id = c("a", "b", "c", "d"),
    x = c(0, 100, 140, 240),
    y = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  cl <- mst_plot$mst_clusters(
    coords$id, c("a", "b", "c"), c("b", "c", "d"), c(1, 90, 1), 10
  )
  expect_identical(cl$node, c("Cluster 1", "Cluster 1", "Cluster 2", "Cluster 2"))
  # A padding wide enough to swallow "c" outright — 12 + 60 reaches x = 172.
  b <- mst_plot$mst_cluster_blobs(
    coords, cl$node, cl$edge, c("a", "b", "c"), c("b", "c", "d"),
    radius = 12, pad = 60
  )[["Cluster 1"]]
  # Daylight, not a touch.
  expect_equal(region_gap(b, 140, 0, 12), impl$MST_BLOB_CLEARANCE)
  # Narrowed only where it had to be: "a" is 140 from the intruder and keeps the
  # width it asked for, "b" is 40 away and gives it up. A single width for the
  # whole cluster could do one or the other, never both.
  expect_equal(b$r, c(72, 25))
  # The band between them tapers to the disc at each end, so a branch leaving a
  # large node no longer runs at that width all the way to a small one.
  expect_equal(b$seg_r[1, ], c(25, 25))

  # A cluster with nothing near it still gets the padding it asked for.
  far <- coords
  far$x[c(3, 4)] <- c(900, 1000)
  b <- mst_plot$mst_cluster_blobs(
    far, cl$node, cl$edge, c("a", "b", "c"), c("b", "c", "d"),
    radius = 12, pad = 60
  )[["Cluster 1"]]
  expect_equal(b$r, c(72, 72))
  expect_equal(b$radius, 72)
})

test_that("a region does not narrow around a node in no cluster", {
  # It is cut instead (see MST_NODE_CASING). Narrowing was the first answer and
  # it scallops the outline: one cluster holding 175 of a collection's 181 nodes
  # came out notched wherever a single unclustered node sat near it, which reads
  # as damage to the shape rather than as a node outside it.
  coords <- data.frame(
    id = c("a", "b", "c"),
    x = c(0, 100, 140),
    y = c(0, 0, 0),
    stringsAsFactors = FALSE
  )
  cl <- mst_plot$mst_clusters(coords$id, c("a", "b"), c("b", "c"), c(1, 90), 10)
  expect_true(is.na(cl$node[[3]]))
  b <- mst_plot$mst_cluster_blobs(
    coords, cl$node, cl$edge, c("a", "b"), c("b", "c"),
    radius = 12, pad = 60
  )[["Cluster 1"]]
  expect_equal(b$r, c(72, 72))
})

test_that("a band tapers between two differently sized nodes", {
  # The reference database's largest cluster holds one 16px merged node among
  # 175 mostly 6px ones. A band at the thicker end's width from end to end is
  # what covered the nodes beside the thin end.
  coords <- data.frame(
    id = c("a", "b"), x = c(0, 300), y = c(0, 0), stringsAsFactors = FALSE
  )
  cl <- mst_plot$mst_clusters(coords$id, "a", "b", 1, 10)
  b <- mst_plot$mst_cluster_blobs(
    coords, cl$node, cl$edge, "a", "b", radius = c(16, 6), pad = 0
  )[["Cluster 1"]]
  expect_equal(b$r, c(16, 6))
  expect_equal(b$seg_r[1, ], c(16, 6))
})

test_that("no region on a real-shaped tree covers a node outside it", {
  set.seed(11)
  edges <- random_tree(60, "random")
  w <- sample(c(1, 2, 3, 4, 30, 90), nrow(edges), replace = TRUE)
  ids <- unique(c(edges$from, edges$to))
  cl <- mst_plot$mst_clusters(ids, edges$from, edges$to, w, 5)
  lens <- mst_plot$mst_edge_lengths(w, "log", spread = 15)
  coords <- mst_plot$mst_layout(edges$from, edges$to, lens$length, ids)
  radius <- rep(14, length(ids))
  blobs <- mst_plot$mst_cluster_blobs(
    coords, cl$node, cl$edge, edges$from, edges$to,
    radius = radius, pad = 40
  )
  expect_true(length(blobs) > 1L)
  for (nm in names(blobs)) {
    b <- blobs[[nm]]
    outside <- which(!is.na(cl$node) & cl$node != nm)
    for (o in outside) {
      expect_gte(
        region_gap(b, coords$x[o], coords$y[o], radius[[o]]),
        impl$MST_BLOB_CLEARANCE - 1e-9
      )
    }
  }
})

test_that("node sizes that vary do not widen a region past its neighbours", {
  # The shape the real failure took: one big merged node in a cluster of small
  # ones, with unclustered nodes close to the small ones.
  set.seed(29)
  edges <- random_tree(80, "random")
  w <- sample(c(1, 1, 2, 3, 40, 120), nrow(edges), replace = TRUE)
  ids <- unique(c(edges$from, edges$to))
  cl <- mst_plot$mst_clusters(ids, edges$from, edges$to, w, 5)
  lens <- mst_plot$mst_edge_lengths(w, "log", spread = 15)
  coords <- mst_plot$mst_layout(edges$from, edges$to, lens$length, ids)
  radius <- sample(c(6, 6, 6, 6, 16), length(ids), replace = TRUE)
  for (pad in c(0, 14, 60)) {
    blobs <- mst_plot$mst_cluster_blobs(
      coords, cl$node, cl$edge, edges$from, edges$to,
      radius = radius, pad = pad
    )
    for (nm in names(blobs)) {
      for (o in which(!is.na(cl$node) & cl$node != nm)) {
        expect_gte(
          region_gap(blobs[[nm]], coords$x[o], coords$y[o], radius[[o]]),
          impl$MST_BLOB_CLEARANCE - 1e-9
        )
      }
    }
  }
})

# --- 5. Node content ---------------------------------------------------------

test_that("a merged node reports how many isolates it stands for", {
  expect_identical(
    mst_plot$mst_node_sizes(c("a", "b\nc", "d\ne\nf")),
    c(1L, 2L, 3L)
  )
})

test_that("a node label lists a few members and counts the rest", {
  meta <- demo_meta()
  labels <- mst_plot$mst_node_labels(
    c("a", "b\nc\nd\ne\nf"), meta, "isolate", max_lines = 2L
  )
  expect_identical(labels[[1]], "a")
  expect_identical(labels[[2]], "b\nc\n+ 3 more")
})

test_that("a label source with no value says so rather than showing a blank", {
  labels <- mst_plot$mst_node_labels("e", demo_meta(), "country")
  expect_identical(labels[[1]], "Not recorded")
})

test_that("node area, not radius, is proportional to the isolate count", {
  r <- mst_plot$mst_node_radii(c(1, 4, 16), c(10, 30))
  expect_equal(r[[1]], 10)
  expect_equal(r[[3]], 30)
  # 4 isolates out of 16 is a quarter of the area, so the radius sits at the
  # square root of the way up the range.
  expect_equal(r[[2]], 10 + 20 * sqrt(3 / 15))
  # One fixed size means one radius for every node, however many isolates.
  expect_equal(mst_plot$mst_node_radii(c(1, 9), 20), c(20, 20))
})

test_that("a node's hover text names its isolates and its cluster", {
  titles <- mst_plot$mst_node_titles(
    c("a", "b\nc"), demo_meta(), fields = "country",
    cluster = c(NA, "Cluster 1")
  )
  expect_true(grepl("<b>a</b>", titles[[1]], fixed = TRUE))
  expect_true(grepl("2 isolates", titles[[2]], fixed = TRUE))
  expect_true(grepl("Cluster 1", titles[[2]], fixed = TRUE))
  expect_true(grepl("Kenya", titles[[2]], fixed = TRUE))
})

# --- 6. Variable mapping -----------------------------------------------------

test_that("a merged node's value is a distribution, not a value", {
  vals <- mst_plot$mst_node_values(c("a", "b\nc"), demo_meta(), "country")
  expect_identical(vals$shares[[1]], stats::setNames(1, "Kenya"))
  expect_equal(sum(vals$shares[[2]]), 1)
  expect_identical(sort(names(vals$shares[[2]])), c("Kenya", "Peru"))
})

test_that("a value nobody recorded is its own level, not a gap", {
  vals <- mst_plot$mst_node_values(c("e", "f"), demo_meta(), "country")
  expect_identical(names(vals$shares[[1]]), "Not recorded")
  expect_true("Not recorded" %in% vals$levels)
})

# A date column the way SQLite hands it over: character, not Date.
date_meta <- function() {
  data.frame(
    isolate = c("a", "b", "c", "d", "e", "f"),
    collected = c("2024-01-05", "2024-01-20", "2024-02-11", "2024-02-28",
                  "2024-03-03", NA),
    stringsAsFactors = FALSE
  )
}

test_that("a mapped date is no longer one slice per distinct date string", {
  # It arrives as character, so without the transform it fell to the discrete
  # branch and drew a level per date — the MST never honoured the layer.
  layer <- list(field = "collected", transform = "as_date",
                granularity = "month")
  vals <- mst_plot$mst_node_values(c("a", "b\nc"), date_meta(), "collected",
                                   layer)

  # Levels come from the whole column, so every month present is one.
  expect_identical(
    vals$levels,
    c("2024-01", "2024-02", "2024-03", "Not recorded")
  )
  # The merged node splits across the two months it actually spans.
  expect_equal(sum(vals$shares[[2]]), 1)
  expect_identical(sort(names(vals$shares[[2]])), c("2024-01", "2024-02"))
})

test_that("an unbinned date drives the gradient rather than a pie", {
  layer <- list(field = "collected", transform = "as_date")
  vals <- mst_plot$mst_node_values(c("a", "f"), date_meta(), "collected", layer)

  expect_null(vals$levels)
  expect_true(vals$date)
  expect_equal(vals$value[[1]], as.numeric(as.Date("2024-01-05")))
  expect_true(is.na(vals$value[[2]]))
})

test_that("a continuous variable collapses to the node's mean", {
  vals <- mst_plot$mst_node_values(c("b\nc", "f"), demo_meta(), "year")
  expect_identical(vals$levels, NULL)
  expect_equal(vals$value[[1]], 2020.5)
  # Nothing recorded at all is NA rather than NaN, so the colour ramp can find
  # it and give it grey.
  expect_true(is.na(vals$value[[2]]))
})

# --- 7. Legend ---------------------------------------------------------------

test_that("the legend names every cluster and the threshold that made them", {
  clusters <- data.frame(
    cluster = c("Cluster 1", "Cluster 2"),
    nodes = c(3L, 2L),
    isolates = c(7L, 2L),
    stringsAsFactors = FALSE
  )
  items <- mst_plot$mst_legend_items(
    clusters = clusters,
    cluster_colors = stats::setNames(c("#111111", "#222222"), clusters$cluster),
    threshold = 12
  )
  kinds <- vapply(items, function(i) i$kind, character(1))
  labels <- vapply(items, function(i) i$label, character(1))
  expect_identical(kinds, c("header", "key", "key"))
  expect_true(grepl("12 alleles", labels[[1]]))
  expect_identical(labels[[2]], "Cluster 1 \u2013 7 isolates")
  # Isolates and nothing else. How many nodes they came out as is a fact about
  # the drawing rather than about the clustering, and the caption gives it once
  # for the whole tree.
  expect_false(any(grepl("node", labels)))
})

test_that("the legend accounts for the isolates no cluster claimed", {
  # The cluster keys are not a partition on their own — a singleton is in no
  # cluster — so without this the reader cannot tell 249 clustered isolates out
  # of 253 from 249 out of 249.
  clusters <- data.frame(
    cluster = "Cluster 1", nodes = 3L, isolates = 9L, stringsAsFactors = FALSE
  )
  colors <- stats::setNames("#111111", "Cluster 1")
  labels <- function(...) {
    vapply(
      mst_plot$mst_legend_items(clusters = clusters, cluster_colors = colors, ...),
      function(i) i$label,
      character(1)
    )
  }
  expect_true(any(grepl("Unclustered . 4 isolates$", labels(unclustered = 4))))
  # Counted in isolates, and singular when there is one of them.
  expect_true(any(grepl("Unclustered . 1 isolate$", labels(unclustered = 1))))
  # Nothing left over, nothing to say.
  expect_false(any(grepl("Unclustered", labels(unclustered = 0))))
  expect_false(any(grepl("Unclustered", labels())))
})

test_that("a legend too long to draw is capped and says how much it left out", {
  layer <- list(
    title = "Country",
    levels = paste("Country", 1:46),
    colors = stats::setNames(rep("#123456", 46), paste("Country", 1:46)),
    shapes = stats::setNames(rep("dot", 46), paste("Country", 1:46))
  )
  items <- mst_plot$mst_legend_items(list(layer))
  keys <- Filter(function(i) identical(i$kind, "key"), items)
  expect_identical(length(keys), as.integer(impl$MST_LEGEND_MAX_KEYS))
  expect_true(grepl(
    "\\+ 22 more",
    items[[length(items)]]$label
  ))
})

test_that("two mapped variables share the key budget instead of doubling it", {
  wide <- function(title, n) {
    lev <- paste(title, seq_len(n))
    list(title = title, levels = lev,
         colors = stats::setNames(rep("#123456", n), lev),
         shapes = stats::setNames(rep("dot", n), lev))
  }
  items <- mst_plot$mst_legend_items(list(wide("A", 40), wide("B", 40)))
  keys <- Filter(function(i) identical(i$kind, "key"), items)
  expect_true(length(keys) <= impl$MST_LEGEND_MAX_KEYS)
  # And each section still says something: a legend of one key per variable
  # would be no legend at all.
  expect_true(length(keys) >= 2L * impl$MST_LEGEND_MIN_KEYS)
})

test_that("the legend gains columns and loses type size as it grows", {
  one <- lapply(1:4, function(i) {
    list(kind = "key", label = paste("Key", i), color = "#111111", shape = "dot")
  })
  many <- lapply(1:60, function(i) {
    list(kind = "key", label = paste("Key", i), color = "#111111", shape = "dot")
  })
  small <- mst_plot$mst_legend_layout(one, c(900, 600))
  big <- mst_plot$mst_legend_layout(many, c(900, 600))
  expect_identical(small$ncol, 1L)
  expect_gt(big$ncol, small$ncol)
  expect_lt(big$font_size, small$font_size)
  # However many keys, the legend may not take the canvas over.
  expect_lt(big$width, 0.35)
})

test_that("the legend layout survives whatever the browser reports", {
  items <- list(list(kind = "key", label = "K", color = "#111", shape = "dot"))
  for (canvas in list(NULL, list(900, 600), c(10, 10), c(NA, NA))) {
    geom <- mst_plot$mst_legend_layout(items, canvas)
    expect_true(is.finite(geom$width))
    expect_gt(geom$font_size, 0)
  }
})

# --- 8. Frames and widget ----------------------------------------------------

base_opts <- function(...) {
  # Assigned one by one rather than through modifyList(), which merges list
  # values recursively and so drops the unnamed records inside `layers`.
  opts <- list(
    show_label = TRUE, field = "isolate", label_lines = 3L,
    show_edge_label = TRUE, node_font_size = 13, edge_font_size = 14,
    node_font_color = "#000000", node_color = "#B2FACA",
    edge_color = "#000000",
    edge_font_color = "#000000", background = "#ffffff", transparent = TRUE,
    scale_nodes = TRUE, node_size = c(10, 24), shape = "dot", shadow = TRUE,
    length_mode = "log", spread = 15, shorten_long = TRUE, cap_mult = 7,
    rotation = 0,
    layers = list(),
    show_clusters = FALSE, cluster_threshold = 10,
    cluster_col_scale = "viridis", cluster_width = 14,
    cluster_opacity = 0.35, cluster_label_size = 18,
    cluster_label_tint = TRUE,
    show_legend = TRUE, legend_ori = "left", show_caption = TRUE,
    canvas_px = c(900, 620)
  )
  overrides <- list(...)
  for (name in names(overrides)) {
    opts[[name]] <- overrides[[name]]
  }
  opts
}

test_that("frames carry a laid-out, crossing-free drawing", {
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  expect_identical(nrow(fr$nodes), 5L)
  expect_identical(nrow(fr$edges), 4L)
  expect_true(all(c("x", "y") %in% names(fr$nodes)))
  expect_identical(
    mst_plot$mst_count_crossings(fr$coords, fr$edges$from, fr$edges$to),
    0L
  )
  # Under log the 400-allele branch is still inside the cap; drawn to scale it
  # is 160 times the median and is capped and dashed instead.
  expect_false(any(fr$edges$dashes))
  real <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(length_mode = "real")
  )
  expect_identical(real$edges$dashes, c(FALSE, FALSE, TRUE, FALSE))
  # Nothing is mapped, so the cheap native shapes are used.
  expect_false(fr$custom)
})

test_that("a mapping turns the node into a pie of its members' values", {
  opts <- base_opts(layers = list(list(
    field = "country", title = "Country", aesthetic = "node_fill",
    palette = "viridis"
  )))
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  expect_true(fr$custom)
  expect_identical(unique(fr$nodes$shape), "custom")
  # The merged node holds one Kenya and one Peru, so its spec carries two
  # half-slices; a single-isolate node carries one.
  merged <- fr$nodes$metadata[fr$nodes$id == "b\nc"]
  expect_identical(lengths(regmatches(merged, gregexpr('"v":', merged))), 2L)
  single <- fr$nodes$metadata[fr$nodes$id == "a"]
  expect_identical(lengths(regmatches(single, gregexpr('"v":', single))), 1L)
  expect_true(length(fr$legend) > 1L)
})

test_that("clusters are drawn as regions, never as node fill", {
  fr <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 5)
  )
  expect_identical(nrow(fr$clusters$table), 1L)
  expect_identical(length(fr$blobs), 1L)
  # The fill still belongs to the node colour: a cluster that took it over is
  # what made switching clusters on with a mapping active a no-op.
  expect_identical(unique(fr$nodes$color.background), "#B2FACA")
  expect_true(any(grepl("Cluster", vapply(
    fr$legend, function(i) i$label %||% "", character(1)
  ))))
})

test_that("clustering never adds edges to the drawing", {
  plain <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  clustered <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 5)
  )
  # The region is painted by a hook, not by a second, thicker copy of every
  # intra-cluster edge — which is what the old "Skeleton" rendering did.
  expect_identical(nrow(clustered$edges), nrow(plain$edges))
})

test_that("the region width is the control, not the node radius", {
  narrow <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 5, cluster_width = 4)
  )
  wide <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 5, cluster_width = 40)
  )
  expect_equal(
    wide$blobs[[1]]$radius - narrow$blobs[[1]]$radius,
    36
  )
})

test_that("every node in no cluster is handed to the renderer to cut around", {
  # The cut is what separates it from the region, so a node left out of this
  # list is a node with nothing between it and the shading.
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 3)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  loose <- which(is.na(fr$clusters$node))
  expect_true(length(loose) > 0L)
  expect_equal(fr$loose$x, fr$coords$x[loose])
  expect_equal(fr$loose$r, fr$nodes$size[loose])

  # Nothing to cut out of when no region is drawn.
  plain <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(show_clusters = FALSE)
  )
  expect_null(plain$loose)

  # And the renderer erases rather than painting: the canvas may be transparent
  # over the panel's own backdrop, where a disc of "background" is a white disc.
  js <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts, fr
  )$x$events$beforeDrawing
  expect_true(grepl("destination-out", js, fixed = TRUE))
})

test_that("the region is one path at the requested opacity", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5,
                    cluster_opacity = 0.5, cluster_label_size = 22)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  hook <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts, fr
  )$x$events$beforeDrawing
  expect_true(grepl("var A=0.5", hook, fixed = TRUE))
  # One beginPath and one fill per region: a fill per disc and capsule is what
  # made a translucent region composite into a patchwork. The second fill is the
  # cut around the nodes in no cluster, which runs once for all of them.
  fills <- lengths(regmatches(hook, gregexpr("ctx.fill()", hook, fixed = TRUE)))
  expect_identical(fills, 2L)
  bare <- mst_plot$build_mst_visnetwork(
    demo_graph(),
    demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 400),
    mst_plot$mst_frames(
      demo_graph(),
      demo_meta(),
      base_opts(show_clusters = TRUE, cluster_threshold = 400)
    )
  )$x$events$beforeDrawing
  # Nothing outside the cluster, nothing to cut.
  expect_identical(
    lengths(regmatches(bare, gregexpr("ctx.fill()", bare, fixed = TRUE))),
    1L
  )
})

test_that("a cluster's name is drawn over the graph, not under it", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5,
                    cluster_label_size = 22)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  widget <- mst_plot$build_mst_visnetwork(demo_graph(), demo_meta(), opts, fr)
  # It used to ride along in the region hook — a beforeDrawing handler — so any
  # name that landed on the tree was painted underneath it and simply vanished.
  expect_false(grepl("LS=22", widget$x$events$beforeDrawing, fixed = TRUE))
  after <- widget$x$events$afterDrawing
  expect_true(grepl("var LS=22", after, fixed = TRUE))
  expect_true(grepl("Cluster 1", after, fixed = TRUE))
  # And a halo behind the glyphs, for the part of a name that does cross a
  # branch.
  expect_true(grepl("strokeText", after, fixed = TRUE))
  # Size 0 is off: no label pass at all.
  off <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 5,
              cluster_label_size = 0)
  )$x$events$afterDrawing
  expect_false(grepl("strokeText", off, fixed = TRUE))
})

test_that("every edge carries an id, so an update updates rather than adds", {
  # vis.js's DataSet.update() looks an item up by id and *appends* the ones it
  # cannot find, so an id-less edge table pushed through visUpdateEdges() added
  # a second copy of every branch. It showed as allelic distances that could be
  # switched on but never off: the new unlabelled edges were drawn on top of
  # the labelled ones, which were still there.
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  expect_true("id" %in% names(fr$edges))
  expect_identical(anyDuplicated(fr$edges$id), 0L)
  # And the same edge keeps the same id when only its styling changes, or the
  # update would still be an append.
  off <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(show_edge_label = FALSE)
  )
  expect_identical(off$edges$id, fr$edges$id)
  # A space, not "": vis.js only overwrites an edge label on update when the
  # new value is truthy, so "" is silently ignored and the old label sticks.
  expect_identical(unique(off$edges$label), " ")
  expect_false(identical(unique(fr$edges$label), " "))
})

test_that("collapsing merges nodes no further apart than the threshold", {
  plain <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  # The demo tree's branches are 1, 2, 400 and 3 alleles.
  at3 <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(collapse_threshold = 3)
  )
  expect_lt(nrow(at3$nodes), nrow(plain$nodes))
  expect_identical(at3$collapsed, 3L)
  # A contraction of a tree is a tree: no branch is lost or duplicated.
  expect_identical(nrow(at3$edges), nrow(at3$nodes) - 1L)
  # No isolate goes missing — the merged node's id is its members' names, which
  # is the same shape a zero-distance merge already produces.
  expect_identical(sum(at3$counts), sum(plain$counts))
  # Only the 400-allele branch is left, so every node but two is now in one.
  expect_true(all(at3$edges$weight > 3))
  # 0 collapses nothing, and is the default.
  expect_identical(
    mst_plot$mst_frames(
      demo_graph(), demo_meta(), base_opts(collapse_threshold = 0)
    )$collapsed,
    0L
  )
})

test_that("a threshold that folds the whole tree still draws", {
  # One node and no edges at all. Every constant edge column has to be built at
  # length zero rather than as a scalar, or data.frame() refuses the frame and
  # the module dies mid-render.
  whole <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(collapse_threshold = 10000)
  )
  expect_identical(nrow(whole$nodes), 1L)
  expect_identical(nrow(whole$edges), 0L)
  expect_identical(whole$counts, 6L)
  expect_silent(
    mst_plot$build_mst_visnetwork(
      demo_graph(), demo_meta(),
      base_opts(collapse_threshold = 10000), whole
    )
  )
})

test_that("rotation moves the drawing without changing the tree", {
  base <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  turned <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(rotation = 90)
  )
  expect_false(isTRUE(all.equal(base$coords$x, turned$coords$x)))
  # A rotation is an isometry: every branch keeps its length and the layout
  # stays crossing-free.
  seg_len <- function(fr) {
    ix <- stats::setNames(seq_len(nrow(fr$coords)), fr$coords$id)
    f <- ix[fr$edges$from]
    t <- ix[fr$edges$to]
    sqrt((fr$coords$x[f] - fr$coords$x[t])^2 +
           (fr$coords$y[f] - fr$coords$y[t])^2)
  }
  expect_equal(seg_len(base), seg_len(turned))
  expect_identical(
    mst_plot$mst_count_crossings(
      turned$coords, turned$edges$from, turned$edges$to
    ),
    0L
  )
  # 360 is a full turn, and 0 is a no-op.
  expect_equal(
    mst_plot$mst_frames(
      demo_graph(), demo_meta(), base_opts(rotation = 360)
    )$coords$x,
    base$coords$x
  )
  # The control runs -180 to 180 rather than 0 to 360, but -90 and 270 are the
  # same rotation, so the geometry has to agree either way.
  expect_equal(
    mst_plot$mst_frames(
      demo_graph(), demo_meta(), base_opts(rotation = -90)
    )$coords,
    mst_plot$mst_frames(
      demo_graph(), demo_meta(), base_opts(rotation = 270)
    )$coords
  )
})

test_that("the widget is built without physics and with the layout baked in", {
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  widget <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), base_opts(), fr
  )
  expect_false(widget$x$options$physics$enabled)
  # Dragging a node would move it away from the coordinates that *are* the
  # data, and the cluster regions — baked into a canvas hook at build time —
  # cannot follow it.
  expect_false(widget$x$options$interaction$dragNodes)
  expect_false(widget$x$options$edges$smooth)
  # Coordinates travel with the nodes, which is what there is instead of a
  # client-side simulation.
  expect_true(all(c("x", "y") %in% names(widget$x$nodes)))
  # Nothing is mapped and nothing is clustered, so there is nothing to key: an
  # empty legend panel over the drawing would be furniture.
  expect_false(grepl("L.items", widget$x$events$afterDrawing, fixed = TRUE))
})

test_that("the branch font travels as edge data, not as a widget option", {
  # In the widget's options it was unreachable: the incremental update path
  # pushes node and edge tables and nothing else, so a font size or colour set
  # there changed nothing at all until the next full rebuild.
  fr <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(edge_font_size = 27, edge_font_color = "#FF0000")
  )
  expect_identical(unique(fr$edges$font.size), 27)
  expect_identical(unique(fr$edges$font.color), "#FF0000")
})

test_that("the size slider drives the radii while duplicates scale them", {
  small <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(scale_nodes = TRUE, node_size = c(6, 12))
  )
  large <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(scale_nodes = TRUE, node_size = c(20, 50))
  )
  # The builder used to fall back to the fitted defaults here, so both handles
  # of the slider did nothing whenever scaling was on.
  expect_equal(range(small$nodes$size), c(6, 12))
  expect_equal(range(large$nodes$size), c(20, 50))
  # One handle, one radius.
  fixed <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(scale_nodes = FALSE, node_size = 31)
  )
  expect_identical(unique(fixed$nodes$size), 31)
})

test_that("a keyed drawing carries its legend into the draw hook", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  widget <- mst_plot$build_mst_visnetwork(demo_graph(), demo_meta(), opts, fr)
  # Drawn onto the main canvas rather than handed to visLegend(), so the
  # evidence is in the after-draw handler: the keys, the side, and the fit
  # giving up the strip they occupy.
  drawn <- widget$x$events$afterDrawing
  expect_true(grepl("Cluster 1", drawn, fixed = TRUE))
  expect_true(grepl('"pos":"left"', drawn, fixed = TRUE))
  expect_true(grepl("__ptReserve", drawn, fixed = TRUE))
  expect_true(is.null(widget$x$legend))
})

test_that("a category name with a quote in it cannot break the legend's JS", {
  meta <- demo_meta()
  meta$country <- c("O'Hare", 'A "B"', "C\\D", "Peru", "Peru", "Chile")
  opts <- base_opts(layers = list(list(
    field = "country", title = "Country", aesthetic = "node_fill",
    palette = "viridis"
  )))
  fr <- mst_plot$mst_frames(demo_graph(), meta, opts)
  drawn <- mst_plot$build_mst_visnetwork(
    demo_graph(), meta, opts, fr
  )$x$events$afterDrawing
  expect_true(grepl('A \\"B\\"', drawn, fixed = TRUE))
})

test_that("the legend can be switched off without hiding the caption", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5,
                    show_legend = FALSE)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  drawn <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts, fr
  )$x$events$afterDrawing
  # The two switches are independent: which length transform drew the branches
  # is not the reader's own choice the way a mapping or a cluster threshold is,
  # so it keeps its own control rather than riding along with the legend's.
  expect_false(grepl("L.items", drawn, fixed = TRUE))
  expect_true(grepl("Branch length", drawn, fixed = TRUE))
})

test_that("the caption has its own switch, off by default disabled", {
  opts <- base_opts(show_caption = FALSE)
  drawn <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts,
    mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  )$x$events$afterDrawing
  expect_false(grepl("Branch length", drawn, fixed = TRUE))
})

test_that("the caption states which length transform drew the branches", {
  # A reader who assumes proportional length while looking at a log-scaled tree
  # misjudges how related two clusters are, and nothing else on the canvas says
  # which of the three drew it.
  expect_match(mst_plot$mst_scale_caption("real"), "proportional to allelic")
  expect_match(mst_plot$mst_scale_caption("log"), "log-scaled")
  expect_match(mst_plot$mst_scale_caption("uniform"), "not to scale")
  # The cap only exists where length carries distance, and states the
  # multiplier actually in force rather than the constant default.
  expect_match(mst_plot$mst_scale_caption("log", TRUE, 12), "12x the median")
  expect_false(grepl("capped", mst_plot$mst_scale_caption("log", FALSE)))
  expect_false(grepl("capped", mst_plot$mst_scale_caption("uniform", TRUE)))

  opts <- base_opts(length_mode = "uniform")
  drawn <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts,
    mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  )$x$events$afterDrawing
  expect_true(grepl("not to scale", drawn, fixed = TRUE))
})

test_that("the caption states what the drawing is made of", {
  # Node count is the only place the collapse — zero-distance merging here — is
  # stated: six isolates drawn as five dots is a different claim from six dots.
  expect_match(
    mst_plot$mst_stats_caption(c(1, 2, 1, 1, 1), c(1, 3, 400, 2)),
    "^6 isolates in 5 nodes;"
  )
  # Median against mean is the long-tailed case the log scale exists for, so
  # both are named rather than one summary.
  expect_match(
    mst_plot$mst_stats_caption(c(1, 2, 1, 1, 1), c(1, 3, 400, 2)),
    "allelic distance median 2.5, mean 101.5, range 1.400"
  )
  # A tree of one node has no branches to summarise, and no plural either.
  expect_identical(mst_plot$mst_stats_caption(1, numeric(0)), "1 isolate in 1 node.")
  expect_identical(mst_plot$mst_stats_caption(numeric(0), numeric(0)), "")
})

test_that("the summary statistics ride on the caption bar and its switch", {
  drawn <- function(...) {
    opts <- base_opts(...)
    mst_plot$build_mst_visnetwork(
      demo_graph(), demo_meta(), opts,
      mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
    )$x$events$afterDrawing
  }
  expect_true(grepl("6 isolates in 5 nodes", drawn(), fixed = TRUE))
  expect_false(grepl("6 isolates in 5 nodes", drawn(show_caption = FALSE), fixed = TRUE))
  # Collapsing is what the node count is there to report.
  expect_true(grepl(
    "6 isolates in 2 nodes",
    drawn(collapse_threshold = 3),
    fixed = TRUE
  ))
})

test_that("the frames count the unclustered isolates for the legend", {
  # At 3, everything but "e" (400 alleles out) joins one cluster.
  fr <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 3)
  )
  labels <- vapply(fr$legend, function(i) i$label, character(1))
  expect_true(any(grepl("Unclustered . 1 isolate", labels)))
  # And nothing is left over once the threshold takes the whole tree.
  loose <- mst_plot$mst_frames(
    demo_graph(), demo_meta(),
    base_opts(show_clusters = TRUE, cluster_threshold = 400)
  )
  expect_false(any(grepl(
    "Unclustered",
    vapply(loose$legend, function(i) i$label, character(1))
  )))
})

test_that("the cap multiplier is a control, not just a constant", {
  # The demo tree's longest branch is 400 alleles against a median of 2.5 —
  # ~160x, so a 2x cap bites and a 200x one does not.
  tight <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(length_mode = "real", cap_mult = 2)
  )
  loose <- mst_plot$mst_frames(
    demo_graph(), demo_meta(), base_opts(length_mode = "real", cap_mult = 200)
  )
  expect_true(any(tight$edges$dashes))
  expect_false(any(loose$edges$dashes))
  expect_gt(max(loose$coords$x) - min(loose$coords$x),
            max(tight$coords$x) - min(tight$coords$x))
})

test_that("a branch leaving a cluster is fanned to the edge of its wedge", {
  # Angle is the one thing an equal-angle layout is free to choose — the lengths
  # carry the allelic distances — so it is spent on pointing the branches that
  # leave a cluster away from the ones that stay in it. Two leaves in the
  # cluster and one out of it: the odd one takes an end of the fan, not the
  # middle, and more of the wedge than its single tip would earn.
  fan <- impl$.fan_layout(
    ch = c(10L, 11L, 12L),
    tips_ch = c(20, 1, 20),
    attached = c(TRUE, FALSE, TRUE)
  )
  expect_identical(fan$ch[[1]], 11L)
  expect_gte(fan$share[[1]], impl$MST_LOOSE_SHARE)
  expect_equal(sum(fan$share), 1)

  # Several of them alternate between the two ends rather than stacking on one.
  fan <- impl$.fan_layout(
    ch = 1:4,
    tips_ch = c(1, 30, 1, 30),
    attached = c(FALSE, TRUE, FALSE, TRUE)
  )
  expect_identical(fan$ch[[1]], 1L)
  expect_identical(fan$ch[[length(fan$ch)]], 3L)

  # With nothing leaving, the fan is exactly the proportional one it always was.
  fan <- impl$.fan_layout(1:3, c(2, 3, 5), rep(TRUE, 3))
  expect_identical(fan$ch, 1:3)
  expect_equal(fan$share, c(0.2, 0.3, 0.5))

  # However many leave, the cluster's own branch keeps a fifth of the wedge —
  # otherwise a node with eight stragglers hanging off it would have its own
  # subtree, however large, squeezed into nothing.
  fan <- impl$.fan_layout(1:9, c(rep(1, 8), 100), c(rep(FALSE, 8), TRUE))
  expect_gte(fan$share[[which(fan$ch == 9L)]], 0.2 - 1e-9)
  expect_equal(sum(fan$share), 1)

  # A floor is a floor, not a cap: a straggler that is itself a large subtree
  # keeps the share its size earns.
  fan <- impl$.fan_layout(1:2, c(90, 10), c(FALSE, TRUE))
  expect_equal(fan$share[[which(fan$ch == 1L)]], 0.9)
})

test_that("a branch leaving a cluster is swung out of it, at its own length", {
  # Ordering the fan is not enough on a real collection: a node in no cluster is
  # usually surrounded by *unrelated* subtrees the radial layout packed against
  # it, which no choice of wedge reaches. Swinging the branch does reach it —
  # and it swings at the length it had, so the allelic distance the branch
  # stands for is untouched.
  #
  # A parent with two cluster arms above it and the straggler placed between
  # them, which is the shape that reads as a hole in the shading.
  x <- c(0, -40, 40, 0)
  y <- c(0, 90, 90, 100)
  cl <- c("A", "A", "A", NA)
  rad <- rep(6, 4)
  skel <- list(
    x = x[1:3], y = y[1:3], r = rep(60, 3),
    seg = matrix(numeric(0), 0, 4), seg_r = numeric(0)
  )
  # Where the fan left it, the region covers it outright.
  expect_lt(impl$.cluster_reach(x[[4]], y[[4]], skel), 0)

  out <- impl$.push_out_of_clusters(
    x, y,
    parent = c(NA, 1L, 1L, 1L),
    subtree = list(1:4, 2L, 3L, 4L),
    cl = cl,
    radius = rad,
    f = c(1L, 1L, 1L),
    t = c(2L, 3L, 4L),
    skel = skel
  )
  # Clear of it now.
  expect_gt(impl$.cluster_reach(out$x[[4]], out$y[[4]], skel), 0)
  # Swung, not moved: the branch is exactly as long as it was.
  expect_equal(
    sqrt((out$x[[4]] - out$x[[1]])^2 + (out$y[[4]] - out$y[[1]])^2),
    100
  )
  # The cluster's own nodes did not move at all.
  expect_equal(out$x[1:3], x[1:3])
  expect_equal(out$y[1:3], y[1:3])
})

test_that("the swing keeps every promise the plain layout made", {
  # It gives up the wedges that made the layout crossing-free, so it checks for
  # crossings itself — and it must not trade one collision for another.
  set.seed(5)
  edges <- random_tree(50, "random")
  w <- sample(c(1, 1, 2, 3, 40, 90), nrow(edges), replace = TRUE)
  ids <- unique(c(edges$from, edges$to))
  cl <- mst_plot$mst_clusters(ids, edges$from, edges$to, w, 5)
  len <- mst_plot$mst_edge_lengths(w, "log", spread = 15)$length
  plain <- mst_plot$mst_layout(edges$from, edges$to, len, ids)
  swung <- mst_plot$mst_layout(
    edges$from, edges$to, len, ids,
    cluster = cl$node, radius = 12, pad = 25
  )

  expect_identical(
    mst_plot$mst_count_crossings(swung, edges$from, edges$to), 0L
  )
  # No pair of nodes brought closer together than the plain layout's tightest.
  spacing <- function(z) {
    d <- as.matrix(stats::dist(cbind(z$x, z$y)))
    diag(d) <- Inf
    min(d)
  }
  expect_gte(spacing(swung), spacing(plain) - 1e-9)
  # Every branch still exactly as long as its allelic distance earned it.
  blen <- function(z) {
    ix <- stats::setNames(seq_len(nrow(z)), z$id)
    sqrt((z$x[ix[edges$to]] - z$x[ix[edges$from]])^2 +
      (z$y[ix[edges$to]] - z$y[ix[edges$from]])^2)
  }
  expect_equal(blen(swung), blen(plain))
})

test_that("the segment-crossing test answers for every segment it is given", {
  # Same trap as .point_seg_dist: vectorised over the second argument, so it
  # cannot be written with ifelse().
  cross <- impl$.crosses_any(0, 0, 10, 0, c(5, 50), c(-5, -5), c(5, 50), c(5, 5), 1e-9)
  expect_true(cross)
  # The crossing is the *second* segment — an implementation that only looked at
  # the first would miss it.
  expect_true(impl$.crosses_any(0, 0, 10, 0, c(50, 5), c(-5, -5), c(50, 5), c(5, 5), 1e-9))
  expect_false(impl$.crosses_any(0, 0, 10, 0, c(50, 60), c(-5, -5), c(50, 60), c(5, 5), 1e-9))
})

test_that("switching the regions on does not move a single node", {
  # Reported: the same database drew a visibly different arrangement with
  # "Show clusters" off. The cluster assignment follows the *threshold*, which
  # is an analysis parameter; whether the regions are painted is a display
  # choice, and a display choice must never reach the layout.
  g <- mst_graph(
    from = c("a", "a", "a", "d", "d", "f"),
    to = c("b", "c", "d", "e", "f", "g"),
    weight = c(1, 2, 40, 1, 3, 90),
    ids = c("a", "b", "c", "d", "e", "f", "g")
  )
  meta <- data.frame(isolate = letters[1:7], stringsAsFactors = FALSE)
  frames <- function(...) {
    mst_plot$mst_frames(g, meta, base_opts(cluster_threshold = 5, ...))
  }
  on <- frames(show_clusters = TRUE)
  off <- frames(show_clusters = FALSE)
  expect_equal(on$coords$x, off$coords$x)
  expect_equal(on$coords$y, off$coords$y)
  # The regions themselves are the only difference.
  expect_true(length(on$blobs) > 0L)
  expect_identical(length(off$blobs), 0L)

  # Nor does any other display-only control.
  for (o in list(
    list(cluster_opacity = 0.9),
    list(cluster_col_scale = "Dark2"),
    list(cluster_label_size = 30),
    list(show_legend = FALSE)
  )) {
    alt <- do.call(frames, c(list(show_clusters = TRUE), o))
    expect_equal(alt$coords$x, on$coords$x, info = names(o))
  }

  # The threshold is allowed to move it — it changes which nodes are in a
  # cluster, so the fan has different branches to steer. Asserted on a real
  # collection rather than here, where seven nodes can fan the same way either
  # way by coincidence.
})

test_that("with nothing clustered the fan is the plain proportional one", {
  # `cl` is all-NA when no clustering is in play, and the membership test
  # answers FALSE for every child of an NA parent — which read as "every branch
  # leaves the cluster" and re-ordered every fan in the tree. A node in no
  # cluster has no region at it and so nothing to point away from.
  set.seed(17)
  edges <- random_tree(40, "random")
  w <- sample(c(1, 2, 3, 20, 60), nrow(edges), replace = TRUE)
  ids <- unique(c(edges$from, edges$to))
  len <- mst_plot$mst_edge_lengths(w, "log", spread = 15)$length

  plain <- mst_plot$mst_layout(edges$from, edges$to, len, ids)
  none <- mst_plot$mst_layout(
    edges$from, edges$to, len, ids,
    cluster = rep(NA_character_, length(ids)), radius = 10, pad = 20
  )
  expect_equal(none$x, plain$x)
  expect_equal(none$y, plain$y)

  # A threshold that clusters nothing is the same thing arriving by another
  # route, and must land in the same place.
  empty <- mst_plot$mst_clusters(ids, edges$from, edges$to, w, 0)$node
  quiet <- mst_plot$mst_layout(
    edges$from, edges$to, len, ids,
    cluster = empty, radius = 10, pad = 20
  )
  expect_equal(quiet$x, plain$x)
})
