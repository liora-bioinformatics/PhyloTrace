box::use(
  igraph[graph_from_data_frame, set_edge_attr],
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

`%||%` <- function(a, b) if (is.null(a)) b else a

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
      for (mode in c("log", "real", "uniform")) {
        len <- mst_plot$mst_edge_lengths(w, mode)$length
        coords <- mst_plot$mst_layout(edges$from, edges$to, len, ids)
        expect_identical(
          mst_plot$mst_count_crossings(coords, edges$from, edges$to),
          0L,
          info = paste(shape, n, mode)
        )
        expect_true(all(is.finite(c(coords$x, coords$y))))
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

test_that("a continuous variable collapses to the node's mean", {
  vals <- mst_plot$mst_node_values(c("b\nc", "f"), demo_meta(), "year")
  expect_identical(vals$levels, NULL)
  expect_equal(vals$value[[1]], 2020.5)
  # Nothing recorded at all is NA rather than NaN, so the colour ramp can find
  # it and give it grey.
  expect_true(is.na(vals$value[[2]]))
})

test_that("shapes are assigned only to levels that have one", {
  shapes <- mst_plot$mst_level_shapes(c("x", "y", "Not recorded"))
  expect_identical(unname(shapes[c("x", "y")]), c("dot", "square"))
  expect_identical(shapes[["Not recorded"]], "triangleDown")
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
  expect_true(grepl("7 isolates", labels[[2]]))
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
    length_mode = "log", spread = 15, shorten_long = TRUE, rotation = 0,
    layers = list(),
    show_clusters = FALSE, cluster_threshold = 10,
    cluster_col_scale = "viridis", cluster_width = 14,
    cluster_opacity = 0.35, cluster_label_size = 18,
    cluster_label_tint = TRUE,
    show_legend = TRUE, legend_ori = "left", canvas_px = c(900, 620)
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

test_that("a mapping onto shape leaves the fill alone", {
  opts <- base_opts(layers = list(list(
    field = "country", title = "Country", aesthetic = "node_shape",
    palette = NULL
  )))
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  expect_identical(unique(fr$nodes$color.background), "#B2FACA")
  # Shape can hold one value per node, so a mixed node takes its commonest —
  # and the legend says which shape means what.
  expect_true(any(grepl("shape", vapply(
    fr$legend, function(i) i$label %||% "", character(1)
  ))))
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

test_that("the region is one path at the requested opacity", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5,
                    cluster_opacity = 0.5, cluster_label_size = 22)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  hook <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), opts, fr
  )$x$events$beforeDrawing
  expect_true(grepl("var A=0.5", hook, fixed = TRUE))
  expect_true(grepl("LS=22", hook, fixed = TRUE))
  # One beginPath and one fill per region: a fill per disc and capsule is what
  # made a translucent region composite into a patchwork.
  expect_identical(
    lengths(regmatches(hook, gregexpr("ctx.fill()", hook, fixed = TRUE))),
    1L
  )
})

test_that("the widget is built without physics and with the layout baked in", {
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), base_opts())
  widget <- mst_plot$build_mst_visnetwork(
    demo_graph(), demo_meta(), base_opts(), fr
  )
  expect_false(widget$x$options$physics$enabled)
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

test_that("the legend can be switched off entirely", {
  opts <- base_opts(show_clusters = TRUE, cluster_threshold = 5,
                    show_legend = FALSE)
  fr <- mst_plot$mst_frames(demo_graph(), demo_meta(), opts)
  widget <- mst_plot$build_mst_visnetwork(demo_graph(), demo_meta(), opts, fr)
  expect_false(grepl("Cluster 1", widget$x$events$afterDrawing, fixed = TRUE))
})
