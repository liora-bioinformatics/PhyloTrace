box::use(
  app / logic / tree_plot,
)

impl <- attr(tree_plot, "namespace")

# The calibration anchor. tree_auto_layout's constants were chosen so that the
# fit reproduces the values this module shipped as fixed defaults at the one
# dataset size they suited — if this drifts, every other fitted plot has moved
# with it.
test_that("the fit reproduces the shipped defaults at ~15 tips", {
  fit <- tree_plot$tree_auto_layout(15, width_in = 5.7, label_chars = 20)
  expect_equal(fit$aspect, 0.6)
  expect_equal(fit$tiplab_size, 4)
  expect_true(fit$labels_legible)
})

test_that("every fitted size keeps the shipped proportions at ~15 tips", {
  fit <- tree_plot$tree_auto_layout(15, width_in = 5.7, label_chars = 20)
  # The other sizes are read off the same row pitch, so they too come back at
  # the values the sidebar shipped with.
  expect_equal(fit$branch_size, 4)
  expect_equal(fit$tippoint_size, 4)
})

test_that("the sizes stay in proportion at every tree size", {
  for (n in c(5, 50, 346)) {
    fit <- tree_plot$tree_auto_layout(n, width_in = 5.7, label_chars = 36)
    # Only the tip labels answer to label width, so they are the first to be
    # squeezed below what the row alone would allow.
    expect_true(fit$tiplab_size <= fit$branch_size)
  }
})

test_that("taller plots and smaller labels as the tree grows", {
  fits <- lapply(
    c(10, 30, 100, 300),
    function(n) tree_plot$tree_auto_layout(n, width_in = 5.7, label_chars = 20)
  )
  aspects <- vapply(fits, function(f) f$aspect, numeric(1))
  sizes <- vapply(fits, function(f) f$tiplab_size, numeric(1))

  expect_false(is.unsorted(aspects))
  expect_false(is.unsorted(rev(sizes)))
  # Strictly, not just weakly, over a range this wide.
  expect_gt(aspects[4], aspects[1])
  expect_lt(sizes[4], sizes[1])
})

test_that("long labels cap the size before the row pitch does", {
  # Few enough tips that the rows leave plenty of vertical room, so only the
  # label width can be what limits the font.
  roomy <- tree_plot$tree_auto_layout(8, width_in = 5.7, label_chars = 6)
  cramped <- tree_plot$tree_auto_layout(8, width_in = 5.7, label_chars = 36)

  expect_lt(cramped$tiplab_size, roomy$tiplab_size)
  # 36-character isolate names on a 5.7in panel: the old fixed size of 4 needed
  # 55% of the panel, which is what clipped them.
  expect_lt(cramped$tiplab_size, 4)
})

test_that("a wider panel earns a squatter plot and larger labels", {
  narrow <- tree_plot$tree_auto_layout(60, width_in = 4, label_chars = 20)
  wide <- tree_plot$tree_auto_layout(60, width_in = 10, label_chars = 20)

  expect_lt(wide$aspect, narrow$aspect)
  expect_gt(wide$tiplab_size, narrow$tiplab_size)
})

test_that("every fitted value stays within its bounds at any tree size", {
  sizes <- c("tiplab_size", "branch_size", "tippoint_size")
  for (n in c(1, 3, 344, 5000)) {
    fit <- tree_plot$tree_auto_layout(n, width_in = 5.7, label_chars = 36)
    expect_gte(fit$aspect, impl$TIP_ASPECT_MIN)
    expect_lte(fit$aspect, impl$TIP_ASPECT_MAX)
    for (s in sizes) {
      expect_gte(fit[[s]], impl$TIP_SIZE_MIN)
      # Free to shrink, but never grown far past what the sidebar ships with —
      # a four-tip tree has rows deep enough to justify 12mm tip points.
      expect_lte(
        fit[[s]],
        impl$TIP_GROWTH * tree_plot$TREE_FIT_DEFAULTS[[s]]
      )
    }
  }
})

test_that("labels stay legible for a few hundred tips and give up past that", {
  expect_true(
    tree_plot$tree_auto_layout(344, width_in = 5.7, label_chars = 36)$
      labels_legible
  )
  expect_false(
    tree_plot$tree_auto_layout(5000, width_in = 5.7, label_chars = 36)$
      labels_legible
  )
})

test_that("a linear tree is drawn to the edges of its canvas", {
  linear <- tree_plot$tree_auto_layout(50, width_in = 5.5)
  circ <- tree_plot$tree_auto_layout(50, width_in = 5.5, layout = "circular")

  # as.ggplot(scale = zoom) spends (1 - zoom) of the canvas on a blank border,
  # half of it above the plot and half below — the empty band over and under a
  # tall tree. Nothing on a linear layout can run off the edge (the tip labels
  # are the only thing drawn past the tree, and .tiplab_xlim reserves for
  # them), so it takes the whole canvas.
  expect_equal(linear$zoom, 1)
  expect_equal(linear$h, 0)

  # Circular labels radiate outward with nothing reserving room for them, so
  # that layout keeps the border it shipped with.
  expect_equal(circ$zoom, tree_plot$TREE_FIT_DEFAULTS$zoom)
  expect_equal(circ$h, tree_plot$TREE_FIT_DEFAULTS$h)
})

test_that("circular layouts fit the font to the circumference", {
  # The aspect ratio is the linear fit either way — a circular tree is rendered
  # square, so the slider is left holding what a switch back would need.
  linear <- tree_plot$tree_auto_layout(400, width_in = 5.7, label_chars = 10)
  circ <- tree_plot$tree_auto_layout(
    400,
    width_in = 5.7,
    layout = "circular",
    label_chars = 10
  )
  expect_equal(circ$aspect, linear$aspect)

  # A tall linear plot has far more room per tip at this count than a square
  # circular one, so the fitted font is smaller — small enough to give up on.
  expect_lt(circ$tiplab_size, linear$tiplab_size)
  expect_false(circ$labels_legible)
})

test_that("an unusable panel width falls back rather than erroring", {
  fallback <- tree_plot$tree_auto_layout(15, width_in = NULL)
  expect_equal(fallback, tree_plot$tree_auto_layout(15, width_in = 5.5))
  expect_equal(fallback, tree_plot$tree_auto_layout(15, width_in = 0))
  expect_equal(fallback, tree_plot$tree_auto_layout(15, width_in = NA_real_))
})

# --- Tip-label reserve -------------------------------------------------------

test_that("the label reserve follows the labels being drawn", {
  md <- data.frame(
    isolate = c("short", "a-much-longer-isolate-name-here"),
    stringsAsFactors = FALSE
  )
  opts <- list(tiplab_show = TRUE, tiplab = "isolate", width_in = 5.7)

  small <- impl$.tiplab_frac(modifyList(opts, list(tiplab_size = 2)), md)
  large <- impl$.tiplab_frac(modifyList(opts, list(tiplab_size = 6)), md)

  expect_lt(small, large)
  expect_lte(large, 0.5)
  # Hidden labels need no reserve at all; an unknown panel width falls back to
  # the flat 0.375 the plot used to reserve unconditionally.
  expect_lt(
    impl$.tiplab_frac(
      modifyList(opts, list(tiplab_show = FALSE, tiplab_size = 4)),
      md
    ),
    0.05
  )
  expect_equal(
    impl$.tiplab_frac(
      list(tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 4),
      md
    ),
    0.375
  )
})

test_that("the x limit hands the labels their share of the whole panel", {
  md <- data.frame(isolate = strrep("A", 36), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE,
    tiplab = "isolate",
    tiplab_size = 2.3,
    width_in = 5.5,
    rootedge_show = TRUE
  )
  max_x <- 1
  tree_data <- data.frame(x = c(0, 0.5, max_x))

  solved <- impl$.tiplab_xlim(opts, md, tree_data, max_x)
  lim <- solved$limit
  frac <- impl$.tiplab_frac(opts, md)
  x_min <- -max_x * 0.05 # the root edge

  # What the labels actually get, once the root edge, ggplot's expansion and the
  # clearance the annotation matrix needs are counted in, is what was asked for.
  expect_equal(
    (lim - max_x) / ((lim - x_min) * impl$X_EXPANSION * impl$HEATMAP_CLEARANCE),
    frac
  )
  # The reserve is where the labels end, so an annotation matrix starting there
  # begins past them and still inside the axis.
  expect_true(solved$reserve > 0)
  expect_lt(max_x + solved$reserve, lim * 1.001)
  # The naive max_x/(1 - frac) is the version that under-reserved and clipped
  # long isolate names at the panel edge.
  expect_gt(lim, max_x / (1 - frac))
})

# --- Annotations, legend and scale bar ---------------------------------------

test_that("the legend wraps once it is taller than the plot", {
  expect_identical(tree_plot$tree_legend_ncol(6), 1L)
  expect_identical(tree_plot$tree_legend_ncol(18), 1L)
  expect_true(tree_plot$tree_legend_ncol(46) > 1L)
  # Never so many columns that the legend crowds out the tree.
  expect_true(tree_plot$tree_legend_ncol(400) <= 4L)
})

test_that("the scale bar carries a round number", {
  expect_equal(tree_plot$tree_nice_width(1.36116098546807), 1)
  expect_equal(tree_plot$tree_nice_width(137.4), 100)
  expect_equal(tree_plot$tree_nice_width(268), 200)
  expect_equal(tree_plot$tree_nice_width(0.084), 0.05)
  # Degenerate trees (a single distance of zero) must not produce Inf or NaN.
  expect_true(is.finite(tree_plot$tree_nice_width(0)))
})

test_that("annotation strips share a budget instead of each taking the tree", {
  one <- tree_plot$tree_annotation_width(1)
  four <- tree_plot$tree_annotation_width(4)

  # pwidth is a multiple of the tree's own width; the 2 this shipped with drew a
  # strip twice as wide as the tree.
  expect_lt(one, 1)
  expect_lt(four, one)
  expect_equal(four * 4, one, tolerance = 0.02)
})

test_that("the branch-label cutoff leaves a readable number of labels", {
  # ~690 branches for the 346-isolate database: labelling 90% of them (the
  # shipped cutoff of 10) is a band of text over the tree.
  big <- tree_plot$tree_branch_cutoff(690)
  expect_true(big > 90 && big < 100)

  # At ~15 tips (27 branches) the fit lands near the 10 the control shipped
  # with, which is where that default was reasonable.
  expect_true(abs(tree_plot$tree_branch_cutoff(27) - 10) < 10)

  # Never past the slider, and never negative on a tree with fewer branches
  # than the target.
  expect_equal(tree_plot$tree_branch_cutoff(3), 0)
  expect_true(tree_plot$tree_branch_cutoff(100000) <= 99)
})

# --- The legend's reserved column --------------------------------------------

test_that("nothing mapped costs the legend no width", {
  md <- data.frame(country = rep("Germany", 5), stringsAsFactors = FALSE)
  expect_equal(tree_plot$tree_legend_width_in(list(), md, 10, 5.5), 0)
})

test_that("the legend reserve grows with the labels it has to hold", {
  short <- data.frame(v = rep(c("a", "b"), 10), stringsAsFactors = FALSE)
  long <- data.frame(
    v = rep(c("a very long category name indeed", "b"), 10),
    stringsAsFactors = FALSE
  )
  layer <- list(field = "v", title = "V", n_levels = 2L)

  narrow <- tree_plot$tree_legend_width_in(list(layer), short, 10, 5.5)
  wide <- tree_plot$tree_legend_width_in(list(layer), long, 10, 5.5)

  expect_gt(narrow, 0)
  expect_gt(wide, narrow)
})

test_that("the legend can never take more than its share of the canvas", {
  # A 46-level mapping with long labels must not squeeze the tree to nothing —
  # which is what an unbounded guide box beside a fixed canvas would do.
  md <- data.frame(
    v = sprintf("an extremely long label number %02d", 1:46),
    stringsAsFactors = FALSE
  )
  layer <- list(field = "v", title = "V", n_levels = 46L)
  w <- tree_plot$tree_legend_width_in(list(layer), md, 10, 5.5)
  expect_lte(w, 0.35 * 5.5)
})

# --- Branch labels -----------------------------------------------------------

test_that("branch numbers are text above the line, not a box on it", {
  # They used to be geom_label2 centred on the branch, which hid the very line
  # the number describes behind an opaque panel. Guards the revert.
  layer <- impl$tree_branch_layer(
    list(branch_show = TRUE, branch_cutoff = 10, branch_size = 4,
      branch_color = "#000000"),
    c(0.1, 0.2, 0.3)
  )
  expect_true(inherits(layer$geom, "GeomText"))
  expect_false(inherits(layer$geom, "GeomLabel"))
  # Lifted clear of the line rather than centred on it.
  expect_lt(layer$aes_params$vjust, 0)
})

test_that("branch labels switched off draw nothing", {
  expect_null(impl$tree_branch_layer(list(branch_show = FALSE), c(0.1, 0.2)))
})

# --- Annotation geometry -----------------------------------------------------

test_that("the axis solve accounts for every annotation drawn beside the tree", {
  # Leaving the tile strips out of this is what drew a mapped strip outside the
  # panel, where it silently disappeared.
  bare <- list(layers = list(), heatmaps = list())
  tiled <- list(
    layers = list(list(aesthetic = "tile", field = "v")),
    heatmaps = list()
  )
  heated <- list(
    layers = list(),
    heatmaps = list(list(kind = "amr", cols = c("a", "b")))
  )

  expect_equal(tree_plot$annotation_total(bare), 0)
  expect_gt(tree_plot$annotation_total(tiled), 0)
  expect_gt(tree_plot$annotation_total(heated), 0)
})

test_that("stacked heatmap panels are laid out end to end, never overlapping", {
  opts <- list(
    layers = list(),
    heatmaps = list(
      list(kind = "amr", cols = c("a", "b"), title = "AMR"),
      list(kind = "custom", cols = "c", title = "Custom")
    )
  )
  panels <- tree_plot$heatmap_panels(opts, tree_span = 1, label_reserve = 0.5)$panels

  expect_identical(length(panels), 2L)
  # The second starts past the end of the first.
  expect_gte(panels[[2]]$offset, panels[[1]]$offset + panels[[1]]$width)
  # Both clear the tip labels.
  expect_gte(panels[[1]]$offset, 0.5)
  # Width follows column count: two columns get more room than one.
  expect_gt(panels[[1]]$width, panels[[2]]$width)
})

test_that("the heatmap header reserve follows the longest column name", {
  short <- list(heatmaps = list(list(kind = "custom", cols = "a")))
  long <- list(heatmaps = list(list(
    kind = "amr",
    cols = "amr_Beta-lactam-and-then-some-more"
  )))
  expect_lt(
    tree_plot$heatmap_header_frac(short, 40),
    tree_plot$heatmap_header_frac(long, 40)
  )
  # No heatmaps, no reserve worth taking from the tree.
  expect_lt(tree_plot$heatmap_header_frac(list(heatmaps = list()), 40), 0.05)
})

test_that("two heatmap panels build as two independent fill scales", {
  # gheatmap chaining is not a documented use, and a shared fill scale across
  # panels would collapse AMR's two-colour key and a custom panel's categories
  # into one legend that explains neither. This is the guard.
  set.seed(11)
  n <- 12
  tree <- ape::rtree(n)
  tree$tip.label <- sprintf("iso%02d", seq_len(n))
  meta <- data.frame(
    isolate = tree$tip.label,
    `amr_Beta-lactam` = rep(c("blaOXA", ""), length.out = n),
    custom_ward = rep(c("ICU", "ER"), length.out = n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  opts <- list(
    root = "Automatic", layout = "rectangular", line_color = "#000000",
    bg = "#ffffff", tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3,
    align = TRUE, tiplab_color = "#000000", layers = list(),
    branch_show = FALSE, branch_size = 3, branch_cutoff = 10,
    branch_color = "#000000", tippoint_show = FALSE, tippoint_alpha = 1,
    tippoint_size = 3, tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221",
    heatmaps = list(
      list(kind = "amr", cols = "amr_Beta-lactam", palette = "Reds",
        title = "AMR screening"),
      list(kind = "custom", cols = "custom_ward", palette = "Blues",
        title = "Custom variables")
    ),
    rootedge_show = TRUE, treescale_show = TRUE, width_in = 7,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )

  # Capture the plot before build_tree_ggtree wraps it into a fixed-size grob,
  # so the tile layers can be inspected as data.
  inner <- NULL
  build <- impl$.build_tree_ggtree
  shadow <- new.env(parent = environment(build))
  assign("as.ggplot", function(plot, ...) {
    inner <<- plot
    ggplotify::as.ggplot(plot, ...)
  }, envir = shadow)
  environment(build) <- shadow

  msgs <- character(0)
  p <- withCallingHandlers(
    suppressWarnings(build(tree, meta, opts)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(inherits(p, "ggplot"))
  # The message ggplot2 emits when a second scale replaces the first — exactly
  # what would mean the two panels had collapsed into one key.
  expect_false(any(grepl("Scale for fill is already present", msgs)))

  # gheatmap warns about "removed rows" while sizing an intermediate plot; the
  # finished one must still carry every isolate in both panels. This is the
  # assertion .muffled_tree_warnings leans on.
  built <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(inner)))
  tiles <- which(vapply(
    inner$layers,
    function(l) grepl("Tile", class(l$geom)[1]),
    logical(1)
  ))
  expect_identical(length(tiles), 2L)
  for (i in tiles) {
    expect_identical(nrow(built$data[[i]]), as.integer(n))
    expect_false(any(is.na(built$data[[i]]$fill)))
  }
})

test_that("a layer naming a column the database no longer has is dropped", {
  # A saved Analysis can outlive the column it mapped; reaching aes() with it
  # would error rather than degrade.
  set.seed(12)
  tree <- ape::rtree(6)
  tree$tip.label <- sprintf("iso%02d", 1:6)
  meta <- data.frame(isolate = tree$tip.label, stringsAsFactors = FALSE)

  opts <- list(
    root = "Automatic", layout = "rectangular", line_color = "#000000",
    bg = "#ffffff", tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3,
    align = TRUE, tiplab_color = "#000000",
    layers = list(list(id = "L1", field = "gone_away", title = "Gone",
      aesthetic = "tiplab_color", palette = "Set1", n_levels = 3L,
      continuous = FALSE, transform = NULL, auto = TRUE)),
    branch_show = FALSE, branch_size = 3, branch_cutoff = 10,
    branch_color = "#000000", tippoint_show = FALSE, tippoint_alpha = 1,
    tippoint_size = 3, tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221", heatmaps = list(),
    rootedge_show = TRUE, treescale_show = TRUE, width_in = 7,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )

  expect_true(inherits(tree_plot$build_tree_ggtree(tree, meta, opts), "ggplot"))
})
