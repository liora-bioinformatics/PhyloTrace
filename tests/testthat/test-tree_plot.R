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
  expect_equal(fit$nodepoint_size, 2.5)
})

test_that("the sizes stay in proportion at every tree size", {
  for (n in c(5, 50, 346)) {
    fit <- tree_plot$tree_auto_layout(n, width_in = 5.7, label_chars = 36)
    # Node points stay the junior partner of tip points, whatever the scale.
    expect_true(fit$nodepoint_size < fit$tippoint_size)
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
  sizes <- c("tiplab_size", "branch_size", "tippoint_size", "nodepoint_size")
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

# --- Fitting the variable mappings -------------------------------------------

# A metadata table shaped like a real one: a column that cannot group (one
# value), one that groups perfectly (a handful of well-filled groups), one with
# more groups than a ColorBrewer palette has colours, one that is nearly all
# empty, and one with a different value per isolate.
meta_fixture <- function(n = 40) {
  data.frame(
    isolate = sprintf("ISO-%03d", seq_len(n)),
    organism = rep("P. aeruginosa", n),
    purpose = rep(c("surveillance", "outbreak", "screening", "referral"),
      length.out = n),
    country = sprintf("C%02d", seq_len(n) %% 20),
    source = c(rep(NA_character_, n - 3), "blood", "urine", "wound"),
    collected_by = sprintf("lab-%03d", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

test_that("only columns that can group are offered for a mapping", {
  fields <- tree_plot$mapping_fields(meta_fixture())

  # A constant column and a per-isolate one group nothing, and `isolate` is
  # never a mapping.
  expect_false("organism" %in% fields)
  expect_false("collected_by" %in% fields)
  expect_false("isolate" %in% fields)
  expect_true(all(c("purpose", "country", "source") %in% fields))
})

test_that("the best mapping column leads, so the default is the best one", {
  fields <- tree_plot$mapping_fields(meta_fixture())

  # Four well-filled groups beats twenty, which beats a column filled for three
  # isolates out of forty.
  expect_identical(fields[1], "purpose")
  expect_lt(match("country", fields), match("source", fields))
})

test_that("the shape picker is offered only what ggplot2 can draw", {
  fields <- tree_plot$mapping_fields(
    meta_fixture(),
    max_levels = tree_plot$MAX_SHAPE_LEVELS
  )
  # Twenty countries would silently drop fourteen levels' worth of tips.
  expect_false("country" %in% fields)
  expect_true("purpose" %in% fields)
})

test_that("an empty or unusable metadata table yields no mapping", {
  expect_identical(tree_plot$mapping_fields(NULL), character(0))
  expect_identical(
    tree_plot$mapping_fields(data.frame(isolate = c("A", "B"))),
    character(0)
  )
})

test_that("the palette family follows the number of groups", {
  # Inside the smallest brewer palette, a qualitative scale reads best.
  expect_identical(
    tree_plot$scale_categories_for(rep(letters[1:4], 5), "Sequential")[1],
    "Qualitative"
  )
  # Past it, brewer runs out of colours and draws the rest grey, so a generated
  # scale leads instead.
  expect_identical(
    tree_plot$scale_categories_for(as.character(1:40), "Sequential")[1],
    "Gradient"
  )
  # Numbers keep the numeric families entirely.
  expect_identical(
    tree_plot$scale_categories_for(1:40, c("Sequential", "Gradient")),
    c("Sequential", "Gradient")
  )
})

test_that("empty strings do not count as a group", {
  expect_identical(tree_plot$field_levels(c("a", "b", "", NA, "  ")), 2L)
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
