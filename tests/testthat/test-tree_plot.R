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

  lim <- impl$.tiplab_xlim(opts, md, tree_data, max_x)
  frac <- impl$.tiplab_frac(opts, md)
  x_min <- -max_x * 0.05 # the root edge

  # What the labels actually get, once the root edge and ggplot's expansion are
  # counted in the panel, is what was asked for.
  expect_equal(
    (lim - max_x) / ((lim - x_min) * impl$X_EXPANSION),
    frac
  )
  # The naive max_x/(1 - frac) is the version that under-reserved and clipped
  # long isolate names at the panel edge.
  expect_gt(lim, max_x / (1 - frac))
})
