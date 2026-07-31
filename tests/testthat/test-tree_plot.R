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
    (lim - max_x) / ((lim - x_min) * impl$X_EXPANSION),
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

test_that("every strip gets the same width, whatever the count", {
  # Strips used to divide a fixed budget between them, which meant one strip
  # took all of it and a heatmap beside it was left the floor — a 15-column
  # matrix in a tenth of the tree's width. They no longer compete: each is sized
  # for legibility and the canvas grows (see tree_panel_width_in).
  one <- tree_plot$tree_annotation_width(1)
  four <- tree_plot$tree_annotation_width(4)

  expect_equal(one, four)
  # Still a fraction of the tree, not a multiple of it: the 2 this shipped with
  # drew a strip twice as wide as the tree it annotates.
  expect_lt(one, 1)
  expect_gt(one, 0)
})

test_that("annotations are sized by what they have to show", {
  tiles <- function(n) {
    list(
      layers = lapply(seq_len(n), function(i) {
        list(aesthetic = "tile", field = paste0("v", i))
      }),
      heatmaps = list()
    )
  }
  heat <- function(n) {
    # `paste0("c", seq_len(0))` is "c", not character(0) — R recycles a
    # zero-length argument to "". So "no heatmap" has to be an empty panel list.
    panels <- if (n) list(list(kind = "amr", cols = paste0("c", seq_len(n)))) else list()
    list(layers = list(), heatmaps = panels)
  }

  # More strips, more room — they do not share a fixed allowance.
  expect_gt(tree_plot$tile_total(tiles(3)), tree_plot$tile_total(tiles(1)))
  # More columns, more room.
  expect_gt(tree_plot$heatmap_total(heat(15)), tree_plot$heatmap_total(heat(3)))
  # Nothing mapped, nothing reserved.
  expect_equal(tree_plot$tile_total(tiles(0)), 0)
  expect_equal(tree_plot$heatmap_total(heat(0)), 0)
})

test_that("a heatmap beside a tile strip keeps its own width", {
  # The reported fault: one strip took the whole budget and the matrix collapsed
  # to the floor. A strip must cost the heatmap nothing.
  cols <- paste0("c", 1:15)
  alone <- list(layers = list(), heatmaps = list(list(kind = "amr", cols = cols)))
  beside <- list(
    layers = list(list(aesthetic = "tile", field = "v")),
    heatmaps = list(list(kind = "amr", cols = cols))
  )
  expect_equal(
    tree_plot$heatmap_total(alone),
    tree_plot$heatmap_total(beside)
  )
})

test_that("a heatmap starts past the tile strips, not on top of them", {
  # gheatmap's offset is absolute from the tree's edge, geom_fruit's is relative
  # to the annotation before it — so the strips are invisible to the heatmap's
  # placement unless counted. Not counting them drew the matrix over the strip.
  cols <- c("a", "b", "c")
  bare <- list(layers = list(), heatmaps = list(list(kind = "amr", cols = cols)))
  tiled <- list(
    layers = list(list(aesthetic = "tile", field = "v")),
    heatmaps = list(list(kind = "amr", cols = cols))
  )
  off_bare <- tree_plot$heatmap_panels(bare, 1, 0.5)$panels[[1]]$offset
  off_tiled <- tree_plot$heatmap_panels(tiled, 1, 0.5)$panels[[1]]$offset

  # Pushed out by at least the strip's own width.
  expect_gte(off_tiled - off_bare, tree_plot$tile_total(tiled) * 0.99)
})

test_that("annotations together never dwarf the tree", {
  huge <- list(
    layers = lapply(1:4, function(i) list(aesthetic = "tile", field = paste0("v", i))),
    heatmaps = list(list(kind = "amr", cols = paste0("c", 1:60)))
  )
  expect_lte(tree_plot$annotation_total(huge), impl$ANNOTATION_SPAN_MAX + 1e-9)
})

test_that("a branch too narrow to hold its number does not get one", {
  # The reported fault, as geometry. One 1600-unit stem takes the whole span
  # and thirty hairlines sit behind it; picking the longest 25 put twenty-five
  # numbers on top of each other where the branches were sub-pixel.
  len <- c(1600.5, 1531.5, 1542, 1563.5, 1579.38, 3.78, 3.81, 9.68, 11.03, 4.5)
  y <- c(18, 31, 33, 34, 35, 8, 10, 14, 16, 20)
  digits <- tree_plot$tree_branch_digits(len)
  keep <- tree_plot$tree_branch_keep(
    len, y, max(len), 3.6, 4 * impl$BRANCH_ABOVE_SHRINK, digits
  )

  # Only the five long ones survive; every hairline is dropped.
  expect_setequal(keep, 1:5)
})

test_that("two labels never land on the same row", {
  # Internal nodes deep in a ladder sit fractions of a row apart, which is the
  # other half of what stacked the numbers.
  len <- rep(100, 5)
  keep <- tree_plot$tree_branch_keep(
    len, c(1, 1.2, 1.4, 5, 9), 100, 5.5, 3, 0L
  )
  expect_equal(length(keep), 3L)

  # Where two compete for a row, the longer branch wins it.
  keep <- tree_plot$tree_branch_keep(
    c(50, 90), c(2, 2.3), 100, 5.5, 3, 0L
  )
  expect_equal(keep, 2L)
})

test_that("branch labels are capped however many would fit", {
  n <- 60L
  keep <- tree_plot$tree_branch_keep(
    rep(50, n), seq_len(n), 50, 5.5, 3, 0L
  )
  expect_equal(length(keep), impl$BRANCH_LABEL_MAX)
})

test_that("nothing labellable draws nothing rather than erroring", {
  expect_length(tree_plot$tree_branch_keep(numeric(0), numeric(0), 1, 5.5, 3, 0L), 0)
  expect_length(tree_plot$tree_branch_keep(rep(0, 5), 1:5, 1, 5.5, 3, 0L), 0)
  expect_length(tree_plot$tree_branch_keep(c(1, 2), c(1, 2), 0, 5.5, 3, 0L), 0)
  expect_length(tree_plot$tree_branch_keep(c(1, 2), c(1, 2), 10, 0, 3, 0L), 0)
})

test_that("one decimal count serves the whole figure", {
  # Allelic distances count mismatched loci, so they are integral; NJ halves
  # them at most. Both print exactly, without dragging "8.00" behind "12.50".
  expect_equal(tree_plot$tree_branch_digits(c(1, 5, 120)), 0L)
  expect_equal(tree_plot$tree_branch_digits(c(1.5, 3, 12.5)), 1L)
  expect_equal(tree_plot$tree_branch_format(c(1.5, 3), 1L), c("1.5", "3.0"))

  # Anything else keeps about three significant figures, so a 1600-unit branch
  # is not printed to two decimals beside a 3-unit one.
  expect_equal(tree_plot$tree_branch_digits(c(3.78, 1600.38)), 0L)
  expect_equal(tree_plot$tree_branch_digits(c(0.013, 0.4)), 2L)
  expect_equal(tree_plot$tree_branch_digits(numeric(0)), 0L)
})

# --- Whole-tree distance axis -------------------------------------------------

test_that("axis ticks land at round distances within the tree's own depth", {
  breaks <- tree_plot$tree_axis_breaks(1637)
  expect_true(all(breaks >= 0 & breaks <= 1637))
  expect_false(is.unsorted(breaks))
  expect_equal(breaks[1], 0)

  # A degenerate span draws no axis at all rather than erroring.
  expect_length(tree_plot$tree_axis_breaks(0), 0)
  expect_length(tree_plot$tree_axis_breaks(NA_real_), 0)
  expect_length(tree_plot$tree_axis_breaks(-5), 0)
})

test_that("the axis switch is independent of the scale bar", {
  # Switched off draws nothing.
  expect_null(impl$tree_axis_layer(list(axis_show = FALSE), 100, -1))

  # Switched on with a real span draws the axis line, the tick marks and the
  # value labels — three layers, the last of which is text.
  layers <- impl$tree_axis_layer(
    list(axis_show = TRUE, line_color = "#000000", branch_size = 4),
    100,
    -1
  )
  expect_equal(length(layers), 3L)
  expect_true(inherits(layers[[1]]$geom, "GeomSegment"))
  expect_true(inherits(layers[[2]]$geom, "GeomSegment"))
  expect_true(inherits(layers[[3]]$geom, "GeomText"))
})

test_that("a zero-depth tree draws no axis rather than a single stuck tick", {
  expect_null(
    impl$tree_axis_layer(list(axis_show = TRUE, line_color = "#000000"), 0, -1)
  )
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
  td <- data.frame(
    branch.length = c(10, 20, 30),
    branch = c(5, 15, 25),
    y = c(1, 2, 3)
  )
  layer <- impl$tree_branch_layer(
    list(branch_show = TRUE, branch_size = 4, branch_color = "#000000"),
    td,
    30,
    5.5
  )
  expect_true(inherits(layer$geom, "GeomText"))
  expect_false(inherits(layer$geom, "GeomLabel"))
  # Lifted clear of the line rather than centred on it.
  expect_lt(layer$aes_params$vjust, 0)
  # The chosen branches carry the layer as their own data, so an unchosen one
  # contributes nothing to it at all.
  expect_true(is.data.frame(layer$data))
  expect_setequal(names(layer$data), c("x", "y", "label"))
})

test_that("branch labels switched off draw nothing", {
  expect_null(impl$tree_branch_layer(list(branch_show = FALSE), NULL, 1, 5.5))
})

test_that("a tree with no drawable branch length draws no label layer", {
  td <- data.frame(branch.length = c(0, NA, 0), branch = 1:3, y = 1:3)
  expect_null(
    impl$tree_branch_layer(
      list(branch_show = TRUE, branch_size = 4, branch_color = "#000000"),
      td,
      1,
      5.5
    )
  )
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
    tiplab_color = "#000000", layers = list(),
    branch_show = FALSE, branch_size = 3,
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
    tiplab_color = "#000000",
    layers = list(list(id = "L1", field = "gone_away", title = "Gone",
      aesthetic = "tiplab_color", palette = "Set1", n_levels = 3L,
      continuous = FALSE, transform = NULL, auto = TRUE)),
    branch_show = FALSE, branch_size = 3,
    branch_color = "#000000", tippoint_show = FALSE, tippoint_alpha = 1,
    tippoint_size = 3, tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221", heatmaps = list(),
    rootedge_show = TRUE, treescale_show = TRUE, width_in = 7,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )

  expect_true(inherits(tree_plot$build_tree_ggtree(tree, meta, opts), "ggplot"))
})

test_that("the background colour is the legend's background too", {
  # ggplot2 leaves the guide box on its own theme colours — white behind the
  # box, grey behind each key — so a legend on a dark background arrived as a
  # pale block stuck to the tree. One colour control, one background.
  set.seed(13)
  tree <- ape::rtree(6)
  tree$tip.label <- sprintf("iso%02d", 1:6)
  meta <- data.frame(
    isolate = tree$tip.label,
    ward = rep(c("ICU", "ER"), 3),
    stringsAsFactors = FALSE
  )

  opts <- list(
    root = "Automatic", layout = "rectangular", line_color = "#ffffff",
    bg = "#101820", tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3,
    tiplab_color = "#ffffff",
    layers = list(list(id = "L1", field = "ward", title = "Ward",
      aesthetic = "tippoint_color", palette = "Set1", n_levels = 2L,
      continuous = FALSE, transform = NULL, auto = TRUE)),
    branch_show = FALSE, branch_size = 3,
    branch_color = "#ffffff", tippoint_show = TRUE, tippoint_alpha = 1,
    tippoint_size = 3, tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221", heatmaps = list(),
    rootedge_show = TRUE, treescale_show = TRUE, width_in = 7,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )

  # The theme is set on the ggtree plot before it is wrapped into a grob, so
  # intercept it there (same trick as the heatmap-panels test above).
  inner <- NULL
  build <- impl$.build_tree_ggtree
  shadow <- new.env(parent = environment(build))
  assign("as.ggplot", function(plot, ...) {
    inner <<- plot
    ggplotify::as.ggplot(plot, ...)
  }, envir = shadow)
  environment(build) <- shadow
  suppressWarnings(suppressMessages(build(tree, meta, opts)))

  for (el in c("legend.background", "legend.box.background", "legend.key")) {
    expect_identical(inner$theme[[el]]$fill, opts$bg)
  }
})

# --- Missing values ----------------------------------------------------------

test_that("blanks and NA become one explicit level, last", {
  m <- tree_plot$mapped_values(c("b", "", "a", NA, "  ", "b"))
  expect_identical(levels(m), c("a", "b", tree_plot$MISSING_LABEL))
  # Three of the six values were unrecorded, one way or another.
  expect_identical(sum(m == tree_plot$MISSING_LABEL), 3L)
})

test_that("a column with nothing missing gains no extra level", {
  m <- tree_plot$mapped_values(c("a", "b", "a"))
  expect_identical(levels(m), c("a", "b"))
  expect_false(tree_plot$MISSING_LABEL %in% levels(m))
})

test_that("numbers and dates pass through untouched", {
  expect_identical(tree_plot$mapped_values(c(1, 2, NA)), c(1, 2, NA))
  d <- as.Date(c("2024-01-01", NA))
  expect_identical(tree_plot$mapped_values(d), d)
})

test_that("the scale covers every level the data actually has", {
  # The reported crash: "Insufficient values in manual scale. 8 needed but only
  # 7 provided." field_levels() counts distinct values *excluding* blanks, but
  # the scale it sized was handed the raw column, where "" is a level of its
  # own. Levels and colours now come from the same place.
  raw <- c(rep(paste0("cat", 1:7), 3), "", NA)
  vals <- tree_plot$mapped_values(raw)
  cols <- tree_plot$tree_level_colors(levels(vals), "Set1")

  expect_identical(length(cols), 8L)
  expect_identical(names(cols), levels(vals))
  expect_true(all(levels(vals) %in% names(cols)))
})

test_that("the missing level is grey whichever palette is chosen", {
  lv <- c("a", "b", tree_plot$MISSING_LABEL)
  for (pal in c("Set1", "viridis", "Blues", NULL)) {
    cols <- tree_plot$tree_level_colors(lv, pal)
    expect_identical(cols[[tree_plot$MISSING_LABEL]], tree_plot$MISSING_COLOR)
    # And it never takes a colour a real category is using.
    expect_false(tree_plot$MISSING_COLOR %in% cols[c("a", "b")])
  }
})

test_that("a mapping with blanks builds without erroring", {
  # End to end, because the crash only appeared once the scale met the data.
  set.seed(21)
  n <- 24
  tree <- ape::rtree(n)
  tree$tip.label <- sprintf("iso%02d", seq_len(n))
  meta <- data.frame(
    isolate = tree$tip.label,
    purpose = c(rep(paste0("cat", 1:7), 3), "", "", NA),
    stringsAsFactors = FALSE
  )

  opts <- list(
    root = "Automatic", layout = "rectangular", line_color = "#000000",
    bg = "#ffffff", tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3,
    tiplab_color = "#000000",
    layers = list(list(id = "L1", field = "purpose", title = "Purpose",
      aesthetic = "tiplab_color", palette = "Set1", n_levels = 7L,
      continuous = FALSE, transform = NULL, auto = TRUE)),
    branch_show = FALSE, branch_size = 3,
    branch_color = "#000000", tippoint_show = FALSE, tippoint_alpha = 1,
    tippoint_size = 3, tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221", heatmaps = list(),
    rootedge_show = TRUE, treescale_show = TRUE, width_in = 5.5,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )

  expect_no_error(suppressWarnings(
    tree_plot$build_tree_ggtree(tree, meta, opts)
  ))
})

test_that("a shape mapping keeps a mark for the missing level", {
  # The engine caps shapeable variables at six levels, but "not recorded" is a
  # seventh the reader still needs to see — TREE_SHAPES alone would run out.
  expect_identical(length(tree_plot$TREE_SHAPES), 6L)
  m <- tree_plot$mapped_values(c(paste0("g", 1:6), ""))
  expect_identical(length(levels(m)), 7L)
})

# --- Legend width ------------------------------------------------------------

test_that("a long legend label is wrapped, not left to widen the guide box", {
  # ggplot2 sizes the guide box from its widest label and caps nothing, so one
  # 37-character category claimed nearly half a 5.5in canvas and left the tree a
  # hairline.
  wrapped <- impl$.wrap_legend_labels("Antimicrobial resistance surveillance")
  expect_true(grepl("\n", wrapped, fixed = TRUE))
  expect_true(all(
    nchar(strsplit(wrapped, "\n", fixed = TRUE)[[1]]) <=
      tree_plot$LEGEND_LABEL_WRAP
  ))
  # Short labels are left alone.
  expect_identical(impl$.wrap_legend_labels("Clinical"), "Clinical")
})

test_that("the legend estimate is bounded by the wrap", {
  short <- data.frame(v = rep("ab", 4), stringsAsFactors = FALSE)
  long <- data.frame(
    v = rep(strrep("x", 120), 4),
    stringsAsFactors = FALSE
  )
  layer <- list(field = "v", title = "V", n_levels = 1L)
  # A 120-character label cannot ask for 120 characters of guide box.
  expect_equal(
    tree_plot$tree_legend_width_in(list(layer), long, 10, 5.5),
    tree_plot$tree_legend_width_in(
      list(layer),
      data.frame(v = rep(strrep("x", 60), 4), stringsAsFactors = FALSE),
      10, 5.5
    )
  )
  expect_gt(
    tree_plot$tree_legend_width_in(list(layer), long, 10, 5.5),
    tree_plot$tree_legend_width_in(list(layer), short, 10, 5.5)
  )
})

# --- The adaptive canvas -----------------------------------------------------

test_that("the panel grows so the tree keeps its own width", {
  md <- data.frame(isolate = strrep("A", 30), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 2.5, width_in = 5.5,
    layers = list(), heatmaps = list()
  )
  # Nothing beside the tree: the panel is the tree's budget, unchanged.
  expect_equal(tree_plot$tree_panel_width_in(opts, md, 5.5), 5.5)

  # A heatmap has to come out of extra canvas, not out of the tree.
  heated <- opts
  heated$heatmaps <- list(list(kind = "amr", cols = c("a", "b", "c")))
  expect_gt(tree_plot$tree_panel_width_in(heated, md, 5.5), 5.5)

  # And a tile strip likewise.
  tiled <- opts
  tiled$layers <- list(list(aesthetic = "tile", field = "v"))
  expect_gt(tree_plot$tree_panel_width_in(tiled, md, 5.5), 5.5)
})

test_that("the grown panel leaves the tree exactly its budget", {
  # This is the invariant the whole fix rests on, so it is checked against the
  # axis solve rather than asserted in prose: of the widened panel, the share
  # the tree and its labels occupy must be the original budget.
  md <- data.frame(isolate = strrep("A", 30), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 2.5, width_in = 5.5,
    layers = list(), rootedge_show = FALSE,
    heatmaps = list(list(kind = "amr", cols = c("a", "b", "c")))
  )
  panel <- tree_plot$tree_panel_width_in(opts, md, 5.5)

  max_x <- 1
  tree_data <- data.frame(x = c(0, 0.5, max_x))
  fit <- impl$.tiplab_xlim(
    opts, md, tree_data, max_x, tree_plot$annotation_total(opts)
  )
  axis <- fit$limit - 0 # x_min is 0 with the root edge off
  # Physical inches the tree and its labels get out of the widened panel.
  tree_and_labels <- panel * (max_x / axis + fit$reserve / axis)

  expect_equal(tree_and_labels, 5.5, tolerance = 0.02)
})
