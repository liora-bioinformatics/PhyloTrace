box::use(
  app / logic / tree_plot,
)

impl <- attr(tree_plot, "namespace")

# The calibration anchor. tree_auto_layout's constants were chosen so that the
# fit reproduces the values this module shipped as fixed defaults at the one
# dataset size they suited — if this drifts, every other fitted plot has moved
# with it.
test_that("the fit reproduces its calibration at ~15 tips", {
  # The anchor. TIP_ROW_IN is the one number the whole linear fit hangs off,
  # and these are what it produces at the one dataset size the module once
  # shipped fixed defaults for — if this drifts, every other fitted plot has
  # moved with it. The values came down with TIP_ROW_IN itself, which was
  # lowered to stop the plots coming out taller than they need to be.
  fit <- tree_plot$tree_auto_layout(15, width_in = 5.7, label_chars = 20)
  expect_equal(fit$aspect, 0.5)
  expect_equal(fit$tiplab_size, 3.3)
  expect_true(fit$labels_legible)
})

test_that("every fitted size keeps its proportions at ~15 tips", {
  fit <- tree_plot$tree_auto_layout(15, width_in = 5.7, label_chars = 20)
  # The other sizes are read off the same row pitch, so they move with it.
  expect_equal(fit$branch_size, 3.3)
  expect_equal(fit$tippoint_size, 3.3)
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
  # Long labels deliberately: the row pitch a linear fit produces does not move
  # with the panel width (it is TIP_USABLE * TIP_ROW_IN either way), so a wider
  # panel only buys type size where the *label length* is what binds.
  narrow <- tree_plot$tree_auto_layout(60, width_in = 4, label_chars = 36)
  wide <- tree_plot$tree_auto_layout(60, width_in = 10, label_chars = 36)

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

test_that("every tree is drawn to the edges of its canvas", {
  linear <- tree_plot$tree_auto_layout(50, width_in = 5.5)
  circ <- tree_plot$tree_auto_layout(50, width_in = 5.5, layout = "circular")

  # as.ggplot(scale = zoom) spends (1 - zoom) of the canvas on a blank border,
  # half of it above the plot and half below. Nothing in either layout can run
  # off the edge now — the tip labels are the only thing drawn past the tree,
  # and .tiplab_xlim reserves for them in both — so both take the whole canvas.
  # The 0.95/-0.05 a radial tree used to carry was compensation for a reserve
  # it did not have.
  expect_equal(linear$zoom, 1)
  expect_equal(linear$h, 0)
  expect_equal(circ$zoom, 1)
  expect_equal(circ$h, 0)
})

test_that("a circular panel is square, whatever the tip count", {
  # The tree is a disc: its height is its width, and the aspect slider has
  # nothing to choose. A linear tree of the same size is far taller than wide.
  for (n in c(20, 80, 400)) {
    circ <- tree_plot$tree_auto_layout(n, width_in = 5.7, layout = "circular")
    expect_equal(circ$aspect, 1)
  }
  expect_gt(tree_plot$tree_auto_layout(400, width_in = 5.7)$aspect, 1)
})

test_that("the circular fit follows the circumference, not a fixed guess", {
  # The old fit assumed the tips sat at 0.35 of the panel however many there
  # were, so it returned the same type size at twenty tips as at eighty — and
  # ran it off the canvas at both. Room per tip is arc length, so it has to
  # fall as tips are added.
  sizes <- vapply(
    c(20, 80, 400),
    function(n) {
      tree_plot$tree_auto_layout(
        n, width_in = 5.7, layout = "circular", label_chars = 20
      )$tiplab_size
    },
    numeric(1)
  )
  expect_true(all(diff(sizes) < 0))

  # And a long label costs type size, because the ring it needs is taken off
  # the circle the tips sit on.
  short <- tree_plot$tree_auto_layout(
    80, width_in = 5.7, layout = "circular", label_chars = 8
  )
  long <- tree_plot$tree_auto_layout(
    80, width_in = 5.7, layout = "circular", label_chars = 40
  )
  expect_gt(short$tiplab_size, long$tiplab_size)
})

test_that("a circular tree of a few hundred tips gives up on its labels", {
  circ <- tree_plot$tree_auto_layout(
    400, width_in = 5.7, layout = "circular", label_chars = 10
  )
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
  # matrix in a tenth of the tree's width. They no longer compete: each is a
  # fixed physical width and the canvas grows (see tree_panel_width_in).
  md <- data.frame(isolate = strrep("A", 20), stringsAsFactors = FALSE)
  base <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3, width_in = 5.5,
    heatmaps = list()
  )
  one <- base
  one$layers <- list(list(aesthetic = "tile", field = "a"))
  four <- base
  four$layers <- lapply(1:4, function(i) {
    list(aesthetic = "tile", field = paste0("v", i))
  })

  expect_equal(
    tree_plot$tree_annotation_width(tree_plot$resolve_annotation_widths(one, md)),
    tree_plot$tree_annotation_width(
      tree_plot$resolve_annotation_widths(four, md)
    )
  )
  # Still a fraction of the tree, not a multiple of it: the 2 this shipped with
  # drew a strip twice as wide as the tree it annotates.
  expect_lt(tree_plot$tree_annotation_width(one), 1)
  expect_gt(tree_plot$tree_annotation_width(one), 0)
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
  # The columns are what the squeeze bounds; the lead and the slack are gutters
  # either side of them and are not part of the ceiling.
  expect_lte(
    tree_plot$annotation_total(huge),
    impl$ANNOTATION_SPAN_MAX + impl$ANNOTATION_LEAD + impl$ANNOTATION_SLACK +
      1e-9
  )
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

# --- Grouping a mapped date --------------------------------------------------

date_md <- function(n = 24) {
  data.frame(
    isolate = sprintf("ISO-%02d", seq_len(n)),
    sample_collection_date = as.character(
      seq(as.Date("2024-01-01"), by = "15 days", length.out = n)
    ),
    stringsAsFactors = FALSE
  )
}

test_that("an unbinned date still reaches the plot as a continuous Date", {
  md <- date_md()
  layer <- list(field = "sample_collection_date", transform = "as_date")

  expect_s3_class(
    impl$.layer_values(layer, md$sample_collection_date),
    "Date"
  )
})

test_that("a binned date reaches the plot as its interval labels", {
  # The whole point: 24 distinct dates would be 24 unordered colours, where
  # twelve months is a legend a reader can follow.
  md <- date_md()
  layer <- list(
    field = "sample_collection_date",
    transform = "as_date",
    granularity = "month"
  )
  out <- impl$.layer_values(layer, md$sample_collection_date)

  expect_s3_class(out, "factor")
  expect_identical(levels(out)[1], "2024-01")
  expect_identical(length(levels(out)), 12L)
})

test_that("the binned column is written back, so scale and data agree", {
  # aes() references md[[field]] directly, so a scale built from binned levels
  # over raw dates is exactly the mismatch that errored.
  md <- date_md()
  opts <- list(layers = list(list(
    field = "sample_collection_date",
    transform = "as_date",
    granularity = "year"
  )))
  out <- impl$.normalize_mapped_columns(opts, md)

  expect_identical(as.character(unique(out$sample_collection_date)), "2024")
})

# --- What the annotations actually draw --------------------------------------

# The finished ggplot, before build_tree_ggtree() wraps it into a fixed-size
# grob, so its layers can be inspected as data. Everything below asks the same
# question — did the tiles survive the x scale — and only the built plot knows.
.built_tree <- function(tree, meta, opts) {
  inner <- NULL
  build <- impl$.build_tree_ggtree
  shadow <- new.env(parent = environment(build))
  assign("as.ggplot", function(plot, ...) {
    inner <<- plot
    ggplotify::as.ggplot(plot, ...)
  }, envir = shadow)
  environment(build) <- shadow
  suppressWarnings(suppressMessages(build(tree, meta, opts)))
  inner
}

# Rows each tile layer of a built plot draws, in layer order. `xlim()` censors
# rather than clips, so an annotation past the limit comes back as a layer of
# NAs — which is what "the strip has a header and a legend but no tiles" is.
.tile_rows <- function(p) {
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  keep <- which(vapply(
    p$layers,
    function(l) grepl("Tile", class(l$geom)[1]),
    logical(1)
  ))
  vapply(keep, function(i) sum(!is.na(b$data[[i]]$xmax)), integer(1))
}

.annot_opts <- function(tips) {
  list(
    root = "Automatic", layout = "rectangular", line_color = "#000000",
    bg = "#ffffff", tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3,
    tiplab_color = "#000000", layers = list(),
    branch_show = FALSE, branch_size = 3, branch_color = "#000000",
    tippoint_show = FALSE, tippoint_alpha = 1, tippoint_size = 3,
    tippoint_color = "#3A4657", tippoint_shape = 16,
    nodelabel_show = FALSE, parentnodes = character(0),
    clade_color = "#D0F221", heatmaps = list(),
    rootedge_show = FALSE, treescale_show = FALSE, width_in = 5.5,
    zoom = 1, h = 0, v = 0, legend_orientation = "vertical", legend_size = 9
  )
}

.annot_fixture <- function(n = 16) {
  set.seed(21)
  tree <- ape::rtree(n)
  tree$tip.label <- sprintf("isolate-%02d", seq_len(n))
  list(
    tree = tree,
    meta = data.frame(
      isolate = tree$tip.label,
      ward = rep(c("ICU", "ER", "Ward"), length.out = n),
      source = rep(c("Blood", "Urine"), length.out = n),
      `amr_Beta-lactam` = rep(c("blaOXA", ""), length.out = n),
      amr_Colistin = rep(c("mcr-1", ""), length.out = n),
      amr_Quinolone = rep(c("gyrA", ""), length.out = n),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}

.tile_layer <- function(id, field, palette) {
  list(
    id = id, field = field, title = field, aesthetic = "tile",
    palette = palette, family = "Qualitative", n_levels = 3L,
    continuous = FALSE, transform = NULL, granularity = NULL, auto = TRUE
  )
}

test_that("a lone tile strip draws its tiles, not just a header and a legend", {
  # The reported fault. The strips fill their reserve exactly, so the far edge
  # of the last one landed on the x limit — and xlim() censors, so the whole
  # column went. Adding a second strip made the first one appear, because it
  # was no longer the outermost.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  expect_identical(.tile_rows(.built_tree(f$tree, f$meta, opts)), 16L)
})

test_that("the outermost of several tile strips draws too", {
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(
    .tile_layer("L1", "ward", "Set1"),
    .tile_layer("L2", "source", "Dark2")
  )

  expect_identical(.tile_rows(.built_tree(f$tree, f$meta, opts)), c(16L, 16L))
})

test_that("every heatmap column is drawn, the last one included", {
  # gheatmap centres column k at offset + k * cell, so the matrix it draws sits
  # half a column further out than heatmap_panels reserved for it — and the
  # outermost column fell off the axis. With one column that is the whole
  # panel: a header and a legend over nothing at all.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$heatmaps <- list(list(
    kind = "amr", level = "class",
    cols = c("amr_Beta-lactam", "amr_Colistin", "amr_Quinolone"),
    palette = "Reds", title = "AMR classes"
  ))

  expect_identical(.tile_rows(.built_tree(f$tree, f$meta, opts)), 48L)
})

test_that("a single-column heatmap draws its one column", {
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$heatmaps <- list(list(
    kind = "amr", level = "class", cols = "amr_Colistin",
    palette = "Reds", title = "AMR classes"
  ))

  expect_identical(.tile_rows(.built_tree(f$tree, f$meta, opts)), 16L)
})

test_that("a tile strip and a heatmap beside it both draw in full", {
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))
  opts$heatmaps <- list(list(
    kind = "amr", level = "class",
    cols = c("amr_Beta-lactam", "amr_Colistin"),
    palette = "Reds", title = "AMR classes"
  ))

  expect_identical(.tile_rows(.built_tree(f$tree, f$meta, opts)), c(16L, 32L))
})

test_that("a grouped date keeps its intervals as the levels the scale colours", {
  # The reported fault: a collection date grouped by year drew one grey strip
  # and no legend, because the binned column was binned a second time — and
  # as.Date("2024") is NA for every tip, so the scale had nothing in it but the
  # missing level.
  n <- 24
  set.seed(22)
  tree <- ape::rtree(n)
  tree$tip.label <- sprintf("ISO-%02d", seq_len(n))
  meta <- data.frame(
    isolate = tree$tip.label,
    collected = as.character(
      seq(as.Date("2023-01-01"), by = "40 days", length.out = n)
    ),
    stringsAsFactors = FALSE
  )
  opts <- .annot_opts()
  opts$layers <- list(list(
    id = "L1", field = "collected", title = "Collection Date",
    aesthetic = "tile", palette = "Set1", family = "Qualitative",
    n_levels = 3L, continuous = FALSE, transform = "as_date",
    granularity = "year", auto = TRUE
  ))

  p <- .built_tree(tree, meta, opts)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  tile <- which(vapply(
    p$layers, function(l) grepl("Tile", class(l$geom)[1]), logical(1)
  ))[[1]]

  # Every year present is drawn, and in a colour of its own rather than the
  # grey that stands for "not recorded".
  fills <- unique(b$data[[tile]]$fill)
  expect_identical(length(fills), 3L)
  expect_false(impl$MISSING_COLOR %in% fills)
})

test_that("the header reserve follows the size the headers are set at", {
  # A fifteen-column matrix fits about a millimetre of type per column, and
  # reserving for HEADER_SIZE_MAX there took a third of the page for headers
  # that needed an eighth of it — which is also what floated the legend that
  # far above the tree.
  wide <- list(heatmaps = list(list(
    kind = "amr", level = "gene", cols = paste0("g", 1:15),
    labels = rep("Amikacin/Kanamycin/Tobramycin", 15)
  )))
  # tree_span 1, axis 3, panel 5.5in: a real solve, so the fitted size applies.
  fitted <- tree_plot$heatmap_header_frac(wide, 45, 1, 3, 5.5)
  capped <- tree_plot$heatmap_header_frac(wide, 45)

  expect_lt(fitted, capped)
  expect_lt(fitted, 0.25)
})

test_that("a heatmap panel is budgeted for the legend it draws", {
  # A panel draws a guide whether or not anything is mapped beside it. Left out
  # of the canvas budget, that guide came out of the panel — and the tip labels
  # it had squeezed were drawn over the matrix.
  md <- data.frame(isolate = strrep("A", 30), stringsAsFactors = FALSE)
  panels <- list(list(kind = "amr", level = "class", cols = "amr_Colistin",
    title = "AMR classes"))

  expect_identical(tree_plot$tree_legend_width_in(list(), md, 9, 5.5), 0)
  expect_gt(
    tree_plot$tree_legend_width_in(list(), md, 9, 5.5, panels),
    0.5
  )
  # A panel with no columns draws nothing, so it needs nothing.
  expect_identical(
    tree_plot$tree_legend_width_in(
      list(), md, 9, 5.5, list(list(kind = "amr", cols = character(0)))
    ),
    0
  )
})

test_that("the outermost annotation stops short of the x limit", {
  # The invariant behind both faults above, stated as geometry rather than as a
  # render: the annotations are placed to fill their reserve exactly, so
  # without ANNOTATION_SLACK the far edge of the outermost one lands *on* the
  # limit — and whether xlim() then censors it is down to floating point.
  md <- data.frame(isolate = strrep("A", 24), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3, width_in = 5.5,
    rootedge_show = FALSE,
    layers = list(
      list(aesthetic = "tile", field = "a"),
      list(aesthetic = "tile", field = "b")
    ),
    heatmaps = list(list(kind = "amr", cols = c("c1", "c2", "c3")))
  )

  max_x <- 4
  tree_data <- data.frame(x = c(0, 2, max_x))
  tree_span <- max_x
  fit <- impl$.tiplab_xlim(
    opts, md, tree_data, max_x, tree_plot$annotation_total(opts)
  )

  panels <- tree_plot$heatmap_panels(opts, tree_span, fit$reserve)$panels
  last <- panels[[length(panels)]]
  # heatmap_panels' offset is the panel's near edge — the builder converts it
  # to gheatmap's own convention, so the matrix ends here.
  heat_edge <- max_x + last$offset + last$width * tree_span

  centres <- tree_plot$tile_centres(
    opts, 2L, fit$reserve / tree_span, max_x, tree_span
  )
  tile_edge <- max(centres) + impl$.tile_span(opts) * tree_span / 2

  expect_lt(tile_edge, heat_edge)
  expect_equal(
    fit$limit - heat_edge,
    impl$ANNOTATION_SLACK * tree_span,
    tolerance = 1e-8
  )
})

# --- What the tip labels are actually given -----------------------------------

# Inches of the finished panel the label reserve takes. The reserve is solved
# as a fraction of the x axis and the axis spans the grown panel, so only the
# two together say how much room the labels really got.
.reserve_in <- function(opts, md, panel_in = 5.5) {
  # Resolved first: the axis solve and the panel solve have to be asking about
  # the same column widths, or they answer about different plots.
  opts <- tree_plot$resolve_annotation_widths(opts, md)
  max_x <- 1
  fit <- impl$.tiplab_xlim(
    opts, md, data.frame(x = c(0, max_x)), max_x,
    tree_plot$annotation_total(opts)
  )
  tree_plot$tree_panel_width_in(opts, md, panel_in) * fit$reserve / fit$limit
}

test_that("the label reserve is a width, not a share of the grown canvas", {
  # `.tiplab_frac()` measures the labels against the tree-and-labels budget,
  # but the reserve is spent on the *axis*, which spans the whole grown panel.
  # Spending the budget's fraction of the grown axis is how a thirty-column
  # heatmap reserved three inches for labels that needed two — a band of dead
  # space between the tips and the first strip, and an inch off the tree.
  md <- data.frame(isolate = strrep("A", 36), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 2.3, width_in = 5.5,
    rootedge_show = FALSE, layers = list(), heatmaps = list()
  )
  needed <- 36 * impl$TIP_CHAR_EM * 2.3 / 25.4 * impl$X_EXPANSION

  narrow <- opts
  narrow$heatmaps <- list(list(kind = "amr", cols = paste0("c", 1:3)))
  wide <- opts
  wide$heatmaps <- list(list(kind = "amr", cols = paste0("c", 1:30)))

  expect_equal(.reserve_in(narrow, md), needed, tolerance = 0.01)
  # The one that mattered: thirty columns must not buy the labels more room.
  expect_equal(.reserve_in(wide, md), needed, tolerance = 0.01)
})

test_that("what the labels stop taking goes to the tree, not the annotations", {
  # The other half of the same solve. The tree-and-labels budget is 5.5in
  # whatever the matrix beside it does, so the tree gets the rest of it.
  md <- data.frame(isolate = strrep("A", 36), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 2.3, width_in = 5.5,
    rootedge_show = FALSE, layers = list(),
    heatmaps = list(list(kind = "amr", cols = paste0("c", 1:30)))
  )
  needed <- 36 * impl$TIP_CHAR_EM * 2.3 / 25.4 * impl$X_EXPANSION

  expect_equal(5.5 - .reserve_in(opts, md), 5.5 - needed, tolerance = 0.02)
})

test_that("a date left ungrouped labels its colour bar with dates", {
  # Days since the epoch is what a Date is to a continuous scale, and a scale
  # not told otherwise puts 17250 and 18000 on the keys — which is what "Exact
  # date" drew. The interval the breaks land on follows the span.
  span <- function(days) {
    v <- seq(as.Date("2020-01-01"), by = "1 day", length.out = days)
    sc <- impl$tree_scale(v, "viridis", "fill", "Collection Date")
    sc$train(range(as.numeric(v)))
    sc$get_labels()
  }

  expect_true(all(grepl("^20\\d\\d$", span(2000))))
  expect_true(all(grepl("20\\d\\d$", span(400))))
  # And nothing anywhere reads as a bare day count.
  expect_false(
    any(grepl("^1[678]\\d{3}$", c(span(60), span(400), span(2000))))
  )
})

test_that("a numeric variable still gets a plain continuous scale", {
  # `transform = "date"` over a column that is not a date would relabel plain
  # numbers as calendar dates, which is the same fault the other way round.
  sc <- impl$tree_scale(c(3, 900), "viridis", "fill", "Alleles")
  sc$train(c(3, 900))

  expect_true(all(grepl("^[0-9]+$", sc$get_labels())))
})

# --- Annotation columns as physical widths -----------------------------------

test_that("an annotation column is a width in inches, not a share of the tree", {
  # The header over a column is set in real type, so the column has to be a
  # real width. Held as a fraction of the tree span it moved with the label
  # reserve: the same matrix drew wide columns beside short tip labels and
  # hairlines beside long ones.
  short <- data.frame(isolate = strrep("A", 8), stringsAsFactors = FALSE)
  long <- data.frame(isolate = strrep("A", 40), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 3, width_in = 5.5,
    layers = list(list(aesthetic = "tile", field = "a")), heatmaps = list()
  )

  inches <- function(md) {
    o <- tree_plot$resolve_annotation_widths(opts, md)
    # Span times the tree's own width in inches is the column on the page.
    tree_plot$tree_annotation_width(o) * 5.5 * (1 - impl$.tiplab_budget_frac(o, md))
  }

  expect_equal(inches(short), impl$TILE_COL_IN, tolerance = 1e-8)
  expect_equal(inches(long), impl$TILE_COL_IN, tolerance = 1e-8)
  # And in tree spans they differ, which is the point: long labels leave the
  # tree less of the page, so the same inch is a bigger share of it.
  expect_gt(
    tree_plot$tree_annotation_width(
      tree_plot$resolve_annotation_widths(opts, long)
    ),
    tree_plot$tree_annotation_width(
      tree_plot$resolve_annotation_widths(opts, short)
    )
  )
})

test_that("a wide gene matrix still gets headers a reader can read", {
  # The reported fault: thirty gene names came out at the minimum type size,
  # because the fit was against the tree-and-labels budget while the panel the
  # matrix is drawn on is nearly twice that wide.
  md <- data.frame(isolate = strrep("A", 36), stringsAsFactors = FALSE)
  opts <- list(
    tiplab_show = TRUE, tiplab = "isolate", tiplab_size = 2.3, width_in = 5.5,
    rootedge_show = FALSE, layers = list(),
    heatmaps = list(list(kind = "amr", level = "gene",
      cols = paste0("g", 1:30), labels = paste0("gene", 1:30)))
  )
  opts <- tree_plot$resolve_annotation_widths(opts, md)

  max_x <- 1
  tree_span <- max_x
  fit <- impl$.tiplab_xlim(
    opts, md, data.frame(x = c(0, max_x)), max_x,
    tree_plot$annotation_total(opts)
  )
  panel <- tree_plot$tree_panel_width_in(opts, md, 5.5)
  axis_units <- fit$limit
  cell <- impl$.heat_span(opts) * tree_span

  # Fitted against the panel the matrix is actually drawn on.
  expect_equal(
    tree_plot$tree_header_size(cell, axis_units, panel),
    impl$HEADER_SIZE_MAX,
    tolerance = 0.15
  )
  # Against the tree's budget alone — the old fit — it would be far smaller.
  expect_lt(tree_plot$tree_header_size(cell, axis_units, 5.5), impl$HEADER_SIZE_MAX)
})

# --- Legends a reader can actually use ---------------------------------------

test_that("a long variable lists a few keys and counts the rest", {
  # 81 patient ids ran the guide box off the bottom of the canvas, and would
  # have been unusable had it fit. Dropping the guide outright lost the reader
  # the values entirely; this keeps the first few and says how many it is not
  # showing, the way the MST legend does.
  n_over <- tree_plot$LEGEND_MAX_KEYS + 144L
  levels <- sprintf("p%03d", seq_len(n_over))
  keys <- tree_plot$tree_legend_breaks(levels)

  expect_identical(length(keys$breaks), tree_plot$LEGEND_MAX_KEYS)
  expect_identical(keys$hidden, 144L)
  expect_identical(keys$breaks, levels[seq_len(tree_plot$LEGEND_MAX_KEYS)])
  expect_match(tree_plot$tree_legend_title("Patient Id", 144L), "\\+ 144 more")

  # A variable that fits lists all of it and says nothing extra.
  short <- tree_plot$tree_legend_breaks(sprintf("p%03d", 1:4))
  expect_identical(short$hidden, 0L)
  expect_identical(tree_plot$tree_legend_title("Ward", 0L), "Ward")
})

test_that("a capped guide still gets a scale, and only its keys are budgeted", {
  # The guide is drawn either way now — what changes is how many rows it has,
  # and so how much canvas it needs.
  many <- sprintf("p%03d", seq_len(tree_plot$LEGEND_MAX_KEYS + 40L))
  sc <- impl$tree_scale(many, "Set1", "fill", "Patient Id")

  expect_s3_class(sc$guide, "Guide")
  expect_identical(length(sc$breaks), tree_plot$LEGEND_MAX_KEYS)
  expect_match(sc$name, "\\+ 40 more")

  md <- data.frame(
    isolate = sprintf("i%03d", 1:60),
    patient = sprintf("patient-%03d", 1:60),
    stringsAsFactors = FALSE
  )
  over <- list(list(
    field = "patient", title = "Patient Id",
    n_levels = tree_plot$LEGEND_MAX_KEYS + 40L
  ))
  under <- list(list(
    field = "patient", title = "Patient Id",
    n_levels = tree_plot$LEGEND_MAX_KEYS
  ))

  # Both need room, and the long one needs no more than the short one: it is
  # sized from the keys it lists, not from the values it has.
  expect_equal(
    tree_plot$tree_legend_width_in(over, md, 9, 5.5),
    tree_plot$tree_legend_width_in(under, md, 9, 5.5)
  )
})

# --- The circular layout ------------------------------------------------------

# modifyList() is deliberately not used here: it recurses into list-valued
# entries and keeps only the *named* ones, which silently empties a heatmap or
# layer list built from unnamed records.
.circ_opts <- function(tiplab_size = 1.4, layers = list(), heatmaps = list(),
                       layout = "circular") {
  list(
    layout = layout, tiplab_show = TRUE, tiplab = "isolate",
    tiplab_size = tiplab_size, width_in = 5.5, rootedge_show = FALSE,
    layers = layers, heatmaps = heatmaps
  )
}

test_that("a circular tree measures itself against its radius", {
  # Its x axis is a radius, and ggplot's CoordPolar draws that across
  # TREE_RADIAL_FRAC of a square panel. Measuring against the panel instead is
  # how the fit sized type for a radius longer than the one it got.
  circ <- .circ_opts()
  linear <- .circ_opts(layout = "rectangular")

  expect_equal(tree_plot$tree_budget_in(linear), 5.5)
  expect_equal(tree_plot$tree_budget_in(circ), 5.5 * impl$TREE_RADIAL_FRAC)
  expect_equal(tree_plot$tree_axis_in(circ, 9), 9 * impl$TREE_RADIAL_FRAC)
  expect_equal(tree_plot$tree_axis_in(linear, 9), 9)
})

test_that("the circular fit and the label reserve agree with each other", {
  # The invariant the layout stands on. The fit picks a type size from the
  # radius it thinks it has; the reserve keeps room for the labels that size
  # produces. Solved against different radii, the labels came out longer than
  # the room kept for them and were clipped mid-word.
  for (n in c(20, 60, 200)) {
    for (chars in c(10, 36)) {
      md <- data.frame(
        isolate = strrep("A", chars),
        stringsAsFactors = FALSE
      )
      fit <- tree_plot$tree_auto_layout(
        n, width_in = 5.5, layout = "circular", label_chars = chars
      )
      opts <- .circ_opts(tiplab_size = fit$tiplab_size)

      needed <- chars * impl$TIP_CHAR_EM * fit$tiplab_size / 25.4
      reserved <- impl$.tiplab_budget_frac(opts, md) *
        tree_plot$tree_budget_in(opts)

      expect_gte(reserved, needed)
    }
  }
})

test_that("a circular tree reserves radius for its labels", {
  # It used to be skipped outright, which is why a radial tree drew its labels
  # off every edge of the canvas.
  md <- data.frame(isolate = strrep("A", 30), stringsAsFactors = FALSE)
  opts <- .circ_opts(tiplab_size = 1.4)
  max_x <- 1
  fit <- impl$.tiplab_xlim(
    opts, md, data.frame(x = c(0, max_x)), max_x,
    tree_plot$annotation_total(opts)
  )

  expect_gt(fit$reserve, 0)
  expect_gt(fit$limit, max_x)
})

test_that("a circular tree gives its rings less room than a linear one", {
  # A column's share of the picture is its width; a ring's is its area, which
  # grows with the radius it sits at. The same ceiling in both left a radial
  # tree a knot at the centre of a dartboard.
  heat <- list(list(kind = "amr", level = "gene", cols = paste0("g", 1:30)))
  circ <- .circ_opts(heatmaps = heat)
  linear <- .circ_opts(heatmaps = heat, layout = "rectangular")

  expect_lt(impl$.annotation_span_max(circ), impl$.annotation_span_max(linear))
  expect_lt(tree_plot$annotation_total(circ), tree_plot$annotation_total(linear))
})

test_that("a circular tree draws its rings past its labels", {
  # The rings used to be pinned to the tree's own edge (gheatmap offset 0 and
  # no label reserve at all), so they were drawn straight over the tip labels.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layout <- "circular"
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  p <- .built_tree(f$tree, f$meta, opts)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  tile <- which(vapply(
    p$layers, function(l) grepl("Tile", class(l$geom)[1]), logical(1)
  ))[[1]]
  lab <- which(vapply(
    p$layers, function(l) grepl("TextGGtree", class(l$geom)[1]), logical(1)
  ))[[1]]

  # Every tile drawn, and the ring starts outside the radius the labels sit at.
  expect_identical(sum(!is.na(b$data[[tile]]$xmax)), 16L)
  expect_gt(min(b$data[[tile]]$xmin), max(b$data[[lab]]$x))
})

test_that("guides flow into columns rather than being cut off", {
  # ggplot2 stacks guides in one column and clips whatever runs past the panel.
  # A tab with a dozen mappings simply lost the last few legends off the bottom
  # of the canvas.
  many <- lapply(1:12, function(i) {
    list(field = paste0("v", i), title = paste("Var", i), n_levels = 8L)
  })

  # Tall enough for all of them: one column.
  expect_identical(tree_plot$tree_legend_cols(many, list(), 9, 40), 1L)
  # Short: they have to flow sideways instead.
  expect_gt(tree_plot$tree_legend_cols(many, list(), 9, 6), 1L)
  # And never wider than the ceiling, whatever is thrown at it.
  expect_lte(
    tree_plot$tree_legend_cols(many, list(), 9, 1),
    impl$LEGEND_MAX_COLS
  )
  # Nothing to draw, nothing to flow.
  expect_identical(tree_plot$tree_legend_cols(list(), list(), 9, 6), 1L)

  # The canvas is widened for every column the box flows into.
  md <- data.frame(
    isolate = sprintf("i%02d", 1:12),
    stringsAsFactors = FALSE
  )
  for (i in 1:12) md[[paste0("v", i)]] <- rep(letters[1:8], length.out = 12)
  tall <- tree_plot$tree_legend_width_in(many, md, 9, 5.5, list(), 40)
  short <- tree_plot$tree_legend_width_in(many, md, 9, 5.5, list(), 6)
  expect_gt(short, tall)
})

test_that("a capped guide counts as the rows it will actually draw", {
  # 150 values is nine rows plus a "+ N more" line, not 150 — the whole point
  # of capping the keys.
  huge <- list(list(field = "v", title = "Patient Id", n_levels = 150L))
  small <- list(list(field = "v", title = "Patient Id", n_levels = 9L))

  expect_identical(
    tree_plot$tree_legend_rows(huge),
    tree_plot$tree_legend_rows(small) + 1L
  )
})

# --- Opening the circle -------------------------------------------------------

test_that("a radial tree opens far enough for its ring headers", {
  # With no wedge, ggtree draws every ring's header at one angle, on top of the
  # others — which is what a circular tree with a strip and a heatmap looked
  # like. The wedge has to fit the longest header as an arc at its own ring's
  # radius, and an inner ring is the tight one.
  md <- data.frame(isolate = strrep("A", 20), stringsAsFactors = FALSE)
  bare <- .circ_opts()
  expect_identical(tree_plot$tree_open_angle(bare, md), 0)

  tiled <- .circ_opts(layers = list(list(
    aesthetic = "tile", field = "v", title = "Collection Date"
  )))
  expect_gt(tree_plot$tree_open_angle(tiled, md), 0)

  # A longer header needs more of the circle than a short one on the same ring.
  short <- .circ_opts(layers = list(list(
    aesthetic = "tile", field = "v", title = "Ward"
  )))
  expect_gt(
    tree_plot$tree_open_angle(tiled, md),
    tree_plot$tree_open_angle(short, md)
  )

  # Never past the ceiling, whatever it is asked to fit.
  silly <- .circ_opts(layers = list(list(
    aesthetic = "tile", field = "v", title = strrep("x", 400)
  )))
  expect_lte(tree_plot$tree_open_angle(silly, md), impl$OPEN_ANGLE_MAX)

  # A linear tree has no circle to open.
  flat <- .circ_opts(layout = "rectangular", layers = tiled$layers)
  expect_identical(tree_plot$tree_open_angle(flat, md), 0)
})

test_that("a wider wedge takes more of the circle", {
  # ggtree opens the circle by widening the angular axis past the tip count and
  # mapping the whole of it onto 360 degrees — so the wedge is visible as how
  # much of that axis the tips do *not* occupy.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layout <- "circular"
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  theta <- function(angle) {
    o <- opts
    o$open_angle <- angle
    p <- .built_tree(f$tree, f$meta, o)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    diff(b$layout$panel_params[[1]]$theta.range)
  }
  expect_gt(theta(60), theta(15))
})

# --- The inward layout --------------------------------------------------------

test_that("an inward tree keeps a clear core so its labels stop converging", {
  # Inward labels run from their tips *toward* the centre, so they close on
  # each other as they go: left to reach the middle they meet at a point. The
  # fit solves their spacing at INWARD_CORE_FRAC of the radius, which is where
  # they stop.
  for (n in c(20, 90, 300)) {
    fit <- tree_plot$tree_auto_layout(
      n, width_in = 5.5, layout = "inward", label_chars = 30
    )
    expect_equal(fit$aspect, 1)
    expect_gt(fit$tiplab_size, 0)
  }
  # Room per label is an arc at a fixed radius, so it falls with the tip count.
  sizes <- vapply(
    c(20, 90, 300),
    function(n) {
      tree_plot$tree_auto_layout(
        n, width_in = 5.5, layout = "inward", label_chars = 30
      )$tiplab_size
    },
    numeric(1)
  )
  expect_true(all(diff(sizes) <= 0))
})

test_that("an inward tree maps onto its tips rather than into rings", {
  # The space past an inward tree's tips is the middle of the disc, where the
  # arc a ring is drawn along shrinks to nothing — a ring there comes out as a
  # filled circle with its labels converging to a point.
  bare <- .circ_opts(layout = "inward")
  tiled <- .circ_opts(
    layout = "inward",
    layers = list(list(aesthetic = "tile", field = "v", title = "Ward")),
    heatmaps = list(list(kind = "amr", cols = paste0("g", 1:5)))
  )

  expect_false(tree_plot$tree_annotations_drawn(tiled))
  expect_true(tree_plot$tree_annotations_drawn(.circ_opts()))
  # So they reserve nothing, and the tree keeps the whole radius.
  expect_identical(tree_plot$annotation_total(tiled), 0)
  expect_identical(
    tree_plot$annotation_total(tiled),
    tree_plot$annotation_total(bare)
  )
})

test_that("an inward tree draws a tree, not a blot", {
  # Its radius is a build argument, and it was being set twice — once through
  # ggtree and again as a scale limit, which clipped the reversed axis instead
  # of extending it. Every tip has to survive that.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layout <- "inward"
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  p <- .built_tree(f$tree, f$meta, opts)
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  # ggtree splits the tip labels across two layers, one per half of the circle,
  # so the count is their sum.
  tips <- which(vapply(
    p$layers, function(l) grepl("TextGGtree", class(l$geom)[1]), logical(1)
  ))

  expect_true(length(tips) > 0)
  expect_identical(sum(vapply(tips, function(i) nrow(b$data[[i]]), 0)), 16)
  # And the tile strip is not drawn into the middle of the disc.
  expect_false(any(vapply(
    p$layers, function(l) grepl("Tile", class(l$geom)[1]), logical(1)
  )))
})

test_that("a radial tree hangs its headers off the edge the rings end at", {
  # The wedge has two edges. The rings stop against the leading one — a
  # straight radial cut a header can be aligned to — while the other is open
  # space, where a header floats with nothing to line up with.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  header_y <- function(layout) {
    o <- opts
    o$layout <- layout
    p <- .built_tree(f$tree, f$meta, o)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    txt <- which(vapply(
      p$layers,
      function(l) identical(class(l$geom)[1], "GeomText"),
      logical(1)
    ))
    b$data[[txt[[1]]]]$y
  }

  # Linear: above the last tip, where the strip ends.
  expect_gt(header_y("rectangular"), 16)
  # Radial: below the first, against the other side of the same opening.
  expect_lt(header_y("circular"), 1)
})

test_that("strip and matrix headers share a baseline, clear of the columns", {
  # gheatmap anchors its column names at `max(y) + 1` and nudges from there,
  # while a strip's header is placed at an absolute y — so the two sat a full
  # tip row apart. And both were anchored at the tip rather than past the
  # column's edge, which put every header inside its own top tile.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))
  # Drawn from the metadata columns, so the panel needs no call matrix to
  # exist — what is under test is where its column names land, not what they
  # say.
  opts$heatmaps <- list(list(
    kind = "amr", level = "class",
    cols = c("amr_Beta-lactam", "amr_Colistin"),
    palette = "Reds", title = "AMR classes"
  ))

  for (layout in c("rectangular", "circular")) {
    o <- opts
    o$layout <- layout
    p <- .built_tree(f$tree, f$meta, o)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    cls <- vapply(p$layers, function(l) class(l$geom)[1], character(1))

    heads <- unlist(lapply(
      which(cls == "GeomText"),
      function(i) unique(b$data[[i]]$y)
    ))
    tiles <- which(grepl("Tile", cls))
    edges <- unlist(lapply(
      tiles,
      function(i) c(b$data[[i]]$ymin, b$data[[i]]$ymax)
    ))

    # The class band sits under the tree, so only the headers above the
    # columns are in question here.
    heads <- if (identical(layout, "circular")) {
      heads[heads < min(edges, na.rm = TRUE)]
    } else {
      heads[heads > max(edges, na.rm = TRUE)]
    }

    expect_gt(length(heads), 1)
    # One baseline for both kinds ...
    expect_identical(length(unique(round(heads, 6))), 1L)
    # ... and it clears the columns rather than sitting inside them.
    gap <- if (identical(layout, "circular")) {
      min(edges, na.rm = TRUE) - heads[[1]]
    } else {
      heads[[1]] - max(edges, na.rm = TRUE)
    }
    expect_equal(gap, impl$TILE_HEADER_GAP, tolerance = 1e-6)
  }
})

# --- Exporting at another size ------------------------------------------------

test_that("the design scales with the page instead of being stretched", {
  # A finished ggplot cannot be rescaled: its type is in millimetres and the
  # reserves beside it are fractions, so printing the preview at another width
  # moves one and not the other. Exported small, the tip labels ran into the
  # annotation; exported large, they shrank into a gutter of dead space.
  md <- data.frame(isolate = strrep("A", 36), stringsAsFactors = FALSE)
  opts <- list(
    layout = "rectangular", tiplab_show = TRUE, tiplab = "isolate",
    tiplab_size = 2.3, branch_size = 3, tippoint_size = 3, legend_size = 9,
    width_in = 5.5, rootedge_show = FALSE,
    layers = list(list(aesthetic = "tile", field = "v", title = "Ward")),
    heatmaps = list(list(kind = "amr", cols = paste0("g", 1:8)))
  )

  for (k in c(0.6, 1.45, 2.32)) {
    big <- tree_plot$scale_tree_opts(opts, k)

    # Every physical length multiplied through ...
    expect_equal(big$width_in, opts$width_in * k)
    expect_equal(big$tiplab_size, opts$tiplab_size * k)
    expect_equal(big$legend_size, opts$legend_size * k)

    # ... and so every *fraction* left exactly where it was, which is what
    # makes the two figures similar rather than merely different sizes.
    expect_equal(
      impl$.tiplab_budget_frac(big, md),
      impl$.tiplab_budget_frac(opts, md)
    )
    expect_equal(
      tree_plot$annotation_total(big),
      tree_plot$annotation_total(opts)
    )
    # An annotation column keeps its share of the tree, and gains its inches.
    small_w <- tree_plot$tree_annotation_width(
      tree_plot$resolve_annotation_widths(opts, md)
    )
    big_w <- tree_plot$tree_annotation_width(
      tree_plot$resolve_annotation_widths(big, md)
    )
    expect_equal(big_w, small_w)
    expect_equal(impl$.tile_col_in(big), impl$.tile_col_in(opts) * k)
  }

  # A nonsense factor changes nothing rather than producing a nonsense plot.
  expect_identical(tree_plot$scale_tree_opts(opts, NA), opts)
  expect_identical(tree_plot$scale_tree_opts(opts, 0), opts)
})

test_that("a scaled build draws the same figure, larger", {
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$layers <- list(.tile_layer("L1", "ward", "Set1"))

  extent <- function(o) {
    p <- .built_tree(f$tree, f$meta, o)
    b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
    tile <- which(vapply(
      p$layers, function(l) grepl("Tile", class(l$geom)[1]), logical(1)
    ))[[1]]
    rng <- b$layout$panel_params[[1]]$x.range
    # Where the strip sits along the axis, as a fraction of it.
    (range(b$data[[tile]]$xmin, b$data[[tile]]$xmax) - rng[[1]]) / diff(rng)
  }

  expect_equal(
    extent(tree_plot$scale_tree_opts(opts, 2)),
    extent(opts),
    tolerance = 1e-6
  )
})

# --- Drawing many branches on one page ----------------------------------------

test_that("a negative branch length is drawn as zero, not backwards", {
  # Neighbour-joining estimates branch lengths independently of the topology,
  # so a few come out negative. Drawn literally they run backwards along their
  # own radius, and the crossing, doubled-back segments are what made a radial
  # NJ tree look broken.
  set.seed(31)
  tree <- ape::rtree(12)
  tree$tip.label <- sprintf("iso%02d", 1:12)
  tree$edge.length[c(2, 5)] <- c(-0.4, -0.01)
  meta <- data.frame(isolate = tree$tip.label, stringsAsFactors = FALSE)

  p <- .built_tree(tree, meta, .annot_opts())
  b <- suppressWarnings(suppressMessages(ggplot2::ggplot_build(p)))
  seg <- which(vapply(
    p$layers, function(l) identical(class(l$geom)[1], "GeomSegment"), logical(1)
  ))[[1]]

  # Nothing is drawn to the left of the root.
  expect_gte(min(b$data[[seg]]$x, na.rm = TRUE), 0)
  expect_gte(min(b$data[[seg]]$xend, na.rm = TRUE), 0)
})

test_that("the branch stroke thins as the branches multiply", {
  # A tree has as many branches as tips and they all share one page, so past a
  # few dozen the stroke has to come down with them or the drawing fills in.
  widths <- vapply(
    c(20, 150, 500),
    function(n) {
      tree_plot$tree_auto_layout(n, 5.5, "circular", 20)$branch_width
    },
    numeric(1)
  )
  expect_true(all(diff(widths) < 0))
  expect_gte(min(widths), impl$BRANCH_WIDTH_MIN)
  expect_lte(max(widths), impl$BRANCH_WIDTH)
})

test_that("leader lines survive the labels being switched off", {
  # They are what makes a tree with ragged tip depths readable without labels:
  # the eye has to carry a row across an empty band to whatever is annotated
  # beside it. Only the inward layout drops them, because there every line
  # converges on the root and they blot it out.
  f <- .annot_fixture()
  opts <- .annot_opts()
  opts$tiplab_show <- FALSE

  tiplab_layers <- function(layout) {
    o <- opts
    o$layout <- layout
    p <- .built_tree(f$tree, f$meta, o)
    sum(vapply(
      p$layers,
      function(l) grepl("TextGGtree", class(l$geom)[1]),
      logical(1)
    ))
  }
  expect_gt(tiplab_layers("rectangular"), 0)
  expect_gt(tiplab_layers("circular"), 0)
  expect_identical(tiplab_layers("inward"), 0L)
})

# --- Following the aspect the user chose --------------------------------------

test_that("the header reserve grows when the rows get shorter", {
  # HEADER_CHAR_ROWS counts rows at the pitch the fit aims for. The aspect is
  # the user's to change, and a squatter plot has shorter rows — so the same
  # column name needs more of them. Measured against the nominal pitch, they
  # were clipped off the top the moment the aspect came down.
  opts <- list(
    layout = "rectangular", tiplab_show = TRUE, tiplab = "isolate",
    tiplab_size = 3, width_in = 5.5, layers = list(),
    heatmaps = list(list(
      kind = "amr", level = "gene", cols = paste0("g", 1:10),
      labels = rep("Amikacin/Kanamycin/Tobramycin", 10)
    ))
  )
  tall <- tree_plot$heatmap_header_frac(opts, 40, 1, 3, 5.5, 12)
  squat <- tree_plot$heatmap_header_frac(opts, 40, 1, 3, 5.5, 4)

  expect_gt(squat, tall)
})

test_that("a guide wraps its own keys rather than the box going sideways", {
  # Thrown sideways, the box spent the whole width on one row of guides and
  # looked worse than the clipping it was avoiding. Each guide wraps instead.
  many <- lapply(1:6, function(i) {
    list(field = paste0("v", i), title = paste("Var", i), n_levels = 9L)
  })

  tall <- tree_plot$tree_legend_max_rows(many, list(), 9, 40)
  squat <- tree_plot$tree_legend_max_rows(many, list(), 9, 5)

  # A short page gives each guide fewer rows, so its keys wrap into more
  # columns and the stack still fits.
  expect_lt(squat, tall)
  expect_gte(squat, 3L)
  expect_gt(tree_plot$tree_legend_ncol(9L, squat), 1L)
  expect_identical(tree_plot$tree_legend_ncol(9L, tall), 1L)
})

test_that("the distance axis is sized for a page, not for a tip row", {
  # It is a single row at the foot of the plot, so unlike a tip label it is not
  # competing with n-1 others for the height. Taking its size from the fitted
  # branch size shrank it to a smear on a tree of a few hundred tips.
  opts <- .annot_opts()
  opts$axis_show <- TRUE

  axis_size <- function(branch) {
    o <- opts
    o$branch_size <- branch
    layers <- impl$tree_axis_layer(o, 1, -1)
    sizes <- vapply(
      layers,
      function(l) l$aes_params$size %||% NA_real_,
      numeric(1)
    )
    sizes[!is.na(sizes)]
  }
  expect_identical(axis_size(0.6), axis_size(4))
})
