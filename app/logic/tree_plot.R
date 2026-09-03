# app/logic/tree_plot.R
#
# Render phylogenetic trees as ggtree/ggplot objects integrated with visual controls.
# Tree computation occurs upstream; this module handles rendering and auto-layout logic.

box::use(
  ggtree[
    ggtree,
    `%<+%`,
    geom_tiplab,
    geom_tippoint,
    geom_treescale,
    geom_rootedge,
    geom_hilight,
    geom_nodelab,
    gheatmap,
    theme_tree,
  ],
  ggtreeExtra[geom_fruit],
  ggnewscale[new_scale_color, new_scale_fill],
  ggplotify[as.ggplot],
  cowplot[ggdraw],
  ggplot2[
    aes,
    geom_text,
    geom_label,
    geom_tile,
    geom_segment,
    ggsave,
    guide_legend,
    theme,
    element_text,
    element_rect,
    margin,
    unit,
    xlim,
    labs,
    scale_color_gradientn,
    scale_color_viridis_d,
    scale_color_distiller,
    scale_color_manual,
    scale_fill_gradientn,
    scale_fill_viridis_d,
    scale_fill_distiller,
    scale_fill_manual,
    scale_shape_manual,
    scale_y_continuous,
    expansion,
  ],
  ape[root],
  stats[setNames],
  utils[head],
  RColorBrewer[brewer.pal, brewer.pal.info],
  viridisLite[viridis],
  grDevices[colorRampPalette],
  rlang[`%||%`],
)

box::use(
  app / logic / date_bins[bin_date_values],
  app / logic / field_labels[field_labels_for],
  app / logic / field_profile[field_levels],
)

.viridis_scales <- c(
  "viridis",
  "magma",
  "plasma",
  "inferno",
  "cividis",
  "turbo",
  "mako"
)

.circular_layouts <- c("circular", "inward")

# The six shapes a mapping may use, ordered by how easily they are told apart
# at tip size: filled circle, triangle, square, diamond, then the two outlined
# ones. ggplot2's own default palette stops at six as well — past that it draws
# no shape at all and the surplus tips vanish, which is why the mapping engine
# never sends a wider variable here.
#' @export
TREE_SHAPES <- c(16, 17, 15, 18, 1, 2)

# --- Layout & Geometry Constants ---------------------------------------------

# Vertical row geometry (in inches and ratios)
#
# TIP_ROW_IN is the calibration the whole linear fit hangs off: the aspect is
# this times the tip count, and the row pitch it produces is what every type
# size is fitted to. It came down from 0.228 because the plots it produced were
# taller than they needed to be — a page of tree that has to be scrolled reads
# worse than a slightly tighter one that does not.
TIP_ROW_IN <- 0.14 # Target inches of plot height per tip
TIP_USABLE <- 0.9 # Share of plot height available excluding margins/title
TIP_ROW_FILL <- 0.77 # Fraction of row pitch occupied by tip label text box

# Horizontal label reservation geometry
TIP_CHAR_EM <- 0.6 # Character width estimate (em) for accession/isolate labels
TIP_LABEL_FRAC <- 0.35 # Maximum fraction of panel width reserved for tip labels

# Most of a circular tree's radius the label ring may take.
#
# The two constraints on a radial label pull against each other through this
# one number: the label runs *outward*, so a longer ring lets it be set larger,
# while the tips it annotates sit on the circle *inside* the ring, whose
# circumference — and so the room between two labels — shrinks as the ring
# grows. `tree_auto_layout()` solves for the split where the two agree, and
# this is the ceiling on that solve: past it the tree is a knot in the middle
# of a wheel of text.
#
# The same number caps the reserve `.tiplab_frac()` asks for, because the fit
# and the reserve have to mean the same thing. Capping one and not the other is
# how the labels came to be drawn longer than the room kept for them.
TIP_RING_FRAC_MAX <- 0.55

# An inward tree's labels run from their tips *toward* the centre, so unlike a
# circular tree's they converge as they go: the room between two of them is the
# arc at their inner ends, not at their tips. Left to reach the middle they
# meet at a point and pile into a blot, which is what an inward tree of any
# size used to look like.
#
# INWARD_CORE_FRAC is the disc kept clear at the centre — the labels stop
# there, and that is the radius their spacing is solved at. INWARD_TREE_MIN is
# what the tree keeps whatever the labels ask for.
INWARD_CORE_FRAC <- 0.28
INWARD_TREE_MIN <- 0.3

#' Default Layout Parameters for Dynamic Sizing Controls
#' @export
TREE_FIT_DEFAULTS <- list(
  aspect = 0.6,
  tiplab_size = 4,
  branch_size = 4,
  tippoint_size = 4,
  zoom = 1,
  h = 0,
  # A closed circle. What a radial tree actually needs depends on the rings
  # drawn on it, which the layout fit knows nothing about — `tree_open_angle()`
  # solves it from those, and the view applies that answer over this one.
  open_angle = 0
)

# Both layouts draw edge to edge: the axis reserves room for whatever sits
# outside the tree, rather than the finished picture being scaled down to hide
# an overflow. Kept as named constants because they are what the fit returns
# and what a Reset restores, and those two must not drift apart.
LINEAR_ZOOM <- 1
LINEAR_H <- 0

# Branch line width, in ggplot2 linewidth units, and the tip count it is drawn
# at. A tree's branch count is its tip count, and they all have to fit the same
# page — so past a few dozen the stroke has to come down with them or the
# drawing fills in solid, which is what a few hundred tips in a circle did.
BRANCH_WIDTH <- 0.5
# A hairline, and no thinner: the preview is drawn at PLOT_RES, where this is
# about one pixel. Below it the stroke stops being a line and starts being a
# grey smudge — thinner on paper, but gone on screen.
BRANCH_WIDTH_MIN <- 0.08
BRANCH_WIDTH_TIPS <- 60
# How hard the stroke falls with the tip count: in step with it, because that
# is how the crowding grows. A square-root fall came down far too slowly — a
# radial tree of a few hundred tips still filled in solid.
BRANCH_WIDTH_FALL <- 1

# Fitting limits
TIP_GROWTH <- 1.5 # Cap size scaling relative to default (150%)
TIP_ASPECT_MIN <- 0.5 # Minimum allowed aspect ratio
# A few hundred tips at the target pitch would be a plot feet tall, so past
# this the rows tighten instead of the page growing. Lowered with TIP_ROW_IN,
# and for the same reason.
TIP_ASPECT_MAX <- 5
TIP_SIZE_MIN <- 0.5 # Minimum size threshold
TIP_SIZE_FLOOR <- 1.2 # Minimum text size for legibility flag

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

#' Calculate Auto-Fitted Layout Parameters
#'
#' Derives optimal aspect ratios, font sizes, and element scaling based on
#' dataset dimensions (tip count) and target device geometry.
#'
#' @param n_tip Integer. Number of tips in the phylogenetic tree.
#' @param width_in Numeric. Device panel width in inches. Default 5.5.
#' @param layout Character. Tree layout mode (e.g., "rectangular", "circular").
#' @param label_chars Numeric. Max expected character length of tip labels.
#'
#' @return List of calculated display parameters and legibility flags.
#' @export
tree_auto_layout <- function(
  n_tip,
  width_in = 5.5,
  layout = "rectangular",
  label_chars = 20
) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    5.5
  } else {
    width_in
  }
  chars <- max(as.numeric(label_chars %||% 1), 1)

  circular <- layout %in% .circular_layouts
  # A circular panel is square: the tree is a disc, so its height is its width.
  aspect <- if (circular) {
    1
  } else {
    .clamp(n * TIP_ROW_IN / w, TIP_ASPECT_MIN, TIP_ASPECT_MAX)
  }

  inward <- identical(layout, "inward")
  if (inward) {
    # Inward: the tree hangs from the rim and the labels run inward from its
    # tips, stopping at INWARD_CORE_FRAC of the radius. Their spacing is the
    # arc *there* — the tightest point along their length — which does not move
    # with the ring, so unlike the circular solve there is no crossing to find:
    # the row constraint is fixed and the ring is only ever widened until the
    # width constraint stops binding.
    radius_in <- w * TREE_RADIAL_FRAC
    by_row <- TIP_ROW_FILL * 25.4 * 2 * pi * radius_in * INWARD_CORE_FRAC / n
    ring <- min(
      by_row * TIP_CHAR_EM * chars / (25.4 * radius_in),
      1 - INWARD_CORE_FRAC - INWARD_TREE_MIN
    )
    by_width <- 25.4 * radius_in * max(ring, 0) / (TIP_CHAR_EM * chars)
    size <- min(by_row, by_width)
  } else if (circular) {
    # The radial solve. Everything is measured along the radius, which is half
    # the panel, and the label ring takes the outer `ring` of it.
    #
    # Two constraints, and they move in opposite directions as the ring grows:
    #
    #   by_width — a label is set along the radius, so a longer ring lets it be
    #     larger. Proportional to `ring`.
    #   by_row — two neighbouring labels are separated by the arc between their
    #     tips, and the tips sit on the circle *inside* the ring, whose
    #     circumference is 2*pi*R*(1 - ring). Proportional to `1 - ring`.
    #
    # One rises and one falls, so the largest legible type is exactly where
    # they cross, and that crossing has a closed form. `k` is the ratio of what
    # the two constraints ask for at ring = 1 and ring = 0 respectively; the
    # crossing is at k / (1 + k).
    #
    # This replaces a fixed guess that the tips sat at 0.35 of the panel
    # whatever the tree held, which is why a radial tree drew the same type
    # size at twenty tips as at eighty and ran it off the canvas at both.
    radius_in <- w * TREE_RADIAL_FRAC
    k <- TIP_ROW_FILL * 2 * pi * TIP_CHAR_EM * chars / n
    ring <- min(k / (1 + k), TIP_RING_FRAC_MAX)
    by_width <- 25.4 * radius_in * ring / (TIP_CHAR_EM * chars)
    by_row <- TIP_ROW_FILL * 25.4 * 2 * pi * radius_in * (1 - ring) / n
    size <- min(by_row, by_width)
  } else {
    pitch_in <- TIP_USABLE * aspect * w / n
    row_mm <- 25.4 * pitch_in
    by_row <- TIP_ROW_FILL * row_mm
    by_width <- 25.4 * TIP_LABEL_FRAC * w / (TIP_CHAR_EM * chars)
    size <- min(by_row, by_width)
  }

  # Scale element sizes while clamping maximum growth
  fit_size <- function(field, value) {
    cap <- floor(10 * TIP_GROWTH * TREE_FIT_DEFAULTS[[field]]) / 10
    .clamp(round(value, 1), TIP_SIZE_MIN, cap)
  }

  list(
    aspect = round(aspect, 1),
    tiplab_size = fit_size("tiplab_size", size),
    branch_size = fit_size("branch_size", by_row),
    tippoint_size = fit_size("tippoint_size", by_row),
    # Both layouts are drawn edge to edge now. The 0.95 shrink and the -0.05
    # nudge were a radial tree's only defence against its own labels — there
    # was no reserve keeping them inside the panel, so the whole picture was
    # scaled down and shoved left in the hope they would fit. The reserve does
    # that job properly (see .tiplab_xlim), and scaling on top of it only wastes
    # canvas.
    zoom = LINEAR_ZOOM,
    h = LINEAR_H,
    # Solved against the annotations by tree_open_angle(), not here: this fit
    # sees only the tree.
    open_angle = TREE_FIT_DEFAULTS$open_angle,
    # Thinner as the branches multiply, so they stay separate lines rather
    # than filling in.
    branch_width = round(
      .clamp(
        BRANCH_WIDTH * (BRANCH_WIDTH_TIPS / n)^BRANCH_WIDTH_FALL,
        BRANCH_WIDTH_MIN,
        BRANCH_WIDTH
      ),
      2
    ),
    labels_legible = size >= TIP_SIZE_FLOOR
  )
}

#' Most keys one legend lists before it starts counting instead.
#'
#' A key list is for looking a value up in, and past a handful of swatches
#' nobody does that: 81 patient ids ran the guide box off the bottom of the
#' canvas, and would have been unusable had it fit. The colours still do their
#' other job — showing where the same value recurs on the tree — so the scale
#' keeps its palette and the guide keeps the first few keys, with the rest
#' reported as a count rather than dropped in silence.
#' @export
LEGEND_MAX_KEYS <- 9L

#' The keys one guide should list, and what to say about the rest.
#'
#' @param levels Character vector of the scale's levels, in draw order.
#' @return list(breaks = <character>, hidden = <integer>).
#' @export
tree_legend_breaks <- function(levels) {
  levels <- as.character(levels)
  if (length(levels) <= LEGEND_MAX_KEYS) {
    return(list(breaks = levels, hidden = 0L))
  }
  list(
    breaks = levels[seq_len(LEGEND_MAX_KEYS)],
    hidden = length(levels) - LEGEND_MAX_KEYS
  )
}

#' A guide title that says how many values it is not showing.
#'
#' Said on the title rather than as a key of its own: ggplot2's guides have no
#' slot for a row that is not a break, and a count dressed up as a swatch would
#' read as another category.
#'
#' @param name Character. The variable's title.
#' @param hidden Integer. Levels the guide is not listing.
#' @return Character.
#' @export
tree_legend_title <- function(name, hidden) {
  if (!isTRUE(hidden > 0)) {
    return(name)
  }
  paste0(name %||% "", "\n+ ", hidden, " more")
}

#' Calculate Legend Column Multiples
#'
#' @param n_levels Integer. Number of categories in the legend.
#' @param max_rows Integer. Target maximum vertical entries per column.
#' @return Integer count of legend columns (1 to 4).
#' @export
tree_legend_ncol <- function(n_levels, max_rows = LEGEND_MAX_ROWS) {
  max_rows <- max(as.integer(max_rows), 1L)
  if (n_levels <= max_rows) {
    return(1L)
  }
  as.integer(min(4L, ceiling(n_levels / max_rows)))
}

#' Calculate Rounded Scale Bar Width
#'
#' Returns a clean 1/2/5 step rounded interval at or below `x`.
#'
#' @param x Numeric target distance.
#' @return Numeric scale bar step.
#' @export
tree_nice_width <- function(x) {
  if (!is.finite(x) || x <= 0) {
    return(1)
  }
  mag <- 10^floor(log10(x))
  step <- c(1, 2, 5, 10)
  step[max(which(step * mag <= x))] * mag
}

# --- Whole-tree distance axis -------------------------------------------------
#
# An alternative to the scale bar, not a replacement for it — the two answer
# different questions and stay independently switchable.
#
# The scale bar (geom_treescale, above) shows what one representative distance
# looks like; reading any other distance off it means eyeballing a multiple of
# that one segment. A phylogram's x position is already cumulative allelic
# distance from the root — that is the entire premise of drawing branch
# lengths to scale rather than as a cladogram — so a real axis, ticked and
# labelled from 0 to the tree's own depth, only makes explicit what the
# drawing already encodes. It does not change what any position means, which
# is why it is fine where the log axis and the truncation considered earlier
# were not.
#
# It does not fix the legibility problem a very unequal tree has, either: the
# ticks are still spaced linearly, so a cluster of near-zero branches still
# collapses to a few pixels near the origin. It is a more precise read-out of
# the same geometry, not a cure for what the geometry does to unequal data —
# that is still the branch labels' job, on the few branches wide enough to
# hold one.

AXIS_TICK_LEN <- 0.4 # Tick length below the axis line, in tip rows.
AXIS_LABEL_GAP <- 0.6 # Label clearance below the tick, in tip rows.
AXIS_TARGET_TICKS <- 6L

# Type size for the distance axis and the scale bar, in millimetres.
#
# Fixed, not fitted: both are a single row at the foot of the plot, so unlike a
# tip label they are not competing with n-1 others for the height. Taking them
# from `branch_size` — which *is* fitted to the tip pitch — is what shrank the
# axis to a smear on a tree with a few hundred tips, next to a legend that had
# stayed readable.
AXIS_LABEL_SIZE <- 2.9

#' Round tick positions for a whole-tree distance axis
#'
#' `pretty()` picks the same human-friendly steps R's own axes use. Its result
#' can overshoot `max_x` by up to half a step, which `tree_nice_width()`'s
#' single value never has to worry about — a tick past the tree's own depth
#' would sit in the label reserve rather than over anything drawn, so it is
#' dropped rather than clipped.
#'
#' @param max_x Numeric. The tree's maximum x (the root sits at 0).
#' @param n Integer. Target tick count.
#' @return Numeric vector of break positions, ascending, within `[0, max_x]`.
#' @export
tree_axis_breaks <- function(max_x, n = AXIS_TARGET_TICKS) {
  if (!isTRUE(is.finite(max_x) && max_x > 0)) {
    return(numeric(0))
  }
  breaks <- pretty(c(0, max_x), n = n)
  breaks[breaks >= 0 & breaks <= max_x * 1.001]
}

#' A ticked, labelled axis under the tree, to the same scale as the branches
#'
#' @param opts List. Resolved tree options.
#' @param max_x Numeric. The tree's own maximum x.
#' @param y0 Numeric. Row position of the axis line.
#' @return A list of ggplot2 layers, or NULL when switched off or degenerate.
tree_axis_layer <- function(opts, max_x, y0) {
  if (!isTRUE(opts$axis_show)) {
    return(NULL)
  }
  breaks <- tree_axis_breaks(max_x)
  if (!length(breaks)) {
    return(NULL)
  }

  digits <- tree_branch_digits(breaks[breaks > 0])
  tick_y <- y0 - AXIS_TICK_LEN
  label_y <- tick_y - AXIS_LABEL_GAP

  line <- data.frame(x = 0, xend = max_x, y = y0, yend = y0)
  ticks <- data.frame(x = breaks, xend = breaks, y = y0, yend = tick_y)
  labels <- data.frame(
    x = breaks,
    y = label_y,
    label = tree_branch_format(breaks, digits)
  )

  seg <- function(d) {
    geom_segment(
      data = d,
      mapping = aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
        y = .data[["y"]],
        yend = .data[["yend"]]
      ),
      inherit.aes = FALSE,
      color = opts$line_color
    )
  }

  list(
    seg(line),
    seg(ticks),
    geom_text(
      data = labels,
      mapping = aes(
        x = .data[["x"]],
        y = .data[["y"]],
        label = .data[["label"]]
      ),
      inherit.aes = FALSE,
      size = AXIS_LABEL_SIZE * .scale_of(opts),
      vjust = 1,
      color = opts$line_color
    )
  )
}

# --- Branch labels -----------------------------------------------------------
#
# Which branches carry their allelic distance in writing.
#
# The tree itself is left alone. Branch lengths are drawn to scale and the
# distances are read from the scale bar — that is the convention every tree
# viewer follows, and the only one under which the drawn distance between two
# tips equals the sum of the branches between them. Neither of the tricks that
# suggest themselves for a tree with one branch far longer than the rest is
# used here: a log axis destroys that additivity outright (a path's drawn
# length stops being the sum of its parts, and the scale bar stops meaning
# anything), and truncating the long branch is only honest with a break glyph
# and the true value printed beside it, which is a figure the *reader* has to
# be told about rather than something to do to a tree silently.
#
# So the length disparity is not the label layer's to fix, and it is not what
# was wrong. What was wrong is that labels were picked by *rank*: the longest
# `BRANCH_LABEL_MAX` branches, whatever they measured. In a tree where one
# branch holds almost the whole span, the 2nd through 25th longest are all
# hairlines inside the same tight cluster, drawn at nearly the same x and y —
# so their numbers printed on top of each other in a blot while the branches
# they belonged to were invisible.
#
# Legibility is geometry, not rank. A branch can carry a label when the branch
# is drawn wide enough to hold the text, and when no label already sits on the
# same row. Both are computable from the axis split that is solved anyway, so
# both are decided here instead of being left to the eye.

BRANCH_ABOVE_SHRINK <- 0.72
BRANCH_VJUST <- -0.35

# Never more than this many, even where they all fit: past it the numbers are
# the figure rather than an annotation on it.
BRANCH_LABEL_MAX <- 25L

# Slack on the width test, so a label that only just fits still has air on
# either side of it rather than butting into the next branch's.
BRANCH_LABEL_PAD <- 1.2

# Minimum vertical separation between two labels, in tip rows. The text is
# fitted to a fraction of the row pitch (tree_auto_layout), so one clear row is
# always enough — and internal nodes deep in a ladder sit fractions of a row
# apart, which is what stacked them.
BRANCH_ROW_GAP <- 1

# What a circular tree's x axis is worth as a fraction of the panel it is drawn
# on.
#
# Not a guess and not a taste: ggtree's circular layouts are `CoordPolar`, whose
# `r_rescale()` maps the radial axis onto `c(0, 0.4)` of a panel it forces
# square. So the whole axis — tree, labels, rings — is drawn across 0.4 of the
# panel's side, and every physical width below has to be measured against that
# rather than against the panel. Read as half, the fit sized type for a radius
# a quarter longer than the one it got, and the labels were clipped mid-word.
TREE_RADIAL_FRAC <- 0.4

# Whether a layout draws the x axis as a radius rather than as a width.
.is_circular <- function(opts) {
  isTRUE((opts$layout %||% "rectangular") %in% .circular_layouts)
}

#' Whether this layout can carry rings and strips beside the tips at all.
#'
#' Every layout but one. An inward tree hangs from the rim and its tips point
#' at the centre, so the space "past the tips" — where every annotation in this
#' module is placed — is the middle of the disc, where the arc a ring is drawn
#' along shrinks to nothing. A ring there is not a ring: it is a filled circle,
#' with the labels under it converging to a point.
#'
#' Mapped variables are not lost by it. The mapping engine is told the strips
#' are unavailable (`off` in `eligible_aesthetics()`), so on an inward tree a
#' variable is drawn onto the tips instead — the one place that layout has room
#' for it.
#'
#' @param opts List. Resolved tree options.
#' @return TRUE when strips and heatmap panels can be drawn.
#' @export
tree_annotations_drawn <- function(opts) {
  !identical(opts$layout %||% "rectangular", "inward")
}

#' Inches the x axis is drawn across on the finished panel.
#'
#' `tree_header_size()` turns a column's width in data units into millimetres
#' of type, and the conversion is how long the axis physically is. For a
#' circular tree that is the radius, not the panel — fitting a ring's header to
#' the whole panel makes it twice the size the ring can actually carry.
#'
#' @param opts List. Resolved tree options.
#' @param panel_in Numeric. Width of the finished panel, in inches.
#' @return Numeric inches.
#' @export
tree_axis_in <- function(opts, panel_in) {
  if (.is_circular(opts)) panel_in * TREE_RADIAL_FRAC else panel_in
}

#' Inches the tree-and-labels budget is drawn across.
#'
#' The panel's width for a linear tree; its *radius* for a circular one, which
#' is half of it. This is the one number that makes every width rule below
#' transfer between the two layouts unchanged: the label reserve, the
#' annotation columns and the header type sizes are all fractions of whatever
#' the x axis is drawn across, and for a circular tree that is the radius.
#'
#' @param opts List. Resolved tree options.
#' @return Numeric inches.
#' @export
tree_budget_in <- function(opts) {
  w <- opts$width_in
  if (is.null(w) || !is.finite(w) || w <= 0) {
    w <- 5.5
  }
  if (.is_circular(opts)) w * TREE_RADIAL_FRAC else w
}

#' Decimal places for a set of branch labels
#'
#' One choice for the whole figure rather than per label: allelic distances
#' printed as "1600.5" beside "3.78" read as different quantities.
#'
#' The two cases that actually arise get exact answers first. An allelic
#' distance matrix counts mismatched loci, so it is integral, and neighbour
#' joining halves it at most — so whole numbers print whole and halves print
#' with one decimal, rather than dragging "8.00" along behind a "12.50".
#' Anything else falls back to about three significant figures.
#'
#' @param x Numeric vector of branch lengths that may be labelled.
#' @return Integer, 0 to 2.
#' @export
tree_branch_digits <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) {
    return(0L)
  }
  resolves_at <- function(step) all(abs(x / step - round(x / step)) < 1e-6)
  if (resolves_at(1)) {
    return(0L)
  }
  if (resolves_at(0.5)) {
    return(1L)
  }
  as.integer(.clamp(3 - floor(log10(max(x))), 0, 2))
}

#' Format branch lengths for printing on a branch
#'
#' @param x Numeric vector of branch lengths.
#' @param digits Integer decimal places, from `tree_branch_digits()`.
#' @return Character vector.
#' @export
tree_branch_format <- function(x, digits) {
  formatC(round(x, digits), format = "f", digits = digits)
}

#' Select the branches whose label can actually be read
#'
#' Two tests, in order. A branch has to be drawn at least as wide as its own
#' text (`BRANCH_LABEL_PAD` times, for air), which is what excludes the
#' hairlines inside a tight cluster however long they are relative to their
#' neighbours. Then, longest first, a branch is taken only if no label already
#' accepted sits within `BRANCH_ROW_GAP` rows of it — greedy, so where two
#' branches compete for a row the longer one wins.
#'
#' @param len Numeric branch lengths.
#' @param y Numeric vertical positions, in tip rows.
#' @param span_x Numeric. The tree's own x span, in tree units.
#' @param span_in Numeric. Inches that span is drawn across.
#' @param size Numeric. Rendered text size, in mm (ggplot2's `size`).
#' @param digits Integer decimal places, from `tree_branch_digits()`.
#' @param max_labels Integer cap.
#' @param row_gap Numeric minimum row separation.
#' @return Integer vector of positions into `len`, longest branch first.
#' @export
tree_branch_keep <- function(
  len,
  y,
  span_x,
  span_in,
  size,
  digits,
  max_labels = BRANCH_LABEL_MAX,
  row_gap = BRANCH_ROW_GAP
) {
  n <- length(len)
  if (!n || !isTRUE(is.finite(span_x) && span_x > 0)) {
    return(integer(0))
  }
  if (!isTRUE(is.finite(span_in) && span_in > 0)) {
    return(integer(0))
  }

  # Both sides in inches: the text from its character count at the rendered
  # size (the em width tip labels are reserved with), the branch from its
  # share of the tree's span.
  chars <- nchar(tree_branch_format(len, digits))
  need <- BRANCH_LABEL_PAD * chars * TIP_CHAR_EM * size / 25.4
  have <- len / span_x * span_in

  fits <- which(is.finite(len) & len > 0 & is.finite(y) & have >= need)
  if (!length(fits)) {
    return(integer(0))
  }

  keep <- integer(0)
  taken_y <- numeric(0)
  for (i in fits[order(len[fits], decreasing = TRUE)]) {
    if (length(taken_y) && min(abs(taken_y - y[i])) < row_gap) {
      next
    }
    keep <- c(keep, i)
    taken_y <- c(taken_y, y[i])
    if (length(keep) >= max_labels) {
      break
    }
  }
  keep
}

# --- Annotation widths -------------------------------------------------------
#
# Every annotation drawn beside the tips — a tile strip, a heatmap column — is
# sized by what it has to show rather than by dividing a fixed budget between
# them. Widths are fractions of the tree's own span.
#
# A fixed budget was the wrong model, and produced both reported faults: one
# tile strip took the whole 0.45 and left the heatmap the 0.1 floor (a 15-column
# matrix in a tenth of the tree's width, illegible), and the strip itself was
# thin because 0.45 of the tree span is only about a fifth of the panel once the
# labels and the axis expansion are counted.
#
# Sizing by content is only safe because the canvas now grows to fit
# (annotation_total feeds tree_panel_width_in): asking for more room adds
# canvas rather than taking it off the tree.

# --- Annotation header type -------------------------------------------------

# Most of the panel's height the header reserve may take. Past this the headers
# are longer than the tree is tall, and clipping one or two of them is the
# better trade.
HEADER_FRAC_MAX <- 0.45

# Annotation headers are set vertically over their own column, so the column's
# *width* is what limits the type size — the same constraint the tip labels
# answer to, applied to the other axis. A fixed size is what let thirty gene
# names overprint each other into a smear.
HEADER_SIZE_MAX <- 2.8
HEADER_SIZE_MIN <- 0.9

# --- Drawing the same figure at another size ---------------------------------
#
# Every type size in this module is a physical one — millimetres of glyph — and
# every reserve beside them is a *fraction* of an axis. That pairing only holds
# at the size the plot was fitted for: printed smaller, the labels keep their
# millimetres while the reserve shrinks under them and they collide; printed
# larger, they shrink into a gutter of their own dead space, beside a scale bar
# that did scale because it is drawn in data units.
#
# `scale` keeps the two in step. It multiplies every physical length below —
# type sizes, annotation column widths, legend geometry — so a plot built at
# `scale = k` on a canvas k times as wide is *geometrically similar* to the one
# on screen rather than the same drawing stretched.
#
# Ratios do not take it. HEADER_CHAR_ROWS is millimetres of type over inches of
# row pitch and both sides scale together, so it is the same number at any
# size; so are TIP_CHAR_EM, HEADER_FILL and every `*_ROWS` and `*_FRAC`.
# Smallest type a printed figure should carry, in points.
#
# The number journals converge on: Nature, Science and PLOS all set their floor
# between 5 and 7 pt, and 5 is the common minimum for a label. Below it the
# figure is not "dense", it is unreadable on paper.
#' @export
MIN_PRINT_PT <- 5

# ggplot2 sizes geom text in millimetres of font height and theme text in
# points, so the two have to be converted before they can be compared.
.MM_TO_PT <- 72 / 25.4

#' Smallest type this plot will print at, in points.
#'
#' Scaling the whole design keeps a figure *proportioned* at any size, which is
#' what makes an export faithful — but proportion says nothing about legibility.
#' A dense tree squeezed onto a journal column is correctly drawn and still too
#' small to read, and the only honest thing to do about that is say so: silently
#' enlarging the type would change the layout it was fitted to and put the
#' labels back on top of each other.
#'
#' @param opts List. Resolved tree options, at the size they will be drawn.
#' @param md Data frame. Per-tip metadata.
#' @param panel_in Numeric. Width of the finished panel, in inches.
#' @return Numeric points.
#' @export
tree_min_type_pt <- function(opts, md, panel_in = NULL) {
  opts <- resolve_annotation_widths(opts, md)
  panel_in <- panel_in %||%
    tree_panel_width_in(
      opts,
      md,
      opts$width_in %||% 5.5
    )
  axis_in <- tree_axis_in(opts, panel_in)
  scale <- .scale_of(opts)

  # An annotation header is the smallest type the plot sets, because it is
  # fitted to a column rather than to a row.
  mm <- numeric(0)
  if (annotation_total(opts) > 0) {
    mm <- c(mm, HEADER_SIZE_MIN * scale)
    if (.n_tiles(opts) > 0) {
      mm <- c(mm, .tile_col_in(opts) * 25.4 * HEADER_FILL)
    }
    if (
      length(Filter(function(h) length(h$cols) > 0L, opts$heatmaps %||% list()))
    ) {
      mm <- c(mm, .heat_col_in(opts) * 25.4 * HEADER_FILL)
    }
  }
  if (isTRUE(opts$tiplab_show)) {
    mm <- c(mm, opts$tiplab_size %||% 4)
  }
  pt <- c(mm * .MM_TO_PT, opts$legend_size %||% 10)
  min(pt[is.finite(pt) & pt > 0], Inf)
}

#' The same plot, designed for a canvas `k` times as wide.
#'
#' Exporting is not rescaling. A finished ggplot printed at another size keeps
#' every glyph at the millimetres it was given while the reserves around them
#' move, so the figure that comes out is not the one that was designed: too
#' small and the tip labels run into the annotation beside them, too large and
#' they shrink into a gutter next to a scale bar that scaled without them.
#'
#' This returns the option set to *rebuild* from instead — every physical
#' length multiplied through, so the export is the preview at another size
#' rather than the preview stretched.
#'
#' @param opts List. Resolved tree options.
#' @param k Numeric. Target canvas width over the designed one.
#' @return `opts`, scaled.
#' @export
scale_tree_opts <- function(opts, k) {
  k <- suppressWarnings(as.numeric(k))
  if (length(k) != 1L || is.na(k) || !is.finite(k) || k <= 0) {
    return(opts)
  }
  opts$scale <- .scale_of(opts) * k
  opts$width_in <- (opts$width_in %||% 5.5) * k
  # The type sizes the user set, at the new size. Their *relative* choices are
  # what they chose; the millimetres were only ever right for one page.
  for (f in c("tiplab_size", "branch_size", "tippoint_size", "legend_size")) {
    if (!is.null(opts[[f]])) {
      opts[[f]] <- opts[[f]] * k
    }
  }
  # Resolved against the old width, so no longer true of this one.
  opts$tile_span <- NULL
  opts$heat_span <- NULL
  opts
}

.scale_of <- function(opts) {
  k <- suppressWarnings(as.numeric(opts$scale %||% 1))
  if (length(k) != 1L || is.na(k) || !is.finite(k) || k <= 0) 1 else k
}
# Share of a column a header may fill across its width, leaving the rest as the
# gap that keeps neighbouring headers apart.
HEADER_FILL <- 0.78

# The reserve is expansion on the axis it is measured in, so adding it pushes
# the rows closer together — this is that compression, and doubles as the gap
# that keeps the topmost header off the panel edge.
HEADER_ROW_PACK <- 1.15

# --- The class band under a heatmap ------------------------------------------
#
# A gene symbol does not say which drug class it belongs to, and a matrix of
# thirty of them is the one place that grouping matters most. The columns are
# already ordered by class (the catalogue files them that way and the view
# keeps that order), so each class is a contiguous run — and a run can be
# bracketed and named.
#
# Below the matrix rather than above it: the space above is spoken for by the
# column names, and a class name over a gene name reads as a second gene. The
# tree's scale bar and axis also sit below, but under the *tree*, so the two
# never meet horizontally.

# Rows of tip pitch between the matrix and the bracket under it.
CLASS_GAP_ROWS <- 0.5
# Height of the tick turned up at each end of a bracket.
CLASS_TICK_ROWS <- 0.35
# And between the bracket and the name hanging under it.
CLASS_LABEL_GAP_ROWS <- 0.45
# Share of a run's width a bracket spans, leaving the rest as the gap that
# tells one run from the next.
CLASS_BRACKET_FILL <- 0.86
# Most of the panel's height the whole band may take, brackets and names
# together. Past this the class names are longer than the tree is tall.
CLASS_FRAC_MAX <- 0.35

# Rows of tip pitch one header character claims when set vertically, at
# HEADER_SIZE_MAX; heatmap_header_frac() scales it down with the fitted size.
#
# Derived rather than measured: ggplot2's text `size` is the type height in
# millimetres and a character's vertical advance is TIP_CHAR_EM of it, so a
# header is `chars * TIP_CHAR_EM * size` mm tall, against a tip row of
# TIP_ROW_IN. It was a flat 0.62 — near twice this — which reserved a third of
# the page for headers that needed an eighth of it.
HEADER_CHAR_ROWS <- HEADER_ROW_PACK *
  TIP_CHAR_EM *
  HEADER_SIZE_MAX /
  (25.4 * TIP_ROW_IN)

# Widths are *physical*, in inches, and converted to tree spans where they are
# drawn (see `resolve_annotation_widths()`). What an annotation column has to
# fit is a header set in real type, and how many inches a tree span is worth
# changes with every plot — so a width in tree spans is a width in the wrong
# unit, and it is why a thirty-column matrix came out with headers at the
# minimum size while a three-column one had room to spare.

# Width of one heatmap column.
#
# HEADER_SIZE_MAX millimetres of type across HEADER_FILL of the column is
# exactly the width at which a header reaches its ceiling size — so this is the
# narrowest a column can be and still carry a full-size label. Narrower and the
# headers shrink with it; wider and the page grows for nothing.
#' @export
HEATMAP_COL_IN <- HEADER_SIZE_MAX / HEADER_FILL / 25.4

# The column widths at this plot's scale. They are derived from the header size
# a column has to carry, so they follow it.
.heat_col_in <- function(opts) HEATMAP_COL_IN * .scale_of(opts)
.tile_col_in <- function(opts) TILE_COL_IN * .scale_of(opts)

# Width of one tile strip. Wider than a heatmap column and for a reason: a
# strip carries one variable rather than a run of them, and its header is a
# variable *name*, several times longer than a gene symbol. Not much wider —
# the strip is read as a band of colour beside the tips, and past this it is
# only a bigger band.
#' @export
TILE_COL_IN <- 1.6 * HEATMAP_COL_IN

# The tree span, in inches, assumed by a caller that has not resolved the
# widths against a metadata table — the budget less a typical label reserve.
# Every path that draws resolves properly; this keeps the width functions
# answerable on their own for the callers that only compare them.
NOMINAL_TREE_IN <- 3.5

# Ceiling on all annotations together, as a multiple of the tree span. The
# canvas grows for them, but the tree must stay the larger part of the picture;
# past this the columns share what is left.
#
# Set against the canvas cap rather than by taste: at this much annotation the
# panel comes out at `TREE_PANEL_IN * CANVAS_MAX_FACTOR`, which is as wide as
# the view module will draw. Past it the columns are squeezed and their headers
# shrink — the graceful end of "one column per gene", at around fifty columns.
ANNOTATION_SPAN_MAX <- 1.9

# The same ceiling for a circular tree, and much tighter, because a ring is not
# a column. A column's share of the picture is its width; a ring's is its
# *area*, which grows with the radius it sits at — so an annotation run as wide
# as the tree's own radius already covers three quarters of the disc. Matching
# the linear ceiling here left the tree a knot at the centre of a dartboard.
ANNOTATION_SPAN_MAX_CIRC <- 1.0

# The ceiling this layout answers to.
.annotation_span_max <- function(opts) {
  if (.is_circular(opts)) ANNOTATION_SPAN_MAX_CIRC else ANNOTATION_SPAN_MAX
}

# Room past the outermost annotation, in tree spans.
#
# The strips and panels are placed to fill their reserve exactly, so without
# this the far edge of the last one lands *on* the x limit — and `xlim()`
# censors rather than clips, so the whole outer column was dropped from the
# plot with only its header and its legend left behind. That is the fault
# where a single tile strip drew no tiles until a second strip was added
# beside it, and where the last heatmap column went missing.
ANNOTATION_SLACK <- 0.02

#' Resolve what one annotation column is worth, in tree spans.
#'
#' `geom_fruit` and `gheatmap` both measure in multiples of the tree's own span,
#' and the header sizes below are in millimetres — so somewhere the two have to
#' meet. They meet here, once, against the one thing that fixes the exchange
#' rate: how many inches the tree's span is drawn across, which is the budget
#' less whatever the tip labels took.
#'
#' Writes `tile_span` and `heat_span` onto `opts`, where every width function
#' below reads them. Called from the two entry points that hold a metadata
#' table — the builder and `tree_panel_width_in()` — so that the plot and the
#' canvas it is drawn on are solved from the same numbers.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame. Per-tip metadata.
#' @return `opts`, with the two spans set.
#' @export
resolve_annotation_widths <- function(opts, md) {
  tree_in <- tree_budget_in(opts) * (1 - .tiplab_budget_frac(opts, md))
  if (!is.finite(tree_in) || tree_in <= 0) {
    tree_in <- NOMINAL_TREE_IN * .scale_of(opts)
  }
  opts$tile_span <- .tile_col_in(opts) / tree_in
  opts$heat_span <- .heat_col_in(opts) / tree_in
  opts
}

# The resolved spans, or what they come to on a nominal tree.
.tile_span <- function(opts) opts$tile_span %||% (TILE_COL_IN / NOMINAL_TREE_IN)
.heat_span <- function(opts) {
  opts$heat_span %||% (HEATMAP_COL_IN / NOMINAL_TREE_IN)
}

#' Width of one tile strip, in tree spans.
#' @param opts List. Resolved tree options.
#' @return Numeric fraction of the tree span.
#' @export
tree_annotation_width <- function(opts) {
  .tile_span(opts)
}

# Number of tile strips in a layer set.
.n_tiles <- function(opts) {
  sum(vapply(
    opts$layers %||% list(),
    function(l) identical(l$aesthetic, "tile"),
    logical(1)
  ))
}

#' Width of the tile strips together, gaps included, in tree spans.
#' @export
tile_total <- function(opts) {
  n <- if (tree_annotations_drawn(opts)) .n_tiles(opts) else 0L
  if (!n) {
    return(0)
  }
  n * (.tile_span(opts) + TILE_GAP)
}

#' Width of the heatmap panels together, gaps included, in tree spans.
#' @export
heatmap_total <- function(opts) {
  hs <- if (tree_annotations_drawn(opts)) opts$heatmaps %||% list() else list()
  # A panel with no columns draws nothing, and the builder drops it — so it must
  # not reserve a gap here either.
  hs <- Filter(function(h) length(h$cols) > 0L, hs)
  if (!length(hs)) {
    return(0)
  }
  n_cols <- sum(vapply(hs, function(h) length(h$cols), integer(1)))
  n_cols * .heat_span(opts) + HEATMAP_GAP * length(hs)
}

# The scale factor that brings the annotations back under ANNOTATION_SPAN_MAX.
.annotation_squeeze <- function(opts) {
  ceiling <- .annotation_span_max(opts)
  want <- tile_total(opts) + heatmap_total(opts)
  if (want <= ceiling || want <= 0) {
    return(1)
  }
  ceiling / want
}

# Most of a linear tree's axis the tip labels may claim. The circular ceiling
# is TIP_RING_FRAC_MAX, which the radial fit is solved against.
TIP_LABEL_AXIS_MAX <- 0.45

.tiplab_frac <- function(opts, md) {
  if (!isTRUE(opts$tiplab_show)) {
    return(0.02)
  }
  w <- opts$width_in
  if (is.null(w) || !is.finite(w) || w <= 0) {
    return(0.375)
  }
  chars <- suppressWarnings(max(nchar(as.character(md[[opts$tiplab]])), 1L))
  if (!is.finite(chars)) {
    chars <- 1L
  }
  cap <- if (identical(opts$layout, "inward")) {
    1 - INWARD_CORE_FRAC - INWARD_TREE_MIN
  } else if (.is_circular(opts)) {
    TIP_RING_FRAC_MAX
  } else {
    TIP_LABEL_AXIS_MAX
  }
  min(
    cap,
    chars *
      TIP_CHAR_EM *
      (opts$tiplab_size %||% 4) /
      25.4 /
      tree_budget_in(opts)
  )
}

# Slack on the tip-label reserve.
#
# `TIP_CHAR_EM` is a mean character advance, and the device's real metrics run
# about a tenth wider than it for an accession — measured off a render, where a
# 36-character label reserved 1.99in and drew 2.21in, so the annotation beside
# it landed on its last word. This covers that difference. It is not a margin:
# the gutter between the labels and the first annotation is ANNOTATION_LEAD,
# which this only stops the labels from eating.
#
# Applied here rather than to TIP_CHAR_EM itself, which also drives the
# type-size fit — the labels are the right size, there was simply less room
# kept for them than they take.
X_EXPANSION <- 1.12

# The labels' share of the tree-and-labels budget — of `opts$width_in`, not of
# the panel. The panel grows for the annotations and the budget does not, so
# the two are different fractions of different things, and the one place that
# mattered is `.tiplab_axis_frac()` below.
.tiplab_budget_frac <- function(opts, md) {
  .clamp(.tiplab_frac(opts, md) * X_EXPANSION, 0, 0.8)
}

# How much wider than the tree-and-labels budget the panel has to be.
#
# The annotations are a multiple of the *tree's* span, and the tree is what is
# left of the budget once the labels have taken their share — so a plot with
# long labels needs less extra canvas for the same annotations than one with
# short labels, not the same amount.
.panel_growth <- function(opts, md, heat = annotation_total(opts)) {
  1 + heat * (1 - .tiplab_budget_frac(opts, md))
}

# The labels' share of the whole x axis, which is what `xlim()` is solved in.
#
# The labels need a fixed number of inches; the axis spans the *grown* panel.
# Spending the budget fraction of the grown axis on them is how a thirty-column
# heatmap came to reserve three inches for labels that needed two, leaving a
# band of dead space between the tips and the first strip — and taking the inch
# it wasted off the tree.
.tiplab_axis_frac <- function(opts, md, heat) {
  .tiplab_budget_frac(opts, md) / .panel_growth(opts, md, heat)
}

# HEATMAP_CLEARANCE is gone. It padded the tip-label reserve by a further 30%
# so an annotation matrix placed at exactly `reserve` would not touch the
# labels. The annotations now carry their own gap (HEATMAP_GAP, TILE_GAP) and
# are placed past it, so the padding had nothing left to do except leave a band
# of dead space between the labels and the matrix as wide as the labels
# themselves.

# Legend geometry, in inches at legend_size 10.
LEGEND_KEY_IN <- 0.16 # key square plus its gap
LEGEND_PAD_IN <- 0.12 # box padding either side
LEGEND_MAX_FRAC <- 0.35 # never more than this share of the canvas
LEGEND_ROW_IN <- 0.19 # one key row, title line or inter-guide gap
LEGEND_MAX_COLS <- 3L # past this the guides are wider than the tree
LEGEND_MAX_ROWS <- 18L # keys in one column before they wrap into another

#' Rows one guide box would stack, given what each guide will list.
#'
#' A title, its keys, the "+ N more" line when there is one, and a blank row
#' before the next guide.
#'
#' @param layers List of mapping layer records.
#' @param heatmaps List of heatmap panel records.
#' @return Integer row count.
#' @export
tree_legend_rows <- function(
  layers,
  heatmaps = list(),
  max_rows = LEGEND_MAX_ROWS
) {
  rows <- function(n_levels, capped) {
    keys <- min(as.integer(n_levels), LEGEND_MAX_KEYS)
    # Wrapped into as many key columns as it needs, so a guide is only as tall
    # as its longest column.
    keys <- ceiling(keys / tree_legend_ncol(keys, max_rows))
    1L + as.integer(keys) + as.integer(capped) + 1L
  }
  per <- vapply(
    layers %||% list(),
    function(l) {
      n <- as.integer(l$n_levels %||% 1L)
      rows(n, n > LEGEND_MAX_KEYS)
    },
    integer(1)
  )
  heat <- vapply(
    Filter(function(h) length(h$cols) > 0L, heatmaps %||% list()),
    function(h) rows(length(AMR_GENE_STATES), FALSE),
    integer(1)
  )
  sum(c(per, heat, 0L))
}

#' Rows one guide box has room for, at the height the plot is drawn.
#'
#' @param legend_size Numeric. Legend text size in points.
#' @param height_in Numeric. Height the plot is drawn at, in inches.
#' @param scale Numeric. This plot's physical scale.
#' @return Integer row budget, at least 1.
#' @export
tree_legend_room <- function(legend_size = 10, height_in = NULL, scale = 1) {
  if (is.null(height_in) || !is.finite(height_in) || height_in <= 0) {
    return(LEGEND_MAX_ROWS)
  }
  row_in <- LEGEND_ROW_IN * scale * (legend_size %||% 10) / 10
  max(as.integer(floor(height_in / row_in)), 1L)
}

#' Rows one guide may run to before its keys wrap into another column.
#'
#' ggplot2 stacks guides in a single column and clips whatever runs past the
#' panel, which is how a legend simply stopped halfway down. The fix is not to
#' throw the whole box sideways — that spent the full width on one row of
#' guides and looked worse than the problem. It is to let each *guide* wrap its
#' own keys into two or three columns, which is what the row budget below
#' decides: share the height the plot has between the guides it has to show,
#' and that is how tall each one may be.
#'
#' @param layers List of mapping layer records.
#' @param heatmaps List of heatmap panel records.
#' @param legend_size Numeric. Legend text size in points.
#' @param height_in Numeric. Height the plot is drawn at, in inches.
#' @param scale Numeric. This plot's physical scale.
#' @return Integer rows per guide, at least 3.
#' @export
tree_legend_max_rows <- function(
  layers,
  heatmaps = list(),
  legend_size = 10,
  height_in = NULL,
  scale = 1
) {
  guides <- length(layers %||% list()) +
    length(Filter(function(h) length(h$cols) > 0L, heatmaps %||% list()))
  room <- tree_legend_room(legend_size, height_in, scale)
  if (guides < 1L) {
    return(LEGEND_MAX_ROWS)
  }
  # Two rows per guide go to its title and the blank line under it, so only
  # what is left can hold keys.
  per <- floor(room / guides) - 2L
  as.integer(.clamp(per, 3L, LEGEND_MAX_ROWS))
}

#' Columns the guide box needs so that no guide is cut off.
#'
#' Kept for the canvas budget: once each guide has wrapped its keys, the box is
#' still this many columns wide and the canvas has to grow for it.
#'
#' @param layers List of mapping layer records.
#' @param heatmaps List of heatmap panel records.
#' @param legend_size Numeric. Legend text size in points.
#' @param height_in Numeric. Height the plot is drawn at, in inches.
#' @param scale Numeric. This plot's physical scale.
#' @return Integer, at least 1.
#' @export
tree_legend_cols <- function(
  layers,
  heatmaps = list(),
  legend_size = 10,
  height_in = NULL,
  scale = 1
) {
  max_rows <- tree_legend_max_rows(
    layers,
    heatmaps,
    legend_size,
    height_in,
    scale
  )
  rows <- tree_legend_rows(layers, heatmaps, max_rows)
  room <- tree_legend_room(legend_size, height_in, scale)
  as.integer(min(max(ceiling(rows / room), 1), LEGEND_MAX_COLS))
}

#' Inches the guide box will take beside the tree.
#'
#' ggplot2 sizes the box itself, but `.tiplab_frac()` measures the tip labels
#' against `opts$width_in` — the whole canvas. With a right-hand legend the
#' panel is narrower than that by this much, so without the correction the
#' label reserve is understated and the labels clip at the panel edge again.
#' `LINEAR_ZOOM` is 1, so a linear tree is drawn edge to edge and has no spare
#' margin for the legend to live in: this is load-bearing, not cosmetic.
#'
#' The estimate uses the same mean character advance the tip-label reserve is
#' built on, applied to the legend's own text. A vertical box stacks its
#' guides, so it is as wide as its widest guide, not as wide as their sum.
#'
#' @param layers List of mapping layer records.
#' @param md Data frame. Per-tip metadata.
#' @param legend_size Numeric. Legend text size in points.
#' @param width_in Numeric. Canvas width in inches.
#' @param heatmaps List. The heatmap panels, each of which draws a guide of its
#'   own — a panel with no mapping layer beside it still needs room for it.
#' @param height_in Numeric. Height the plot is drawn at, for deciding how many
#'   columns the guides have to flow into so that none is cut off.
#' @return Numeric width in inches; 0 when the plot draws no guide at all.
#' @export
tree_legend_width_in <- function(
  layers,
  md,
  legend_size,
  width_in,
  heatmaps = list(),
  height_in = NULL,
  scale = 1
) {
  layers <- layers %||% list()
  # A panel with no columns draws nothing and so carries no guide either.
  heatmaps <- Filter(function(h) length(h$cols) > 0L, heatmaps %||% list())
  if (!length(layers) && !length(heatmaps)) {
    return(0)
  }
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    5.5
  } else {
    width_in
  }
  size <- legend_size %||% 10
  guide_in <- function(chars, ncol) {
    if (!is.finite(chars)) {
      chars <- 1L
    }
    ncol * (LEGEND_KEY_IN * scale + chars * TIP_CHAR_EM * size / 72)
  }
  per <- vapply(
    layers,
    function(l) {
      # Only the keys the guide will list, since that is all it is sized from.
      labs <- head(unique(as.character(md[[l$field]])), LEGEND_MAX_KEYS)
      chars <- suppressWarnings(max(nchar(c(labs, l$title %||% "")), 1L))
      guide_in(
        chars,
        tree_legend_ncol(min(l$n_levels %||% 1L, LEGEND_MAX_KEYS))
      )
    },
    numeric(1)
  )
  # Each panel's key labels are the fixed AMR states, whatever it is showing —
  # the *values* in the matrix never reach the legend. Leaving these out is how
  # a single-column matrix with no mapping beside it came out with a guide box
  # nothing had budgeted for, drawn over the tip labels it had squeezed.
  heat <- vapply(
    heatmaps,
    function(h) {
      states <- if (identical(h$level, "gene")) {
        AMR_GENE_STATES
      } else {
        c(AMR_PRESENT, AMR_ABSENT)
      }
      chars <- suppressWarnings(
        max(nchar(c(states, h$title %||% "")), 1L)
      )
      guide_in(chars, 1L)
    },
    numeric(1)
  )
  widest <- max(c(per, heat, 0))
  if (widest <= 0) {
    return(0)
  }
  # A box that has to flow into several columns is that many times as wide.
  cols <- tree_legend_cols(layers, heatmaps, size, height_in, scale)
  cols * min(widest + LEGEND_PAD_IN * scale, LEGEND_MAX_FRAC * w)
}

# The inward layout's radius is solved by the same axis split every other
# layout uses (`.tiplab_xlim`), which is what `tree_inward_xlim()` used to do on
# its own — worse, because it knew about the tip labels and nothing else, so an
# inward tree with a tile strip reserved no room for it.

HEATMAP_GAP <- 0.02

# Gutter between the tip labels and the first annotation, in tree spans.
#
# Wider than the gap *between* annotations, and there for a different reason:
# that one separates two blocks of colour, this one separates colour from text
# whose reserve is an estimate — `.tiplab_frac()` measures a mean character
# advance, not the glyphs it will actually set. Left at the inter-annotation
# gap, the last character of every tip label sits against the first strip.
ANNOTATION_LEAD <- 0.01

# Fixed two-colour fill for the AMR panel. Its cells hold comma-joined gene
# symbols, so a shared categorical scale over the raw strings would give one
# colour per distinct *combination* of genes — dozens of them, none comparable.
# What the panel is actually read for is whether a class was hit at all.
AMR_PRESENT <- "Detected"
AMR_ABSENT <- "Not detected"

# Gene-level calls carry abritamr's own confidence, which a presence/absence
# recode would throw away: an exact match and a partial hit are not the same
# claim about the isolate. The states are ordered strongest first so the legend
# reads as a scale.
#' @export
AMR_GENE_STATES <- c("Match", "Inexact", "Partial", AMR_ABSENT)

#' Fills for the AMR heatmap, at either level.
#'
#' One family of reds so the panel reads as a single measurement, darkest for
#' the strongest call, and a neutral grey for absence — which is most of the
#' matrix and must not compete with the hits.
#' @export
AMR_HEATMAP_FILL <- c(
  Detected = "#B2182B",
  Match = "#B2182B",
  Inexact = "#E08214",
  Partial = "#F4C99B",
  `Not detected` = "#EDEDED"
)

#' Inches of canvas the panel needs so the tree and its labels still get
#' `panel_in` of it once the annotations have taken their share.
#'
#' `.tiplab_xlim()` divides the x axis three ways — the tree, the label reserve,
#' and the annotations — and the annotations' share is expressed against the
#' *tree's span*, not against the panel. So growing the canvas by
#' `panel_in * annotation_total()` overshoots: it hands the annotations less
#' physical width than it charged for and leaves the difference as dead space
#' between the labels and the matrix. This solves the axis split for the canvas
#' that makes the tree-plus-labels come out at exactly `panel_in`, which is the
#' only reason the two agree.
#'
#' Lives here, beside the solve it has to match, rather than in the view module
#' that calls it.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame. Per-tip metadata.
#' @param panel_in Numeric. Inches the tree and its labels are to keep.
#' @return Numeric panel width in inches, never less than `panel_in`.
#' @export
tree_panel_width_in <- function(opts, md, panel_in) {
  opts <- resolve_annotation_widths(opts, md)
  heat <- annotation_total(opts)
  if (!isTRUE(heat > 0)) {
    return(panel_in)
  }
  growth <- .panel_growth(opts, md, heat)
  if (!is.finite(growth) || growth <= 0) {
    return(panel_in)
  }
  max(panel_in * growth, panel_in)
}

#' Total width of every annotation drawn to the right of the tip labels, as a
#' fraction of the tree's own span.
#'
#' Tile strips and heatmap panels both sit beyond the labels, and the x axis
#' has to be solved for all of them at once: `xlim()` is what stops the labels
#' clipping, and anything it does not know about is drawn outside the panel and
#' silently disappears. Leaving the tile strips out of this is precisely how a
#' mapped tile strip rendered as nothing at all.
#'
#' @param opts List. Resolved tree options.
#' @return Numeric fraction of the tree span.
#' @export
annotation_total <- function(opts) {
  want <- (tile_total(opts) + heatmap_total(opts)) * .annotation_squeeze(opts)
  if (want <= 0) {
    return(0)
  }
  # The lead is a gutter, not a column, so the squeeze leaves it alone: an
  # annotation run wide enough to be squeezed still has to clear the labels.
  want + ANNOTATION_LEAD + ANNOTATION_SLACK
}

#' Widths and offsets for the heatmap panels, in x-axis data units.
#'
#' `gheatmap`'s `width` is a multiple of the tree's own span and `offset` is in
#' the same units, so the panels have to be solved together: each starts where
#' the last one ended, and the run as a whole shares the annotation budget with
#' the tile strips so the tree stays the larger part of the picture.
#'
#' `offset` here is where the panel's *near edge* goes. gheatmap's own is not:
#' it centres column k at `offset + k * cell`, so the matrix it draws sits half
#' a column further out than it was asked for. The builder takes that half
#' column off again — it is the one place the real column count is known — so
#' every offset in this list means the same thing as `tile_centres`'.
#'
#' @param opts List. Resolved tree options.
#' @param tree_span Numeric. Width of the tree in x-axis units.
#' @param label_reserve Numeric. Room already given to the tip labels.
#' @return list(panels = <list>, total = <numeric fraction>).
#' @export
heatmap_panels <- function(opts, tree_span, label_reserve = 0) {
  hs <- opts$heatmaps %||% list()
  if (!length(hs)) {
    return(list(panels = list(), total = 0))
  }
  squeeze <- .annotation_squeeze(opts)

  # Each panel is as wide as it has columns. Sharing a fixed budget is what left
  # a 15-column matrix in a tenth of the tree's width once a tile strip had
  # taken the rest.
  n_cols <- vapply(hs, function(h) length(h$cols), integer(1))
  widths <- n_cols * .heat_span(opts) * squeeze
  gaps <- HEATMAP_GAP * squeeze

  # The first panel starts past the tip labels *and* past the tile strips.
  # gheatmap's offset is absolute from the tree's edge, unlike geom_fruit's,
  # which is relative to the annotation before it — so the tiles are invisible
  # to this calculation unless counted here. Not counting them is what drew the
  # heatmap straight over the tile strip.
  base <- label_reserve +
    (ANNOTATION_LEAD + tile_total(opts) * squeeze) * tree_span
  starts <- cumsum(c(0, head(widths, -1)))

  panels <- Map(
    function(h, w, s, i) {
      c(h, list(width = w, offset = base + (s + gaps * i) * tree_span))
    },
    hs,
    widths,
    starts,
    seq_along(hs)
  )
  list(panels = panels, total = sum(widths) + gaps * length(hs))
}

#' Fraction of the panel height to keep clear above the tree for the heatmap
#' column headers.
#'
#' The headers are set vertically, so a drug-class name is as tall as it is
#' long — and the y axis ends at the last tip, which clips anything drawn above
#' it. This is the vertical counterpart of the tip-label reserve: room measured
#' from the text that will actually go in it.
#'
#' Measured from the type size each header is *actually* set at, not from the
#' ceiling. A fifteen-column matrix fits about a millimetre of type per column,
#' and reserving for `HEADER_SIZE_MAX` there bought a band of empty page a
#' third the height of the plot — which also carried the legend, top-aligned to
#' the plot, that far up away from the tree.
#'
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @param tree_span Numeric. Width of the tree in x-axis units.
#' @param axis_units Numeric. Full width of the x axis, same units.
#' @param panel_in Numeric. Physical width of the panel, in inches.
#' @return Numeric multiplicative expansion for the top of the y scale.
#' @export
heatmap_header_frac <- function(
  opts,
  n_tip,
  tree_span = NULL,
  axis_units = NA_real_,
  panel_in = NULL,
  height_in = NULL
) {
  hs <- opts$heatmaps %||% list()
  tiles <- Filter(
    function(l) identical(l$aesthetic, "tile"),
    opts$layers %||% list()
  )
  if (!length(hs) && !length(tiles)) {
    return(0.02)
  }

  squeeze <- .annotation_squeeze(opts)
  width_in <- tree_axis_in(opts, panel_in %||% opts$width_in %||% 5.5)
  # Rows one header character claims, at the size that header is drawn. Without
  # a solved axis — a circular layout — the headers take the cap, so the
  # reserve does too.
  rows_per_char <- function(col_span) {
    size <- if (is.null(tree_span) || !is.finite(axis_units)) {
      HEADER_SIZE_MAX * .scale_of(opts)
    } else {
      tree_header_size(
        col_span * squeeze * tree_span,
        axis_units,
        width_in,
        .scale_of(opts)
      )
    }
    HEADER_CHAR_ROWS * size / (HEADER_SIZE_MAX * .scale_of(opts))
  }

  # Both kinds of annotation carry a vertical header, so both claim room — and
  # a tile strip's header is its variable's *name*, which is far longer than a
  # gene symbol, over a column several times as wide. Sizing this from the
  # heatmap alone is what clipped the strip headers off the top of the panel.
  heat_rows <- vapply(
    hs,
    function(h) {
      labs <- if (identical(h$level, "gene")) {
        h$labels %||% h$cols
      } else {
        field_labels_for(h$cols)
      }
      suppressWarnings(max(nchar(labs), 1L)) * rows_per_char(.heat_span(opts))
    },
    numeric(1)
  )

  tile_rows <- vapply(
    tiles,
    function(l) {
      suppressWarnings(max(nchar(l$title %||% l$field), 1L)) *
        rows_per_char(.tile_span(opts))
    },
    numeric(1)
  )

  rows <- suppressWarnings(max(c(heat_rows, tile_rows), 1))
  if (!is.finite(rows)) {
    rows <- 1
  }
  # Plus the gap the headers are held off the columns by, which is room above
  # the last tip just as much as the text is.
  rows <- rows + TILE_HEADER_OFFSET
  n <- max(as.integer(n_tip %||% 1L), 1L)
  # HEADER_CHAR_ROWS counts rows at TIP_ROW_IN, the pitch the fit *aims* for.
  # The aspect ratio is the user's to change, and a squatter plot has shorter
  # rows — so the same header needs more of them. Measured against the pitch
  # the plot is actually drawn at, which is the only one it will be read at:
  # without this, lowering the aspect clipped the column names off the top.
  if (!is.null(height_in) && is.finite(height_in) && height_in > 0) {
    rows <- rows * TIP_ROW_IN / (height_in / n)
  }
  .clamp(rows / n, 0.04, HEADER_FRAC_MAX)
}

#' The runs of columns one panel's drug classes occupy.
#'
#' Columns arrive grouped by class, so each class is one contiguous run — but
#' only the columns actually drawn count, and `.heatmap_frame()` may draw fewer
#' than the record names when the call matrix has moved on.
#'
#' @param panel One heatmap panel record.
#' @param drawn Character vector of the column labels the panel drew, in order.
#' @return Data frame with `class`, `from` and `to` (1-based column indices);
#'   zero rows when the panel carries no classes.
#' @export
heatmap_class_runs <- function(panel, drawn) {
  classes <- panel$classes
  labels <- panel$labels %||% panel$cols
  if (is.null(classes) || !length(classes) || !length(drawn)) {
    return(.empty_class_runs())
  }
  hit <- match(drawn, labels)
  cls <- classes[hit]
  # A column whose class is unknown breaks the run rather than joining it: the
  # alternative is a bracket that claims a grouping the data does not have.
  cls[is.na(cls) | !nzchar(trimws(cls))] <- NA_character_
  if (all(is.na(cls))) {
    return(.empty_class_runs())
  }
  # A sentinel no drug class can collide with, so runs either side of an
  # unknown column stay separate instead of being joined across it.
  gap <- "\u2400 none"
  r <- rle(ifelse(is.na(cls), gap, cls))
  ends <- cumsum(r$lengths)
  keep <- r$values != gap
  if (!any(keep)) {
    return(.empty_class_runs())
  }
  data.frame(
    class = r$values[keep],
    from = (ends - r$lengths + 1L)[keep],
    to = ends[keep],
    stringsAsFactors = FALSE
  )
}

.empty_class_runs <- function() {
  data.frame(
    class = character(0),
    from = integer(0),
    to = integer(0),
    stringsAsFactors = FALSE
  )
}

# --- How far a circular tree has to open --------------------------------------

# Most of the circle the wedge may take. Past a quarter turn the tree is a
# horseshoe, and the headers would be better set somewhere else entirely.
OPEN_ANGLE_MAX <- 90
# Slack on the solved wedge, so a header sits *in* the gap rather than exactly
# filling it.
OPEN_ANGLE_PAD <- 1.25

#' Degrees of the circle a radial tree has to leave open for its headers.
#'
#' Every ring's header is set in the wedge between the last tip and the first,
#' reading across it — so what has to fit there is the header's *length*, as an
#' arc at the radius its own ring sits at. An inner ring's is the tight one:
#' the same words over a shorter arc.
#'
#' With no wedge at all ggtree draws them all at one angle, on top of each
#' other, which is what a circular tree with a tile strip and a heatmap came
#' out looking like.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame. Per-tip metadata.
#' @param panel_in Numeric. Width of the finished panel, in inches.
#' @return Degrees, 0 when nothing needs the room.
#' @export
tree_open_angle <- function(opts, md, panel_in = NULL) {
  if (!.is_circular(opts)) {
    return(0)
  }
  heat <- annotation_total(opts)
  if (!isTRUE(heat > 0)) {
    return(0)
  }
  f <- .tiplab_budget_frac(opts, md)
  # The axis, and everything on it, in multiples of the tree's own span.
  axis <- (1 + heat) / (1 - f)
  label_frac <- f * axis
  axis_in <- tree_axis_in(opts, panel_in %||% opts$width_in %||% 5.5)
  squeeze <- .annotation_squeeze(opts)

  # One entry per header: how long it is, against how far out it sits.
  want <- function(chars, col_span, radius) {
    size <- tree_header_size(col_span, axis, axis_in, .scale_of(opts))
    arc <- max(chars, 1L) * TIP_CHAR_EM * size / 25.4
    r_in <- axis_in * radius / axis
    if (!is.finite(r_in) || r_in <= 0) 0 else arc / r_in
  }

  tiles <- Filter(
    function(l) identical(l$aesthetic, "tile"),
    opts$layers %||% list()
  )
  frac <- .tile_span(opts) * squeeze
  gap <- TILE_GAP * squeeze
  needed <- numeric(0)
  for (i in seq_along(tiles)) {
    centre <- 1 +
      label_frac +
      ANNOTATION_LEAD +
      gap +
      frac / 2 +
      (i - 1L) * (frac + gap)
    needed <- c(
      needed,
      want(
        nchar(tiles[[i]]$title %||% tiles[[i]]$field),
        frac,
        centre
      )
    )
  }

  # A panel's columns share a header row, and its innermost column has the
  # shortest arc — so that is where the whole row has to fit.
  cell <- .heat_span(opts) * squeeze
  hs <- Filter(function(h) length(h$cols) > 0L, opts$heatmaps %||% list())
  start <- 1 + label_frac + ANNOTATION_LEAD + tile_total(opts) * squeeze
  for (h in hs) {
    start <- start + HEATMAP_GAP * squeeze
    labs <- h$labels %||% h$cols
    needed <- c(
      needed,
      want(
        suppressWarnings(max(nchar(labs), 1L)),
        cell,
        start + cell / 2
      )
    )
    start <- start + length(h$cols) * cell
  }

  if (!length(needed)) {
    return(0)
  }
  .clamp(round(max(needed) * OPEN_ANGLE_PAD * 180 / pi), 0, OPEN_ANGLE_MAX)
}

#' Fraction of the panel to keep clear below the tree for the class band.
#'
#' The vertical counterpart of `heatmap_header_frac()`, and measured the same
#' way: from the longest name that will actually go in it, at the size it will
#' be set at.
#'
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @param runs List of class-run frames, one per drawn panel.
#' @param size Numeric. Type size the class names are set at.
#' @return Numeric multiplicative expansion for the bottom of the y scale.
#' @export
heatmap_class_frac <- function(opts, n_tip, runs = list(), size = NULL) {
  runs <- Filter(function(r) nrow(r) > 0L, runs %||% list())
  if (!length(runs)) {
    return(0.02)
  }
  chars <- suppressWarnings(max(
    vapply(
      runs,
      function(r) suppressWarnings(max(nchar(r$class), 1L)),
      numeric(1)
    ),
    1
  ))
  if (!is.finite(chars)) {
    chars <- 1
  }
  rows_per_char <- HEADER_CHAR_ROWS *
    (size %||% HEADER_SIZE_MAX) /
    HEADER_SIZE_MAX
  band <- CLASS_GAP_ROWS +
    CLASS_TICK_ROWS +
    CLASS_LABEL_GAP_ROWS +
    chars * rows_per_char
  n <- max(as.integer(n_tip %||% 1L), 1L)
  .clamp(band / n, 0.02, CLASS_FRAC_MAX)
}

# The bracket-and-name layers for one panel's class runs.
#
# `centres` are the drawn columns' x positions, so a run's bracket spans from
# the first column's centre to the last's — widened by half a column either
# side so it reads as covering them rather than as joining them.
.heatmap_class_layers <- function(runs, centres, cell, size, colour) {
  if (!nrow(runs) || !length(centres)) {
    return(NULL)
  }
  half <- cell * CLASS_BRACKET_FILL / 2
  x0 <- centres[runs$from] - half
  x1 <- centres[runs$to] + half
  y_bar <- 0.5 - CLASS_GAP_ROWS
  y_tick <- y_bar + CLASS_TICK_ROWS
  y_lab <- y_bar - CLASS_LABEL_GAP_ROWS

  bar <- data.frame(x = x0, xend = x1, y = y_bar, yend = y_bar)
  ticks <- data.frame(
    x = c(x0, x1),
    xend = c(x0, x1),
    y = y_bar,
    yend = y_tick
  )
  labs <- data.frame(
    x = (x0 + x1) / 2,
    y = y_lab,
    label = runs$class,
    stringsAsFactors = FALSE
  )

  seg <- function(d, width = 0.3) {
    geom_segment(
      data = d,
      mapping = aes(
        x = .data[["x"]],
        xend = .data[["xend"]],
        y = .data[["y"]],
        yend = .data[["yend"]]
      ),
      inherit.aes = FALSE,
      colour = colour,
      linewidth = width
    )
  }
  out <- list(
    seg(bar),
    seg(ticks),
    geom_text(
      data = labs,
      mapping = aes(
        x = .data[["x"]],
        y = .data[["y"]],
        label = .data[["label"]]
      ),
      inherit.aes = FALSE,
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = size,
      colour = colour
    )
  )
  out
}

#' Type size for an annotation header, fitted to the column it sits over.
#'
#' @param col_units Numeric. Width of one column, in x-axis data units.
#' @param axis_units Numeric. Full width of the x axis, same units.
#' @param panel_in Numeric. Physical width of the panel, in inches.
#' @return Numeric ggplot2 text size.
#' @export
tree_header_size <- function(col_units, axis_units, panel_in, scale = 1) {
  lo <- HEADER_SIZE_MIN * scale
  hi <- HEADER_SIZE_MAX * scale
  if (!is.finite(col_units) || !is.finite(axis_units) || axis_units <= 0) {
    return(lo)
  }
  col_mm <- 25.4 * panel_in * col_units / axis_units
  .clamp(col_mm * HEADER_FILL, lo, hi)
}

# The frame one panel draws, with its rows keyed the way gheatmap matches them.
#
# At gene level the source is the separate call matrix
# (database_functions$load_amr_matrix), which is already a factor of
# Match/Inexact/Partial per gene; absence is simply the NA it leaves behind.
# This is what the tree's own panels draw — both of them, resistance and
# virulence/stress, which differ only in which genes they carry.
#
# The other branch takes a set of metadata columns and reads them as
# presence/absence, one colour for "there is something here" and one for
# "there is not". A shared categorical scale over the raw values would give one
# colour per distinct *combination* of genes instead — dozens of them, none
# comparable. The tree no longer offers this: a drug-class column is positive
# exactly when one of its gene columns is, so a class panel beside a gene panel
# said the same thing twice. It stays because it is the renderer's general
# answer to "draw these columns as a matrix", not because a control produces
# it.
.heatmap_frame <- function(panel, md, amr_matrix = NULL) {
  if (identical(panel$level, "gene")) {
    if (is.null(amr_matrix)) {
      return(NULL)
    }
    cols <- intersect(panel$cols, names(amr_matrix))
    if (!length(cols)) {
      return(NULL)
    }
    idx <- match(md$isolate, amr_matrix$isolate)
    heat <- amr_matrix[idx, cols, drop = FALSE]
    heat[] <- lapply(heat, function(v) {
      v <- as.character(v)
      v[is.na(v)] <- AMR_ABSENT
      factor(v, levels = AMR_GENE_STATES)
    })
    names(heat) <- panel$labels %||% cols
    rownames(heat) <- md$label
    return(heat)
  }

  cols <- intersect(panel$cols, names(md))
  if (!length(cols)) {
    return(NULL)
  }
  heat <- md[, cols, drop = FALSE]
  heat[] <- lapply(heat, function(v) {
    present <- !is.na(v) & nzchar(trimws(as.character(v)))
    factor(
      ifelse(present, AMR_PRESENT, AMR_ABSENT),
      levels = c(AMR_PRESENT, AMR_ABSENT)
    )
  })
  names(heat) <- field_labels_for(cols)
  rownames(heat) <- md$label
  heat
}

# Solves x-axis plot range ensuring tip labels and heatmaps fit without clipping
.tiplab_xlim <- function(opts, md, tree_data, max_x, heat = 0) {
  frac <- .tiplab_axis_frac(opts, md, heat)
  x_min <- suppressWarnings(min(tree_data$x, na.rm = TRUE))
  if (!is.finite(x_min)) {
    x_min <- 0
  }
  if (isTRUE(opts$rootedge_show)) {
    x_min <- x_min - max_x * 0.05
  }
  span <- (max_x - x_min) * (1 + heat)
  range <- span / (1 - frac)
  list(
    limit = x_min + range,
    reserve = range * frac
  )
}

#' Prepare Tip Metadata Dataframe for ggtree
#'
#' @param tree phylo Object.
#' @param metadata data.frame containing metadata with an `isolate` column.
#' @return Structured data.frame with key matching tip labels.
#' @export
tree_tip_metadata <- function(tree, metadata) {
  data.frame(
    label = metadata$isolate,
    metadata,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Discrete colours from a ColorBrewer palette, at any level count.
#'
#' RColorBrewer's palettes are tabulated, not generated: Set1 and Pastel1 stop
#' at 9 entries and Set2/Dark2/Accent at 8. Asked for more, `brewer.pal()`
#' warns ("n too large, allowed maximum for palette Set1 is 9") and returns a
#' short vector, and ggplot2 draws every level past the end in grey — which is
#' what took the colour off 37 of this database's 46 countries and made the
#' plot look broken.
#'
#' At or below capacity this is the tabulated palette unchanged, so nothing
#' that worked before changes. Above it, the palette is interpolated through
#' its own hues instead of truncated.
#'
#' @param palette Name of a ColorBrewer palette.
#' @param n Integer. Number of levels to colour.
#' @return Character vector of `n` hex colours.
#' @export
tree_discrete_colors <- function(palette, n) {
  n <- max(as.integer(n), 1L)
  info <- brewer.pal.info
  if (!palette %in% rownames(info)) {
    return(colorRampPalette(brewer.pal(8, "Dark2"))(n))
  }
  max_n <- info[palette, "maxcolors"]
  # brewer.pal() rejects n < 3 outright, so always ask for at least that.
  base <- brewer.pal(min(max(n, 3L), max_n), palette)
  if (n <= max_n) base[seq_len(n)] else colorRampPalette(base)(n)
}

#' Label and colour for values the database does not have.
#'
#' A blank string and NA mean the same thing to a reader — nobody recorded it —
#' but to ggplot2 `""` is an ordinary level, which is why an unlabelled swatch
#' appeared in the legend beside the real categories. Naming the case makes the
#' plot say what it means, and pinning it to a neutral grey keeps it from
#' competing with the values that do carry information.
#' @export
MISSING_LABEL <- "Not recorded"

# Characters a legend key label may run to before it wraps onto another line.
#
# ggplot2 sizes the guide box from its widest label, and nothing caps it — so
# "Antimicrobial resistance surveillance" alone claimed nearly half a 5.5in
# canvas, twice over for two mappings, and left the tree a hairline. Wrapping
# bounds the width without truncating: the reader still gets the whole category
# name, on two lines instead of one. It also makes tree_legend_width_in()'s
# estimate an upper bound it can actually rely on.
#' @export
LEGEND_LABEL_WRAP <- 22L

# Wrap legend key labels at LEGEND_LABEL_WRAP characters.
.wrap_legend_labels <- function(x) {
  vapply(
    as.character(x),
    function(s) {
      if (is.na(s)) {
        return(MISSING_LABEL)
      }
      paste(strwrap(s, width = LEGEND_LABEL_WRAP), collapse = "\n")
    },
    character(1),
    USE.NAMES = FALSE
  )
}
# Steps the generated viridis ramp is built from. ggplot2's own continuous
# viridis scale uses the same resolution; past this a gradient is smooth to the
# eye and the extra colours are only work.
GRADIENT_STEPS <- 256L

#' A mid grey, not a pale one: this colour has to work as tip-label *text* as
#' well as a heatmap swatch, and anything lighter reads as invisible rather than
#' as de-emphasised.
#' @export
MISSING_COLOR <- "#9E9E9E"

# The shape reserved for the missing level, outside TREE_SHAPES so a mapping of
# six real values plus "not recorded" still has somewhere to put it.
TREE_MISSING_SHAPE <- 4

# A plain `sort()` on text orders "1, 10, 11, ..., 2, 20" — correct for names,
# wrong for a category whose levels happen to be numbers (patient or ward
# IDs). Levels that parse as a number are ordered by value and come first
# (a ward numbered 1-25 alongside a literal "ER" is the case this is for);
# whatever is left is ordered lexically after them.
.level_order <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  is_num <- !is.na(num)
  c(x[is_num][order(num[is_num])], sort(x[!is_num]))
}

#' Normalise a mapped column: one explicit level for "not recorded", last.
#'
#' Also the fix for a crash. `field_levels()` counts distinct values *excluding*
#' blanks, but the scale it sized was handed the raw column, where `""` counts —
#' so a 7-category variable with some blanks asked a 7-colour scale to cover 8
#' levels and errored with "Insufficient values in manual scale". Levels are
#' decided here, once, and the scale is built from them.
#' @export
mapped_values <- function(v) {
  if (is.numeric(v) || inherits(v, "Date")) {
    return(v)
  }
  # Already normalised. `.normalize_mapped_columns()` writes its result back
  # into `md` and the scales are built from the frame afterwards, so this runs
  # twice over the same column; a second sort would file "Not recorded" among
  # the real values and leave the legend disagreeing with the strip.
  if (is.factor(v) && !anyNA(v) && all(nzchar(trimws(levels(v))))) {
    return(v)
  }
  ch <- trimws(as.character(v))
  ch[!nzchar(ch)] <- NA_character_
  present <- .level_order(unique(ch[!is.na(ch)]))
  if (!anyNA(ch)) {
    return(factor(ch, levels = present))
  }
  ch[is.na(ch)] <- MISSING_LABEL
  factor(ch, levels = c(present, MISSING_LABEL))
}

#' Colours for a discrete scale's levels, as a named vector.
#'
#' Built explicitly rather than left to `scale_*_viridis_d`/`_brewer` because
#' the missing level has to be pinned to grey wherever it appears, and only a
#' manual scale can say which level gets which colour.
#' @export
tree_level_colors <- function(levels, palette) {
  real <- setdiff(levels, MISSING_LABEL)
  n <- max(length(real), 1L)
  cols <- if (is.null(palette) || palette %in% .viridis_scales) {
    viridis(n, option = if (is.null(palette)) "viridis" else palette)
  } else {
    tree_discrete_colors(palette, n)
  }
  out <- setNames(cols[seq_along(real)], real)
  if (MISSING_LABEL %in% levels) {
    out[[MISSING_LABEL]] <- MISSING_COLOR
  }
  out
}

tree_scale <- function(
  values,
  palette,
  aesthetic,
  name = NULL,
  max_rows = LEGEND_MAX_ROWS
) {
  numeric <- is.numeric(values) || inherits(values, "Date")
  viridis_pal <- is.null(palette) || palette %in% .viridis_scales
  opt <- if (is.null(palette) || !viridis_pal) "viridis" else palette

  if (numeric) {
    # A Date reaches a continuous scale as days since the epoch, so a scale not
    # told what it is holding labels its keys 17250, 18000, 18500 — which is
    # what a collection date left at "Exact date" drew, and a reader cannot
    # guess their way back to a calendar from that. `transform = "date"` is
    # what turns them into dates; `scale_*_viridis_c()` has nowhere to take it,
    # so the viridis ramp is laid out by hand through gradientn instead.
    transform <- if (inherits(values, "Date")) "date" else "identity"
    fill <- identical(aesthetic, "fill")
    return(
      if (viridis_pal) {
        ramp <- viridis(GRADIENT_STEPS, option = opt)
        if (fill) {
          scale_fill_gradientn(
            colours = ramp,
            transform = transform,
            name = name,
            na.value = MISSING_COLOR
          )
        } else {
          scale_color_gradientn(
            colours = ramp,
            transform = transform,
            name = name,
            na.value = MISSING_COLOR
          )
        }
      } else if (fill) {
        scale_fill_distiller(
          palette = palette,
          transform = transform,
          name = name,
          na.value = MISSING_COLOR
        )
      } else {
        scale_color_distiller(
          palette = palette,
          transform = transform,
          name = name,
          na.value = MISSING_COLOR
        )
      }
    )
  }

  values <- mapped_values(values)
  cols <- tree_level_colors(levels(values), palette)
  keys <- tree_legend_breaks(names(cols))
  guide <- guide_legend(
    ncol = tree_legend_ncol(length(keys$breaks), max_rows)
  )
  title <- tree_legend_title(name, keys$hidden)

  if (identical(aesthetic, "fill")) {
    scale_fill_manual(
      values = cols,
      breaks = keys$breaks,
      guide = guide,
      name = title,
      labels = .wrap_legend_labels,
      na.value = MISSING_COLOR
    )
  } else {
    scale_color_manual(
      values = cols,
      breaks = keys$breaks,
      guide = guide,
      name = title,
      labels = .wrap_legend_labels,
      na.value = MISSING_COLOR
    )
  }
}

#' The mapping layer driving one aesthetic, or NULL.
#'
#' @param opts List. Resolved tree options.
#' @param aesthetic Character. Aesthetic name.
#' @return A layer record, or NULL.
#' @export
layer_for <- function(opts, aesthetic) {
  hit <- Filter(
    function(l) identical(l$aesthetic, aesthetic),
    opts$layers %||% list()
  )
  if (length(hit)) hit[[1]] else NULL
}

# The column a layer maps, parsed if its declared type says so. A date arrives
# from SQLite as character, and a discrete scale over 300 distinct dates is 300
# unordered colours — this is the one place a declared type changes the draw.
# With a granularity set, the date comes back as an ordered factor of interval
# labels instead, which every discrete path below then handles unchanged.
#
# Takes the *raw* column, never the frame: the result is written back into `md`,
# and re-running it over its own output asked `as.Date()` to parse "2021" — NA
# for every tip, a scale with nothing but the missing level in it, and so a
# grouped collection date drawn as one grey strip with no legend at all.
.layer_values <- function(layer, values) {
  if (!identical(layer$transform, "as_date")) {
    return(mapped_values(values))
  }
  # Through mapped_values() as well, so a date with gaps in it names its
  # missing level the same way every other variable does.
  mapped_values(bin_date_values(values, layer$granularity))
}

# The mapped column as the plot must see it. The aes() references md[[field]]
# directly, so the normalisation has to be written back into the frame — a
# scale built from normalised levels over raw data is exactly the mismatch that
# errored.
.normalize_mapped_columns <- function(opts, md) {
  fields <- unique(vapply(
    opts$layers %||% list(),
    function(l) l$field,
    character(1)
  ))
  for (f in fields) {
    layer <- layer_for_field(opts, f)
    md[[f]] <- .layer_values(layer, md[[f]])
  }
  md
}

layer_for_field <- function(opts, field) {
  hit <- Filter(function(l) identical(l$field, field), opts$layers %||% list())
  if (length(hit)) hit[[1]] else NULL
}

tree_tiplab_layer <- function(opts, layer = NULL) {
  # Labels off still draws the leader lines. They are what makes a tree with
  # ragged tip depths readable without labels: without them the eye has to
  # carry a row across an empty band to whatever is annotated beside it. The
  # label itself becomes a single space — an empty string makes ggtree drop the
  # layer, and with it the lines.
  hidden <- !isTRUE(opts$tiplab_show)
  if (hidden && identical(opts$layout, "inward")) {
    # Inward leader lines all converge on the root, so there is nothing here
    # worth drawing without labels to anchor them.
    return(NULL)
  }

  mapped <- !is.null(layer) && !hidden
  mapping <- if (mapped) {
    aes(label = .data[[opts$tiplab]], color = .data[[layer$field]])
  } else if (hidden) {
    aes(label = " ")
  } else {
    aes(label = .data[[opts$tiplab]])
  }

  inward <- identical(opts$layout, "inward")

  params <- list(
    mapping = mapping,
    size = opts$tiplab_size,
    # Aligning draws a leader line from each tip out to the axis limit. In an
    # inward tree every one of those runs toward the centre, where they all
    # converge into a solid blot over the root — so the layout that makes a
    # linear tree readable is the one that ruins this one.
    align = !inward,
    geom = "text"
  )

  # An inward tree grows from the outside in, so its x axis is reversed and a
  # label left-anchored at its tip would run off the canvas instead of toward
  # the centre. Right-anchoring is the layout's own convention.
  if (inward) {
    params$hjust <- 1
  }

  # Fixed color is assigned only when no aesthetic color mapping is active
  if (!mapped) {
    params$color <- opts$tiplab_color
  }

  do.call(geom_tiplab, params)
}

#' Whether a mapping's aesthetic is drawn at all on this plot.
#'
#' A scale with no geom behind it is not harmless: ggplot2 finds no data values
#' matching its keys and says so ("No shared levels found ..."), on every draw,
#' for every such mapping. Tip labels switched off with a tip-label colour
#' mapping still on is the ordinary way to get there.
#'
#' @param opts List. Resolved tree options.
#' @param aesthetic Character. Aesthetic name, or NULL.
#' @return TRUE when the layer that carries it is drawn.
#' @export
tree_aesthetic_drawn <- function(opts, aesthetic) {
  if (is.null(aesthetic)) {
    return(FALSE)
  }
  if (identical(aesthetic, "tiplab_color")) {
    return(isTRUE(opts$tiplab_show))
  }
  if (aesthetic %in% c("tippoint_color", "tippoint_shape")) {
    return(isTRUE(opts$tippoint_show))
  }
  TRUE
}

#' Allelic distances written on the branches that can hold them
#'
#' The selection is made here rather than by a `subset` inside the aesthetic,
#' because it is geometry (see `tree_branch_keep()`) and needs the axis split
#' the caller has already solved. What survives is drawn from its own data
#' frame, so a branch that was not chosen contributes nothing to the layer at
#' all.
#'
#' @param opts List. Resolved tree options.
#' @param tree_data Data frame. `ggtree()`'s plot data.
#' @param span_x Numeric. The tree's x span, in tree units.
#' @param span_in Numeric. Inches that span is drawn across.
#' @return A ggplot2 layer, or NULL when nothing can be labelled legibly.
tree_branch_layer <- function(opts, tree_data, span_x, span_in) {
  if (!isTRUE(opts$branch_show)) {
    return(NULL)
  }

  len <- tree_data$branch.length
  if (is.null(len) || !any(is.finite(len) & len > 0)) {
    return(NULL)
  }

  size <- opts$branch_size * BRANCH_ABOVE_SHRINK
  digits <- tree_branch_digits(len)
  keep <- tree_branch_keep(
    len,
    tree_data$y,
    span_x,
    span_in,
    size,
    digits
  )
  if (!length(keep)) {
    return(NULL)
  }

  # `branch` is the midpoint of the branch, which is where the number goes.
  labels <- data.frame(
    x = tree_data$branch[keep],
    y = tree_data$y[keep],
    label = tree_branch_format(len[keep], digits),
    stringsAsFactors = FALSE
  )

  geom_text(
    data = labels,
    mapping = aes(
      x = .data[["x"]],
      y = .data[["y"]],
      label = .data[["label"]]
    ),
    inherit.aes = FALSE,
    size = size,
    vjust = BRANCH_VJUST,
    color = opts$branch_color
  )
}

tree_tippoint_layer <- function(opts, color_layer = NULL, shape_layer = NULL) {
  if (!isTRUE(opts$tippoint_show)) {
    return(NULL)
  }

  aes_list <- list()
  if (!is.null(color_layer)) {
    aes_list$color <- as.name(color_layer$field)
  }
  if (!is.null(shape_layer)) {
    aes_list$shape <- as.name(shape_layer$field)
  }

  params <- list(
    alpha = opts$tippoint_alpha,
    size = opts$tippoint_size
  )
  if (length(aes_list)) {
    params$mapping <- do.call(aes, aes_list)
  }
  if (is.null(aes_list$color)) {
    params$color <- opts$tippoint_color
  }
  if (is.null(aes_list$shape)) {
    params$shape <- opts$tippoint_shape
  }

  do.call(geom_tippoint, params)
}

tree_clade_layers <- function(opts) {
  nodes <- suppressWarnings(as.integer(opts$parentnodes))
  nodes <- nodes[!is.na(nodes)]
  if (!length(nodes)) {
    return(NULL)
  }
  lapply(nodes, function(n) {
    geom_hilight(node = n, fill = opts$clade_color)
  })
}

TILE_GAP <- 0.012

# Where an annotation header sits, in tip rows.
#
# Measured from the *edge* of the column, not from the last tip: a tile is a
# row tall and centred on its tip, so the strip reaches half a row past the
# last one. Anchoring at the tip put every header inside its own top tile.
TILE_TOP_ROWS <- 0.5
TILE_HEADER_GAP <- 0.35
TILE_HEADER_OFFSET <- TILE_TOP_ROWS + TILE_HEADER_GAP

# gheatmap anchors its column names at `max(y) + 1` and nudges from there,
# while a tile strip's header is placed at an absolute y — so the two agree
# only if the nudge cancels that row out. Left uncancelled, the gene names sat
# a full tip row above the strip headers beside them.
GHEATMAP_NAME_BASE <- 1

#' Where each tile strip's centre sits, in x-axis data units.
#'
#' The same arithmetic geom_fruit is given below, run again so the headers can
#' be placed over the strips they name. geom_fruit computes its own positions
#' internally and reports none of them, so a header has no other way to find its
#' strip.
#'
#' @param opts List. Resolved tree options.
#' @param n Integer. Number of strips.
#' @param label_frac Numeric. Tip-label reserve, as a fraction of the tree span.
#' @param max_x Numeric. Deepest tip position.
#' @param tree_span Numeric. Width of the tree in x-axis units.
#' @return Numeric vector of centres, one per strip.
#' @export
tile_centres <- function(opts, n, label_frac, max_x, tree_span) {
  if (!n) {
    return(numeric(0))
  }
  squeeze <- .annotation_squeeze(opts)
  frac <- .tile_span(opts) * squeeze
  gap <- TILE_GAP * squeeze
  first <- label_frac + ANNOTATION_LEAD + gap + frac / 2
  max_x + (first + (seq_len(n) - 1L) * (frac + gap)) * tree_span
}

# Absolute y an annotation header is drawn at: past the far edge of the column
# run on a linear tree, and past the near edge on a radial one, where the
# strips end against the leading edge of the wedge.
.header_y <- function(opts, n_tip) {
  if (.is_circular(opts)) {
    1 - TILE_HEADER_OFFSET
  } else {
    n_tip + TILE_HEADER_OFFSET
  }
}

# The same y, as the nudge gheatmap needs to reach it from `max(y) + 1`.
.heatmap_name_offset <- function(opts, n_tip) {
  .header_y(opts, n_tip) - (n_tip + GHEATMAP_NAME_BASE)
}

tree_tile_layers <- function(
  opts,
  md,
  tiles = NULL,
  label_frac = 0,
  tree_span = 1,
  max_x = NULL,
  n_tip = NULL,
  axis_units = NULL,
  panel_in = NULL,
  legend_max_rows = LEGEND_MAX_ROWS
) {
  axis_in <- tree_axis_in(opts, panel_in %||% opts$width_in %||% 5.5)
  if (is.null(tiles)) {
    tiles <- Filter(
      function(l) identical(l$aesthetic, "tile"),
      opts$layers %||% list()
    )
  }
  if (!length(tiles)) {
    return(NULL)
  }
  # Each strip gets the same width whatever the count — they do not compete for
  # a budget, the canvas grows for them instead (see annotation_total). Squeezed
  # only once the whole annotation run would dwarf the tree.
  #
  # geom_fruit measures `offset` from the previous annotation, so only the first
  # strip has to clear the tip labels — `label_frac` is that reserve as a
  # fraction of the tree's span. Without it the first strip starts at the tree's
  # own edge, directly over the labels.
  squeeze <- .annotation_squeeze(opts)
  frac <- .tile_span(opts) * squeeze
  gap <- TILE_GAP * squeeze

  # `pwidth` is documented as a fraction of the tree width, but for a
  # single-column tile strip geom_fruit says so itself — "the `pwidth` will be
  # as `width`" — and uses it as the tile's width in *data units*. On a cgMLST
  # tree whose span runs into the hundreds, a pwidth of 0.45 is a hairline,
  # which is exactly how the strip came out. Scaling by the span is what makes
  # the requested fraction the fraction actually drawn.
  pwidth <- frac * tree_span

  # `offset` *is* fractional, and measures to the strip's centre rather than its
  # near edge — hence the half-width in the first strip's clearance.
  layers <- list()
  for (i in seq_along(tiles)) {
    tile <- tiles[[i]]
    layers <- c(
      layers,
      list(
        new_scale_fill(),
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = .data[[tile$field]]),
          alpha = 1,
          pwidth = pwidth,
          offset = if (i == 1L) {
            label_frac + ANNOTATION_LEAD + gap + frac / 2
          } else {
            gap + frac
          }
        ),
        tree_scale(
          md[[tile$field]],
          tile$palette,
          "fill",
          name = tile$title,
          max_rows = legend_max_rows
        )
      )
    )
  }

  # A header over each strip. Without one, several strips side by side are a
  # block of colour with nothing saying which variable is which — the legends
  # name the values, not the columns. Drawn the same way the heatmap's column
  # names are: vertical, above the last tip, in the space the header reserve
  # keeps clear.
  if (!is.null(max_x) && !is.null(n_tip) && n_tip > 0) {
    # A linear tree has one edge to hang these off: past where the strips end.
    # A radial one has two, because the gap is a wedge — and the strips end
    # against its *leading* edge, the straight radial cut a header can be
    # aligned to. Set against the other edge they floated in the middle of the
    # opening with nothing to line up with, which is how they read.
    header_y <- .header_y(opts, n_tip)
    centres <- tile_centres(opts, length(tiles), label_frac, max_x, tree_span)
    header_size <- if (is.null(axis_units)) {
      HEADER_SIZE_MAX * .scale_of(opts)
    } else {
      tree_header_size(pwidth, axis_units, axis_in, .scale_of(opts))
    }
    layers <- c(
      layers,
      list(
        geom_text(
          data = data.frame(
            .x = centres,
            .y = header_y,
            .label = vapply(
              tiles,
              function(t) t$title %||% t$field,
              character(1)
            ),
            stringsAsFactors = FALSE
          ),
          mapping = aes(
            x = .data[[".x"]],
            y = .data[[".y"]],
            label = .data[[".label"]]
          ),
          inherit.aes = FALSE,
          angle = 90,
          hjust = if (.is_circular(opts)) 1 else 0,
          vjust = 0.5,
          size = header_size,
          colour = opts$line_color %||% "#000000"
        )
      )
    )
  }
  layers
}

# Warnings the plot emits about itself that say nothing about the plot.
#
# The "Removed N rows" one is the subtle member: `gheatmap()` builds the plot
# it is given in order to find its extent, and a second panel does that while
# the first panel's tiles are momentarily outside a not-yet-widened x scale.
# The finished plot keeps every row — asserted by the two-panel test in
# test-tree_plot.R, which checks the built layer data rather than trusting
# this — so the warning describes an intermediate that is never drawn.
# Messages gheatmap emits about an intermediate it builds and discards.
#
# It re-derives a tree from the plot's own data to place its columns, and that
# frame is not a valid `phylo` — it carries the plot's columns rather than an
# edge matrix — so tidytree says so, twice per panel, in three different ways.
# The finished matrix is correct (the column tests in test-tree_plot.R assert
# every cell of it), so this is the library talking to itself.
.muffled_tree_messages <- paste(
  "Invaild edge matrix",
  "invalid tbl_tree object",
  sep = "|"
)

.muffled_tree_warnings <- paste(
  "size.*aesthetic for lines",
  "linewidth",
  "label\\.size",
  "Removed \\d+ rows containing missing values",
  "one unique value with `geom = geom_tile`",
  sep = "|"
)

#' Build Tree Graphic (Warning Muffled Wrapper)
#'
#' @param tree phylo Object.
#' @param metadata data.frame Metadata table.
#' @param opts List of rendering control parameters.
#' @return Rendered ggplot/ggdraw plot object.
#' @export
build_tree_ggtree <- function(tree, metadata, opts) {
  withCallingHandlers(
    .build_tree_ggtree(tree, metadata, opts),
    warning = function(w) {
      if (grepl(.muffled_tree_warnings, conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    },
    message = function(m) {
      if (grepl(.muffled_tree_messages, conditionMessage(m))) {
        invokeRestart("muffleMessage")
      }
    }
  )
}

.build_tree_ggtree <- function(tree, metadata, opts) {
  # Attach ggplot2 namespace if unattached (required for ggtreeExtra::geom_fruit)
  if (!"package:ggplot2" %in% search()) {
    base::attachNamespace("ggplot2")
  }

  # Neighbour-joining estimates branch lengths independently of the topology it
  # builds, so some come out negative — six in a few hundred is ordinary. They
  # are an artefact of the estimator rather than a distance anyone measured,
  # and drawn literally they run *backwards* along their own radius: the
  # crossing, doubled-back segments that make a radial NJ tree look broken.
  # Treating them as zero is what ape's own documentation and ggtree's
  # `ignore.negative.edge` both do.
  if (!is.null(tree$edge.length)) {
    tree$edge.length[tree$edge.length < 0] <- 0
  }

  if (!is.null(opts$root) && !identical(opts$root, "Automatic")) {
    og <- which(metadata$isolate == opts$root)
    if (length(og)) {
      tree <- root(tree, outgroup = og, resolve.root = TRUE)
    }
  }

  md <- tree_tip_metadata(tree, metadata)

  # Validate selections against current metadata columns
  cols <- names(md)
  valid <- function(field) !is.null(field) && field %in% cols
  if (!valid(opts$tiplab)) {
    opts$tiplab <- "isolate"
  }
  # A saved Analysis can outlive the column it mapped, so a layer naming a
  # column this database no longer has is dropped rather than allowed to reach
  # aes() and error.
  opts$layers <- Filter(function(l) valid(l$field), opts$layers %||% list())
  # Gene-level panels draw from the call matrix, not from the metadata table, so
  # only the drug-class ones are validated against its columns.
  opts$heatmaps <- Filter(
    function(h) length(h$cols) > 0L,
    lapply(opts$heatmaps %||% list(), function(h) {
      if (!identical(h$level, "gene")) {
        h$cols <- intersect(h$cols, cols)
      }
      h
    })
  )

  # Mapped columns are normalised in place, so every scale is built from the
  # same levels the geoms will actually draw.
  md <- .normalize_mapped_columns(opts, md)

  # What one annotation column is worth in tree spans, solved once against this
  # plot's own label reserve. Every width below reads the answer off `opts`.
  opts <- resolve_annotation_widths(opts, md)
  # Inches the whole panel spans. `opts$width_in` is only the tree-and-labels
  # budget — the canvas grows past it for the annotations — so it is the wrong
  # width to fit a header to. Fitting to it is what drew a thirty-column matrix
  # with headers at the minimum size on a panel nearly twice as wide.
  panel_in <- tree_panel_width_in(opts, md, opts$width_in %||% 5.5)
  # What that panel is worth along the x axis: its width for a linear tree, its
  # radius for a circular one.
  axis_in <- tree_axis_in(opts, panel_in)
  # Every physical length this build draws is multiplied by it (see .scale_of).
  scale <- .scale_of(opts)
  # How tall this plot is drawn. It decides two things nothing else can: how
  # many rows a header of a given height occupies, and whether the guides fit
  # in one column. A circular panel is square and grows with its rings, so its
  # height is the *panel* — not the tree-and-labels budget times an aspect
  # ratio that layout does not use.
  plot_height_in <- if (opts$layout %in% .circular_layouts) {
    panel_in
  } else {
    (opts$width_in %||% 5.5) * (opts$aspect %||% 1)
  }
  # How tall any one guide may run before its keys wrap into another column.
  legend_max_rows <- tree_legend_max_rows(
    opts$layers,
    opts$heatmaps,
    opts$legend_size,
    plot_height_in,
    scale
  )

  circular <- opts$layout %in% .circular_layouts
  label_reserve <- 0
  # ggtree's own name for the inward-facing radial layout is "inward_circular".
  # This used to translate it to plain "circular", which silently drew the
  # ordinary outward tree instead — picking Inward changed nothing at all.
  # Degrees of circle left open between the last tip and the first. Solved from
  # what the headers need unless the user has set it — the control is theirs,
  # the default is not a guess.
  open_angle <- if (circular) {
    a <- suppressWarnings(as.numeric(opts$open_angle))
    if (length(a) == 1L && !is.na(a) && a >= 0) {
      .clamp(a, 0, OPEN_ANGLE_MAX)
    } else {
      tree_open_angle(opts, md, panel_in)
    }
  } else {
    0
  }

  # ggtree opens the circle only in its "fan" layout — "circular" is the same
  # thing with the wedge closed, so a wedge means asking for the fan.
  layout <- if (identical(opts$layout, "inward")) {
    "inward_circular"
  } else if (circular && open_angle > 0) {
    "fan"
  } else {
    opts$layout
  }

  # An inward tree's radius is a *build* argument, not a scale limit: ggtree
  # maps its axis outward-in and the range it is given becomes the radius. So
  # the axis has to be solved before the plot exists — and solving it needs the
  # tree's depth, which only a built plot reports. One throwaway pass in the
  # default layout answers that; it draws nothing.
  #
  # Setting it twice is what broke the layout outright: the `xlim()` added
  # below fought the range ggtree had already been given, and the tree came out
  # as a blot at the centre with its annotations outside the root.
  inward <- identical(opts$layout, "inward")
  inward_xlim <- if (inward) {
    probe <- suppressWarnings(ggtree(tree)$data)
    probe_x <- suppressWarnings(max(probe$x, na.rm = TRUE))
    c(
      .tiplab_xlim(opts, md, probe, probe_x, annotation_total(opts))$limit,
      0
    )
  }

  build_base <- function(alpha = NULL) {
    args <- list(
      tree,
      color = opts$line_color,
      linewidth = (opts$branch_width %||% BRANCH_WIDTH) * scale,
      layout = layout,
      ladderize = TRUE,
      xlim = inward_xlim
    )
    if (identical(layout, "fan")) {
      args$open.angle <- open_angle
    }
    if (!is.null(alpha)) {
      args$alpha <- alpha
    }
    do.call(ggtree, args)
  }

  # Node-label view dims the tree so the internal node numbers read over it.
  base <- if (isTRUE(opts$nodelabel_show)) build_base(0.2) else build_base()

  tree_data <- base$data
  max_x <- max(tree_data$x, na.rm = TRUE)

  p <- base %<+% md

  # A mapping whose aesthetic is not drawn contributes neither geom nor scale.
  # Resolving it to NULL here is what keeps the two in step: the scale blocks
  # below are all guarded on these being non-NULL.
  drawn <- function(aes) {
    l <- layer_for(opts, aes)
    if (tree_aesthetic_drawn(opts, aes)) l else NULL
  }
  lab_l <- drawn("tiplab_color")
  pt_l <- drawn("tippoint_color")
  shp_l <- drawn("tippoint_shape")
  tile_ls <- if (tree_annotations_drawn(opts)) {
    Filter(function(l) identical(l$aesthetic, "tile"), opts$layers %||% list())
  } else {
    list()
  }

  # Solve the axis before anything is drawn on it. The tip labels, the tile
  # strips and the heatmap panels all occupy the space to the right of the
  # tree, and each needs to know where the one before it ended: a strip placed
  # without knowing the label reserve lands on top of the labels, which is what
  # clipped the first characters off every isolate name.
  tree_span <- max_x - suppressWarnings(min(tree_data$x, na.rm = TRUE))
  if (!is.finite(tree_span) || tree_span <= 0) {
    tree_span <- max_x
  }
  annot_total <- annotation_total(opts)
  # `opts$width_in` is the tree-and-labels budget, not the whole canvas: the
  # caller grows the canvas for the legend and the annotations rather than
  # taking their room out of the tree (see TREE_PANEL_IN in
  # visualization_tree.R). So the reserve is solved against it unmodified —
  # subtracting the legend here too would charge the tree for it twice, which
  # is what turned a few hundred isolates into a hairline.
  #
  # Solved for a circular tree on the same terms. Its x axis is a radius rather
  # than a width, which `tree_budget_in()` accounts for — and that is the whole
  # difference. Skipping the solve is what let a radial tree draw its labels
  # off every edge of the canvas and its rings straight over them.
  fit <- .tiplab_xlim(opts, md, tree_data, max_x, annot_total)
  label_reserve <- fit$reserve

  # How much x axis the panel spans, which is what turns a column's width in
  # data units into its width on the page — and so into a type size that fits
  # it.
  axis_units <- fit$limit - suppressWarnings(min(tree_data$x, na.rm = TRUE))

  # Inches the tree's *own* span is drawn across — what is left of the
  # tree-and-labels budget once the labels have taken their fraction. This is
  # the only thing that says whether a given branch is physically wide enough
  # to print a number on (tree_branch_keep); the annotations are paid for by a
  # wider canvas, so they do not come out of it.
  span_in <- tree_budget_in(opts) * (1 - .tiplab_budget_frac(opts, md))

  # Assemble plot layers (order maintains visual hierarchy).
  #
  # The new_scale_color() invariant, stated so it survives future edits: emit
  # it *after* the scale it closes and *before* the next geom that maps colour.
  # Each colour-carrying layer contributes exactly one scale + new_scale_color()
  # pair; the shape layer contributes neither, because nothing else in the plot
  # maps shape.
  layers <- c(
    tree_clade_layers(opts),
    list(tree_tiplab_layer(opts, lab_l)),
    if (!is.null(lab_l)) {
      list(
        tree_scale(
          md[[lab_l$field]],
          lab_l$palette,
          "color",
          name = lab_l$title,
          max_rows = legend_max_rows
        ),
        new_scale_color()
      )
    },
    list(
      tree_branch_layer(opts, tree_data, tree_span, span_in),
      tree_tippoint_layer(opts, pt_l, shp_l)
    ),
    if (!is.null(pt_l)) {
      list(
        tree_scale(
          md[[pt_l$field]],
          pt_l$palette,
          "color",
          name = pt_l$title,
          max_rows = legend_max_rows
        ),
        new_scale_color()
      )
    },
    if (!is.null(shp_l)) {
      # Levels come off the normalised column, not the layer's recorded count:
      # "not recorded" is a level the reader needs a mark for, and it is not
      # part of the count the mapping engine capped at six.
      shp_levels <- levels(md[[shp_l$field]])
      shp_real <- setdiff(shp_levels, MISSING_LABEL)
      shp_values <- setNames(
        TREE_SHAPES[seq_along(shp_real)],
        shp_real
      )
      if (MISSING_LABEL %in% shp_levels) {
        shp_values[[MISSING_LABEL]] <- TREE_MISSING_SHAPE
      }
      shp_keys <- tree_legend_breaks(names(shp_values))
      list(scale_shape_manual(
        values = shp_values,
        breaks = shp_keys$breaks,
        name = tree_legend_title(shp_l$title, shp_keys$hidden),
        labels = .wrap_legend_labels,
        guide = guide_legend(
          ncol = tree_legend_ncol(length(shp_keys$breaks), legend_max_rows)
        )
      ))
    },
    if (isTRUE(opts$nodelabel_show)) {
      list(geom_nodelab(aes(label = .data[["node"]])))
    },
    tree_tile_layers(
      opts,
      md,
      tile_ls,
      label_reserve / tree_span,
      tree_span,
      max_x,
      sum(tree_data$isTip),
      axis_units,
      panel_in,
      legend_max_rows
    )
  )
  layers <- Filter(Negate(is.null), layers)
  for (layer in layers) {
    p <- p + layer
  }

  # A root edge is a stub drawn *before* the root, at negative x. The inward
  # layout's scale is reversed and bounded at 0, so that stub falls outside it
  # and ggplot drops it with a "removed 1 row" warning — it was never drawn
  # there, so this only stops it being asked for.
  if (isTRUE(opts$rootedge_show) && !identical(opts$layout, "inward")) {
    p <- p + geom_rootedge(rootedge = max_x * 0.05)
  }
  if (isTRUE(opts$treescale_show) && !circular) {
    p <- p +
      geom_treescale(
        x = max_x * 0.5,
        y = -1,
        width = tree_nice_width(max_x * 0.1),
        color = opts$line_color,
        fontsize = AXIS_LABEL_SIZE * .scale_of(opts)
      )
  }
  if (!circular) {
    # Stacked below the scale bar rather than sharing its row, so switching
    # both on at once still leaves each legible instead of drawing one over
    # the other.
    axis_y <- if (isTRUE(opts$treescale_show)) -2 else -1
    for (layer in tree_axis_layer(opts, max_x, axis_y) %||% list()) {
      p <- p + layer
    }
  }

  # `fit` was solved before the layers were assembled, because the tile strips
  # needed the label reserve to place themselves. An inward tree already
  # carries it as its build range (see `inward_xlim`), and adding it again as a
  # scale limit clips the reversed axis instead of extending it.
  if (!inward) {
    p <- p + xlim(NA, fit$limit)
  }
  if (!circular) {
    if (annotation_total(opts) > 0) {
      # Room above the last tip for the annotation headers, which are set
      # vertically and would otherwise be clipped by the panel. Replacing
      # ggtree's y scale is the point, so its announcement is not news.
      p <- suppressMessages(
        p +
          scale_y_continuous(
            expand = expansion(
              mult = c(
                0.02,
                heatmap_header_frac(
                  opts,
                  sum(tree_data$isTip),
                  tree_span,
                  axis_units,
                  panel_in,
                  plot_height_in
                )
              )
            )
          )
      )
    }
  }

  # A *numeric* legend.position floats the guide box inside the panel, over the
  # tips, with nothing stopping it running off the canvas — which is what put
  # the legend on top of the tree and clipped it at the edge. A string position
  # makes ggplot2's gtable allocate a real guide-box column outside the panel,
  # sized to the widest key label. That is the reserved area, computed by the
  # layout engine rather than guessed at with two sliders.
  legend_cols <- tree_legend_cols(
    opts$layers,
    opts$heatmaps,
    opts$legend_size,
    plot_height_in,
    scale
  )
  p <- p +
    theme_tree(bgcolor = opts$bg) +
    theme(
      plot.margin = margin(6, 6, 6, 6),
      # One rule for both layouts. A circular tree used to put its guides
      # underneath, which took the room out of a panel that has to stay square
      # — so the disc shrank as guides were added, and with nothing reserving
      # room for the labels they were drawn over the keys anyway. Beside the
      # tree the guide box is a column the canvas grows for, exactly as it is
      # for a linear one.
      legend.position = "right",
      legend.direction = opts$legend_orientation,
      # Always stacked. Guides that will not fit wrap their own keys into more
      # columns (see tree_legend_max_rows) rather than the box being thrown
      # sideways, which spent the whole width on a single row of them.
      legend.box = "vertical",
      # Align the guide box to the plot rather than to the panel, so it does
      # not drift as the panel's own width changes with the label reserve.
      legend.location = "plot",
      legend.justification = "top",
      legend.box.spacing = unit(4, "pt"),
      legend.title = element_text(
        color = opts$line_color,
        size = opts$legend_size * 1.2
      ),
      legend.text = element_text(
        color = opts$line_color,
        size = opts$legend_size
      ),
      legend.key.size = unit(0.05 * opts$legend_size, "cm"),
      # The guide box keeps ggplot2's own theme colours unless it is told
      # otherwise — a white backdrop and grey key squares — so every legend on
      # a dark background arrived as a pale block with paler tiles behind the
      # keys. The background colour is one colour for the whole plot; the guides
      # sit on it like everything else.
      legend.background = element_rect(fill = opts$bg, color = NA),
      legend.box.background = element_rect(fill = opts$bg, color = NA),
      legend.key = element_rect(fill = opts$bg, color = NA),
      plot.background = element_rect(fill = opts$bg, color = opts$bg)
    )

  # Each panel gets its own fill scale (new_scale_fill closes the previous
  # one), so AMR's fixed two-colour key and a custom panel's categorical one
  # coexist as separate legends rather than collapsing into one that explains
  # neither.
  # The class runs each panel draws, collected as the panels are drawn so the
  # band below them can be sized from the longest name before it is placed.
  class_runs <- list()
  class_layers <- list()
  panels <- if (tree_annotations_drawn(opts)) {
    heatmap_panels(opts, tree_span, label_reserve)$panels
  } else {
    list()
  }
  for (pan in panels) {
    frame <- .heatmap_frame(pan, md, opts$amr_matrix)
    if (is.null(frame)) {
      next
    }
    # gheatmap centres its first column one whole cell past `offset`, so the
    # matrix it draws runs from offset + cell/2 out to offset + (ncol + 0.5) *
    # cell — half a column past the room heatmap_panels reserved for it, which
    # is the half `xlim()` censored the outermost column out of. Backing the
    # offset off by half a cell puts the drawn matrix where the solve says it
    # is.
    cell <- pan$width * tree_span / max(ncol(frame), 1L)
    p <- gheatmap(
      p + new_scale_fill(),
      data = frame,
      # The same solve in both layouts: gheatmap's offset is x-axis units, and
      # for a circular tree those units are radius. Pinning it to 0 there drew
      # the matrix from the tips outward over the labels.
      offset = pan$offset - cell / 2,
      width = pan$width,
      legend_title = pan$title,
      # Headers above the matrix, reading upward. Below it they ran into the
      # tree scale bar and off the bottom of the panel, because a drug class
      # name set vertically is taller than the row of space under the last tip.
      # Above, the space the deleted title block used to hold is free.
      colnames_position = "top",
      colnames_angle = 90,
      hjust = if (circular) 1 else 0,
      # Level with the tile strips' own headers (tree_tile_layers draws those
      # at n_tip + TILE_HEADER_OFFSET), so a row of annotations reads as one
      # row rather than as two at different heights.
      # The same place the tile strips' headers go (see tree_tile_layers),
      # expressed as the nudge that gets there from gheatmap's own baseline.
      colnames_offset_y = .heatmap_name_offset(opts, sum(tree_data$isTip)),
      # Fitted to the column, not fixed: thirty gene names at a fixed size
      # overprint each other into a smear.
      font.size = tree_header_size(cell, axis_units, axis_in, scale)
    )
    # gheatmap installs a default fill scale of its own, so replacing it is the
    # intended move — but ggplot2 announces every replacement, and this one is
    # not news. Deliberate, so silenced here rather than logged on every draw.
    p <- suppressMessages(
      p +
        scale_fill_manual(
          values = AMR_HEATMAP_FILL[levels(frame[[1]])],
          # Without explicit breaks the legend sorts its keys alphabetically —
          # "Inexact, Match, Not detected, Partial" — which reads as four
          # unrelated categories. These are a confidence scale, so they are
          # listed as one, strongest first.
          breaks = levels(frame[[1]]),
          name = pan$title,
          na.value = AMR_HEATMAP_FILL[[AMR_ABSENT]],
          drop = FALSE
        )
    )

    # Which classes this panel's columns fall into, and where they sit. A
    # circular panel has no room under it — "below the matrix" is the centre of
    # the disc — so the band is a linear-layout annotation only.
    runs <- heatmap_class_runs(pan, names(frame))
    if (!circular && nrow(runs)) {
      # Column k is centred one cell past the offset gheatmap was given, which
      # is `pan$offset - cell / 2` — so the first column lands on pan$offset +
      # cell / 2, the middle of the space reserved for it.
      centres <- max_x + pan$offset - cell / 2 + seq_len(ncol(frame)) * cell
      class_runs <- c(class_runs, list(runs))
      class_layers <- c(
        class_layers,
        list(list(
          runs = runs,
          centres = centres,
          cell = cell,
          # Set vertically, so what has to fit across is the type height against
          # one column — the same constraint the column names answer to above.
          size = tree_header_size(cell, axis_units, axis_in, scale)
        ))
      )
    }
  }

  # Placed after the loop so every panel's band is set at one size — two panels
  # whose classes were named at different sizes would read as two kinds of
  # thing rather than one row of annotation.
  if (length(class_layers)) {
    band_size <- min(vapply(class_layers, function(l) l$size, numeric(1)))
    for (l in class_layers) {
      for (layer in .heatmap_class_layers(
        l$runs,
        l$centres,
        l$cell,
        band_size,
        opts$line_color %||% "#000000"
      )) {
        p <- p + layer
      }
    }
    # Room under the last tip for the names hanging there, measured the same
    # way the header reserve above is. Replacing the y scale a second time is
    # deliberate, and its announcement is not news.
    p <- suppressMessages(
      p +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              heatmap_class_frac(
                opts,
                sum(tree_data$isTip),
                class_runs,
                band_size
              ),
              heatmap_header_frac(
                opts,
                sum(tree_data$isTip),
                tree_span,
                axis_units,
                panel_in,
                plot_height_in
              )
            )
          )
        )
    )
  }

  out <- as.ggplot(p, scale = opts$zoom, hjust = opts$h, vjust = opts$v)
  ggdraw(out) +
    theme(plot.background = element_rect(fill = opts$bg, color = opts$bg))
}

MAX_PLOT_PX <- 10000

#' Export Tree Visualization to File
#'
#' Saves the plot to disk while safeguarding against excessively high-resolution rasters.
#'
#' @param plot ggplot/ggdraw Object.
#' @param file Character. Destination file path.
#' @param filetype Character. Output format/device ("png", "pdf", "svg", etc.).
#' @param aspect_ratio Numeric. Height-to-width ratio.
#' @param width Numeric. Output width in inches. Default 10.
#' @param dpi Numeric. Desired resolution DPI.
#' @export
save_tree_plot <- function(
  plot,
  file,
  filetype,
  aspect_ratio,
  width = 10,
  dpi = 300
) {
  height <- width * aspect_ratio
  if (!identical(filetype, "svg")) {
    dpi <- max(48, min(dpi, floor(MAX_PLOT_PX / max(width, height))))
  }
  ggsave(
    filename = file,
    plot = plot,
    device = filetype,
    width = width,
    height = height,
    dpi = dpi,
    limitsize = FALSE
  )
}
