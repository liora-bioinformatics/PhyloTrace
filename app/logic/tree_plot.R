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
    geom_rect,
    geom_segment,
    ggsave,
    guide_colourbar,
    guide_legend,
    theme,
    element_text,
    element_rect,
    margin,
    unit,
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
    scale_x_continuous,
    scale_y_continuous,
    expansion,
  ],
  ape[root],
  stats[dist, hclust, setNames],
  utils[head, tail],
  RColorBrewer[brewer.pal, brewer.pal.info],
  viridisLite[viridis],
  grDevices[colorRampPalette],
  rlang[`%||%`],
)

box::use(
  app /
    logic /
    amr_plot[
      AMR_CLUSTER_DISTANCE_DEFAULT,
      AMR_CLUSTER_METHOD_DEFAULT,
      AMR_CONFIDENCE_STATES,
      AMR_ELEMENT_TYPES,
      amr_confidence_palette,
      amr_fit_scale,
      amr_palette
    ],
  app / logic / date_bins[bin_date_values],
  app / logic / field_labels[field_labels_for],
  app / logic / field_profile[field_levels],
  app / logic / mapping_engine[crowded_tips],
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

# The row pitch a tree with no isolate labels is fitted to instead.
#
# TIP_ROW_IN buys a row deep enough to set a name in. A tree whose names cannot
# be set at any legible size (`tree_auto_layout()` decides that, and past
# `TIP_MAPPING_MAX` tips so does the view) is paying for room it will not use —
# 253 isolates came out at the aspect ceiling, five times as tall as it is
# wide, to hold labels that are never drawn. Without them the only thing a row
# has to do is keep its branch a separate line from its neighbours, which takes
# about a third of the depth: the same tree at 2.5 shows the same topology and
# reads far better.
TIP_ROW_BARE_IN <- 0.055
TIP_USABLE <- 0.9 # Share of plot height available excluding margins/title
TIP_ROW_FILL <- 0.77 # Fraction of row pitch occupied by tip label text box

# Horizontal label reservation geometry
TIP_CHAR_EM <- 0.6 # Character width estimate (em) for accession/isolate labels

# Advance of one character, in ems, for the characters a caption is made of.
#
# `TIP_CHAR_EM` is a mean, and a mean is the right measure for a reserve that
# has to cover labels nobody has typed yet — an accession is a fixed shape and
# a mean over it is exact enough. A clade caption is the other case: one known
# string, typed by the reader, drawn at a size fitted to a column measured from
# it. There the mean is 20% short of what an all-capital word sets, and 20%
# short of the column is three letters drawn past the edge of the panel.
#
# Helvetica's own widths, which the export devices' sans faces are within a
# percent of. Anything not listed takes `CHAR_EM_UNKNOWN`.
CHAR_EM <- c(
  " " = 0.278, "!" = 0.278, "\"" = 0.355, "#" = 0.556, "$" = 0.556,
  "%" = 0.889, "&" = 0.667, "'" = 0.191, "(" = 0.333, ")" = 0.333,
  "*" = 0.389, "+" = 0.584, "," = 0.278, "-" = 0.333, "." = 0.278,
  "/" = 0.278,
  "0" = 0.556, "1" = 0.556, "2" = 0.556, "3" = 0.556, "4" = 0.556,
  "5" = 0.556, "6" = 0.556, "7" = 0.556, "8" = 0.556, "9" = 0.556,
  ":" = 0.278, ";" = 0.278, "<" = 0.584, "=" = 0.584, ">" = 0.584,
  "?" = 0.556, "@" = 1.015,
  "A" = 0.667, "B" = 0.667, "C" = 0.722, "D" = 0.722, "E" = 0.667,
  "F" = 0.611, "G" = 0.778, "H" = 0.722, "I" = 0.278, "J" = 0.5,
  "K" = 0.667, "L" = 0.556, "M" = 0.833, "N" = 0.722, "O" = 0.778,
  "P" = 0.667, "Q" = 0.778, "R" = 0.722, "S" = 0.667, "T" = 0.611,
  "U" = 0.722, "V" = 0.667, "W" = 0.944, "X" = 0.667, "Y" = 0.667,
  "Z" = 0.611,
  "[" = 0.278, "\\" = 0.278, "]" = 0.278, "^" = 0.469, "_" = 0.556,
  "`" = 0.333,
  "a" = 0.556, "b" = 0.556, "c" = 0.5, "d" = 0.556, "e" = 0.556,
  "f" = 0.278, "g" = 0.556, "h" = 0.556, "i" = 0.222, "j" = 0.222,
  "k" = 0.5, "l" = 0.222, "m" = 0.833, "n" = 0.556, "o" = 0.556,
  "p" = 0.556, "q" = 0.556, "r" = 0.333, "s" = 0.5, "t" = 0.278,
  "u" = 0.556, "v" = 0.5, "w" = 0.722, "x" = 0.5, "y" = 0.5,
  "z" = 0.5,
  "{" = 0.334, "|" = 0.26, "}" = 0.334, "~" = 0.584,
  "\u2026" = 1.0
)

# What a character outside `CHAR_EM` is booked at.
#
# The table is Latin and a caption need not be: a gene name carrying a Greek
# letter, a range written with an en dash, a unit with a middle dot. Nearly all
# of those set *narrower* than the Latin mean, but the widest — an em dash, an
# arrow, a CJK glyph — set a full em, two thirds over it, and a column short by
# that much is the caption clipped at the panel edge.
#
# So an unknown character is booked at the widest a character gets rather than
# at the average one. The guess does not cost the same in both directions: too
# wide leaves a little white space at the end of a caption, too narrow loses
# the end of it.
CHAR_EM_UNKNOWN <- 1

# Ems one string sets in, measured character by character.
.string_em <- function(x) {
  ch <- strsplit(as.character(x %||% ""), "", fixed = TRUE)[[1]]
  if (!length(ch)) {
    return(0)
  }
  em <- CHAR_EM[ch]
  em[is.na(em)] <- CHAR_EM_UNKNOWN
  sum(em)
}
TIP_LABEL_FRAC <- 0.35 # Maximum fraction of panel width reserved for tip labels

# Air between a tip point and the isolate label that starts beside it, in
# millimetres of the finished plot.
#
# `geom_tiplab` anchors the label at the tip, so with no nudge its first glyph
# is drawn over the tip point. The gap is set as a physical length — like every
# type size in this module — and scales with the point, because a larger mark
# needs a wider berth. Converted to x-axis units at the point it is applied
# (the builder) and to a budget fraction where the label reserve is solved
# (`.tiplab_frac`), so the strip past the labels clears the nudge too.
TIPLAB_POINT_GAP_MM <- 0.8
TIPLAB_POINT_GAP_PER_SIZE <- 0.28

# Millimetres the isolate label is shifted clear of its tip point.
#
# `opts$tippoint_size` is already at this plot's scale (scale_tree_opts scales
# it with the other type sizes), so only the constant base term is multiplied
# by `.scale_of()` — scaling both would put a `k^2` on the per-size term and
# break the "same figure at another size" invariant the reserve depends on.
.tiplab_point_gap_mm <- function(opts) {
  if (!isTRUE(opts$tiplab_show)) {
    return(0)
  }
  k <- .scale_of(opts)
  pt <- suppressWarnings(as.numeric(
    opts$tippoint_size %||% (TREE_FIT_DEFAULTS$tippoint_size * k)
  ))
  if (!is.finite(pt) || pt < 0) {
    pt <- TREE_FIT_DEFAULTS$tippoint_size * k
  }
  TIPLAB_POINT_GAP_MM * k + TIPLAB_POINT_GAP_PER_SIZE * pt
}

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
# The thinnest the branches are allowed to get. The preview is drawn at
# PLOT_RES, where this is a fraction of a pixel and reads as a fine grey line;
# the vector exports (cairo PDF/SVG) draw it crisp at any size. Below it the
# on-screen line fades toward nothing, so a thousand tips sit here and the
# reader drops it further by hand if the export needs finer still.
BRANCH_WIDTH_MIN <- 0.05
BRANCH_WIDTH_TIPS <- 60
# How hard the stroke falls with the tip count: in step with it, because that
# is how the crowding grows. A square-root fall came down far too slowly — a
# radial tree of a few hundred tips still filled in solid.
BRANCH_WIDTH_FALL <- 1

# The leader line's stroke, as a share of the tree's own.
#
# A leader is a guide rather than data — it carries the eye from a tip across
# the empty band to whatever is set beside it — so it is drawn lighter than the
# branch it leaves. ggtree's own default is a flat 0.5 whatever the tree holds,
# which past a few hundred tips is six times the stroke the branches have
# already been thinned to (`tree_branch_width()`), and the leaders then read as
# the drawing with the tree as a sketch under it.
LEADER_WIDTH_FRAC <- 0.6

# Millimetres of row a leader line needs to read as a line of its own.
#
# A dotted line's dots are as wide across as the line is thick and about three
# times that apart along it. Each tip's dots start at its own depth, so no two
# neighbouring rows are in phase, and once the rows are tighter than the dots
# are long every vertical slice through the band lands on ink in most of them:
# the field fills in. On a linear tree that reads as bars across the plot; on a
# radial one, where the same band is wrapped around a circle, it reads as the
# rings a thousand tips drew. Past this the leaders are left off — a row the
# eye cannot pick out is a row no leader can lead it along.
LEADER_MIN_PITCH_MM <- 1

#' The stroke a dendrogram of `n_leaves` is drawn with.
#'
#' One rule for both dendrograms on the figure. The tree's branches take it
#' from the tip count (`tree_auto_layout()` puts the answer in the sidebar,
#' where it stays the reader's to override); a clustered heatmap panel's
#' column dendrogram takes the *tree's* stroke, so the two read as one drawing
#' rather than as a tree with a hairline sketch under it — thinning only where
#' its own leaves are packed tighter than the tips are, which is the same
#' crowding this answers for either of them.
#'
#' @param n_leaves Integer. Leaves the dendrogram draws.
#' @return Numeric ggplot2 linewidth.
#' @export
tree_branch_width <- function(n_leaves) {
  n <- max(as.integer(n_leaves %||% 1L), 1L)
  round(
    .clamp(
      BRANCH_WIDTH * (BRANCH_WIDTH_TIPS / n)^BRANCH_WIDTH_FALL,
      BRANCH_WIDTH_MIN,
      BRANCH_WIDTH
    ),
    2
  )
}

# Fitting limits
TIP_GROWTH <- 1.5 # Cap size scaling relative to default (150%)
TIP_ASPECT_MIN <- 0.5 # Minimum allowed aspect ratio

#' How far the image may grow past the tree's own budget, in either direction.
#'
#' `tree_panel_width_in()` is a request, not a promise: without a ceiling, four
#' wide legends and three heatmap panels ask for a canvas no screen can show
#' and no export can rasterise. Past this the annotations share what is left.
#'
#' Here rather than in the view because the axis solve needs it too. Every
#' reserve measured in inches — the axis overhang, the caption column — is
#' solved against the panel the annotations *asked* for, and once the ceiling
#' bites that panel is not the one being drawn on.
#' @export
TREE_CANVAS_MAX_FACTOR <- 2.6

# The tallest aspect ratio the fit will ask for — the sidebar slider's own
# ceiling (`ASPECT_MAX` in the view), not TREE_CANVAS_MAX_FACTOR. That one
# bounds how far the *annotations* may grow the canvas, a separate question
# from how tall a bare tree of a thousand branches may be drawn. The two were
# one number, and at 2.6 the rows of a thousand-tip tree packed to a couple of
# pixels and the branches filled in; 8 gives each branch a row it can be told
# apart in.
#
# Raised safely only because `tree_canvas_height_in()` now draws whatever
# aspect it is handed up to this: the fit and the drawing used to agree on 2.6
# by both stopping there, and when they drifted the fit solved a thousand tips
# onto 27.5in of paper while the image was cut to 14.3 and every row arrived
# half the height its type was chosen for.
TIP_ASPECT_MAX <- 8
TIP_SIZE_MIN <- 0.5 # Minimum size threshold
TIP_SIZE_FLOOR <- 1.2 # Minimum text size for legibility flag

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

#' Largest tip label a layout can carry, in millimetres.
#'
#' The one solve behind both halves of the tip-label rule. `tree_auto_layout()`
#' calls it to *choose* a size; `.tiplab_room()` calls it to find out whether a
#' size that has already been chosen will fit. They were separate arithmetic
#' once, and drifted: the fit measured a label against the ring it wanted while
#' the drawing measured it against the ring it got, so a radial tree hid labels
#' that fitted and drew labels that did not.
#'
#' Three geometries, one rule — the size is the smaller of what a row can hold
#' and what a label's own length allows:
#'
#' - **Linear.** Rows are the panel height over the tip count; width is the
#'   share of the budget the labels may claim, spread over the longest label.
#' - **Circular.** Everything is measured along the radius, and the label ring
#'   takes its outer part. The two constraints move in opposite directions as
#'   that ring grows — a longer ring sets a larger label, while the tips it
#'   annotates sit on the smaller circle *inside* it — so the largest legible
#'   type is exactly where they cross, and that crossing has a closed form.
#'   `k` is the ratio of what the two ask for at ring = 1 and ring = 0; they
#'   cross at k / (1 + k). This replaced a fixed guess that the tips sat at
#'   0.35 of the panel whatever the tree held, which is why a radial tree drew
#'   the same type size at twenty tips as at eighty and ran it off the canvas
#'   at both.
#' - **Inward.** The labels run from the rim *toward* the centre and stop at
#'   INWARD_CORE_FRAC of the radius, so they converge rather than diverge: the
#'   room between two of them is the arc at their inner ends, which does not
#'   move with the ring. There is no crossing to find — the row constraint is
#'   fixed and the ring only widens until the width constraint stops binding.
#'
#' @param n_tip Integer. Number of tips.
#' @param width_in Numeric. Tree-and-labels budget, in inches.
#' @param layout Character. Tree layout mode.
#' @param label_chars Numeric. Characters in the longest label.
#' @param aspect Numeric. The plot's aspect ratio; linear layouts only.
#' @param label_frac Numeric. Share of the budget the labels may claim. The fit
#'   asks for TIP_LABEL_FRAC, which is a design choice; the drawing is bounded
#'   by TIP_LABEL_AXIS_MAX, which is the hard limit. The gap between them is
#'   headroom the text-size control can spend.
#' @param gap_mm Numeric. Millimetres already spoken for by the tip-point nudge.
#' @param row_only Logical. Return the row constraint alone, which is what the
#'   element sizes fitted to the pitch (branch labels, tip points) answer to.
#' @return Numeric millimetres.
#' @export
tree_tiplab_room <- function(
  n_tip,
  width_in = 5.5,
  layout = "rectangular",
  label_chars = 20,
  aspect = 1,
  label_frac = NULL,
  gap_mm = 0,
  row_only = FALSE
) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    5.5
  } else {
    width_in
  }
  chars <- max(as.numeric(label_chars %||% 1), 1)
  radius_in <- w * TREE_RADIAL_FRAC
  # Each layout has its own ceiling on the labels' share, and only the linear
  # one is ever asked for a different number (the fit's 0.35 against the
  # drawing's 0.45). The radial ceilings are what the crossing is solved
  # against, so overriding those would change the fit rather than bound it.
  frac <- label_frac %||%
    if (identical(layout, "inward")) {
      1 - INWARD_CORE_FRAC - INWARD_TREE_MIN
    } else if (layout %in% .circular_layouts) {
      TIP_RING_FRAC_MAX
    } else {
      TIP_LABEL_FRAC
    }
  # Millimetres of label a given ring (or width share) buys, net of the nudge
  # that holds the label off its tip point.
  along <- function(run_in) {
    max(25.4 * run_in - gap_mm, 0) / (TIP_CHAR_EM * chars)
  }

  if (identical(layout, "inward")) {
    by_row <- TIP_ROW_FILL * 25.4 * 2 * pi * radius_in * INWARD_CORE_FRAC / n
    ring <- min(by_row * TIP_CHAR_EM * chars / (25.4 * radius_in), frac)
    by_width <- along(radius_in * max(ring, 0))
  } else if (layout %in% .circular_layouts) {
    k <- TIP_ROW_FILL * 2 * pi * TIP_CHAR_EM * chars / n
    ring <- min(k / (1 + k), frac)
    by_row <- TIP_ROW_FILL * 25.4 * 2 * pi * radius_in * (1 - ring) / n
    by_width <- along(radius_in * ring)
  } else {
    a <- suppressWarnings(as.numeric(aspect %||% 1))
    if (length(a) != 1L || !isTRUE(is.finite(a)) || a <= 0) {
      a <- 1
    }
    by_row <- TIP_ROW_FILL * 25.4 * TIP_USABLE * a * w / n
    by_width <- along(frac * w)
  }
  if (isTRUE(row_only)) by_row else min(by_row, by_width)
}

#' Calculate Auto-Fitted Layout Parameters
#'
#' Derives optimal aspect ratios, font sizes, and element scaling based on
#' dataset dimensions (tip count) and target device geometry.
#'
#' @param n_tip Integer. Number of tips in the phylogenetic tree.
#' @param width_in Numeric. Device panel width in inches. Default 5.5.
#' @param layout Character. Tree layout mode (e.g., "rectangular", "circular").
#' @param label_chars Numeric. Max expected character length of tip labels.
#' @param labels Logical. Whether the isolate labels are to be drawn, which
#'   decides how much height a row is worth buying. `NA`, the default, lets the
#'   fit answer it from the room the labels would have.
#'
#' @return List of calculated display parameters and legibility flags.
#' @export
tree_auto_layout <- function(
  n_tip,
  width_in = 5.5,
  layout = "rectangular",
  label_chars = 20,
  labels = NA
) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    5.5
  } else {
    width_in
  }
  chars <- max(as.numeric(label_chars %||% 1), 1)

  circular <- layout %in% .circular_layouts
  linear_aspect <- function(row_in) {
    .clamp(n * row_in / w, TIP_ASPECT_MIN, TIP_ASPECT_MAX)
  }
  # A circular panel is square: the tree is a disc, so its height is its width.
  aspect <- if (circular) 1 else linear_aspect(TIP_ROW_IN)

  size <- tree_tiplab_room(n, w, layout, chars, aspect)
  # Whether the labels earn the height the pitch above was buying for them.
  # Both halves of the view's own rule, so the aspect it applies and the labels
  # it draws are decided by the same test (see `refit_layout()`) — unless the
  # caller has already settled it, which is a switch the reader set by hand.
  wanted <- if (is.na(labels)) {
    size >= TIP_SIZE_FLOOR && !crowded_tips(n)
  } else {
    isTRUE(labels)
  }
  if (!circular && !wanted) {
    aspect <- linear_aspect(TIP_ROW_BARE_IN)
    size <- tree_tiplab_room(n, w, layout, chars, aspect)
  }
  by_row <- tree_tiplab_room(n, w, layout, chars, aspect, row_only = TRUE)

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
    branch_width = tree_branch_width(n),
    labels_legible = size >= TIP_SIZE_FLOOR
  )
}

#' Keys a guide lists when nothing has told it how much room it has.
#'
#' The answer to "how long a list is worth drawing" is mostly the box's height
#' (`tree_legend_key_budget()`), and every guide the builder draws is solved
#' against it. This is what a scale built outside that solve falls back to — a
#' handful of swatches, which is what a key list is read for.
#' @export
LEGEND_MAX_KEYS <- 9L

#' Fewest keys a guide is cut back to before it stops being worth drawing.
#'
#' The floor the budget starts every guide at, and the one number in it that is
#' not negotiable: what a guide may list past this depends on how much height
#' the box has and how many other guides are sharing it, but a guide cut below
#' four keys is not worth the rows it stands in. Where even the floor will not
#' fit, the type is shrunk instead (`tree_legend_size()`).
LEGEND_MIN_KEYS <- 4L

#' Keys any one guide lists, however much room the box has.
#'
#' One column's worth — the same `LEGEND_MAX_ROWS` a guide's keys wrap at,
#' written again here because it is declared further down the file. Tying the
#' two together is the point: a guide allowed more keys than a column holds
#' buys them by folding into a second column, and a second column is width
#' taken off the tree. So the height decides everything under this, and past it
#' a scale is a population rather than a vocabulary — the colours still say
#' where the same value recurs on the tree, which is the job they go on doing
#' when the guide only samples them.
#'
#' It used to be nine, with everything longer than twenty levels cut back to it
#' whatever the figure's height was. That is what listed nine of twenty-seven
#' wards down the side of a plot with a hand's width of blank paper beside them.
#' @export
LEGEND_FULL_MAX <- 18L

#' The blank key that stands where a run of levels was left out.
#'
#' A trimmed guide reads as a complete list unless it says otherwise. The title
#' says how many levels there are ("9 of 81 shown"), but not *where* the gap
#' falls — and for an ordered scale, whose keys come from both ends, that is
#' the one thing the reader has to know: the two halves are not neighbours.
#' Drawn as a swatch with no colour in it, so it reads as a break in the list
#' rather than as another category.
#'
#' Three full stops, not the typographic ellipsis: U+22EF is missing from
#' enough of the fonts these plots are exported through that R substitutes a
#' single dot for each of its bytes and warns while doing it.
#' @export
LEGEND_GAP_KEY <- "..."

# The colour a gap key's swatch is filled with, which is none.
LEGEND_GAP_COLOR <- "transparent"

#' Keys one guide may list, given the rows it has been budgeted.
#'
#' @param max_rows Integer. Rows per guide, from `tree_legend_max_rows()`.
#' @return Integer key budget.
#' @export
tree_legend_max_keys <- function(max_rows = LEGEND_MAX_ROWS) {
  rows <- suppressWarnings(as.integer(max_rows))
  if (length(rows) != 1L || is.na(rows)) {
    rows <- LEGEND_MAX_ROWS
  }
  as.integer(.clamp(rows, LEGEND_MIN_KEYS, LEGEND_MAX_KEYS))
}

# Rows one guide stands in, listing `keys` of its `demand` levels in `ncol`
# columns: a title, the keys, and the blank line before the next guide — plus,
# where it is not listing everything, the title's second line ("9 of 81 shown")
# and the gap key.
.legend_guide_rows <- function(keys, demand, ncol = 1L) {
  as.integer(ceiling(keys / pmax(ncol, 1L))) + 2L + 2L * (keys < demand)
}

#' Keys each guide may list, sharing the rows the box has between them.
#'
#' An equal share was the wrong answer twice over. It counted a guide that
#' wants four keys as costing the same as one that wants eighty, so eight drug
#' classes were listed as "7 of 8" beside four confidence tiers that had three
#' rows going spare; and a guide *one key short* of complete pays two extra
#' rows for saying so, which an equal share never noticed it could recover.
#'
#' So: fill the short lists first, shortest first, because completing a guide
#' costs less than it looks and is worth more than a longer sample of a list
#' nobody can read to the end anyway. Whatever is left over is then handed round
#' the guides that are still trimmed, one key at a time, so they grow together
#' rather than the first of them taking the lot.
#'
#' No guide lists more than `LEGEND_FULL_MAX` keys whatever the room; under it
#' the height is the only thing that trims a guide, so a figure with paper to
#' spare lists every level it has.
#'
#' @param demands Integer vector. Levels each guide holds, in stacking order.
#' @param room Integer. Rows the whole guide box has.
#' @return Integer vector of key budgets, one per guide.
#' @export
tree_legend_key_budget <- function(demands, room = LEGEND_MAX_ROWS) {
  d <- suppressWarnings(as.integer(demands))
  d <- d[!is.na(d)]
  n <- length(d)
  if (!n) {
    return(integer(0))
  }
  d <- pmax(d, 1L)
  cap <- pmin(d, LEGEND_FULL_MAX)
  give <- pmin(cap, LEGEND_MIN_KEYS)
  cost <- function(g) sum(.legend_guide_rows(g, d))
  # The floor is not negotiable: a guide cut below it is not worth drawing, and
  # a box that cannot hold the floor is shrunk instead (`tree_legend_size()`).
  budget <- max(suppressWarnings(as.integer(room %||% LEGEND_MAX_ROWS)), cost(give))
  for (i in order(d)) {
    trial <- give
    trial[[i]] <- cap[[i]]
    if (cost(trial) <= budget) {
      give <- trial
    }
  }
  repeat {
    moved <- FALSE
    for (i in which(give < cap)) {
      trial <- give
      trial[[i]] <- trial[[i]] + 1L
      if (cost(trial) <= budget) {
        give <- trial
        moved <- TRUE
      }
    }
    if (!moved) {
      break
    }
  }
  as.integer(give)
}

# Whether a set of levels has ends worth showing.
#
# Numbers, and the four shapes a binned date takes ("2024", "2024-03",
# "2024-W12", "2024-03-05") — all of which `.level_order()` has already put in
# order, the dates because they sort lexically into chronological order. For
# anything else "first" and "last" are accidents of the alphabet.
.levels_are_ordered <- function(x) {
  if (length(x) < 2L) {
    return(FALSE)
  }
  if (!anyNA(suppressWarnings(as.numeric(x)))) {
    return(TRUE)
  }
  all(grepl("^\\d{4}(-(W\\d{2}|\\d{2}(-\\d{2})?))?$", x))
}

# How often each level occurs in the column the scale was built from. Absent
# data leaves every level equal, which falls back to the scale's own order.
.level_counts <- function(levels, values) {
  if (is.null(values)) {
    return(rep(1L, length(levels)))
  }
  tab <- table(as.character(values))
  counts <- as.integer(tab[levels])
  counts[is.na(counts)] <- 0L
  counts
}

#' The keys one guide should list, and what to say about the rest.
#'
#' Which keys survive is not the same question for every scale, and answering
#' it with "the first nine" was wrong for both kinds:
#'
#' - An **ordered** scale (numbers, or a binned date) is read for its range.
#'   Nine consecutive keys off the front of eighty say nothing about the other
#'   seventy-one, so the budget is split between the two ends and the reader
#'   gets the extremes every colour on the figure lies between.
#' - A **nominal** scale has no ends. Its keys go to the levels the reader will
#'   actually meet — the most frequent ones — restored to the scale's own order
#'   so the guide still reads down the palette rather than down a ranking.
#'
#' "Not recorded" keeps its key wherever it appears. It is the one level whose
#' colour cannot be guessed from the others, and an unexplained grey swatch is
#' worse than one fewer real category.
#'
#' @param levels Character vector of the scale's levels, in draw order.
#' @param values The mapped column, for the frequency order. Optional; without
#'   it a nominal scale falls back to its own level order.
#' @param max_keys Integer. Keys this guide has room for.
#' @return list(breaks = <character>, hidden = <integer>, total = <integer>).
#' @export
tree_legend_breaks <- function(
  levels,
  values = NULL,
  max_keys = LEGEND_MAX_KEYS
) {
  levels <- as.character(levels)
  n <- length(levels)
  k <- max(suppressWarnings(as.integer(max_keys)), 2L)
  if (is.na(k) || n <= k) {
    return(list(breaks = levels, hidden = 0L, total = n))
  }
  missing <- intersect(MISSING_LABEL, levels)
  real <- setdiff(levels, MISSING_LABEL)
  budget <- max(k - length(missing), 1L)
  keep <- if (.levels_are_ordered(real)) {
    head_n <- ceiling(budget / 2)
    c(head(real, head_n), tail(real, budget - head_n))
  } else {
    ranked <- order(-.level_counts(real, values), seq_along(real))
    real[sort(head(ranked, budget))]
  }
  list(
    breaks = c(.with_gap_key(real, keep), missing),
    hidden = n - length(keep) - length(missing),
    total = n
  )
}

# The kept keys with one blank key marking where the list was cut.
#
# One marker, at the first place the list stops being contiguous — counting the
# two ends, so a guide whose missing levels all fall past its last key still
# says so somewhere the reader can see it and not only in the count on the
# title. One and not three: a guide scattered across a long scale would
# otherwise spend half its rows on punctuation.
.with_gap_key <- function(all, keep) {
  at <- match(keep, all)
  at <- at[!is.na(at)]
  if (!length(at) || length(at) == length(all)) {
    return(keep)
  }
  if (at[[1L]] > 1L) {
    return(c(LEGEND_GAP_KEY, keep))
  }
  gap <- which(diff(at) > 1L)
  if (length(gap)) {
    i <- gap[[1L]]
    return(c(head(keep, i), LEGEND_GAP_KEY, tail(keep, length(keep) - i)))
  }
  if (at[[length(at)]] < length(all)) c(keep, LEGEND_GAP_KEY) else keep
}

# A scale's palette with a colourless swatch added for the gap key, and the
# limits that admit it. A break outside the scale's limits is dropped without
# comment, and a discrete scale's limits are its data's levels — which the gap
# key, being no level of anything, is not one of.
.legend_values <- function(cols, breaks, blank = LEGEND_GAP_COLOR) {
  if (!LEGEND_GAP_KEY %in% breaks) {
    return(cols)
  }
  c(cols, setNames(blank, LEGEND_GAP_KEY))
}

.legend_limits <- function(levels, breaks) {
  if (!LEGEND_GAP_KEY %in% breaks) {
    return(NULL)
  }
  c(as.character(levels), LEGEND_GAP_KEY)
}

#' A guide title that says how many values it is not showing.
#'
#' Said on the title rather than as a key of its own: ggplot2's guides have no
#' slot for a row that is not a break, and a count dressed up as a swatch would
#' read as another category.
#'
#' Stated as "9 of 81 shown" rather than "+ 72 more", because with the keys
#' taken from both ends of an ordered scale the reader has to know the list is
#' a *sample* of the levels and not the head of them.
#'
#' @param name Character. The variable's title.
#' @param hidden Integer. Levels the guide is not listing.
#' @param total Integer. Levels the scale holds. Optional.
#' @return Character.
#' @export
tree_legend_title <- function(name, hidden, total = NULL) {
  if (!isTRUE(hidden > 0)) {
    return(name)
  }
  if (is.null(total) || !isTRUE(is.finite(total) && total > hidden)) {
    return(paste0(name %||% "", "\n+ ", hidden, " more"))
  }
  paste0(name %||% "", "\n", total - hidden, " of ", total, " shown")
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
  as.integer(min(LEGEND_KEY_COLS, ceiling(n_levels / max_rows)))
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

# Type size for the internal node numbers, in millimetres — ggplot2's own text
# default, so node view looks the same as it always did at text scale 1.
NODE_LABEL_SIZE <- 3.88


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
      size = AXIS_LABEL_SIZE * .type_of(opts),
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

#' Legend type size, in points.
#'
#' The legend is furniture, like the distance axis, and it is the only text on
#' the figure that competes with nothing: the tip labels are fitted to the row
#' pitch, the column names to the column width, and both shrink as the data
#' grows, while a guide box has its own column and stays wherever it was put.
#' At ggplot2's own 10pt it ended up half again the size of the axis it sits
#' beside and several times the size of the names it explains. Setting it to
#' the axis's size is what makes the two read as one figure; the reader's text
#' scale still moves both together.
#'
#' Points rather than millimetres because that is what `theme()` takes.
#' @export
LEGEND_SIZE_PT <- round(AXIS_LABEL_SIZE * .MM_TO_PT, 1)

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
  want <- HEADER_SIZE_MAX * .type_of(opts)

  # An annotation header is the smallest type the plot sets, because it is
  # fitted to a column rather than to a row. Only the ones that will actually
  # be drawn count: a header the columns cannot hold legibly is left off the
  # figure (`.header_drawn()`), so it says nothing about how small the figure
  # prints.
  mm <- numeric(0)
  if (annotation_total(opts) > 0) {
    header <- function(col_in) {
      size <- .fitted_type(want, col_in * 25.4 * HEADER_FILL, scale)
      if (.header_drawn(size, scale)) size else numeric(0)
    }
    if (.n_tiles(opts) > 0) {
      mm <- c(mm, header(.tile_col_in(opts)))
    }
    if (
      length(Filter(function(h) length(h$cols) > 0L, opts$heatmaps %||% list()))
    ) {
      mm <- c(mm, header(.heat_col_in(opts)))
    }
  }
  if (tree_tiplab_drawn(opts, md)) {
    mm <- c(mm, .tiplab_size(opts, md))
  }
  pt <- c(mm * .MM_TO_PT, tree_legend_size(opts))
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

# --- The reader's own text size ----------------------------------------------
#
# One control over every piece of type on the figure, and over nothing else.
#
# It is a *bias*, not a size. The engine already solves each label's size from
# the room it has — a tip label from the row pitch, a column header from the
# column under it, a legend key from the height beside the tree — and those
# solves are what keep a figure legible as the data changes shape. This
# multiplies what each of them asks for, and every one of them still ends
# inside the same two rules:
#
#   as big as possible   — a label never grows past the room it has, so
#                          turning the control up fills the gaps and then
#                          stops. It cannot make two labels overlap.
#   as small as necessary — a label never shrinks past what can be read, so
#                          turning the control down reaches the legibility
#                          floor and then stops.
#
# When the room itself cannot hold legible type — three hundred tips at half
# an inch of pitch, a hundred gene columns across five inches — there is no
# size that satisfies both, and the label is not drawn at all. That is the
# only way an element disappears, and it is why the control cannot produce an
# unreadable figure at either end of its range.
#
# Deliberately *not* folded into `opts$scale`. That one is the whole design's
# physical scale: it moves the annotation columns, the key squares and the
# branch strokes along with the type, which is what makes an export at another
# width the same figure (see scale_tree_opts). Text size has to move the type
# and leave the layout where it is, or "does this still fit?" has no answer.
TEXT_SCALE_MIN <- 0.6
TEXT_SCALE_MAX <- 2

#' Text size the plot is drawn at when the reader has not said otherwise.
#' @export
TEXT_SCALE_DEFAULT <- 1

.text_of <- function(opts) {
  k <- suppressWarnings(as.numeric(opts$text_scale %||% TEXT_SCALE_DEFAULT))
  if (length(k) != 1L || is.na(k) || !is.finite(k) || k <= 0) {
    return(TEXT_SCALE_DEFAULT)
  }
  .clamp(k, TEXT_SCALE_MIN, TEXT_SCALE_MAX)
}

# The scale a piece of *type* is set at: the design's physical scale times the
# reader's bias. Geometry takes `.scale_of()` alone.
.type_of <- function(opts) .scale_of(opts) * .text_of(opts)

# Type size for a label fitted to a slot, under the two rules above.
#
# `want` is what the design asks for at this text scale, `room` the largest
# size the slot can hold, and HEADER_SIZE_MIN * scale the smallest worth
# reading. When `room` falls under that floor the result is `room` itself —
# below the floor, which is exactly what `.header_drawn()` tests for.
.fitted_type <- function(want, room, scale) {
  lo <- HEADER_SIZE_MIN * scale
  if (!is.finite(room)) {
    return(lo)
  }
  .clamp(want, lo, room)
}

# Whether a slot-fitted label is worth drawing at the size it came out at.
.header_drawn <- function(size, scale) {
  isTRUE(is.finite(size) && size >= HEADER_SIZE_MIN * scale)
}

#' Legend type size, at the reader's text scale and inside the plot's height.
#'
#' ggplot2 draws the guide box at whatever its contents need and lets it run
#' off the bottom of the figure — there is no "wrap the guides into a second
#' column", and `legend.box = "vertical"` means exactly one column. So the only
#' two things that can keep a legend on the page are how many keys each guide
#' The mappings and heatmap panels this layout will actually draw a guide for.
#'
#' An inward tree has no room past its tips for anything (`tree_annotations_
#' drawn()`), so its tile strips and heatmap panels are not drawn — and a guide
#' with no marks behind it is not drawn either. Reserved anyway, they left a
#' fifth of the image blank beside a disc with nothing to explain.
#'
#' Mappings on the tree's *own* marks survive, because those are drawn on the
#' tree rather than beside it: a tip label's colour, a tip point's colour or
#' shape. So this is not "an inward tree has no legend" — it is the same rule
#' the builder draws by, said once where the canvas is measured.
#'
#' @param opts List. Resolved tree options.
#' @return List with `layers` and `heatmaps`, each possibly empty.
#' @export
tree_guide_inputs <- function(opts) {
  list(
    layers = Filter(
      function(l) tree_aesthetic_drawn(opts, l$aesthetic %||% NA_character_),
      opts$layers %||% list()
    ),
    heatmaps = if (tree_annotations_drawn(opts)) {
      opts$heatmaps %||% list()
    } else {
      list()
    }
  )
}

#' lists (`tree_legend_max_keys()`) and how large they are set, and this is the
#' second: the requested size, cut back until the box fits the height beside
#' the tree, and no further than a journal's own type floor.
#'
#' Solved by repeated halving-toward-fit rather than in closed form because the
#' row count is itself a function of the size — a smaller guide lists more keys
#' per column, so shrinking does not reduce the rows proportionally. Four
#' passes settle it; the loop stops as soon as it fits.
#'
#' Exported because the canvas budget is solved in the view and has to reserve
#' the guide box at the size it will really be drawn at (see
#' `tree_legend_width_in()`), not at the size the control was declared with.
#' The view and the builder pass the same height, so they agree.
#'
#' @param opts List. Resolved tree options.
#' @param height_in Numeric. Height the plot is drawn at, in inches. Without
#'   it the requested size is returned unchecked.
#' @return Numeric points.
#' @export
tree_legend_size <- function(opts, height_in = NULL, md = NULL) {
  size <- (opts$legend_size %||% 10) * .text_of(opts)
  if (!isTRUE(is.finite(height_in) && height_in > 0)) {
    return(size)
  }
  scale <- .scale_of(opts)
  floor_pt <- MIN_PRINT_PT * scale
  # The guide box gets the figure's height less the plot margin, which is
  # outside it — the same inches the tip-label reserve does not receive.
  room_in <- height_in - PLOT_MARGIN_IN * scale
  guides <- tree_guide_inputs(opts)
  fits <- function(pt) {
    plan <- tree_legend_plan(
      guides$layers,
      guides$heatmaps,
      pt,
      height_in,
      scale,
      md
    )
    need <- plan$rows * .legend_row_in(pt, scale) * LEGEND_HEIGHT_SAFETY
    !isTRUE(is.finite(need)) || need <= room_in
  }
  if (size <= floor_pt || fits(size)) {
    return(max(size, floor_pt))
  }
  # Bisect rather than step towards it. Shrinking the type also *lengthens* the
  # box — a shorter row means more rows fit, so each guide is allowed more keys
  # — and stepping into that feedback converges slowly enough that a fixed
  # number of steps stopped one short and left the last guide off the page.
  lo <- floor_pt
  hi <- size
  for (i in seq_len(LEGEND_SIZE_STEPS)) {
    mid <- (lo + hi) / 2
    if (fits(mid)) lo <- mid else hi <- mid
  }
  lo
}

#' Inches of height the guide box will stand in.
#'
#' The counterpart of `tree_legend_width_in()`, and there for the same reason:
#' the guide box is drawn beside the tree at whatever size it needs, and where
#' the figure is not tall enough for it ggplot2 clips it — the last guide
#' simply is not there, with nothing to say it was. Past a point the type
#' cannot be shrunk any further (`MIN_PRINT_PT`), and then the only honest
#' answer left is a taller canvas.
#'
#' Measured at the size `tree_legend_size()` settles on for the height it is
#' given, so the two agree, and inclusive of the plot margin the box sits
#' inside.
#'
#' @param opts List. Resolved tree options.
#' @param height_in Numeric. Height the plot would otherwise be drawn at.
#' @return Numeric inches.
#' @export
tree_legend_height_in <- function(opts, height_in = NULL, md = NULL) {
  size <- tree_legend_size(opts, height_in, md)
  scale <- .scale_of(opts)
  guides <- tree_guide_inputs(opts)
  plan <- tree_legend_plan(
    guides$layers,
    guides$heatmaps,
    size,
    height_in,
    scale,
    md
  )
  if (!plan$rows) {
    return(0)
  }
  plan$rows *
    .legend_row_in(size, scale) *
    LEGEND_HEIGHT_SAFETY +
    PLOT_MARGIN_IN * scale
}

# Share of a square panel that a radial layout's drawing actually covers.
#
# ggplot2's CoordPolar rescales the radius into the first four tenths of the
# panel and draws the disc about its centre, so a full circle covers four
# fifths of the panel however tightly the axis is fitted to it. The remaining
# fifth is a blank ring no axis setting reaches — and on a figure sized to the
# panel it is the band of white a reader ends up cropping off by hand.
#
# The fit already works in these terms: `tree_budget_in()` hands a radial
# layout `TREE_RADIAL_FRAC` (0.4) of the width, which is this same number as a
# radius. This is the diameter, used where the *image* is being sized rather
# than the drawing inside it.
COORD_POLAR_FRAC <- 0.8

# Whether this plot draws a guide box beside the tree at all.
#
# The margin and the image width are solved from it in two places — the builder
# draws the margin, the view sizes the canvas — and they have to reach the same
# answer or the box is cropped off the edge of a figure that reserved room for
# it.
.has_guides <- function(opts) {
  guides <- tree_guide_inputs(opts)
  length(guides$layers) > 0L ||
    length(Filter(function(h) length(h$cols) > 0L, guides$heatmaps)) > 0L
}

#' How tall a linear tree's panel is drawn, in inches.
#'
#' The aspect ratio says how tall the tree wants to be and the guide box says
#' how tall it has to be to hold the keys; the ceiling says what the image can
#' actually be. All three have to be settled in one place, because the answer
#' is both the canvas the view reserves *and* the height every reserve inside
#' the drawing is measured against — a header band in rows, the axis numbers'
#' depth, the tip pitch the labels are fitted to. Solved twice, they drifted:
#' the view capped the image and the builder did not, so at a thousand tips the
#' figure was designed for 27.5 inches and printed on 14.3.
#'
#' The ceiling binds what the *engine* asks for, not what the reader does. A
#' guide box may grow the canvas up to it and no further; the aspect ratio is
#' the reader's own control and is drawn as set, and the fit never asks for
#' more than the ceiling anyway (`TIP_ASPECT_MAX`).
#'
#' Radial layouts have no aspect ratio — their panel is square and its side is
#' `tree_panel_width_in()` — so this is for linear ones only.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame or NULL. Tip metadata, for the guide box's height.
#' @return Numeric inches.
#' @export
tree_canvas_height_in <- function(opts, md = NULL) {
  base <- opts$width_in %||% 5.5
  if (!isTRUE(is.finite(base) && base > 0)) {
    base <- 5.5
  }
  aspect <- suppressWarnings(as.numeric(opts$aspect %||% 1))
  if (length(aspect) != 1L || !is.finite(aspect) || aspect <= 0) {
    aspect <- 1
  }
  want <- base * aspect
  max(want, min(tree_legend_height_in(opts, want, md), base * TREE_CANVAS_MAX_FACTOR))
}

#' Height the image stands at, for a panel of a given side.
#'
#' The two are the same thing on a linear tree and are not on a radial one: a
#' disc covers `COORD_POLAR_FRAC` of the square it is drawn in, so an image
#' sized to the square carries a tenth of its height in blank ring above the
#' drawing and another tenth below. The image is sized to the *disc* instead,
#' and the panel is allowed to overflow it — what leaves the page is ring.
#'
#' `tree_plot_margin_in()` is the other half of the same arrangement and has to
#' be read with it: the panel only reaches its full side because the margin is
#' pulled in by exactly what this leaves out.
#'
#' @param opts List. Resolved tree options.
#' @param panel_in Numeric. The panel's side, in inches.
#' @return Numeric inches.
#' @export
tree_image_height_in <- function(opts, panel_in) {
  if (!.is_circular(opts)) {
    return(panel_in)
  }
  .drawn_panel_in(opts, panel_in) * COORD_POLAR_FRAC +
    PLOT_MARGIN_IN * .scale_of(opts)
}

#' Width the image stands at, for a panel of a given side.
#'
#' A radial figure with no guides is its disc, so the image is square. With
#' guides it is the disc, the guide box, and — between them — the one piece of
#' ring that cannot be cropped: the box is a gtable column outside the panel,
#' so the panel's own right-hand ring stands between the drawing and the first
#' key whatever the margin does.
#'
#' @param opts List. Resolved tree options.
#' @param panel_in Numeric. The panel's side, in inches.
#' @param legend_in Numeric. Width the guide box was reserved, in inches.
#' @return Numeric inches.
#' @export
tree_image_width_in <- function(opts, panel_in, legend_in = 0) {
  if (!.is_circular(opts)) {
    return(panel_in + legend_in)
  }
  disc <- tree_image_height_in(opts, panel_in)
  if (!.has_guides(opts)) {
    return(disc)
  }
  disc +
    (1 - COORD_POLAR_FRAC) / 2 * .drawn_panel_in(opts, panel_in) +
    legend_in
}

#' The margin drawn around the plot, in inches, clockwise from the top.
#'
#' Negative on a radial layout, which is not a mistake: the image is sized to
#' the disc (`tree_image_height_in()`), so the square panel the disc is drawn
#' in is larger than the image and has to hang over its edges to be drawn at
#' full size. Only blank ring hangs over — the drawing is bounded by the disc,
#' and the ordinary margin is still there between the disc and the paper.
#'
#' The right edge is the exception, and only when there are guides: a negative
#' margin there does not crop the ring, it pushes the guide box that many
#' inches past the edge of the paper. So that side keeps its ordinary margin
#' and the ring under the box stays, which `tree_image_width_in()` counts.
#'
#' @param opts List. Resolved tree options.
#' @param panel_in Numeric. The panel's side, in inches.
#' @return Numeric vector of four inches: top, right, bottom, left.
#' @export
tree_plot_margin_in <- function(opts, panel_in) {
  edge <- PLOT_MARGIN_PT * .scale_of(opts) / 72
  if (!.is_circular(opts) || !isTRUE(is.finite(panel_in) && panel_in > 0)) {
    return(rep(edge, 4L))
  }
  crop <- edge -
    (1 - COORD_POLAR_FRAC) / 2 * .drawn_panel_in(opts, panel_in)
  c(crop, if (.has_guides(opts)) edge else crop, crop, crop)
}

# Branch-label type size, at the reader's text scale. Which branches can hold
# a number at that size is then `tree_branch_keep()`'s decision, so growing the
# type thins the labelling rather than overprinting it.
.branch_size <- function(opts) {
  (opts$branch_size %||% TREE_FIT_DEFAULTS$branch_size) * .text_of(opts)
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
# together. The class names are shrunk to keep the band inside this
# (`.class_name_size()`), rather than the band being clamped and the names
# clipped at their far end; matched to HEADER_FRAC_MAX, the same ceiling the
# column headers above the matrix answer to.
CLASS_FRAC_MAX <- 0.45

# What a *clustered* panel puts in that same band instead of the brackets.
#
# Clustering orders the columns by call pattern, so the classes are scattered
# and a bracket over them would claim a grouping the matrix does not have (see
# heatmap_class_runs). A colour strip says the same thing without claiming
# contiguity — one tile per column, keyed in the guide — which is the swap the
# AMR-plot engine makes for the same reason under "Cluster All".
#
# Under it hangs the dendrogram the ordering came from, leaves up against the
# strip. Outward from the matrix that reads matrix / annotation / tree, the
# same order ComplexHeatmap stacks a column dendrogram and its annotations in
# on the AMR tab, mirrored because the room here is below rather than above —
# above is spoken for by the gene names.
CLASS_STRIP_GAP_ROWS <- 0.4
CLASS_STRIP_ROWS <- 0.9
DEND_GAP_ROWS <- 0.5

# What that band is worth as a share of the figure, in percent of the tip count.
#
# The three constants above are tip rows, which is the right unit for a band
# beside a matrix whose cells are tip rows — until there are a thousand of
# them. A tenth of a percent of the panel is not a strip, it is a line: the
# drug classes under a heatmap of 991 isolates were a coloured hairline nobody
# could read a colour off.
#
# The dendrogram that hangs under the same strip has always been a percentage
# of the tip count (`.dend_rows()`), so that it stays the same share of the
# figure whatever the tree's height. This is that rule applied to the band
# above it — the whole group scaled together, so the composition the constants
# describe is preserved and only its size follows the figure.
#
# A share *or* the rows, whichever is larger: a small tree keeps exactly what
# it drew before, and the strip only starts growing where a fixed row stops
# being visible (around ninety tips).
CLASS_STRIP_PCT <- 1

# How much of a clustered panel's band is scaled up for the tip count.
.class_band_scale <- function(n_tip) {
  n <- max(as.numeric(n_tip %||% 1), 1)
  max(n * CLASS_STRIP_PCT / 100 / CLASS_STRIP_ROWS, 1)
}

# --- The element-type label over (or under) a panel --------------------------
#
# "Resistance" / "Virulence" / "Stress" — which screen the panel's columns came
# out of. Two panels side by side are otherwise told apart only by their guide
# titles, which sit off to the side of the figure and pair with a matrix by
# colour rather than by position.
#
# Set horizontally and centred on the panel's own run of columns, unlike every
# other annotation here, which is vertical. It is one short word over a run
# several columns wide, so it reads across; and horizontal is what keeps it to a
# single row of pitch whatever it says, which is the whole reason the reserve it
# costs can be a constant rather than a measurement.
#
# Which end it goes to is the reader's (`element_pos`): over the gene names, or
# under whatever the band below already holds.
ELEMENT_LABEL_GAP_ROWS <- 0.55

# Default end for that label, when a panel names none.
#' @export
ELEMENT_POS_DEFAULT <- "top"

# Rows of tip pitch one horizontal line of type at `size` mm claims. The
# vertical headers' own measure with a single character's worth of height, which
# is exactly what a horizontal line is.
.element_label_rows <- function(size) {
  HEADER_CHAR_ROWS * size / HEADER_SIZE_MAX
}

# Millimetres of tip pitch the plot is actually drawn at.
#
# Not `height_in / n_tip`. The reserves above and below the tips are *expansion*
# on the y scale, so booking them compresses the rows into what is left — a tall
# stack of gene names and a deep class band together can take a third of the
# panel, and the rows then come out a third shorter than the nominal pitch.
#
# Every other annotation here is placed in rows and rides that compression
# without noticing, because it is measured in rows too. The element label is the
# exception: it has to clear a stack of *rotated text*, whose height is fixed in
# millimetres however the rows move under it. Converting that height at the
# nominal pitch is what drew it through the middle of the gene names.
#
# One pass, not a fixed point: the label's own row is already in the reserves
# this reads, so re-solving would move the pitch by less than the gap absorbs.
.drawn_row_mm <- function(height_in, span_rows, top_frac, bottom_frac) {
  if (
    is.null(height_in) ||
      !is.finite(height_in) ||
      height_in <= 0 ||
      !is.finite(span_rows) ||
      span_rows <= 0
  ) {
    return(25.4 * TIP_ROW_IN)
  }
  25.4 * height_in / (span_rows * (1 + top_frac + bottom_frac))
}

# Rows that a stack of `chars` rotated characters at `size` mm occupies, at the
# pitch the plot is really drawn at.
.rotated_text_rows <- function(chars, size, row_mm) {
  if (!is.finite(row_mm) || row_mm <= 0) {
    return(0)
  }
  HEADER_ROW_PACK * chars * TIP_CHAR_EM * size / row_mm
}

# Type size the element label is set at: fitted so the whole word fits across
# the run of columns it names, and never larger than a column header.
#
# The floor is that panel's *own* header size, not HEADER_SIZE_MIN — a title
# set smaller than the names it titles reads as a footnote, and a three-column
# panel fitted honestly to "Virulence" lands far below legible. A word too wide
# for its panel at that floor overhangs into the gutter beside it instead, which
# is the one place on the figure there is room to lend: the panels are held
# apart by HEATMAP_GAP and the outermost one has the legend's own margin past
# it.
.element_label_size <- function(
  label,
  run_units,
  axis_units,
  panel_in,
  scale,
  floor_size = HEADER_SIZE_MIN * scale,
  text = 1
) {
  lo <- floor_size
  hi <- max(HEADER_SIZE_MAX * scale * text, lo)
  chars <- suppressWarnings(max(nchar(label %||% ""), 1L))
  if (
    !is.finite(run_units) ||
      !is.finite(axis_units) ||
      axis_units <= 0 ||
      !is.finite(chars)
  ) {
    return(lo)
  }
  run_mm <- 25.4 * panel_in * run_units / axis_units
  .clamp(run_mm * HEADER_FILL / (chars * TIP_CHAR_EM), lo, hi)
}

# Whether a panel draws its element-type label at all. Gene-level panels only:
# the dormant presence/absence branch has no element type to name.
.element_label_drawn <- function(panel) {
  isTRUE(panel$show_element_type) &&
    identical(panel$level, "gene") &&
    nzchar(.element_label_text(panel))
}

# What that label says. The panel's own title minus the " genes" the view
# appends to it, so the label is the element type itself rather than a second
# copy of the guide's heading.
.element_label_text <- function(panel) {
  txt <- as.character(panel$title %||% "")
  if (!length(txt) || is.na(txt[[1]])) {
    return("")
  }
  trimws(sub("\\s+genes$", "", txt[[1]]))
}

# Which end a panel's element label goes to, defaulted and validated — an
# unrecognised value reads as the default rather than drawing nothing.
.element_pos <- function(panel) {
  pos <- panel$element_pos %||% ELEMENT_POS_DEFAULT
  if (identical(pos, "bottom")) "bottom" else "top"
}

# Depth of the dendrogram, as a percentage of the tip count — so it stays the
# same share of the figure whatever the tree's height, the way the AMR tab's
# dendrogram keeps the centimetres it was given. The panel carries its own
# (`dend_depth`, the edit modal's slider); this is what a panel that carries
# none falls back to. Zero — the default — keeps the clustered column order but
# draws no tree, which is what a reader who wants the blocks but not the
# dendrogram gets without touching the slider.
#' @export
DEND_DEPTH_DEFAULT <- 0

# The floor under that share, for the small trees a percentage would leave
# invisible. Not applied at zero, which is a deliberate "no dendrogram".
DEND_MIN_ROWS <- 2.5

.dend_rows <- function(n_tip, panel) {
  pct <- suppressWarnings(as.numeric(panel$dend_depth %||% DEND_DEPTH_DEFAULT))
  if (!is.finite(pct) || pct <= 0) {
    return(0)
  }
  max(as.numeric(n_tip) * pct / 100, DEND_MIN_ROWS)
}

# The class strip's default palette. visualization_amr.R's own
# CLASS_SCALE_DEFAULT, so a drug class starts the same colour on both engines'
# heatmaps. A panel may override it with its own `strip_scale`.
#' @export
CLASS_STRIP_SCALE <- "Set2"

#' One qualitative family per element type for the class strips.
#'
#' Two panels' strips on the same palette read as one scale: the reader takes
#' the aminoglycoside green under the resistance matrix and the mercury green
#' under the stress one for the same class, which they are not — they are not
#' even drawn from the same vocabulary. A family per element type is what makes
#' the two strips visibly separate keys. Fitted to the class count before it is
#' used (`amr_fit_scale()`), so a panel with more classes than its family has
#' colours falls through to one that can carry them.
#' @export
CLASS_STRIP_SCALES <- c(
  Resistance = "Set2",
  Virulence = "Dark2",
  Stress = "Accent",
  Unclassified = "Pastel1"
)

# What a panel's element type is called on the figure. The record carries the
# database's own code ("AMR", "STRESS"); `AMR_ELEMENT_TYPES` names it.
.element_label <- function(panel) {
  el <- as.character(panel$element %||% "")
  if (length(el) != 1L || is.na(el) || !nzchar(el)) {
    return("")
  }
  hit <- names(AMR_ELEMENT_TYPES)[match(el, AMR_ELEMENT_TYPES)]
  if (is.na(hit)) el else hit
}

# The palette one panel's class strip draws under: the reader's own pick where
# the Colors tab has made one, else the family its element type owns.
.class_strip_scale <- function(panel) {
  pick <- panel$strip_scale
  if (!is.null(pick) && length(pick) == 1L && !is.na(pick) && nzchar(pick)) {
    return(as.character(pick))
  }
  el <- .element_label(panel)
  if (el %in% names(CLASS_STRIP_SCALES)) {
    CLASS_STRIP_SCALES[[el]]
  } else {
    CLASS_STRIP_SCALE
  }
}

# Title over the class strip's guide.
CLASS_STRIP_TITLE <- "Drug class"

# The same, said of one panel: "Resistance drug class", not a second "Drug
# class" the reader has to work out the owner of from where it sits in the box.
.class_guide_title <- function(panel) {
  el <- .element_label(panel)
  if (!nzchar(el)) {
    return(CLASS_STRIP_TITLE)
  }
  paste(el, tolower(CLASS_STRIP_TITLE))
}

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

# --- What one tip row can hold -----------------------------------------------
#
# The vertical half of the tip-label rule. `.tiplab_frac()` below books the
# room the labels need *across* the panel and the canvas grows for it; nothing
# grows for the room they need *down* it, because that room is one tip row and
# the row pitch is fixed by the tip count and the aspect ratio. So this is the
# one dimension a label can be asked for more of than exists — which is what
# turns three hundred isolate names into a black band, and what the text-size
# control would do to any tree if it were only a multiplier.

# Millimetres of type the labels have room for, for this plot's actual shape.
#
# `tree_tiplab_room()` solved against the reserve's own ceiling rather than the
# fit's share, so the answer is what will fit rather than what looks best — the
# gap between the two is the headroom the text-size control spends. Long
# isolate names (a 36-character assembly accession is ordinary) are what make
# the width half the binding constraint on a small tree; the tip count makes
# the row half bind on a large one.
.tiplab_room <- function(opts, md) {
  n <- if (is.null(md)) 0L else nrow(md)
  if (!isTRUE(is.finite(n)) || n < 1L) {
    return(Inf)
  }
  room <- tree_tiplab_room(
    n,
    opts$width_in %||% 5.5,
    opts$layout %||% "rectangular",
    .tiplab_chars(opts, md),
    opts$aspect %||% 1,
    # The hard limit rather than the fit's own share: what is being asked here
    # is what will fit, not what looks best.
    .tiplab_cap(opts),
    .tiplab_point_gap_mm(opts)
  )
  # The rows the labels are really set on, once the builder has solved the
  # reserves that compress them (see `row_mm` there). Without it the labels are
  # fitted to a pitch the plot does not have: the header band over a heatmap
  # can take more than half the height, and thirty isolate names sized for the
  # nominal pitch then print as one black bar. Linear only — a radial tree's
  # rows are arcs and no y-scale expansion touches them.
  if (!.is_circular(opts) && isTRUE(is.finite(opts$row_mm %||% NA))) {
    room <- min(room, TIP_ROW_FILL * opts$row_mm)
  }
  room
}

# Most of the tree-and-labels budget the labels may claim, by layout.
.tiplab_cap <- function(opts) {
  if (identical(opts$layout, "inward")) {
    1 - INWARD_CORE_FRAC - INWARD_TREE_MIN
  } else if (.is_circular(opts)) {
    TIP_RING_FRAC_MAX
  } else {
    TIP_LABEL_AXIS_MAX
  }
}

# Characters in the longest label this plot will set.
#
# The label source is a control the server resolves from the loaded database,
# so it is legitimately unset — or naming a column a since-replaced database no
# longer has — every time something asks about the labels before a plot exists.
# The builder validates it against the frame before it draws; everything
# upstream of that reaches here, and a missing column is one character rather
# than an error.
.tiplab_chars <- function(opts, md) {
  field <- opts$tiplab
  known <- length(field) == 1L &&
    !is.na(field) &&
    isTRUE(field %in% names(md))
  if (!known) {
    return(1L)
  }
  chars <- suppressWarnings(max(nchar(as.character(md[[field]])), 1L))
  if (!is.finite(chars) || chars < 1L) 1L else chars
}

# The size the reader has asked for, before the row it has to fit in is
# considered.
.tiplab_want <- function(opts) {
  (opts$tiplab_size %||% TREE_FIT_DEFAULTS$tiplab_size) * .text_of(opts)
}

# The size the labels are actually set at: what was asked for, never larger
# than the row can hold and never smaller than legible.
#
# TIPLAB_ROOM_SLACK_MM keeps the cap off `tree_auto_layout()`'s own answer.
# That fit lands the size on the room and then rounds it to one decimal, so a
# bare `min()` would shave a rounded-up fit on every draw — invisible on the
# page, but enough to make "the fit is what the room holds" false in a test.
# Absolute, not proportional, because that is what rounding to one decimal is:
# a fraction would be far too generous at the sizes a crowded tree sets.
TIPLAB_ROOM_SLACK_MM <- 0.05

.tiplab_size <- function(opts, md) {
  room <- .tiplab_room(opts, md) + TIPLAB_ROOM_SLACK_MM
  floor_mm <- TIP_SIZE_FLOOR * .scale_of(opts)
  # Never past the room, never under what can be read. Where the room itself is
  # under the floor the two rules cannot both hold and the labels are left off
  # instead (`tree_tiplab_drawn()`), so the floor wins here without consequence.
  .clamp(.tiplab_want(opts), floor_mm, max(room, floor_mm))
}

#' Whether the isolate labels can be drawn legibly at all.
#'
#' False when a tip row cannot hold type at the smallest size worth reading,
#' whatever the reader has asked for. The labels are then not drawn — shrinking
#' them further would produce a black band rather than a list of names, and
#' drawing them at a legible size would produce the same band with the names on
#' top of each other.
#'
#' Exported because the view has to say so in the sidebar: labels vanishing
#' with no explanation reads as a bug.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame. Per-tip metadata.
#' @return TRUE when the labels are drawn.
#' @export
tree_tiplab_drawn <- function(opts, md) {
  if (!isTRUE(opts$tiplab_show)) {
    return(FALSE)
  }
  room <- .tiplab_room(opts, md) + TIPLAB_ROOM_SLACK_MM
  isTRUE(!is.finite(room) || room >= TIP_SIZE_FLOOR * .scale_of(opts))
}

.tiplab_frac <- function(opts, md) {
  if (!tree_tiplab_drawn(opts, md)) {
    # No label, so nothing to reserve — just a hair of clearance so the first
    # strip does not butt straight onto the leader-line dots and the tip
    # points. Kept small on purpose: with the labels off the reader wants the
    # strip up against the tree, not floated off it.
    return(0.008)
  }
  .tiplab_frac_at(opts, md, .tiplab_size(opts, md))
}

# The reserve for a stated label size. Split out from `.tiplab_frac()` because
# the circular row solve needs it before the size is capped (see
# `.tiplab_room()`), and running it at the capped size there would be circular.
.tiplab_frac_at <- function(opts, md, size_mm) {
  w <- opts$width_in
  if (is.null(w) || !is.finite(w) || w <= 0) {
    return(0.375)
  }
  cap <- .tiplab_cap(opts)

  # Inches of label text — a mean-advance estimate. `TIP_CHAR_EM` was checked
  # against the real grid text path (`grobWidth`) for an accession at tip size
  # and lands within ~2%, so measuring per render buys nothing and would only
  # make the reserve depend on device and font state.
  label_in <- .tiplab_chars(opts, md) * TIP_CHAR_EM * size_mm / 25.4

  # Plus the nudge that holds the label off its tip point: the label starts
  # there, so the reserve — and the strip placed past it — has to account for
  # it. X_EXPANSION covers only the residual after this: the finished panel
  # coming out a shade narrower than its budget.
  label_in <- label_in + .tiplab_point_gap_mm(opts) / 25.4

  # Against the inches the *panel* gets, not the inches the figure is. The plot
  # margin is outside the panel, so a reserve taken as a fraction of the full
  # width buys fewer inches than it asked for — three tenths of a percent short
  # at an accession's length, which is a clipped final glyph.
  margin_in <- PLOT_MARGIN_IN * .scale_of(opts)
  min(cap, label_in / max(tree_budget_in(opts) - margin_in, 0.5))
}

# Safety factor on the tip-label reserve.
#
# `.tiplab_frac()` now measures the real rendered width of the widest label
# rather than estimating it, so this no longer has to cover a biased estimate —
# it is a small margin for the two things measurement still cannot see: the
# finished ggplot panel coming out a little narrower than `opts$width_in` once
# its gtable has taken a column for the legend, and the tip-point nudge the
# label carries (`.tiplab_point_gap_mm`). It was 1.28 back when it also had to
# absorb a `chars * 0.6 * size` estimate that ran ~15% long for an accession;
# with the measurement in place that slack became a visible band of dead space
# between the labels and the strip.
#
# It is not the gutter between the labels and the first annotation: that is
# ANNOTATION_LEAD. This only stops the labels from crossing into it.
X_EXPANSION <- 1.06

# The plot margin, in inches: `plot.margin` in the builder's theme, both sides,
# at scale 1. Outside the panel, so it is width the tip-label reserve never
# receives — and it moves with `opts$scale` like every other physical length
# here, or a figure drawn twice the size would not be the same figure.
PLOT_MARGIN_PT <- 6
PLOT_MARGIN_IN <- 2 * PLOT_MARGIN_PT / 72

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

# How much larger a guide's title is set than its keys.
LEGEND_TITLE_RATIO <- 1.1

# Legend geometry, in inches at legend_size 10.
LEGEND_KEY_IN <- 0.16 # key square plus its gap
LEGEND_PAD_IN <- 0.12 # box padding either side
# A sanity ceiling on one guide column's estimated width, not a budget the
# legend is squeezed into. ggplot2 draws the guide box at whatever its widest
# (wrapped) label needs and takes the room from the panel if the canvas is too
# narrow — so an estimate clamped below what will actually be drawn does not
# shrink the legend, it silently steals from the tip labels. It was 0.35, which
# a wrapped-but-unbreakable taxon name ("pneumoniae/variicola/quasipneumoniae")
# blew straight past; the real backstop is CANVAS_MAX_FACTOR in the view.
LEGEND_MAX_FRAC <- 0.6
# One key row, title line or inter-guide gap, in inches.
#
# Two terms because the drawn row is two things: the key square, which follows
# the type size (`legend.key.size` is set from it), and the spacing ggplot2 puts
# between keys, which does not. Measured off real guide boxes at 6.5, 10 and 20
# pt — a single proportional term fitted the largest of those and ran 18% short
# at the smallest, which is exactly where it matters, since shrinking the type
# is how a tall legend is made to fit at all. It stopped shrinking one step too
# early and the last guide ran off the bottom.
LEGEND_ROW_PAD_IN <- 0.040
LEGEND_ROW_PT_IN <- 0.017
LEGEND_MAX_COLS <- 3L # past this the guides are wider than the tree
LEGEND_MAX_ROWS <- 18L # keys in one column before they wrap into another

# Columns one guide's own keys may wrap into.
#
# One fold, not three. Wrapping is what keeps a guide taller than its share of
# the box on the page at all, and the key budget is allowed to count on it —
# which makes it a way of buying keys, and at four columns it buys them with
# width the tree is holding. Past a fold the answer is fewer keys.
LEGEND_KEY_COLS <- 2L

.legend_row_in <- function(legend_size = 10, scale = 1) {
  size <- suppressWarnings(as.numeric(legend_size %||% 10))
  if (length(size) != 1L || !is.finite(size) || size <= 0) {
    size <- 10
  }
  (LEGEND_ROW_PAD_IN + LEGEND_ROW_PT_IN * size) * scale
}
# Rounding-up factor on the finished legend-width estimate — `guide_in` counts
# a few percent short against real title and key spacing. Applied in
# tree_legend_width_in(); see the note there.
LEGEND_SAFETY <- 1.03

# Rounding-up factor on the guide box's estimated *height*, used only by
# `tree_legend_size()`. `.legend_row_in()` is measured against plain guide
# boxes; a title that wraps onto a second line, or a guide whose keys wrap into
# a second column, runs taller than the row count says. Across the test
# databases the worst of those — nine guides on a squat figure, half of them
# wrapped — came out a quarter over the row count, so this covers it. It only
# bites where the box has to be shrunk or the canvas grown at all: a legend
# that already fits is left at the size it asked for.
LEGEND_HEIGHT_SAFETY <- 1.3

# Bisection steps `tree_legend_size()` takes between the legibility floor and
# the size that was asked for. Twelve halvings of a 15pt range settle it to
# under a hundredth of a point, which is finer than the answer means.
LEGEND_SIZE_STEPS <- 12L

# Keys one mapping layer's guide would list if nothing trimmed it.
#
# Not the layer's own `n_levels`, which counts the values the column *holds*.
# The guide lists the scale's levels, and those are two different numbers
# whenever the column has gaps in it — "Not recorded" is a level with a swatch
# and no value behind it. Budgeting for the smaller of the two is what listed
# a four-level scale as "2 of 4 shown" beside a figure with room for forty.
.layer_demand <- function(layer, md = NULL) {
  field <- layer$field %||% NA_character_
  if (
    !is.null(md) &&
      length(field) == 1L &&
      !is.na(field) &&
      field %in% names(md)
  ) {
    v <- mapped_values(md[[field]])
    if (is.factor(v)) {
      return(max(length(levels(v)), 1L))
    }
  }
  max(as.integer(layer$n_levels %||% 1L), 1L)
}

#' The guide box's plan: which guides it holds, how many keys each may list,
#' and the order they stack in.
#'
#' One solve for the whole box, because the questions are not separable: what a
#' guide may list depends on what the others need, and what order they stack in
#' is the difference between a heatmap's two guides reading as a pair and them
#' being scattered through the mapped variables.
#'
#' The order is the reading order of the figure itself — the mapped variables,
#' which are drawn against the tips, then each heatmap panel's own two guides
#' (its confidence tiers, then the drug classes under it) in the order the
#' panels are drawn left to right.
#'
#' @param layers List of mapping layer records.
#' @param heatmaps List of heatmap panel records.
#' @param legend_size Numeric. Legend text size in points.
#' @param height_in Numeric. Height the plot is drawn at, in inches.
#' @param scale Numeric. This plot's physical scale.
#' @param md Data frame. The metadata the scales are built from, for the level
#'   counts. Optional; without it the layers' recorded counts are used.
#' @return list(ids, demand, keys, order, rows), the last four named by id.
#' @export
tree_legend_plan <- function(
  layers,
  heatmaps = list(),
  legend_size = 10,
  height_in = NULL,
  scale = 1,
  md = NULL
) {
  ls <- layers %||% list()
  drawn <- Filter(function(h) length(h$cols) > 0L, heatmaps %||% list())
  ids <- character(0)
  demand <- integer(0)
  add <- function(id, n) {
    ids <<- c(ids, id)
    demand <<- c(demand, max(as.integer(n), 1L))
  }
  for (l in ls) {
    add(legend_guide_id("layer", l), .layer_demand(l, md))
  }
  for (i in seq_along(drawn)) {
    h <- drawn[[i]]
    add(legend_guide_id("heat", h, i), length(.tier_guide_levels(h)))
    n <- length(.class_guide_levels(h))
    if (n) {
      add(legend_guide_id("class", h, i), n)
    }
  }
  keys <- tree_legend_key_budget(
    demand,
    tree_legend_room(legend_size, height_in, scale)
  )
  # A guide too tall for its share wraps its own keys into a second column
  # rather than being cut back further — ggplot2 will not wrap the box itself,
  # and a stack of guides that runs off the bottom is simply clipped. Kept for
  # the whole box, so the guides that wrap all wrap the same way.
  #
  # The budget above does not count on it: costing a guide at one column while
  # the render folds it is conservative, and the alternative is worse — a
  # budget that can buy keys by folding spends the tree's width on them.
  max_rows <- tree_legend_max_rows(
    layers,
    heatmaps,
    legend_size,
    height_in,
    scale
  )
  ncol <- vapply(keys, tree_legend_ncol, integer(1), max_rows = max_rows)
  list(
    ids = ids,
    demand = setNames(demand, ids),
    keys = setNames(keys, ids),
    ncol = setNames(as.integer(ncol), ids),
    order = setNames(seq_along(ids), ids),
    max_rows = max_rows,
    rows = as.integer(sum(.legend_guide_rows(keys, demand, ncol)))
  )
}

#' The name one guide is filed under in a `tree_legend_plan()`.
#'
#' The plan is solved before the layers are assembled and read back as each
#' scale is built, so the two have to agree on what a guide is called without
#' passing an index around. A mapping layer is named by what it maps; a panel
#' by its own id, falling back to its position for a record old enough not to
#' carry one.
#'
#' @param kind One of "layer", "heat", "class".
#' @param x The layer or panel record.
#' @param i Integer. Its position, for a panel with no id.
#' @return Character.
#' @export
legend_guide_id <- function(kind, x, i = 0L) {
  if (identical(kind, "layer")) {
    return(paste0("layer:", x$aesthetic %||% "", ":", x$field %||% ""))
  }
  id <- x$id %||% NA_character_
  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    id <- as.character(i)
  }
  paste0(kind, ":", id)
}

# Keys and stacking order for one guide, from the plan the builder solved.
# A guide the plan did not see — nothing should reach here, but a scale built
# outside the loop would — takes the floor and stacks last.
.plan_keys <- function(plan, id) {
  k <- (plan$keys %||% integer(0))[id]
  if (length(k) != 1L || is.na(k)) LEGEND_MIN_KEYS else as.integer(k)
}

.plan_order <- function(plan, id) {
  o <- (plan$order %||% integer(0))[id]
  if (length(o) != 1L || is.na(o)) 99L else as.integer(o)
}

.plan_ncol <- function(plan, id) {
  n <- (plan$ncol %||% integer(0))[id]
  if (length(n) != 1L || is.na(n)) 1L else as.integer(n)
}

#' Rows one guide box has room for, at the height the plot is drawn.
#'
#' The same inches, and the same rounding-up on them, that `tree_legend_size()`
#' tests the finished box against — the plot margin the box sits inside is
#' taken off, and each row is costed at `LEGEND_HEIGHT_SAFETY`. Measured any
#' other way the two disagree, and they disagree in the worst direction: the
#' budget hands out keys until the raw height is full, the fit then finds the
#' box too tall and shrinks the type to the print floor to hold what the budget
#' had already promised.
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
  row_in <- .legend_row_in(legend_size, scale) * LEGEND_HEIGHT_SAFETY
  usable <- height_in - PLOT_MARGIN_IN * scale
  max(as.integer(floor(usable / row_in)), 1L)
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
  drawn <- Filter(function(h) length(h$cols) > 0L, heatmaps %||% list())
  # A clustered panel shows two guides, its tiers and its drug-class strip.
  guides <- length(layers %||% list()) +
    length(drawn) +
    sum(vapply(drawn, function(h) length(.class_guide_levels(h)) > 0L, logical(1)))
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
  scale = 1,
  md = NULL
) {
  rows <- tree_legend_plan(
    layers,
    heatmaps,
    legend_size,
    height_in,
    scale,
    md
  )$rows
  room <- tree_legend_room(legend_size, height_in, scale)
  as.integer(min(max(ceiling(rows / room), 1), LEGEND_MAX_COLS))
}

# Longest line a legend string draws as, after the same wrapping the guide
# itself renders under (`.wrap_legend_labels`). The width estimate has to count
# what ggplot2 will actually set, not the raw string: a taxon name like
# "Klebsiella pneumoniae/variicola/quasipneumoniae" wraps to a ~35-character
# line, and measuring the un-wrapped 47 was half the reason the estimate came
# in low and the panel — and the tip-label reserve with it — got squeezed.
.legend_text_cols <- function(x) {
  wrapped <- .wrap_legend_labels(as.character(x %||% ""))
  lines <- unlist(strsplit(wrapped, "\n", fixed = TRUE))
  suppressWarnings(max(nchar(lines), 0L))
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
#' built on, applied to the legend's own text — after the same wrapping the
#' guide renders under, so a long category name is measured as the line it
#' becomes and not as the string it started as. A vertical box stacks its
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
  # The same key-wrapping budget the renderer solves each guide against
  # (tree_legend_max_rows, used at build time). Estimating every guide at one
  # key column while the render wrapped a nine-key scale into two is how the
  # legend came out wider than budgeted and squeezed the tip labels.
  max_rows <- tree_legend_max_rows(layers, heatmaps, size, height_in, scale)
  plan <- tree_legend_plan(layers, heatmaps, size, height_in, scale, md)
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
      id <- legend_guide_id("layer", l)
      labs <- head(unique(as.character(md[[l$field]])), .plan_keys(plan, id))
      chars <- suppressWarnings(max(
        vapply(c(labs, l$title %||% ""), .legend_text_cols, integer(1)),
        1L
      ))
      guide_in(chars, .plan_ncol(plan, id))
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
      states <- .tier_guide_levels(h, AMR_CONFIDENCE_STATES)
      chars <- suppressWarnings(max(
        vapply(c(states, h$title %||% ""), .legend_text_cols, integer(1)),
        1L
      ))
      guide_in(chars, .plan_ncol(plan, legend_guide_id("heat", h)))
    },
    numeric(1)
  )
  # A clustered panel's second guide names drug classes, which run far longer
  # than a tier does — left out, the strip's keys were drawn over the tips.
  strip <- vapply(
    seq_along(heatmaps),
    function(i) {
      h <- heatmaps[[i]]
      id <- legend_guide_id("class", h, i)
      lvls <- head(.class_guide_levels(h), .plan_keys(plan, id))
      if (!length(lvls)) {
        return(0)
      }
      chars <- suppressWarnings(max(
        vapply(
          c(lvls, .class_guide_title(h)),
          .legend_text_cols,
          integer(1)
        ),
        1L
      ))
      guide_in(chars, .plan_ncol(plan, id))
    },
    numeric(1)
  )
  widest <- max(c(per, heat, strip, 0))
  if (widest <= 0) {
    return(0)
  }
  # A box that has to flow into several columns is that many times as wide.
  cols <- tree_legend_cols(layers, heatmaps, size, height_in, scale, md)
  box <- cols * min(widest + LEGEND_PAD_IN * scale, LEGEND_MAX_FRAC * w)
  # LEGEND_SAFETY rounds the estimate up: `guide_in` counts a few percent short
  # against a real guide's title and key spacing, and this estimate has one job
  # — keep the canvas wide enough that the panel gets its whole budget and the
  # tip-label reserve holds. Over is free (the canvas grows, the tree does not
  # shrink); under is the band of squeezed labels this path exists to stop.
  box * LEGEND_SAFETY
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

# Two-colour fill for the presence/absence branch of `.heatmap_frame()` — the
# renderer's general "draw these metadata columns as a matrix" path. No tree
# control now produces it, but a caller may still pass one and it has to render.
AMR_PRESENT <- "Detected"
AMR_ABSENT <- "Absent"

AMR_PRESENCE_FILL <- c(
  Detected = "#B2182B",
  Absent = "#EDEDED"
)

# The one tier a gene panel's guide lists only when the screen reached it.
#
# "Putative" is an HMM-only call: real, but rare enough that most screens never
# produce one, and a key for a tier nothing on the figure carries is a category
# the reader goes looking for and cannot find.
AMR_TIER_RARE <- "Putative"

#' The confidence tiers one panel's guide lists.
#'
#' Fixed rather than taken from the data. A gene panel is a *scale*, and a
#' scale that lists two tiers on one panel and four on the next beside it says
#' the two were measured differently — they were not; the stress panel simply
#' had no partial call in it. Absent is the case the reader most needs the key
#' for, and it is precisely the one an all-positive panel would drop.
#'
#' @param panel List. A heatmap panel record.
#' @param seen Character. Tiers the panel's own data reached, for the rare one.
#' @return Character vector of levels, weakest first.
.tier_guide_levels <- function(panel, seen = character(0)) {
  if (!identical(panel$level, "gene")) {
    return(c(AMR_PRESENT, AMR_ABSENT))
  }
  keep <- AMR_CONFIDENCE_STATES != AMR_TIER_RARE |
    AMR_CONFIDENCE_STATES %in% as.character(seen)
  AMR_CONFIDENCE_STATES[keep]
}

#' The four colours a gene heatmap's confidence scale is built from.
#'
#' The AMR-plot engine's own gene-heatmap defaults (visualization_amr.R's
#' ABSENT / PARTIAL / STRONG / PRESENT pickers), so a panel left on its defaults
#' reads at the same tier *and* the same colour in either engine. Putative is
#' not among them: `amr_confidence_palette()` blends it out of absent and
#' partial (see its own note on why it has no picker).
#'
#' A heatmap panel carries these four as `color_absent` / `color_partial` /
#' `color_strong` / `color_present`; this is what it starts on and what
#' `.heatmap_fill()` falls back to.
#' @export
AMR_CONFIDENCE_COLORS <- c(
  absent = "#EFEFEF",
  partial = "#E5C494",
  strong = "#8C6E3D",
  present = "#000000"
)

#' Fills for the gene heatmap's confidence tiers, on the shared defaults.
#'
#' `AMR_CONFIDENCE_STATES` runs weakest-first; the legend lists it reversed, as
#' a scale.
#' @export
AMR_CONFIDENCE_FILL <- amr_confidence_palette(
  AMR_CONFIDENCE_COLORS[["absent"]],
  AMR_CONFIDENCE_COLORS[["partial"]],
  AMR_CONFIDENCE_COLORS[["strong"]],
  AMR_CONFIDENCE_COLORS[["present"]]
)

#' The sequential palette a panel in "scale" colour mode falls back to.
#'
#' Light-to-dark is what makes a sequential family readable as a confidence
#' ladder at all — Absent takes the palette's lightest stop and Perfect its
#' darkest, the same direction the four hand-picked tiers run in.
#' @export
HEAT_SCALE_DEFAULT <- "Greys"

# The five tier fills a sequential palette gives, weakest stop to strongest.
#
# No `amr_fit_scale()` in front of it, unlike the class strip's: that fitter
# only drops *qualitative* families too small for the level count, and every
# sequential family carries at least five stops natively — so for five tiers it
# is a no-op, and nothing here is interpolated.
.heat_scale_fill <- function(scale) {
  amr_palette(AMR_CONFIDENCE_STATES, scale %||% HEAT_SCALE_DEFAULT)
}

# The fill scale one panel draws under. A gene panel carries its own four
# confidence colours (the Heatmap tab's colour modal), so two panels on one tree
# can be told apart by more than their headers; a panel that carries none — a
# restored Analysis saved before they existed — falls back to the shared
# defaults and looks exactly as it did. The presence/absence branch has no
# tiers to colour and keeps its fixed two-colour key.
#
# `color_mode` picks which of the two the modal's segmented control left live:
# the four pickers ("tiers", the default) or one sequential ramp ("scale")
# spread across the same five tiers.
.heatmap_fill <- function(panel) {
  if (!identical(panel$level, "gene")) {
    return(AMR_PRESENCE_FILL)
  }
  if (identical(panel$color_mode, "scale")) {
    return(.heat_scale_fill(panel$heat_scale))
  }
  pick <- function(key) {
    v <- panel[[paste0("color_", key)]]
    if (is.null(v) || !length(v) || is.na(v[[1]]) || !nzchar(v[[1]])) {
      AMR_CONFIDENCE_COLORS[[key]]
    } else {
      as.character(v[[1]])
    }
  }
  amr_confidence_palette(
    pick("absent"),
    pick("partial"),
    pick("strong"),
    pick("present")
  )
}

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
  # A clade caption is a physical width rather than a share of the tree's span,
  # so it is added to the canvas rather than solved into the split above.
  #
  # A circular tree needs more canvas than the caption is wide: its disc is
  # drawn across `TREE_RADIAL_FRAC` of the panel's side, so a square that grew
  # by the caption's own inches would hand the *radius* only a fraction of them
  # and take the rest out of the disc. Every other annotation escapes this by
  # being a multiple of the tree's span, which grows with the panel.
  clade_in <- .clade_edge_in(opts)
  if (.is_circular(opts) && clade_in > 0) {
    clade_in <- clade_in / TREE_RADIAL_FRAC
  }
  if (!isTRUE(heat > 0)) {
    return(panel_in + clade_in)
  }
  growth <- .panel_growth(opts, md, heat)
  if (!is.finite(growth) || growth <= 0) {
    return(panel_in + clade_in)
  }
  max(panel_in * growth, panel_in) + clade_in
}

#' How much of the panel it asked for the figure is actually drawn at.
#'
#' One below the ceiling and less than one above it. A fraction of the tree's
#' span rides a squeeze out untouched — it is a fraction of whatever the panel
#' turns out to be — but a physical width does not, and a caption is a physical
#' width whose type was already set in millimetres against the panel it was
#' promised. Off by a sixth, that is three letters drawn past the panel edge
#' and clipped there, which is exactly how it was reported.
#'
#' @param opts List. Resolved tree options.
#' @param panel_in Numeric. Inches `tree_panel_width_in()` asked for.
#' @param legend_in Numeric. Inches the guide box takes beside it.
#' @return Numeric factor in (0, 1].
#' @export
tree_panel_squeeze <- function(opts, panel_in, legend_in = 0) {
  base <- opts$width_in %||% 5.5
  leg <- suppressWarnings(as.numeric(legend_in %||% 0))
  if (length(leg) != 1L || !is.finite(leg) || leg < 0) {
    leg <- 0
  }
  room <- base * TREE_CANVAS_MAX_FACTOR - leg
  # A disc costs less width than its panel: the image is sized to the drawing
  # rather than to the square it is drawn in, so an inch of panel buys
  # `COORD_POLAR_FRAC` of an inch of image, plus the half-ring the guides need
  # beside it (`tree_image_width_in()`). Dividing by that is what turns "inches
  # of image left over" into "inches of panel they will pay for" — without it
  # the disc was squeezed by a tenth more than the ceiling actually asked, and
  # the image kept the height of the panel it did not get.
  if (.is_circular(opts)) {
    per_in <- COORD_POLAR_FRAC +
      if (.has_guides(opts)) (1 - COORD_POLAR_FRAC) / 2 else 0
    room <- (room - PLOT_MARGIN_IN * .scale_of(opts)) / per_in
  }
  if (!isTRUE(is.finite(panel_in) && panel_in > 0) || !isTRUE(room > 0)) {
    return(1)
  }
  .clamp(room / panel_in, 0.2, 1)
}

# The panel a radial figure is really drawn on, which is not the one the
# annotations asked for once the ceiling bites (`tree_panel_squeeze()`).
#
# Every piece of the radial image's geometry is measured from it — the image's
# height, its width, and the negative margin that lets the panel overflow both
# — so they are all read through here. Measured from the requested panel
# instead, the image kept the height of a disc a tenth larger than the one
# drawn in it, and the difference came out as blank bands above and below.
.drawn_panel_in <- function(opts, panel_in) {
  k <- suppressWarnings(as.numeric(opts$panel_squeeze %||% 1))
  if (length(k) != 1L || !is.finite(k) || k <= 0) {
    k <- 1
  }
  panel_in * k
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
      HEADER_SIZE_MAX * .type_of(opts)
    } else {
      tree_header_size(
        col_span * squeeze * tree_span,
        axis_units,
        width_in,
        .scale_of(opts),
        .text_of(opts)
      )
    }
    HEADER_CHAR_ROWS * size / (HEADER_SIZE_MAX * .scale_of(opts))
  }

  # Both kinds of annotation carry a vertical header, so both claim room — and
  # a tile strip's header is its variable's *name*, which is far longer than a
  # gene symbol, over a column several times as wide. Sizing this from the
  # heatmap alone is what clipped the strip headers off the top of the panel.
  # The element-type label a panel puts *above* its gene names, at the size it
  # will actually be set at — the same solve the drawing runs, so the reserve
  # and the label cannot disagree about how deep it is. Its gap is charged even
  # when the names below it are off, because the label still has to clear the
  # matrix edge.
  element_rows <- function(h) {
    if (!.element_label_drawn(h) || !identical(.element_pos(h), "top")) {
      return(0)
    }
    size <- if (is.null(tree_span) || !is.finite(axis_units)) {
      HEADER_SIZE_MAX * .type_of(opts)
    } else {
      .element_label_size(
        .element_label_text(h),
        length(h$cols) * .heat_span(opts) * squeeze * tree_span,
        axis_units,
        width_in,
        .scale_of(opts),
        # Never smaller than the gene names it titles — the same floor the
        # drawing uses, so the reserve matches what goes in it.
        floor_size = tree_header_size(
          .heat_span(opts) * squeeze * tree_span,
          axis_units,
          width_in,
          .scale_of(opts),
          .text_of(opts)
        ),
        text = .text_of(opts)
      )
    }
    ELEMENT_LABEL_GAP_ROWS + .element_label_rows(size)
  }

  heat_rows <- vapply(
    hs,
    function(h) {
      # A panel drawn without its gene names needs no room above it for them —
      # but may still have an element label up there, which then sits where the
      # names would have.
      if (isFALSE(h$show_gene_names)) {
        return(0)
      }
      labs <- if (identical(h$level, "gene")) {
        h$labels %||% h$cols
      } else {
        field_labels_for(h$cols)
      }
      suppressWarnings(max(nchar(labs), 1L)) * rows_per_char(.heat_span(opts))
    },
    numeric(1)
  )

  # A strip whose header will not be drawn (`tree_tile_layers()` drops the ones
  # the column cannot carry legibly) reserves nothing for it.
  tile_header <- tree_header_size(
    .tile_span(opts) * squeeze * tree_span,
    axis_units,
    width_in,
    .scale_of(opts),
    .text_of(opts)
  )
  tile_rows <- if (
    !is.null(tree_span) &&
      is.finite(axis_units) &&
      !tree_header_drawn(tile_header, .scale_of(opts))
  ) {
    numeric(0)
  } else {
    vapply(
      tiles,
      function(l) {
        suppressWarnings(max(nchar(l$title %||% l$field), 1L)) *
          rows_per_char(.tile_span(opts))
      },
      numeric(1)
    )
  }

  rows <- suppressWarnings(max(c(heat_rows, tile_rows), 1))
  if (!is.finite(rows)) {
    rows <- 1
  }
  # The element labels sit on one line clear of the *tallest* set of headers on
  # the figure, not each above its own panel's — so they are added once, on top
  # of that maximum, which is exactly where the drawing puts them.
  rows <- rows +
    suppressWarnings(max(c(vapply(hs, element_rows, numeric(1)), 0)))
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
  .clamp(rows / .y_span_rows(opts, n), 0.04, HEADER_FRAC_MAX)
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
  # A clustered panel is ordered by call pattern, not by class, so its classes
  # are no longer contiguous — the same reason an unknown class breaks a run
  # below, one step further: a bracket over what is left would claim a grouping
  # the drawn order does not have. (The AMR plot draws no class split under
  # "Cluster All" either.) It gets a colour strip instead.
  if (isTRUE(panel$cluster) || isFALSE(panel$show_class_names)) {
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

# The y limit a radial tree's scale has to carry, for a wedge of `angle` degrees.
#
# ggtree cuts the wedge by leaving room on the y scale past the last tip and
# mapping the whole scale onto the circle (`ggtree:::open_tree`), so this is its
# arithmetic, written out because we have to restore the scale after gheatmap
# replaces it. `n + 1` is the closed case: a whole tip row of slack, which is
# what a full circle already has between the last tip and the first.
.radial_y_limit <- function(n_tip, angle) {
  n <- max(as.numeric(n_tip %||% 1), 1)
  a <- suppressWarnings(as.numeric(angle %||% 0))
  if (length(a) != 1L || !is.finite(a) || a <= 0) {
    return(n + 1)
  }
  a <- .clamp(a, 0, OPEN_ANGLE_MAX)
  max(n * (1 + a / (360 - a)), n + 1)
}

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
    size <- tree_header_size(
      col_span,
      axis,
      axis_in,
      .scale_of(opts),
      .text_of(opts)
    )
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

  wedge <- if (length(needed)) max(needed) * OPEN_ANGLE_PAD * 180 / pi else 0

  # And the band under the first tip — a bracketed panel's class names, a
  # clustered one's colour strip and dendrogram. On a linear tree that band is
  # a y expansion; here it is degrees, because it is measured in tip rows and a
  # tip row is an angle. The two ends of the wedge are the same wedge, so the
  # deeper of the two claims it rather than the two adding up.
  n_tip <- max(nrow(md %||% data.frame()), 1L)
  band <- .bottom_band_rows(
    opts,
    n_tip,
    .class_band_runs(opts),
    .nominal_header_size(opts)
  ) *
    360 /
    n_tip

  .clamp(round(max(wedge, band)), 0, OPEN_ANGLE_MAX)
}

# Rows of tip pitch the band under the matrices already holds, before any
# element-type label is hung below it — the deeper of what a bracketed panel
# and a clustered one put there.
#
# One function rather than two copies because the reserve
# (`heatmap_class_frac()`) and the drawing both have to place the element label
# under exactly this, and a second copy is how they would come to disagree.
.bottom_band_rows <- function(opts, n_tip, runs = list(), size = NULL) {
  n <- max(as.integer(n_tip %||% 1L), 1L)

  # A clustered panel spends the band on a colour strip and the dendrogram
  # under it instead of on brackets and vertical names, so its reserve is
  # geometry rather than text — and the two kinds of panel can be on the same
  # tree, in which case the band has to hold whichever is deeper.
  clustered <- Filter(
    function(h) isTRUE(h$cluster) && length(h$cols),
    opts$heatmaps %||% list()
  )
  cluster_band <- suppressWarnings(max(
    vapply(
      clustered,
      function(h) .cluster_band(n, h, .strip_drawn(h))$rows,
      numeric(1)
    ),
    0
  ))

  runs <- Filter(function(r) nrow(r) > 0L, runs %||% list())
  bracket_band <- if (!length(runs)) {
    0
  } else {
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
    CLASS_GAP_ROWS +
      CLASS_TICK_ROWS +
      CLASS_LABEL_GAP_ROWS +
      chars * rows_per_char
  }

  max(bracket_band, cluster_band)
}

# How much of that band the layers put on the y scale themselves.
#
# A strip tile, a dendrogram segment and a class name's anchor are all data:
# ggplot trains the scale on them, so the range already reaches down past them
# before a single row is reserved. Only the ink it cannot see needs reserving —
# a class name's extent below the anchor, an element label's below its own.
.bottom_band_drawn <- function(opts, n_tip, runs = list(), size = NULL) {
  n <- max(as.integer(n_tip %||% 1L), 1L)

  clustered <- Filter(
    function(h) isTRUE(h$cluster) && length(h$cols),
    opts$heatmaps %||% list()
  )
  cluster_drawn <- suppressWarnings(max(
    vapply(
      clustered,
      function(h) .cluster_band(n, h, .strip_drawn(h))$rows,
      numeric(1)
    ),
    0
  ))

  # Down to the class names' anchor, no further: the text hanging off it is
  # exactly the part ggplot has no measure of.
  runs <- Filter(function(r) nrow(r) > 0L, runs %||% list())
  bracket_drawn <- if (length(runs)) {
    CLASS_GAP_ROWS + CLASS_LABEL_GAP_ROWS
  } else {
    0
  }

  drawn <- max(bracket_drawn, cluster_drawn)
  # An element label sent below hangs under the whole band, class names and
  # all, so its own anchor — not the geometry above it — is the lowest thing
  # ggplot sees. Only the half of the word below that anchor is left to reserve.
  if (.element_label_below(opts)) {
    drawn <- max(
      drawn,
      .bottom_band_rows(opts, n, runs, size) + ELEMENT_LABEL_GAP_ROWS
    )
  }
  drawn
}

# Rows the distance axis and the scale bar hang below the first tip.
#
# They are data — ggplot trains the y scale on them — so they are part of the
# range every expansion below is a fraction of, and part of the height the tip
# rows have to share. Left out, a tree with an axis under it came out with its
# rows a tenth shorter than they were fitted for.
.axis_rows <- function(opts) {
  if (.is_circular(opts)) {
    return(0)
  }
  axis <- isTRUE(opts$axis_show)
  bar <- isTRUE(opts$treescale_show)
  if (!axis && !bar) {
    return(0)
  }
  # Where the axis line sits (one row down, two when the scale bar has the
  # first), plus its ticks and the gap under them. The row of numbers itself is
  # not counted here: it hangs below the anchor ggplot trained on, so it is an
  # expansion rather than part of the range (`.axis_frac()`).
  if (axis) {
    (if (bar) 2 else 1) + AXIS_TICK_LEN + AXIS_LABEL_GAP
  } else {
    2
  }
}

# Most of the y range the axis numbers' own depth may claim. A backstop only:
# one line of type is a fraction of a plot, never a third of it.
AXIS_FRAC_MAX <- 0.2

# Millimetres of type that hang *below* everything the y scale was trained on.
#
# The axis numbers are anchored at their top (`vjust = 1`) and so is the scale
# bar's, so ggplot2 trains the range on the anchor and the glyphs fall outside
# it — cut in half by the panel edge, which is exactly how they were drawn. The
# row of numbers `.axis_rows()` counts is the same line; that count sizes the
# rows, this reserves the space.
.axis_text_mm <- function(opts) {
  if (.is_circular(opts)) {
    return(0)
  }
  if (!isTRUE(opts$axis_show) && !isTRUE(opts$treescale_show)) {
    return(0)
  }
  AXIS_LABEL_SIZE * .type_of(opts)
}

# That depth as the bottom expansion the y scale needs, alongside whatever the
# class band is already asking for. Pitch-corrected the same way the header and
# class reserves are: the type is a physical height, so a shorter row is more
# rows of it.
.axis_frac <- function(
  opts,
  n_tip,
  runs = list(),
  size = NULL,
  height_in = NULL
) {
  mm <- .axis_text_mm(opts)
  if (mm <= 0) {
    return(0)
  }
  n <- max(as.integer(n_tip %||% 1L), 1L)
  row_mm <- if (isTRUE(is.finite(height_in) && height_in > 0)) {
    25.4 * height_in / n
  } else {
    25.4 * TIP_ROW_IN
  }
  .clamp(mm / row_mm / .y_span_rows(opts, n, runs, size), 0, AXIS_FRAC_MAX)
}

# The class-name band's depth, from the panels alone.
#
# `heatmap_class_runs()` needs the columns a panel actually drew, which is not
# known until the panel loop — but the band is only ever as deep as its longest
# class *name*, and every panel carries its own classes. Standing in for the
# real runs lets the row pitch be solved before the tip labels are set, which
# is the one place it is needed early.
.class_band_runs <- function(opts) {
  named <- Filter(
    function(h) {
      length(h$cols) > 0L &&
        !isTRUE(h$cluster) &&
        !isFALSE(h$show_class_names) &&
        length(h$classes)
    },
    opts$heatmaps %||% list()
  )
  lapply(named, function(h) {
    data.frame(
      class = unique(as.character(h$classes)),
      stringsAsFactors = FALSE
    )
  })
}

# Millimetres of the column-name stack over the tallest panel that draws one.
#
# The same measure the builder takes as it draws (`header_mm_max`), taken
# early: the stack is what an element-type label above it has to clear, and
# where that label lands is part of the y range the tip rows have to share.
.header_stack_mm <- function(opts, size, tiles = FALSE) {
  drawn <- Filter(
    function(h) length(h$cols) > 0L && !isFALSE(h$show_gene_names),
    opts$heatmaps %||% list()
  )
  chars <- vapply(
    drawn,
    function(h) suppressWarnings(max(nchar(h$labels %||% h$cols), 1L)),
    numeric(1)
  )
  # A tile strip's header is a variable *name*, several times longer than a
  # gene symbol, and it stands in the same band. Counted only where the caller
  # is measuring the whole band rather than what an element label must clear.
  if (isTRUE(tiles)) {
    chars <- c(
      chars,
      vapply(
        Filter(
          function(l) identical(l$aesthetic, "tile"),
          opts$layers %||% list()
        ),
        function(l) suppressWarnings(max(nchar(l$title %||% l$field), 1L)),
        numeric(1)
      )
    )
  }
  chars <- suppressWarnings(max(chars))
  if (!isTRUE(is.finite(size)) || !is.finite(chars) || chars <= 0) {
    return(0)
  }
  HEADER_ROW_PACK * chars * TIP_CHAR_EM * size
}

# The type size an annotation header comes out at, before the axis is solved.
#
# `tree_header_size()` fits a header to its column in *data* units, which only
# exist once there is a tree to measure them against. The same answer in
# physical units: the column's own width, squeezed if the annotation run is
# wider than the tree can carry, against the design ceiling.
.nominal_header_size <- function(opts) {
  squeeze <- .annotation_squeeze(opts)
  col_in <- max(
    if (length(Filter(
      function(h) length(h$cols) > 0L,
      opts$heatmaps %||% list()
    ))) {
      .heat_col_in(opts)
    } else {
      0
    },
    if (.n_tiles(opts) > 0) .tile_col_in(opts) else 0
  )
  if (col_in <= 0) {
    return(0)
  }
  .fitted_type(
    HEADER_SIZE_MAX * .type_of(opts),
    col_in * squeeze * 25.4 * HEADER_FILL,
    .scale_of(opts)
  )
}

#' Inches of plot height the annotations take from the tip rows.
#'
#' A heatmap's column names are set vertically, so a twenty-two character gene
#' symbol is a band nearly two inches deep — and that band is *taken out of*
#' the plot's height rather than added to it, because a y-scale expansion
#' compresses the rows already there. Twelve isolates fitted to a 2.75 inch
#' page therefore ended up with three quarters of a millimetre per row and no
#' labels the engine would agree to draw.
#'
#' `tree_auto_layout()` cannot see any of this — it is handed a tip count and a
#' width, and knows nothing of what is drawn beside the tree. So the aspect
#' ratio it returns is corrected by this, the same way the circle-opening angle
#' it returns is corrected by `tree_open_angle()`.
#'
#' Zero for a radial layout, whose panel is square and whose annotations are
#' rings that grow it in both directions at once.
#'
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @return Numeric inches.
#' @export
tree_band_in <- function(opts, n_tip) {
  # Radial panels are square and their annotations are rings; a plain tree's
  # own furniture — its margins, its distance axis — is what TIP_USABLE already
  # holds back, so counting it again here would make every tree taller for
  # nothing.
  if (.is_circular(opts) || annotation_total(opts) <= 0) {
    return(0)
  }
  n <- max(as.integer(n_tip %||% 1L), 1L)
  size <- .nominal_header_size(opts)
  mm <- .header_stack_mm(opts, size, tiles = TRUE)
  if (.element_label_above(opts)) {
    mm <- mm + ELEMENT_LABEL_GAP_ROWS * 25.4 * TIP_ROW_IN + size
  }
  # The class names hang below at the size the drawing caps them to, so the
  # band is measured from that rather than from the header size above it.
  mm <- mm +
    .bottom_band_rows(
      opts,
      n,
      .class_band_runs(opts),
      .class_name_size(size, n, .class_band_runs(opts), .scale_of(opts))
    ) *
      25.4 *
      TIP_ROW_IN
  mm / 25.4
}

#' The aspect ratio a fitted tree needs once its annotations are counted.
#'
#' @param aspect Numeric. `tree_auto_layout()`'s answer.
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @return Numeric aspect ratio, within the fit's own limits.
#' @export
tree_fitted_aspect <- function(aspect, opts, n_tip) {
  a <- suppressWarnings(as.numeric(aspect))
  if (length(a) != 1L || !isTRUE(is.finite(a)) || a <= 0) {
    return(aspect)
  }
  w <- opts$width_in
  if (is.null(w) || !is.finite(w) || w <= 0) {
    w <- 5.5
  }
  round(
    .clamp(a + tree_band_in(opts, n_tip) / w, TIP_ASPECT_MIN, TIP_ASPECT_MAX),
    1
  )
}

# Millimetres of tip pitch the finished panel is really drawn at.
#
# Not `height_in / n_tip`, and not `.drawn_row_mm()` on the tip count either:
# a panel's element-type label is placed as *data*, one clear line above the
# tallest stack of column names on the figure, so the y range ggplot trains on
# reaches that far and the rows are squeezed into what is left of the height.
# How far that is depends on the pitch, and the pitch depends on it — so it is
# solved once and once only. Iterating to a fixed point would be *wrong*, not
# merely slow: the drawing places that label with a single pass too, so a
# converged answer here would describe a figure nobody draws.
.drawn_tip_pitch <- function(
  opts,
  n_tip,
  height_in,
  top_frac,
  bottom_frac,
  runs = list(),
  size = NULL,
  header_mm = 0
) {
  span <- .y_span_rows(opts, n_tip, runs, size) + .axis_rows(opts)
  # The axis numbers' own depth is an expansion, not part of the range (see
  # `.axis_frac()`), so it is charged where the scale charges it — adding it to
  # the span as well would book the same line of type twice.
  bottom_frac <- bottom_frac + .axis_frac(opts, n_tip, runs, size, height_in)
  row_mm <- .drawn_row_mm(height_in, span, top_frac, bottom_frac)
  if (!isTRUE(header_mm > 0) || !.element_label_above(opts)) {
    return(row_mm)
  }
  # The stack, the clear line over it, and the label's own line — all three sit
  # above the last tip in the range ggplot trains on.
  extra <- (header_mm +
    ELEMENT_LABEL_GAP_ROWS * 25.4 * TIP_ROW_IN +
    (size %||% HEADER_SIZE_MAX)) /
    row_mm
  .drawn_row_mm(height_in, span + extra, top_frac, bottom_frac)
}

# Millimetres between two neighbouring tips on a radial tree.
#
# The counterpart of `.drawn_tip_pitch()` for a disc. Rows there are arcs
# rather than bands, and the tightest they ever are is on the circle the tips
# themselves stand on: a leader line runs outward from there, so that is where
# two of them are closest. The fan's opening is circumference the tips are not
# spread over, so it comes off first.
#
# @param panel_in Numeric. Side of the square panel, in inches.
# @param tip_frac Numeric. Share of the radius the tip circle stands at.
.radial_tip_pitch_mm <- function(opts, panel_in, tip_frac, n_tip) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  frac <- suppressWarnings(as.numeric(tip_frac))
  if (length(frac) != 1L || !is.finite(frac)) {
    return(NA_real_)
  }
  r_in <- tree_axis_in(opts, panel_in) * .clamp(frac, 0, 1)
  open <- suppressWarnings(as.numeric(opts$open_angle %||% 0))
  if (length(open) != 1L || !is.finite(open)) {
    open <- 0
  }
  25.4 * 2 * pi * r_in * (1 - .clamp(open, 0, 359) / 360) / n
}

# Whether any panel puts its element-type label above its column names.
.element_label_above <- function(opts) {
  any(vapply(
    opts$heatmaps %||% list(),
    function(h) .element_label_drawn(h) && identical(.element_pos(h), "top"),
    logical(1)
  ))
}

# The y range ggplot will train the scale on, in rows of tip pitch.
#
# `expansion(mult = )` is a fraction of *this*, not of the tip count — so a
# reserve divided by the tip count is charged again for every drawn row under
# the matrix. That is what put a page of white under a deep dendrogram: the
# segments stretched the range by their own depth, and the same depth came back
# a second time as expansion measured against it.
.y_span_rows <- function(opts, n_tip, runs = list(), size = NULL) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  n + TILE_HEADER_OFFSET + .bottom_band_drawn(opts, n, runs, size)
}

# Whether any panel hangs its element-type label under that band.
.element_label_below <- function(opts) {
  any(vapply(
    opts$heatmaps %||% list(),
    function(h) .element_label_drawn(h) && identical(.element_pos(h), "bottom"),
    logical(1)
  ))
}

#' Fraction of the panel to keep clear below the tree for the class band.
#'
#' The vertical counterpart of `heatmap_header_frac()`, and measured the same
#' way: from the longest name that will actually go in it, at the size it will
#' be set at — plus the element-type label, when one hangs below the band.
#'
#' Less whatever the band draws for itself. A strip tile and a dendrogram
#' segment are data, so the scale has already been trained on them; reserving
#' their depth on top of that is how a deep dendrogram used to hang a page of
#' white under the figure.
#'
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @param runs List of class-run frames, one per drawn panel.
#' @param size Numeric. Type size the class names are set at.
#' @param height_in Numeric. Height the plot is drawn at, in inches. Without it
#'   the band is measured at the pitch the fit aims for rather than the pitch
#'   the plot has, which under-reserves on a squat figure exactly as it did
#'   over the headers — the last class name was clipped off the bottom edge.
#' @return Numeric multiplicative expansion for the bottom of the y scale.
#' @export
heatmap_class_frac <- function(
  opts,
  n_tip,
  runs = list(),
  size = NULL,
  height_in = NULL
) {
  n <- max(as.integer(n_tip %||% 1L), 1L)
  band <- .bottom_band_rows(opts, n, runs, size)

  # An element-type label sent to the bottom hangs under whichever of the two
  # bands is deeper, so every panel's label sits on one line rather than each
  # under its own. Reserved at the size the band names are set at, which is the
  # size the drawing caps it to — so the reserve is never the smaller of the two.
  if (.element_label_below(opts)) {
    band <- band +
      ELEMENT_LABEL_GAP_ROWS +
      .element_label_rows(size %||% HEADER_SIZE_MAX)
  }

  drawn <- .bottom_band_drawn(opts, n, runs, size)
  if (band <= drawn) {
    return(0.02)
  }
  rows <- band - drawn
  # The same pitch correction the header reserve makes: these rows are a
  # physical depth of type, and a shorter row means more of them.
  if (isTRUE(is.finite(height_in) && height_in > 0)) {
    rows <- rows * TIP_ROW_IN / (height_in / n)
  }
  .clamp(rows / .y_span_rows(opts, n, runs, size), 0.02, CLASS_FRAC_MAX)
}

#' The type size the class names under a bracketed panel are set at.
#'
#' They would otherwise take the column-header size, and a long class name at
#' that size wants a band deeper than `CLASS_FRAC_MAX` of the plot — which
#' `heatmap_class_frac()` then clamps, leaving the name clipped at its far end.
#' Shrink the size until the band it needs fits the cap, with the same floor the
#' headers have: below `HEADER_SIZE_MIN` a name is not worth reading, and the
#' reserve carries whatever still hangs past the cap.
#'
#' @param size Numeric. The size the names would be set at (the header size).
#' @param n_tip Integer. Number of tips.
#' @param runs List of class-run frames, one per drawn panel.
#' @param scale Numeric. The plot's design scale, for the floor.
#' @return Numeric type size, never larger than `size`.
#' @export
.class_name_size <- function(size, n_tip, runs, scale = 1) {
  runs <- Filter(function(r) nrow(r) > 0L, runs %||% list())
  if (!length(runs) || !is.finite(size) || size <= 0) {
    return(size)
  }
  chars <- suppressWarnings(max(
    vapply(
      runs,
      function(r) suppressWarnings(max(nchar(r$class), 1L)),
      numeric(1)
    ),
    1
  ))
  n <- max(as.integer(n_tip %||% 1L), 1L)
  gaps <- CLASS_GAP_ROWS + CLASS_TICK_ROWS + CLASS_LABEL_GAP_ROWS
  budget <- CLASS_FRAC_MAX * n - gaps
  floor_size <- HEADER_SIZE_MIN * scale
  if (budget <= 0 || !is.finite(chars) || chars <= 0) {
    return(min(size, floor_size))
  }
  fit <- budget * HEADER_SIZE_MAX / (chars * HEADER_CHAR_ROWS)
  min(size, max(fit, floor_size))
}

# The element-type labels, one per panel that asked for one, at a single y.
#
# `specs` are the per-panel records the panel loop collected: what the label
# says, where its run of columns is centred, and the size it fits that run at.
# They are emitted together and after the loop because the y they share is only
# known once every panel has reported — the top one clears the tallest set of
# gene names on the figure, and the bottom one clears the deepest band under it.
# Drawn at one y and, at the bottom, one size, so a row of them reads as one
# annotation rather than as a caption per matrix.
.element_label_layers <- function(specs, y, colour, cap = NULL) {
  if (!length(specs)) {
    return(NULL)
  }
  # One layer per panel, each at its own fitted size — not one size for the
  # whole row. A three-column panel beside a thirty-column one is honestly
  # constrained, and shrinking the wide panel's title to match the narrow one's
  # cost both of them their legibility to buy a consistency the reader was not
  # going to notice: the two sit over visibly different widths.
  #
  # Separate layers rather than one with `size` mapped, because a mapped size
  # needs a size *scale*, and this plot already spends that scale on a mapped
  # tip-point aesthetic. At most three panels, so at most three layers.
  lapply(specs, function(s) {
    size <- if (!is.null(cap) && is.finite(cap)) min(s$size, cap) else s$size
    geom_text(
      data = data.frame(
        x = s$x,
        y = y,
        label = s$label,
        stringsAsFactors = FALSE
      ),
      mapping = aes(
        x = .data[["x"]],
        y = .data[["y"]],
        label = .data[["label"]]
      ),
      inherit.aes = FALSE,
      hjust = 0.5,
      vjust = 0.5,
      size = size,
      colour = colour
    )
  })
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
#' HEADER_FILL of the column is the room: the rest is the gap that keeps
#' neighbouring headers apart, so a name set to the full column width would
#' touch the one beside it. The design size is the ceiling at text scale 1 and
#' the reader can ask past it, up to that room and no further.
#'
#' A result below `HEADER_SIZE_MIN * scale` means the column cannot carry a
#' legible name at all. It is returned rather than clamped up, so
#' `.header_drawn()` can tell that case from a merely small header and leave
#' the names off — printed anyway, they are a smear over the matrix rather
#' than a set of labels.
#'
#' @param col_units Numeric. Width of one column, in x-axis data units.
#' @param axis_units Numeric. Full width of the x axis, same units.
#' @param panel_in Numeric. Physical width of the panel, in inches.
#' @param scale Numeric. The plot's design scale.
#' @param text Numeric. The reader's text-size bias.
#' @return Numeric ggplot2 text size.
#' @export
tree_header_size <- function(
  col_units,
  axis_units,
  panel_in,
  scale = 1,
  text = 1
) {
  if (!is.finite(col_units) || !is.finite(axis_units) || axis_units <= 0) {
    return(HEADER_SIZE_MIN * scale)
  }
  col_mm <- 25.4 * panel_in * col_units / axis_units
  .fitted_type(HEADER_SIZE_MAX * scale * text, col_mm * HEADER_FILL, scale)
}

#' Whether an annotation header is worth drawing at the size it fits at.
#'
#' @param size Numeric. A `tree_header_size()` result.
#' @param scale Numeric. The plot's design scale.
#' @return TRUE when the header is drawn.
#' @export
tree_header_drawn <- function(size, scale = 1) {
  .header_drawn(size, scale)
}

# Panels whose columns cannot carry a legible gene name lose the names.
#
# Decided once and written back into the panel records, because three things
# downstream have to agree about it: the band reserved above the tree
# (`heatmap_header_frac()`), the matrix that is actually drawn (gheatmap's
# `colnames`), and the element-type label that has to clear whatever the names
# left behind. Taken independently they disagreed, and the reserve was the one
# that showed — a strip of empty page over a matrix with no names on it.
#
# The reader's own "Gene names" switch still wins where it says no; this can
# only take names away, never put them back.
.resolve_header_visibility <- function(opts, axis_units, axis_in, tree_span) {
  squeeze <- .annotation_squeeze(opts)
  scale <- .scale_of(opts)
  col_span <- .heat_span(opts) * squeeze * tree_span
  size <- tree_header_size(
    col_span,
    axis_units,
    axis_in,
    scale,
    .text_of(opts)
  )
  drawn <- tree_header_drawn(size, scale)
  lapply(opts$heatmaps %||% list(), function(h) {
    h$show_gene_names <- !isFALSE(h$show_gene_names) && drawn
    h
  })
}

# Gene columns reordered so that genes carried by the same isolates sit
# together, the way the AMR-plot engine's own gene axis is ordered — same
# distance and same linkage (amr_plot's AMR_CLUSTER_DISTANCES /
# AMR_CLUSTER_METHODS), so a pair that clusters together there clusters
# together here.
#
# Only the gene axis. The AMR plot clusters both, but this heatmap's rows are
# the tree's tips in the tree's own order, and reordering them is the one thing
# a tree panel cannot do — the tree *is* the row order. So there is no isolate
# dendrogram to offer and none is implied.
#
# Clustered on presence, not on the drawn tier: the default distance is
# Jaccard, which `dist()` only defines against 0/1, and presence is what the
# AMR plot clusters on too (.column_dist_fn there closes over the presence
# matrix for exactly this reason). Non-finite distances — two genes with
# identical all-absent columns under Jaccard — become 0 rather than aborting
# the render, and a linkage that fails leaves the catalogue order in place.
.cluster_heat_cols <- function(heat, distance, method) {
  if (ncol(heat) < 3L || !nrow(heat)) {
    return(heat)
  }
  mat <- matrix(
    unlist(lapply(heat, function(v) as.integer(as.integer(v) > 1L))),
    nrow = nrow(heat),
    dimnames = list(NULL, names(heat))
  )
  d <- dist(t(mat), method = distance)
  d[!is.finite(d)] <- 0
  hc <- tryCatch(hclust(d, method = method), error = function(e) NULL)
  if (is.null(hc)) {
    return(heat)
  }
  out <- heat[, hc$order, drop = FALSE]
  # Carried on the frame rather than recomputed where the dendrogram is drawn:
  # it is the same tree, and clustering the matrix twice would be the one way
  # for the drawn dendrogram and the drawn column order to disagree.
  attr(out, "hclust") <- hc
  out
}

# The clustering as segments, hung from `top` and running `depth` rows down.
#
# hclust's own coordinates, read the way every dendrogram plot reads them:
# leaves sit where `$order` puts them (so leaf k is at drawn position
# `order(hc$order)[k]`), an internal node sits at its merge height and midway
# between its children, and each merge draws two uprights and the crossbar
# between them. Heights are normalised to the tallest merge, so the depth is
# the reader's to size rather than the distance metric's.
.dendrogram_segments <- function(hc, centres, top, depth) {
  n <- if (is.null(hc$merge)) 0L else nrow(hc$merge)
  if (n < 1L || length(centres) != n + 1L) {
    return(NULL)
  }
  at <- order(hc$order)
  tallest <- suppressWarnings(max(hc$height))
  if (!is.finite(tallest) || tallest <= 0) {
    tallest <- 1
  }
  node_x <- numeric(n)
  node_y <- numeric(n)
  seg <- vector("list", n)
  for (k in seq_len(n)) {
    kids <- hc$merge[k, ]
    kx <- numeric(2)
    ky <- numeric(2)
    for (s in 1:2) {
      if (kids[[s]] < 0) {
        kx[[s]] <- centres[[at[[-kids[[s]]]]]]
        ky[[s]] <- top
      } else {
        kx[[s]] <- node_x[[kids[[s]]]]
        ky[[s]] <- node_y[[kids[[s]]]]
      }
    }
    y <- top - hc$height[[k]] / tallest * depth
    node_x[[k]] <- mean(kx)
    node_y[[k]] <- y
    seg[[k]] <- data.frame(
      x = c(kx[[1]], kx[[2]], kx[[1]]),
      xend = c(kx[[1]], kx[[2]], kx[[2]]),
      y = c(ky[[1]], ky[[2]], y),
      yend = c(y, y, y)
    )
  }
  do.call(rbind, seg)
}

# Which drug classes a clustered panel's guide lists — nothing for a panel that
# is not clustered, or whose columns carry no class. The legend solves are
# sized from this, so it is the one place that decides whether the strip earns
# a guide.
.class_guide_levels <- function(panel) {
  # `!isFALSE` rather than `isTRUE` for the switch, here and for the other
  # show_* flags, so a snapshot saved before these switches existed restores
  # with them on — which is how it was drawn when it was saved.
  if (!isTRUE(panel$cluster) || isFALSE(panel$show_class_strip)) {
    return(character(0))
  }
  cls <- panel$classes %||% character(0)
  cls <- cls[!is.na(cls) & nzchar(trimws(cls))]
  sort(unique(as.character(cls)))
}

# Where a clustered panel's annotations sit under the matrix, and how deep the
# band is altogether — solved in one place so the reserve
# (`heatmap_class_frac()`) and the drawing cannot disagree about it.
#
# `has_strip` is false for a panel whose genes carry no drug class at all: the
# strip and the gap under it collapse, and the dendrogram moves up into the
# room they were holding rather than hanging below an empty band.
.cluster_band <- function(n_tip, panel, has_strip) {
  k <- .class_band_scale(n_tip)
  strip_rows <- CLASS_STRIP_ROWS * k
  strip_top <- 0.5 - CLASS_STRIP_GAP_ROWS * k
  strip_bottom <- if (has_strip) strip_top - strip_rows else strip_top
  depth <- .dend_rows(n_tip, panel)
  dend_top <- if (has_strip && depth > 0) {
    strip_bottom - DEND_GAP_ROWS * k
  } else {
    strip_bottom
  }
  list(
    strip_y = (strip_top + strip_bottom) / 2,
    strip_rows = strip_rows,
    dend_top = dend_top,
    depth = depth,
    rows = 0.5 - (dend_top - depth)
  )
}

# Whether a clustered panel draws its drug-class strip — which is exactly
# whether its guide has classes to list.
.strip_drawn <- function(panel) {
  length(.class_guide_levels(panel)) > 0L
}

# The colour strip and the dendrogram one clustered panel draws under its
# matrix, with the fill scale the strip is keyed by.
#
# `classes` runs alongside `centres`: one drug class per drawn column, NA where
# the column has none. Those columns get no tile rather than a grey one — a
# gap in the strip says "not classified" where a swatch would have to be
# explained in the guide.
.heatmap_cluster_layers <- function(
  panel,
  classes,
  centres,
  cell,
  hc,
  n_tip,
  colour,
  levels,
  max_keys = LEGEND_MAX_KEYS,
  order = 99L,
  ncol = 1L,
  linewidth = BRANCH_WIDTH
) {
  if (!length(centres)) {
    return(NULL)
  }
  band <- .cluster_band(n_tip, panel, length(levels) > 0L)
  out <- list()

  if (length(levels)) {
    keep <- !is.na(classes) & nzchar(trimws(as.character(classes)))
    # The strip's guide answers to the same budget every other guide does, and
    # its keys go to the classes that actually colour the most columns.
    strip_keys <- tree_legend_breaks(levels, classes[keep], max_keys)
    fills <- amr_palette(
      levels,
      amr_fit_scale(.class_strip_scale(panel), length(levels))
    )
    tiles <- data.frame(
      x = centres[keep],
      y = band$strip_y,
      class = factor(as.character(classes)[keep], levels = levels),
      stringsAsFactors = FALSE
    )
    out <- list(
      new_scale_fill(),
      geom_tile(
        data = tiles,
        mapping = aes(
          x = .data[["x"]],
          y = .data[["y"]],
          fill = .data[["class"]]
        ),
        inherit.aes = FALSE,
        width = cell,
        height = band$strip_rows
      ),
      scale_fill_manual(
        values = .legend_values(fills, strip_keys$breaks),
        limits = .legend_limits(levels, strip_keys$breaks),
        breaks = strip_keys$breaks,
        name = tree_legend_title(
          .class_guide_title(panel),
          strip_keys$hidden,
          strip_keys$total
        ),
        guide = guide_legend(ncol = ncol, order = order),
        drop = FALSE
      )
    )
  }

  segs <- if (band$depth > 0) {
    .dendrogram_segments(hc, centres, band$dend_top, band$depth)
  }
  if (!is.null(segs)) {
    out <- c(
      out,
      list(geom_segment(
        data = segs,
        mapping = aes(
          x = .data[["x"]],
          xend = .data[["xend"]],
          y = .data[["y"]],
          yend = .data[["yend"]]
        ),
        inherit.aes = FALSE,
        colour = colour,
        linewidth = linewidth
      ))
    )
  }
  out
}

# The frame one panel draws, with its rows keyed the way gheatmap matches them.
#
# At gene level the source is `amr_matrix` — the wide confidence frame the view
# builds from `amr_plot$amr_confidence_frame()`, one factor column per gene in
# `AMR_CONFIDENCE_STATES` (Absent .. Perfect). Same table (`amr_results`) and
# same method ranking as the AMR-plot engine's own gene heatmap, so a gene
# reads at the identical tier in both. Both tree panels draw it — resistance
# and virulence/stress — differing only in which genes they carry.
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
    labels <- panel$labels %||% cols
    if (length(labels) == length(panel$cols)) {
      labels <- labels[match(cols, panel$cols)]
    }
    # abritamr rolls one gene up under every drug class it acts on, so a single
    # symbol can arrive as two positional matrix columns — aac(6')-Ib under both
    # aminoglycoside and quinolone, say — carrying the same per-isolate calls
    # under different classes. gheatmap gathers the frame by column name and
    # rejects duplicates, so the repeat is dropped here rather than aborting the
    # whole render; the first occurrence keeps the gene in its leading class run.
    keep <- !duplicated(labels)
    cols <- cols[keep]
    labels <- labels[keep]
    idx <- match(md$isolate, amr_matrix$isolate)
    heat <- amr_matrix[idx, cols, drop = FALSE]
    heat[] <- lapply(heat, function(v) {
      v <- as.character(v)
      v[is.na(v)] <- AMR_ABSENT
      factor(v, levels = AMR_CONFIDENCE_STATES)
    })
    names(heat) <- labels
    rownames(heat) <- md$label
    if (isTRUE(panel$cluster)) {
      heat <- .cluster_heat_cols(
        heat,
        panel$cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
        panel$cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
      )
    }
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

# Inches of the last axis number that hang past the tree's own maximum.
#
# The numbers are centred on their tick (`hjust` 0.5) and `pretty()` can put
# the last tick on the tree's depth itself, so half that glyph is drawn beyond
# everything the x range was solved for — the right edge cut "30" down its
# middle. Half a number, not a whole one: only the overhang is unaccounted for.
#
# This is the same region the tip labels reserve, so it is a floor on that
# reserve rather than an addition to it: a tree with labels already keeps far
# more room than a two-digit number needs, and a bare one kept none.
.axis_edge_in <- function(opts, max_x) {
  if (.is_circular(opts) || !isTRUE(opts$axis_show)) {
    return(0)
  }
  breaks <- tree_axis_breaks(max_x)
  if (!length(breaks)) {
    return(0)
  }
  label <- tree_branch_format(
    breaks[[length(breaks)]],
    tree_branch_digits(breaks[breaks > 0])
  )
  0.5 * nchar(label) * TIP_CHAR_EM * AXIS_LABEL_SIZE * .type_of(opts) / 25.4
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

  # The overhang is a fixed number of inches and the axis it has to be
  # expressed in is the one being solved, so it is solved with it rather than
  # converted through the range it is about to change: the panel is `panel_in`
  # inches wide and `limit - x_min` units, and the number needs `edge_in` of
  # those inches past `max_x`.
  # The caption column past the annotations is physical width too, and the
  # canvas has already been grown for it (`tree_panel_width_in`), so it is part
  # of the panel these overhangs are solved against.
  clade_in <- .clade_edge_in(opts)
  # Times the squeeze, because these inches have to be the ones the figure will
  # really be drawn across. Where the annotations asked for more canvas than the
  # ceiling allows (`tree_panel_squeeze`), solving against the request reserves
  # a column wider than the panel that arrives, and the caption inside it is
  # drawn off the edge.
  panel_in <- (max(
    tree_budget_in(opts) - PLOT_MARGIN_IN * .scale_of(opts),
    0.5
  ) *
    .panel_growth(opts, md, heat) +
    clade_in) *
    (opts$panel_squeeze %||% 1)

  edge_in <- .axis_edge_in(opts, max_x)
  limit <- x_min + range
  if (edge_in > 0) {
    k <- .clamp(edge_in / panel_in, 0, 0.5)
    limit <- max(limit, (max_x - k * x_min) / (1 - k))
  }

  # Same solve again for the captions, from the edge everything else ended at:
  # the column needs `clade_in` of the panel's inches past it, and enlarging the
  # axis by exactly that fraction leaves every width already placed at the size
  # it was placed for.
  clade <- NULL
  if (clade_in > 0) {
    k <- .clamp(clade_in / panel_in, 0, 0.5)
    grown <- (limit - k * x_min) / (1 - k)
    per_in <- (grown - x_min) / panel_in
    scale <- .scale_of(opts)
    bar_x <- limit + CLADE_BAR_GAP_MM * scale / 25.4 * per_in
    clade <- list(
      x = bar_x,
      text_x = bar_x +
        (CLADE_BAR_MM + CLADE_TEXT_GAP_MM) * scale / 25.4 * per_in
    )
    limit <- grown
  }
  list(
    limit = limit,
    reserve = range * frac,
    clade = clade
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
  #
  # `droplevels` because a column can arrive carrying levels nothing in it
  # uses — an AMR gene call is a factor of every state the caller can report,
  # and a screen that found the gene outright holds two of them. A key for a
  # level with no mark on the figure is a colour the reader goes looking for
  # and cannot find, and it costs the guide a key that a real level wanted.
  # A no-op on the second pass, which is why it can live on this branch.
  if (is.factor(v) && !anyNA(v) && all(nzchar(trimws(levels(v))))) {
    return(droplevels(v))
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
  max_rows = LEGEND_MAX_ROWS,
  max_keys = tree_legend_max_keys(max_rows),
  order = 99L,
  ncol = NULL
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
    bar <- guide_colourbar(order = order)
    return(
      if (viridis_pal) {
        ramp <- viridis(GRADIENT_STEPS, option = opt)
        if (fill) {
          scale_fill_gradientn(
            colours = ramp,
            transform = transform,
            name = name,
            guide = bar,
            na.value = MISSING_COLOR
          )
        } else {
          scale_color_gradientn(
            colours = ramp,
            transform = transform,
            name = name,
            guide = bar,
            na.value = MISSING_COLOR
          )
        }
      } else if (fill) {
        scale_fill_distiller(
          palette = palette,
          transform = transform,
          name = name,
          guide = bar,
          na.value = MISSING_COLOR
        )
      } else {
        scale_color_distiller(
          palette = palette,
          transform = transform,
          name = name,
          guide = bar,
          na.value = MISSING_COLOR
        )
      }
    )
  }

  values <- mapped_values(values)
  cols <- tree_level_colors(levels(values), palette)
  keys <- tree_legend_breaks(names(cols), values, max_keys)
  guide <- guide_legend(
    ncol = ncol %||% tree_legend_ncol(length(keys$breaks), max_rows),
    order = order
  )
  title <- tree_legend_title(name, keys$hidden, keys$total)
  fills <- .legend_values(cols, keys$breaks)
  limits <- .legend_limits(names(cols), keys$breaks)

  if (identical(aesthetic, "fill")) {
    scale_fill_manual(
      values = fills,
      limits = limits,
      breaks = keys$breaks,
      guide = guide,
      name = title,
      labels = .wrap_legend_labels,
      na.value = MISSING_COLOR
    )
  } else {
    scale_color_manual(
      values = fills,
      limits = limits,
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

# Whether the leader lines are worth drawing.
#
# Three ways they are not. An inward tree's leaders all converge on the root,
# where they pile into a blot over it. A tree with nothing set beside it — no
# labels, no tile strips, no heatmap — has leaders that lead nowhere, which is
# ink for its own sake. And past `LEADER_MIN_PITCH_MM` the rows are tighter
# than the dots are long, so the band of leaders fills in solid and hides the
# tree instead of pointing into it.
#
# `opts$row_mm` is the pitch the panel is really drawn at, written by the
# builder before the layers are assembled; without it — a caller building the
# layer on its own — the pitch is unknown and the leaders are drawn, which is
# what they did before there was a rule at all.
.leaders_drawn <- function(opts) {
  if (identical(opts$layout, "inward")) {
    return(FALSE)
  }
  if (!isTRUE(opts$tiplab_show) && annotation_total(opts) <= 0) {
    return(FALSE)
  }
  pitch <- suppressWarnings(as.numeric(opts$row_mm %||% NA))
  if (length(pitch) != 1L || !is.finite(pitch)) {
    return(TRUE)
  }
  pitch >= LEADER_MIN_PITCH_MM * .scale_of(opts)
}

tree_tiplab_layer <- function(opts, md, layer = NULL, offset = 0) {
  # Labels off still draws the leader lines where there is something for them
  # to lead to. They are what makes a tree with ragged tip depths readable
  # without labels: without them the eye has to carry a row across an empty
  # band to whatever is annotated beside it. The label itself becomes a single
  # space — an empty string makes ggtree drop the layer, and with it the lines.
  hidden <- !isTRUE(opts$tiplab_show)
  align <- .leaders_drawn(opts)
  if (hidden && !align) {
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
    size = .tiplab_size(opts, md),
    # Aligning draws a leader line from each tip out to the axis limit
    # (`.leaders_drawn()` decides whether that is worth doing).
    align = align,
    # At the tree's own stroke, lightened. ggtree's default is a constant, so
    # the one part of the drawing that is not fitted to the tip count was the
    # part there is most of.
    linesize = (opts$branch_width %||% tree_branch_width(nrow(md))) *
      LEADER_WIDTH_FRAC *
      .scale_of(opts),
    geom = "text"
  )

  # An inward tree grows from the outside in, so its x axis is reversed and a
  # label left-anchored at its tip would run off the canvas instead of toward
  # the centre. Right-anchoring is the layout's own convention.
  if (inward) {
    params$hjust <- 1
  }

  # Nudge the label clear of its tip point. The offset is in x-axis units and
  # solved by the caller (it needs the tree's span), so an empty tip point and
  # a fat one leave the same visible gap. Skipped for an inward tree, whose
  # labels run toward a reversed axis where a positive offset would push them
  # the wrong way.
  if (!inward && !hidden && isTRUE(is.finite(offset) && offset > 0)) {
    params$offset <- offset
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
  if (is.null(aesthetic) || !length(aesthetic)) {
    return(FALSE)
  }
  if (identical(aesthetic, "tiplab_color")) {
    return(isTRUE(opts$tiplab_show))
  }
  if (aesthetic %in% c("tippoint_color", "tippoint_shape")) {
    return(isTRUE(opts$tippoint_show))
  }
  # A tile strip stands past the tips, and an inward tree has nowhere past its
  # tips to put one (`tree_annotations_drawn()`). The marks are already left
  # off there; without this the scale behind them was not, which is both the
  # warning above and a guide column reserved on the canvas for keys nothing
  # was drawn in.
  if (identical(aesthetic, "tile")) {
    return(tree_annotations_drawn(opts))
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

  size <- .branch_size(opts) * BRANCH_ABOVE_SHRINK
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

# --- Clade highlights ---------------------------------------------------------
#
# A highlight names a group of tips: a wash behind the clade, optionally with a
# bar and a caption beside it. The reader adds them one node at a time and each
# carries its own colour and its own text, so everything about one highlight
# lives on its record rather than in a control shared by all of them.

# Behind the tree rather than over it, so the branches a highlight groups stay
# readable through it. `to.bottom` in tree_clade_layers() is the other half of
# that; the alpha alone is not enough at these saturations.
CLADE_ALPHA <- 0.45

# Type size a caption is set at, in mm at scale 1 before the reader's text bias.
# A shade over the axis numbers: a caption names a group rather than being read
# off one.
CLADE_LABEL_SIZE <- 3.2

# The caption column in mm at scale 1: the bar's thickness, the gutter between
# the last annotation and the bar, the gap from the bar to its text, and the
# air after the text.
#
# That last one is not symmetry for its own sake. Without it the column ends
# exactly where the widest caption ends, and the only thing between the last
# glyph and the paper's edge is `CLADE_TEXT_SLACK` — a percent, which is a
# pixel or two. The caption fitted, and looked cut off, which is how it was
# reported; a column is read by its air as much as by its width.
CLADE_BAR_MM <- 1.1
CLADE_BAR_GAP_MM <- 2.4
CLADE_TEXT_GAP_MM <- 1.4
CLADE_TAIL_GAP_MM <- 1.4

# Millimetres one ggplot2 `linewidth` draws, so a stroke that has to match a
# width booked in millimetres can be asked for in the units it was booked in.
LINEWIDTH_MM <- 72.27 / 96

# How far past its outermost tips a bar runs, in tip rows. A tile reaches half a
# row past the last tip (TILE_TOP_ROWS), so a bar that stopped at the tip itself
# would read as falling short of the clade it brackets.
CLADE_BAR_EXTEND <- 0.5

# Most of the figure's width the caption column may claim. The canvas grows for
# it (`tree_panel_width_in()`), so this is not the tree paying for the captions
# — it is the ceiling past which a long caption is set smaller, and then elided,
# instead of the figure growing wider without end.
CLADE_LABEL_CAP <- 0.3

# Safety factor on the caption column, for the same reason X_EXPANSION is one
# on the tip-label reserve: the right of the axis carries no ggplot2 expansion
# to absorb a caption that runs a little long, so without it the last glyph of
# the widest caption sits exactly on the panel edge — a clipped glyph rather
# than a tight fit. `CHAR_EM` is within about a percent of what the export
# devices set, and this covers the percent.
CLADE_TEXT_SLACK <- 1.06

#' Colours a clade highlight is given as the reader adds one.
#'
#' Cycled by the view so no two highlights open in the same colour — telling
#' them apart is the whole reason a reader adds a second one. Mid-tone
#' throughout: each has to survive being washed to `CLADE_ALPHA` behind the tree
#' *and* read as a solid bar beside it, which rules out the pastels at one end
#' and the near-blacks at the other.
#' @export
CLADE_PALETTE <- c(
  "#4E79A7",
  "#F28E2B",
  "#59A14F",
  "#E15759",
  "#B07AA1",
  "#76B7B2",
  "#FF9DA7",
  "#9C755F"
)

# The highlights as records, whatever shape the caller had them in.
#
# A tree saved before highlights carried their own colour holds a list of nodes
# and one shared colour instead. Rebuilding those here is what stops a saved
# Analysis losing its highlights the first time it is reopened.
.clade_records <- function(opts) {
  given <- opts$clades %||% list()
  if (!length(given)) {
    nodes <- suppressWarnings(as.integer(opts$parentnodes %||% integer(0)))
    nodes <- nodes[!is.na(nodes)]
    if (!length(nodes)) {
      return(list())
    }
    fill <- opts$clade_color %||% CLADE_PALETTE[[1]]
    return(lapply(nodes, function(n) list(node = n, color = fill, label = "")))
  }
  out <- lapply(given, function(x) {
    n <- suppressWarnings(as.integer(x$node %||% NA))
    if (is.na(n)) {
      return(NULL)
    }
    list(
      node = n,
      color = x$color %||% CLADE_PALETTE[[1]],
      label = trimws(as.character(x$label %||% ""))
    )
  })
  Filter(Negate(is.null), out)
}

# The highlights that carry a caption. A blank caption is the ordinary case —
# the wash is the annotation — so an uncaptioned highlight reserves nothing.
#
# An inward tree has no room past its tips for any annotation
# (`tree_annotations_drawn()`); its highlights still draw, but nothing is
# written beside them.
.clade_captions <- function(opts) {
  if (!tree_annotations_drawn(opts)) {
    return(list())
  }
  Filter(function(x) nzchar(x$label), .clade_records(opts))
}

# Ems the widest caption sets in, as it will be set.
#
# Measured rather than counted (`CHAR_EM`): the widest caption is not always
# the longest one, and the column has to hold whichever it is.
.clade_ems <- function(opts) {
  caps <- .clade_captions(opts)
  if (!length(caps)) {
    return(0)
  }
  set <- vapply(caps, function(x) .clade_caption_text(opts, x$label), "")
  max(vapply(set, .string_em, numeric(1)), 0)
}

# The part of the caption column that is not text.
.clade_gaps_mm <- function(opts) {
  (CLADE_BAR_MM + CLADE_BAR_GAP_MM + CLADE_TEXT_GAP_MM + CLADE_TAIL_GAP_MM) *
    .scale_of(opts)
}

# Inches the caption column may claim.
#
# Against the figure's own width, not `tree_budget_in()`. That one is the
# *radius* on a circular tree, and a caption set along the outside of the disc
# is on the page rather than on the radius — capping it against a radius left
# every circular caption at the legibility floor.
.clade_cap_in <- function(opts) {
  budget <- tree_budget_in(opts)
  if (.is_circular(opts)) {
    budget <- budget / TREE_RADIAL_FRAC
  }
  CLADE_LABEL_CAP * budget
}

# Ems the column holds at the smallest size worth reading.
#
# A caption is the one piece of type on this figure the engine did not choose
# the content of, so it is the one place the two fitting rules can genuinely
# fail to meet: past this width there is no size that is both inside the column
# and legible. Elided rather than shrunk into a smudge, and rather than
# widening the figure for a caption nobody could read at the end of it.
.clade_max_em <- function(opts) {
  room_mm <- max(.clade_cap_in(opts) * 25.4 - .clade_gaps_mm(opts), 0)
  floor_mm <- TIP_SIZE_FLOOR * .scale_of(opts)
  max(room_mm / (floor_mm * CLADE_TEXT_SLACK), 1)
}

# One caption as it will actually be set.
#
# Cut by width rather than by character count, since that is what runs out —
# eight capitals set wider than eleven lowercase letters.
.clade_caption_text <- function(opts, label) {
  label <- as.character(label %||% "")
  limit <- .clade_max_em(opts)
  if (.string_em(label) <= limit) {
    return(label)
  }
  ch <- strsplit(label, "", fixed = TRUE)[[1]]
  room <- limit - .string_em("\u2026")
  used <- 0
  keep <- 0L
  for (i in seq_along(ch)) {
    used <- used + .string_em(ch[[i]])
    if (used > room) {
      break
    }
    keep <- i
  }
  paste0(substr(label, 1L, max(keep, 1L)), "\u2026")
}

# Type size the captions are set at, under the same two rules as every other
# label on the figure: what the reader's text bias asks for, never wider than
# the column may claim, never under what can be read.
.clade_label_size <- function(opts) {
  want <- CLADE_LABEL_SIZE * .type_of(opts)
  ems <- .clade_ems(opts)
  if (!ems) {
    return(want)
  }
  floor_mm <- TIP_SIZE_FLOOR * .scale_of(opts)
  room_mm <- max(.clade_cap_in(opts) * 25.4 - .clade_gaps_mm(opts), 0)
  fits <- room_mm / (ems * CLADE_TEXT_SLACK)
  .clamp(want, floor_mm, max(fits, floor_mm))
}

# Inches of canvas the caption column needs, past every other annotation.
#
# Unlike a tile strip or a heatmap panel this is a physical width rather than a
# multiple of the tree's span: a caption is text the reader typed, and text does
# not grow with the tree. So it is booked the way the axis overhang is
# (`.axis_edge_in`) — solved into the axis by `.tiplab_xlim()`, with the canvas
# grown to match in `tree_panel_width_in()` so the column is not taken off the
# tree and its labels.
.clade_edge_in <- function(opts) {
  ems <- .clade_ems(opts)
  if (!ems) {
    return(0)
  }
  text_mm <- ems * .clade_label_size(opts) * CLADE_TEXT_SLACK
  (.clade_gaps_mm(opts) + text_mm) / 25.4
}

# The tips under a node, found by walking the parent links down from it. The
# bar beside a clade spans exactly these rows.
.clade_tip_rows <- function(tree_data, node) {
  front <- node
  seen <- integer(0)
  repeat {
    kids <- tree_data$node[
      tree_data$parent %in% front & tree_data$node != tree_data$parent
    ]
    kids <- setdiff(kids, seen)
    if (!length(kids)) {
      break
    }
    seen <- c(seen, kids)
    front <- kids
  }
  tree_data[tree_data$node %in% c(node, seen) & tree_data$isTip, , drop = FALSE]
}

# The tree data a highlight takes its corners from.
#
# Every tip on an aligned tree is drawn out to the same edge — that is what the
# leader lines do — so a wash that stopped at its own clade's deepest tip ended
# partway along the band it was naming, and the reader had to carry the group
# across the gap the highlight was there to close. Nested clades were worse: two
# highlights over the same rows ended at two different places for no reason a
# reader could see.
#
# Done by handing `geom_hilight()` a copy of the tree data with the tips already
# aligned, rather than by moving the rect afterwards: it computes its own
# corners (`ggtree:::get_clade_position_`), and this frame is the one input it
# takes them from. Only the tips move, so each clade's *left* edge stays on its
# own root branch.
#
# An inward tree draws its tips where they fall — there is no alignment line to
# reach — so it keeps ggtree's own corners.
.aligned_clade_data <- function(opts, tree_data) {
  if (identical(opts$layout, "inward") || !isTRUE(any(tree_data$isTip))) {
    return(tree_data)
  }
  edge <- suppressWarnings(max(tree_data$x[tree_data$isTip], na.rm = TRUE))
  if (!isTRUE(is.finite(edge))) {
    return(tree_data)
  }
  tree_data$x[tree_data$isTip] <- edge
  tree_data
}

# The wash behind each highlighted clade.
#
# Added in reverse so the *first* highlight ends up deepest in the stack: each
# `to.bottom` rect is inserted at the bottom as it is added, which would
# otherwise bury a clade nested inside one the reader added earlier.
tree_clade_layers <- function(opts, tree_data) {
  clades <- .clade_records(opts)
  # A highlight on a node this tree does not have cannot be drawn, and asking
  # for one is an error rather than an empty layer. It is reachable: a Generate
  # on a smaller isolate selection leaves fewer internal nodes than the
  # highlight was added against.
  clades <- Filter(function(cl) cl$node %in% tree_data$node, clades)
  if (!length(clades)) {
    return(NULL)
  }
  frame <- .aligned_clade_data(opts, tree_data)
  lapply(rev(clades), function(cl) {
    geom_hilight(
      data = frame,
      node = cl$node,
      fill = cl$color,
      alpha = CLADE_ALPHA,
      to.bottom = TRUE
    )
  })
}

# The bar and caption beside each highlighted clade.
#
# Two layers for the whole set rather than two per clade, and the colours are
# passed as a per-row parameter rather than mapped: a mapped colour would open a
# scale and put a guide in the legend box, and the bar already sits next to the
# thing it names.
#
# The bar takes the clade's colour and the caption takes the figure's ink. A
# caption set in its own highlight's colour is the one piece of type here that
# has no room to be fitted — it is as wide as what was typed — so it is the one
# piece that cannot afford to be set in a colour chosen for a wash.
tree_cladelab_layers <- function(opts, tree_data, at) {
  caps <- .clade_captions(opts)
  if (!length(caps) || is.null(at)) {
    return(NULL)
  }
  circular <- .is_circular(opts)
  rows <- lapply(caps, function(cl) {
    if (!cl$node %in% tree_data$node) {
      return(NULL)
    }
    tips <- .clade_tip_rows(tree_data, cl$node)
    ys <- tips$y[is.finite(tips$y)]
    if (!length(ys)) {
      return(NULL)
    }
    # A radial tree's y is an angle, so the caption is set along the tip it
    # sits beside; on the left half of the disc that reading is upside down,
    # and the fix is the one ggtree uses for its own tip labels — turn the
    # text through 180 and anchor it at the other end, so it still grows
    # outward from the rim.
    ang <- if (circular) mean(range(tips$angle)) else 0
    flip <- circular && ang > 90 && ang < 270
    data.frame(
      label = .clade_caption_text(opts, cl$label),
      color = cl$color,
      x = at$x,
      text_x = at$text_x,
      lo = min(ys) - CLADE_BAR_EXTEND,
      hi = max(ys) + CLADE_BAR_EXTEND,
      mid = mean(range(ys)),
      angle = if (flip) ang - 180 else ang,
      hjust = if (flip) 1 else 0,
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  list(
    geom_segment(
      data = rows,
      mapping = aes(x = x, xend = x, y = lo, yend = hi),
      colour = rows$color,
      linewidth = CLADE_BAR_MM * .scale_of(opts) / LINEWIDTH_MM,
      lineend = "round",
      inherit.aes = FALSE,
      show.legend = FALSE
    ),
    geom_text(
      data = rows,
      mapping = aes(
        x = text_x,
        y = mid,
        label = label,
        angle = angle,
        hjust = hjust
      ),
      colour = opts$line_color,
      size = .clade_label_size(opts),
      vjust = 0.5,
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
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
  legend_max_rows = LEGEND_MAX_ROWS,
  legend_plan = NULL
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
          max_rows = legend_max_rows,
          max_keys = .plan_keys(legend_plan, legend_guide_id("layer", tile)),
          order = .plan_order(legend_plan, legend_guide_id("layer", tile)),
          ncol = .plan_ncol(legend_plan, legend_guide_id("layer", tile))
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
      HEADER_SIZE_MAX * .type_of(opts)
    } else {
      tree_header_size(
        pwidth,
        axis_units,
        axis_in,
        .scale_of(opts),
        .text_of(opts)
      )
    }
    # A strip too narrow to carry its variable's name legibly carries none: the
    # colours still mean what the guide says they mean, and a smear of type
    # above them says nothing at all.
    if (!tree_header_drawn(header_size, .scale_of(opts))) {
      return(layers)
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
#
# ggtree replaces its own y scale while assembling a circular/fan base, and the
# annotation and heatmap reserves replace it again (the `scale_y_continuous()`
# calls below), on purpose and sometimes twice. ggplot2 announces each
# replacement — as a message in some versions and a warning in others — so the
# pattern sits in both muffler lists rather than trusting the inline
# `suppressMessages()`, which never saw ggtree's own and misses the warning form.
.scale_present_chatter <- "Scale for .* is already present"

.muffled_tree_messages <- paste(
  "Invaild edge matrix",
  "invalid tbl_tree object",
  .scale_present_chatter,
  sep = "|"
)

.muffled_tree_warnings <- paste(
  "size.*aesthetic for lines",
  "linewidth",
  "label\\.size",
  "Removed \\d+ rows containing missing values",
  "one unique value with `geom = geom_tile`",
  .scale_present_chatter,
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

  # Isolate labels the rows cannot carry legibly are switched off here, before
  # anything is measured against them — the label reserve, the annotation
  # offsets, the aesthetics a mapping may still be drawn on and the guide it
  # would contribute all read `opts$tiplab_show`, and they have to read the
  # same answer. A tree at three hundred tips and half an inch of pitch is the
  # ordinary way to get here; so is winding the text-size control up on one at
  # eighty.
  if (!tree_tiplab_drawn(opts, md)) {
    opts$tiplab_show <- FALSE
  }

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
  # ...and every piece of type by this as well (see .text_of).
  text <- .text_of(opts)
  # How tall this plot is drawn. It decides two things nothing else can: how
  # many rows a header of a given height occupies, and whether the guides fit
  # in one column. A circular panel is square and grows with its rings, so its
  # height is the *panel* — not the tree-and-labels budget times an aspect
  # ratio that layout does not use.
  plot_height_in <- if (opts$layout %in% .circular_layouts) {
    panel_in
  } else {
    tree_canvas_height_in(opts, md)
  }
  # The height of the *image*, which is the panel's on a linear tree and the
  # disc's on a radial one (see `tree_image_height_in`). Everything measured
  # against the rows — the tip pitch, the header and class bands — belongs to
  # the panel and keeps `plot_height_in`; the guide box stands in the image and
  # is fitted to this, which is also what the view reserves the canvas from.
  #
  # Provisional on a radial tree, because the disc it is measured from is not
  # yet known: how far the ceiling squeezes the panel depends on how wide the
  # guide box is, and how wide the guide box is depends on how tall it may run.
  # One pass settles it — the height only decides how many columns the keys
  # wrap into, and a column either fits or it does not.
  image_height_in <- tree_image_height_in(opts, plot_height_in)
  # How tall any one guide may run before its keys wrap into another column.
  #
  # `tree_guide_inputs()` rather than `opts` throughout, so the box is budgeted
  # for the guides this layout will really draw: an inward tree's tile strips
  # and heatmap panels are not drawn at all, and a column reserved for their
  # keys is a column of blank paper.
  guides <- tree_guide_inputs(opts)
  legend_size <- tree_legend_size(opts, image_height_in, md)
  legend_max_rows <- tree_legend_max_rows(
    guides$layers,
    guides$heatmaps,
    legend_size,
    image_height_in,
    scale
  )
  # How much of the panel the annotations asked for the image can actually hold
  # (`tree_panel_squeeze`). Carried on `opts` because every physical reserve is
  # solved against it in `.tiplab_xlim()`, which is called from more than one
  # place and has no way of measuring the guide box for itself.
  opts$panel_squeeze <- tree_panel_squeeze(
    opts,
    panel_in,
    tree_legend_width_in(
      guides$layers,
      md,
      legend_size,
      opts$width_in %||% 5.5,
      guides$heatmaps,
      image_height_in,
      scale
    )
  )
  # The disc as it will really be drawn, now that the squeeze is known.
  image_height_in <- tree_image_height_in(opts, plot_height_in)
  # One solve for the whole guide box: what each guide may list, and where it
  # stacks. Read back by id as each scale is built, so a guide's key budget and
  # its place in the box are decided together rather than each scale guessing.
  legend_plan <- tree_legend_plan(
    guides$layers,
    guides$heatmaps,
    legend_size,
    image_height_in,
    scale,
    md
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
      # Thinned with the tip count (`tree_branch_width`) so a few hundred
      # branches stay separate lines rather than filling in. Solved here, not
      # read off `opts`: there is no sidebar control for branch width, so
      # nothing in the view carries the fitted value onto it.
      linewidth = (opts$branch_width %||% tree_branch_width(nrow(md))) * scale,
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

  # The pitch the tip rows are *really* drawn at.
  #
  # A y-scale expansion does not grow the plot, it compresses what is already
  # in it: the band the column headers need above the last tip and the class
  # names below the first are taken out of the height the rows had, and over a
  # wide heatmap the two together can leave the tips little more than a third
  # of it. Fitting an isolate label to `height_in / n_tip` therefore sizes it
  # for rows the plot does not have, and thirty names sized that way print as
  # one black bar — which is what they did.
  #
  # Two passes, and only two. The reserve above depends on the header sizes,
  # which depend on how much x axis is left once the labels have taken their
  # share, which is what the first `.tiplab_xlim()` above answers; the labels
  # are then fitted to the pitch that comes out of it and the axis re-solved so
  # the reserve beside them matches the size finally drawn. The second round
  # can only shrink the labels, so there is nothing for a third to settle.
  #
  # Linear only. A radial tree's rows are arcs of its own disc and no y
  # expansion touches them.
  if (!circular) {
    n_tip <- sum(tree_data$isTip)
    band_runs <- .class_band_runs(opts)
    band_type <- tree_header_size(
      .heat_span(opts) * .annotation_squeeze(opts) * tree_span,
      fit$limit - suppressWarnings(min(tree_data$x, na.rm = TRUE)),
      axis_in,
      scale,
      text
    )
    opts$row_mm <- .drawn_tip_pitch(
      opts,
      n_tip,
      plot_height_in,
      heatmap_header_frac(
        opts,
        n_tip,
        tree_span,
        fit$limit - suppressWarnings(min(tree_data$x, na.rm = TRUE)),
        panel_in,
        plot_height_in
      ),
      heatmap_class_frac(opts, n_tip, band_runs, band_type, plot_height_in),
      band_runs,
      band_type,
      .header_stack_mm(opts, band_type)
    )
    if (!tree_tiplab_drawn(opts, md)) {
      opts$tiplab_show <- FALSE
    }
    fit <- .tiplab_xlim(opts, md, tree_data, max_x, annot_total)
  } else {
    # A disc has a pitch too, and the leader lines are decided from it
    # (`.leaders_drawn()`). Nothing else on a radial tree reads it — the label
    # room there is solved along the ring instead (`.tiplab_room()`).
    x_min <- suppressWarnings(min(tree_data$x, na.rm = TRUE))
    span <- fit$limit - x_min
    opts$row_mm <- .radial_tip_pitch_mm(
      opts,
      panel_in,
      if (isTRUE(span > 0)) (max_x - x_min) / span else NA_real_,
      sum(tree_data$isTip)
    )
  }
  label_reserve <- fit$reserve

  # How much x axis the panel spans, which is what turns a column's width in
  # data units into its width on the page — and so into a type size that fits
  # it.
  axis_units <- fit$limit - suppressWarnings(min(tree_data$x, na.rm = TRUE))

  # Now the axis is solved, how much of a column's name fits on it is knowable.
  opts$heatmaps <- .resolve_header_visibility(
    opts,
    axis_units,
    axis_in,
    tree_span
  )

  # Inches the tree's *own* span is drawn across — what is left of the
  # tree-and-labels budget once the labels have taken their fraction. This is
  # the only thing that says whether a given branch is physically wide enough
  # to print a number on (tree_branch_keep); the annotations are paid for by a
  # wider canvas, so they do not come out of it.
  span_in <- tree_budget_in(opts) * (1 - .tiplab_budget_frac(opts, md))

  # The tip-label nudge, in x-axis units: a physical gap (mm) becomes data
  # units through how many inches the tree's own span is drawn across. The
  # label reserve already carries the same gap as slack (see X_EXPANSION), so
  # the strip past the labels clears it without a separate booking.
  tiplab_offset <- if (isTRUE(is.finite(span_in) && span_in > 0)) {
    .tiplab_point_gap_mm(opts) / 25.4 * tree_span / span_in
  } else {
    0
  }

  # Assemble plot layers (order maintains visual hierarchy).
  #
  # The new_scale_color() invariant, stated so it survives future edits: emit
  # it *after* the scale it closes and *before* the next geom that maps colour.
  # Each colour-carrying layer contributes exactly one scale + new_scale_color()
  # pair; the shape layer contributes neither, because nothing else in the plot
  # maps shape.
  layers <- c(
    tree_clade_layers(opts, tree_data),
    list(tree_tiplab_layer(opts, md, lab_l, tiplab_offset)),
    if (!is.null(lab_l)) {
      list(
        tree_scale(
          md[[lab_l$field]],
          lab_l$palette,
          "color",
          name = lab_l$title,
          max_rows = legend_max_rows,
          max_keys = .plan_keys(legend_plan, legend_guide_id("layer", lab_l)),
          order = .plan_order(legend_plan, legend_guide_id("layer", lab_l)),
          ncol = .plan_ncol(legend_plan, legend_guide_id("layer", lab_l))
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
          max_rows = legend_max_rows,
          max_keys = .plan_keys(legend_plan, legend_guide_id("layer", pt_l)),
          order = .plan_order(legend_plan, legend_guide_id("layer", pt_l)),
          ncol = .plan_ncol(legend_plan, legend_guide_id("layer", pt_l))
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
      shp_id <- legend_guide_id("layer", shp_l)
      shp_keys <- tree_legend_breaks(
        names(shp_values),
        md[[shp_l$field]],
        .plan_keys(legend_plan, shp_id)
      )
      # `NA` rather than a colour for the gap key here: a shape scale's values
      # are glyph codes, and NA is the one that draws nothing — the same empty
      # key the colour scales get from a transparent swatch.
      list(scale_shape_manual(
        values = .legend_values(shp_values, shp_keys$breaks, blank = NA),
        limits = .legend_limits(names(shp_values), shp_keys$breaks),
        breaks = shp_keys$breaks,
        name = tree_legend_title(
          shp_l$title,
          shp_keys$hidden,
          shp_keys$total
        ),
        labels = .wrap_legend_labels,
        guide = guide_legend(
          ncol = .plan_ncol(legend_plan, shp_id),
          order = .plan_order(legend_plan, shp_id)
        )
      ))
    },
    if (isTRUE(opts$nodelabel_show)) {
      list(geom_nodelab(
        aes(label = .data[["node"]]),
        size = NODE_LABEL_SIZE * .type_of(opts)
      ))
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
      legend_max_rows,
      legend_plan
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
        fontsize = AXIS_LABEL_SIZE * .type_of(opts)
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

  # Past every annotation, at the x `fit` reserved for them.
  for (layer in tree_cladelab_layers(opts, tree_data, fit$clade) %||% list()) {
    p <- p + layer
  }

  # `fit` was solved before the layers were assembled, because the tile strips
  # needed the label reserve to place themselves. An inward tree already
  # carries it as its build range (see `inward_xlim`), and adding it again as a
  # scale limit clips the reversed axis instead of extending it.
  #
  # `expand` is pinned to zero on the right: every annotation past the tips is
  # placed in x-axis units measured against `fit$limit`, so ggplot2's default
  # 5% expansion there would stretch the axis under them and shrink the gap
  # each was given — a tip label that cleared the strip in the solve ended up
  # under it on the page. The right margin the annotations need is
  # ANNOTATION_SLACK, already inside `fit$limit`.
  #
  # On the left a linear tree keeps the default, so the root and the leftmost
  # branch do not sit on the panel edge. A radial tree takes zero there too,
  # and for the opposite reason: its x axis is a *radius*, its left edge is the
  # centre of the disc, and expansion there is not a margin — it is a hole
  # punched through the middle that pushes every ring outward and costs the
  # drawing the outermost twentieth of its radius. The disc has margin enough:
  # CoordPolar draws it across four fifths of a square panel however tightly
  # the axis is fitted (`COORD_POLAR_FRAC`).
  if (!inward) {
    left_expand <- if (circular) 0 else 0.05
    p <- p +
      scale_x_continuous(
        limits = c(NA, fit$limit),
        expand = expansion(mult = c(left_expand, 0))
      )
  }
  # Room above the last tip for the annotation headers, which are set
  # vertically and would otherwise be clipped by the panel, and room under the
  # first for the axis numbers' own depth. Set unconditionally rather than only
  # where there are annotations: a bare tree still carries an axis, and
  # ggtree's default 5% was never enough to hold a line of type — which is how
  # the numbers came out sliced along their middle. Replacing ggtree's y scale
  # is the point, so its announcement is not news — it is muffled by
  # `build_tree_ggtree()` (`.muffled_tree_warnings`).
  if (!circular) {
    p <- suppressMessages(
      p +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              max(
                0.02,
                .axis_frac(
                  opts,
                  sum(tree_data$isTip),
                  height_in = plot_height_in
                )
              ),
              if (annotation_total(opts) > 0) {
                heatmap_header_frac(
                  opts,
                  sum(tree_data$isTip),
                  tree_span,
                  axis_units,
                  panel_in,
                  plot_height_in
                )
              } else {
                0.02
              }
            )
          )
        )
    )
  }

  # A *numeric* legend.position floats the guide box inside the panel, over the
  # tips, with nothing stopping it running off the canvas — which is what put
  # the legend on top of the tree and clipped it at the edge. A string position
  # makes ggplot2's gtable allocate a real guide-box column outside the panel,
  # sized to the widest key label. That is the reserved area, computed by the
  # layout engine rather than guessed at with two sliders.
  p <- p +
    theme_tree(bgcolor = opts$bg) +
    theme(
      plot.margin = do.call(
        margin,
        c(as.list(tree_plot_margin_in(opts, panel_in)), list(unit = "in"))
      ),
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
      # Top-aligned beside a linear tree, which is read from the top down.
      # Centred beside a disc, which has no top — and, more to the point, a
      # radial figure's plot margin is pulled inside the image
      # (`tree_plot_margin_in`), so a box justified to the top of the *plot*
      # starts above the top of the paper.
      legend.justification = if (circular) "centre" else "top",
      legend.box.spacing = unit(4, "pt"),
      # A shade over the keys rather than ggplot2's 1.2: the title is the
      # longest string in the box and the box is a column of the figure, so
      # every point of it is width taken from the tree.
      legend.title = element_text(
        color = opts$line_color,
        size = legend_size * LEGEND_TITLE_RATIO
      ),
      legend.text = element_text(
        color = opts$line_color,
        size = legend_size
      ),
      legend.key.size = unit(0.05 * legend_size, "cm"),
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
  # The element-type labels, split by which end they were sent to, and the
  # tallest stack of column headers any panel drew — the top labels clear that.
  element_specs <- list(top = list(), bottom = list())
  # Held in millimetres, not rows: a stack of rotated names is a fixed physical
  # height, and the rows under it move with the reserves (see `.drawn_row_mm()`).
  header_mm_max <- 0
  panels <- if (tree_annotations_drawn(opts)) {
    heatmap_panels(opts, tree_span, label_reserve)$panels
  } else {
    list()
  }
  for (pan_i in seq_along(panels)) {
    pan <- panels[[pan_i]]
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
      # A panel of a hundred genes is a texture rather than a list, and the
      # names over it are a smear; switched off, the matrix keeps its meaning
      # through the guide and the class strip.
      colnames = !isFALSE(pan$show_gene_names),
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
      font.size = tree_header_size(cell, axis_units, axis_in, scale, text)
    )

    # gheatmap installs a default fill scale of its own, so replacing it is the
    # intended move — but ggplot2 announces every replacement, and this one is
    # not news. Deliberate, so silenced here rather than logged on every draw.
    # Gene panels colour by AMRFinderPlus's method tiers on this panel's own
    # `amr_confidence_palette`; the dormant presence/absence branch keeps its
    # two-colour fill. Either way the guide is one confidence scale.
    fill <- .heatmap_fill(pan)
    lvls <- levels(frame[[1]])
    # The tiers this panel's guide lists. Fixed, not taken from the matrix:
    # gheatmap gathers the frame into a long column whose values ggplot2 then
    # trains the scale on, so a panel that happens to hold no partial call had
    # no key for one either — two panels side by side listing two tiers and
    # four, as though they had been scored differently.
    keys <- .tier_guide_levels(
      pan,
      unlist(lapply(frame, function(v) as.character(unique(v))))
    )
    # `limits` puts the missing tiers back in the guide, but ggplot2 draws a
    # key's swatch only where some layer's *data* holds that value — so they
    # arrived as labels beside an empty square. One zero-area rectangle per
    # tier, at a coordinate this panel already occupies, is the data the guide
    # needs and nothing at all on the figure.
    #
    # A rectangle rather than a tile so that "the layers that draw this matrix"
    # stays exactly the tile layers, here and in the tests that count them.
    p <- p +
      geom_rect(
        data = data.frame(
          .x = max_x + pan$offset,
          .y = 1,
          .tier = factor(keys, levels = lvls)
        ),
        mapping = aes(
          xmin = .data[[".x"]],
          xmax = .data[[".x"]],
          ymin = .data[[".y"]],
          ymax = .data[[".y"]],
          fill = .data[[".tier"]]
        ),
        inherit.aes = FALSE
      )
    p <- suppressMessages(
      p +
        scale_fill_manual(
          values = fill[lvls],
          limits = keys,
          # Without explicit breaks the legend sorts its keys alphabetically —
          # "Absent, Partial, Perfect, Putative, Strong" — which reads as five
          # unrelated categories. These are a confidence scale, so the guide
          # lists them as one, strongest tier first.
          breaks = if (identical(pan$level, "gene")) rev(keys) else keys,
          name = pan$title,
          na.value = fill[[AMR_ABSENT]],
          guide = guide_legend(
            ncol = .plan_ncol(legend_plan, legend_guide_id("heat", pan, pan_i)),
            order = .plan_order(
              legend_plan,
              legend_guide_id("heat", pan, pan_i)
            )
          ),
          drop = FALSE
        )
    )

    # Which classes this panel's columns fall into, and where they sit. A
    # circular panel has no room under it — "below the matrix" is the centre of
    # the disc — so the band is a linear-layout annotation only.
    #
    # Column k is centred one cell past the offset gheatmap was given, which is
    # `pan$offset - cell / 2` — so the first column lands on pan$offset +
    # cell / 2, the middle of the space reserved for it.
    centres <- max_x + pan$offset - cell / 2 + seq_len(ncol(frame)) * cell

    # How tall this panel's own headers stand *in millimetres*, so the element
    # labels above them can clear the tallest set on the figure once the drawn
    # row pitch is known. Zero when the names are off — the element label then
    # takes the row they would have had.
    if (!isFALSE(pan$show_gene_names)) {
      header_mm_max <- max(
        header_mm_max,
        HEADER_ROW_PACK *
          suppressWarnings(max(nchar(names(frame)), 1L)) *
          TIP_CHAR_EM *
          tree_header_size(cell, axis_units, axis_in, scale, text)
      )
    }

    # This panel's element-type label, filed under the end it was sent to. The
    # x is the middle of its own run of columns, so the label reads as naming
    # that matrix and no other.
    if (!circular && .element_label_drawn(pan)) {
      pos <- .element_pos(pan)
      element_specs[[pos]] <- c(
        element_specs[[pos]],
        list(list(
          label = .element_label_text(pan),
          x = (centres[[1L]] + centres[[length(centres)]]) / 2,
          size = .element_label_size(
            .element_label_text(pan),
            pan$width * tree_span,
            axis_units,
            axis_in,
            scale,
            # Never smaller than this panel's own gene names.
            floor_size = tree_header_size(cell, axis_units, axis_in, scale, text),
            text = text
          )
        ))
      )
    }

    runs <- heatmap_class_runs(pan, names(frame))
    if (!circular && nrow(runs)) {
      class_runs <- c(class_runs, list(runs))
      class_layers <- c(
        class_layers,
        list(list(
          runs = runs,
          centres = centres,
          cell = cell,
          # Set vertically, so what has to fit across is the type height against
          # one column — the same constraint the column names answer to above.
          size = tree_header_size(cell, axis_units, axis_in, scale, text)
        ))
      )
    }

    # A clustered panel gets the strip and the dendrogram in place of those
    # brackets (heatmap_class_runs returns none for it). Added inside the loop
    # so the strip's own fill scale is closed by the next panel's
    # `new_scale_fill()`, which is what keeps two panels' guides apart.
    if (!circular && isTRUE(pan$cluster)) {
      for (layer in .heatmap_cluster_layers(
        pan,
        (pan$classes %||% character(0))[
          match(names(frame), pan$labels %||% pan$cols)
        ],
        centres,
        cell,
        attr(frame, "hclust"),
        sum(tree_data$isTip),
        opts$line_color %||% "#000000",
        .class_guide_levels(pan),
        .plan_keys(legend_plan, legend_guide_id("class", pan, pan_i)),
        .plan_order(legend_plan, legend_guide_id("class", pan, pan_i)),
        .plan_ncol(legend_plan, legend_guide_id("class", pan, pan_i)),
        # The tree's own stroke, so the two dendrograms on the figure are one
        # drawing — thinner only where this panel's columns are packed tighter
        # than the tips are.
        min(
          (opts$branch_width %||% tree_branch_width(sum(tree_data$isTip))),
          tree_branch_width(ncol(frame))
        ) *
          scale
      )) {
        p <- suppressMessages(p + layer)
      }
    }
  }

  # Placed after the loop so every panel's band is set at one size — two panels
  # whose classes were named at different sizes would read as two kinds of
  # thing rather than one row of annotation. Shrunk from the header size when a
  # long class name would otherwise want a band deeper than the reserve can
  # hold, so the names are never clipped at their far end.
  band_size <- if (length(class_layers)) {
    .class_name_size(
      min(vapply(class_layers, function(l) l$size, numeric(1))),
      sum(tree_data$isTip),
      class_runs,
      .scale_of(opts)
    )
  } else {
    NULL
  }
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

  # The element-type labels, placed once the two things they have to clear are
  # known: above, the tallest stack of column headers; below, the deepest band —
  # the same `.bottom_band_rows()` the reserve under them was measured from.
  #
  # The headers' height is physical, so it is converted at the pitch the plot is
  # really drawn at rather than the nominal one. The reserves it needs for that
  # are the very ones set on the scale below, asked for here first.
  if (length(element_specs$top) || length(element_specs$bottom)) {
    n_tip_drawn <- sum(tree_data$isTip)
    band_rows <- .bottom_band_rows(opts, n_tip_drawn, class_runs, band_size)
    # The pitch the tip labels were fitted to, so the label clears the stack it
    # is meant to clear. Taken at the nominal pitch it landed *inside* the gene
    # names: the row it was placed on was a row the plot does not have.
    row_mm <- .drawn_tip_pitch(
      opts,
      n_tip_drawn,
      plot_height_in,
      heatmap_header_frac(
        opts,
        n_tip_drawn,
        tree_span,
        axis_units,
        panel_in,
        plot_height_in
      ),
      heatmap_class_frac(
        opts,
        n_tip_drawn,
        class_runs,
        band_size,
        plot_height_in
      ),
      class_runs,
      band_size,
      header_mm_max
    )
    nudge <- 25.4 * TIP_ROW_IN / row_mm

    if (length(element_specs$top)) {
      for (layer in .element_label_layers(
        element_specs$top,
        .header_y(opts, n_tip_drawn) +
          header_mm_max / row_mm +
          ELEMENT_LABEL_GAP_ROWS * nudge,
        opts$line_color %||% "#000000"
      )) {
        p <- p + layer
      }
    }
    if (length(element_specs$bottom)) {
      for (layer in .element_label_layers(
        element_specs$bottom,
        0.5 - band_rows - ELEMENT_LABEL_GAP_ROWS * nudge,
        opts$line_color %||% "#000000",
        cap = band_size
      )) {
        p <- p + layer
      }
    }
  }
  # Room under the last tip for whatever hangs there — the class names, or a
  # clustered panel's strip and dendrogram, which were drawn in the loop but
  # need the same reserve. Measured the same way the header reserve above is.
  # Replacing the y scale a second time is deliberate, and its announcement is
  # not news — muffled by `build_tree_ggtree()` (`.muffled_tree_warnings`).
  #
  # Linear only, like the header reserve above and for a sharper reason. On a
  # radial tree y *is* the angle, and the room before the first tip and after
  # the last one is one thing: the wedge. ggtree cuts that wedge by setting the
  # y scale's limits (`ggtree:::open_tree`), so a scale added here replaces it
  # and the reader's Circle opening silently stops meaning anything — 0° and
  # 90° drew the identical picture, the only visible difference being the
  # rotation of a clade caption, which ggtree derives separately. What the band
  # needs is charged to the wedge instead (`tree_open_angle()`).
  # A radial tree's wedge, put back. `gheatmap()` sets a y scale of its own
  # whenever it is drawing a matrix without column names (`expand = c(0, 0)`),
  # which replaces the one ggtree cut the wedge with — so the reader's Circle
  # opening stopped meaning anything the moment a panel had too many genes to
  # name, which is every panel that needs a wedge in the first place. The only
  # visible difference between 0° and 90° was the rotation of a clade caption,
  # which ggtree derives from a separate column.
  if (circular) {
    p <- suppressMessages(
      p +
        scale_y_continuous(
          limits = c(0, .radial_y_limit(sum(tree_data$isTip), open_angle)),
          expand = expansion(mult = c(0, 0))
        )
    )
  }
  if (length(panels) && !circular) {
    p <- suppressMessages(
      p +
        scale_y_continuous(
          expand = expansion(
            mult = c(
              heatmap_class_frac(
                opts,
                sum(tree_data$isTip),
                class_runs,
                band_size,
                plot_height_in
              ) +
                .axis_frac(
                  opts,
                  sum(tree_data$isTip),
                  class_runs,
                  band_size,
                  plot_height_in
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
