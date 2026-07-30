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
    geom_text2,
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
    ggsave,
    guide_legend,
    theme,
    element_text,
    element_rect,
    margin,
    unit,
    xlim,
    labs,
    scale_color_viridis_c,
    scale_color_viridis_d,
    scale_color_distiller,
    scale_color_manual,
    scale_fill_viridis_c,
    scale_fill_viridis_d,
    scale_fill_distiller,
    scale_fill_manual,
    scale_shape_manual,
    scale_y_continuous,
    expansion,
  ],
  ape[root],
  stats[quantile, setNames],
  utils[head],
  RColorBrewer[brewer.pal, brewer.pal.info],
  viridisLite[viridis],
  grDevices[colorRampPalette],
)

box::use(
  app / logic / field_labels[field_labels_for],
  app / logic / field_profile[field_levels],
  app / logic / field_types[as_date_safe],
)

# Scale classification definitions
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
TIP_ROW_IN <- 0.228 # Target inches of plot height per tip
TIP_USABLE <- 0.9 # Share of plot height available excluding margins/title
TIP_ROW_FILL <- 0.77 # Fraction of row pitch occupied by tip label text box

# Horizontal label reservation geometry
TIP_CHAR_EM <- 0.6 # Character width estimate (em) for accession/isolate labels
TIP_LABEL_FRAC <- 0.35 # Maximum fraction of panel width reserved for tip labels
TIP_CIRC_K <- 2 * pi * 0.35 # Angular row pitch scaling factor for circular layouts

#' Default Layout Parameters for Dynamic Sizing Controls
#' @export
TREE_FIT_DEFAULTS <- list(
  aspect = 0.6,
  tiplab_size = 4,
  branch_size = 4,
  tippoint_size = 4,
  zoom = 0.95,
  h = -0.05
)

# Linear layout defaults (no radial label overflow compensation required)
LINEAR_ZOOM <- 1
LINEAR_H <- 0

# Fitting limits
TIP_GROWTH <- 1.5 # Cap size scaling relative to default (150%)
TIP_ASPECT_MIN <- 0.5 # Minimum allowed aspect ratio
TIP_ASPECT_MAX <- 8 # Maximum allowed aspect ratio
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
  aspect <- .clamp(n * TIP_ROW_IN / w, TIP_ASPECT_MIN, TIP_ASPECT_MAX)

  # Calculate pitch based on active layout geometry
  pitch_in <- if (circular) {
    TIP_CIRC_K * w / n
  } else {
    TIP_USABLE * aspect * w / n
  }
  row_mm <- 25.4 * pitch_in
  by_row <- TIP_ROW_FILL * row_mm
  by_width <- 25.4 * TIP_LABEL_FRAC * w / (TIP_CHAR_EM * chars)
  size <- min(by_row, by_width)

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
    zoom = if (circular) TREE_FIT_DEFAULTS$zoom else LINEAR_ZOOM,
    h = if (circular) TREE_FIT_DEFAULTS$h else LINEAR_H,
    labels_legible = size >= TIP_SIZE_FLOOR
  )
}

#' Calculate Legend Column Multiples
#'
#' @param n_levels Integer. Number of categories in the legend.
#' @param max_rows Integer. Target maximum vertical entries per column.
#' @return Integer count of legend columns (1 to 4).
#' @export
tree_legend_ncol <- function(n_levels, max_rows = 18L) {
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

BRANCH_ABOVE_SHRINK <- 0.72
BRANCH_VJUST <- -0.35
BRANCH_LABEL_TARGET <- 25L

#' Calculate Branch Length Filtering Cutoff
#'
#' Converts target label count into a percentile cutoff for branch filtering.
#'
#' @param n_branches Integer. Total branch count.
#' @param target Integer. Desired number of labeled branches.
#' @return Percentile threshold (0-99).
#' @export
tree_branch_cutoff <- function(n_branches, target = BRANCH_LABEL_TARGET) {
  n <- max(as.integer(n_branches %||% 0L), 1L)
  round(.clamp(100 * (1 - target / n), 0, 99))
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

# Width of one tile strip. A strip carries a single variable, so its width is
# about being seen rather than about resolving columns.
TILE_SPAN_EACH <- 0.16

# Width of one heatmap column. Enough that a column of a 15-class AMR matrix is
# still a readable block rather than a hairline.
HEATMAP_COL_SPAN <- 0.055

# Ceiling on all annotations together, as a multiple of the tree span. The
# canvas grows for them, but the tree must stay the larger part of the picture;
# past this the columns share what is left.
ANNOTATION_SPAN_MAX <- 1.1

# Kept for the callers and tests that ask "how wide is one strip": now a
# constant per strip rather than a budget split, since strips no longer compete.
#' @export
tree_annotation_width <- function(n_strips) {
  TILE_SPAN_EACH
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
  n <- .n_tiles(opts)
  if (!n) {
    return(0)
  }
  n * (TILE_SPAN_EACH + TILE_GAP)
}

#' Width of the heatmap panels together, gaps included, in tree spans.
#' @export
heatmap_total <- function(opts) {
  hs <- opts$heatmaps %||% list()
  # A panel with no columns draws nothing, and the builder drops it — so it must
  # not reserve a gap here either.
  hs <- Filter(function(h) length(h$cols) > 0L, hs)
  if (!length(hs)) {
    return(0)
  }
  n_cols <- sum(vapply(hs, function(h) length(h$cols), integer(1)))
  n_cols * HEATMAP_COL_SPAN + HEATMAP_GAP * length(hs)
}

# The scale factor that brings the annotations back under ANNOTATION_SPAN_MAX.
.annotation_squeeze <- function(opts) {
  want <- tile_total(opts) + heatmap_total(opts)
  if (want <= ANNOTATION_SPAN_MAX || want <= 0) {
    return(1)
  }
  ANNOTATION_SPAN_MAX / want
}

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
  min(0.45, chars * TIP_CHAR_EM * (opts$tiplab_size %||% 4) / 25.4 / w)
}

X_EXPANSION <- 1.1

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
#' @return Numeric width in inches; 0 when nothing is mapped.
#' @export
tree_legend_width_in <- function(layers, md, legend_size, width_in) {
  layers <- layers %||% list()
  if (!length(layers)) {
    return(0)
  }
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    5.5
  } else {
    width_in
  }
  size <- legend_size %||% 10
  per <- vapply(
    layers,
    function(l) {
      labs <- unique(as.character(md[[l$field]]))
      chars <- suppressWarnings(max(nchar(c(labs, l$title %||% "")), 1L))
      if (!is.finite(chars)) {
        chars <- 1L
      }
      ncol <- tree_legend_ncol(l$n_levels %||% 1L)
      ncol * (LEGEND_KEY_IN + chars * TIP_CHAR_EM * size / 72)
    },
    numeric(1)
  )
  min(max(per) + LEGEND_PAD_IN, LEGEND_MAX_FRAC * w)
}

# Share of an inward tree's radius the labels may claim. They run from the rim
# toward the centre, where the circumference available to them shrinks to
# nothing, so they need markedly more room than the same labels on a linear
# tree — but the tree still has to be the larger half of the picture.
INWARD_LABEL_MAX <- 0.6

#' Outer radius an inward-circular layout should be drawn to.
#'
#' `layout_inward_circular` maps the x axis outward-in, so whatever range it is
#' given becomes the radius, and the tree fills it. Left at the tree's own depth
#' the tip labels have only the root's immediate surroundings to occupy and pile
#' into an unreadable blot over the centre; widening the range holds the tree
#' out at the rim and leaves the inner disc for the labels.
#'
#' @param opts List. Resolved tree options.
#' @param md Data frame. Per-tip metadata.
#' @param max_x Numeric. Deepest tip position in the tree.
#' @return Numeric outer limit for `ggtree(xlim = c(<this>, 0))`.
#' @export
tree_inward_xlim <- function(opts, md, max_x) {
  if (!is.finite(max_x) || max_x <= 0) {
    return(1)
  }
  w <- opts$width_in
  if (is.null(w) || !is.finite(w) || w <= 0) {
    w <- 5.5
  }
  chars <- suppressWarnings(max(nchar(as.character(md[[opts$tiplab]])), 1L))
  if (!is.finite(chars) || !isTRUE(opts$tiplab_show)) {
    chars <- 1L
  }
  label_in <- chars * TIP_CHAR_EM * (opts$tiplab_size %||% 4) / 25.4

  # Twice the fraction .tiplab_frac would give. That one measures a label
  # against the panel's full width, because a linear tree spends its whole
  # width on one row of them. Here the label lies along a *radius*, which is
  # half the panel, so the same label costs twice as much of it.
  frac <- .clamp(2 * label_in / w, 0, INWARD_LABEL_MAX)
  max_x / (1 - frac)
}

HEATMAP_GAP <- 0.02
ANNOTATION_CLEARANCE <- 1.12

# Fixed two-colour fill for the AMR panel. Its cells hold comma-joined gene
# symbols, so a shared categorical scale over the raw strings would give one
# colour per distinct *combination* of genes — dozens of them, none comparable.
# What the panel is actually read for is whether a class was hit at all.
AMR_PRESENT <- "Resistance detected"
AMR_ABSENT <- "Not detected"
#' @export
AMR_HEATMAP_FILL <- c("#B2182B", "#E8E8E8")

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
  heat <- annotation_total(opts)
  if (!isTRUE(heat > 0)) {
    return(panel_in)
  }
  frac <- .clamp(.tiplab_frac(opts, md) * X_EXPANSION, 0, 0.8)
  share <- (1 - frac) / (1 + heat) + frac
  if (!is.finite(share) || share <= 0) {
    return(panel_in)
  }
  max(panel_in / share, panel_in)
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
  (tile_total(opts) + heatmap_total(opts)) * .annotation_squeeze(opts)
}

#' Widths and offsets for the heatmap panels, in x-axis data units.
#'
#' `gheatmap`'s `width` is a multiple of the tree's own span and `offset` is in
#' the same units, so the panels have to be solved together: each starts where
#' the last one ended, and the run as a whole shares the annotation budget with
#' the tile strips so the tree stays the larger part of the picture.
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
  widths <- n_cols * HEATMAP_COL_SPAN * squeeze
  gaps <- HEATMAP_GAP * squeeze

  # The first panel starts past the tip labels *and* past the tile strips.
  # gheatmap's offset is absolute from the tree's edge, unlike geom_fruit's,
  # which is relative to the annotation before it — so the tiles are invisible
  # to this calculation unless counted here. Not counting them is what drew the
  # heatmap straight over the tile strip.
  base <- label_reserve + tile_total(opts) * squeeze * tree_span
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
#' @param opts List. Resolved tree options.
#' @param n_tip Integer. Number of tips.
#' @return Numeric multiplicative expansion for the top of the y scale.
#' @export
heatmap_header_frac <- function(opts, n_tip) {
  hs <- opts$heatmaps %||% list()
  if (!length(hs)) {
    return(0.02)
  }
  chars <- max(vapply(
    hs,
    function(h) {
      suppressWarnings(max(nchar(field_labels_for(h$cols)), 1L))
    },
    numeric(1)
  ))
  if (!is.finite(chars)) {
    chars <- 1
  }
  n <- max(as.integer(n_tip %||% 1L), 1L)
  .clamp(chars * HEADER_CHAR_ROWS / n, 0.04, 0.4)
}

# Rows of tip pitch one header character occupies when set vertically, at the
# header font size gheatmap is given below.
HEADER_CHAR_ROWS <- 0.62

# The frame one panel draws, with its rows keyed the way gheatmap matches them.
.heatmap_frame <- function(panel, md) {
  heat <- md[, panel$cols, drop = FALSE]
  if (identical(panel$kind, "amr")) {
    # Presence/absence, not the gene strings themselves.
    heat[] <- lapply(heat, function(v) {
      present <- !is.na(v) & nzchar(trimws(as.character(v)))
      factor(
        ifelse(present, AMR_PRESENT, AMR_ABSENT),
        levels = c(AMR_PRESENT, AMR_ABSENT)
      )
    })
  }
  names(heat) <- field_labels_for(panel$cols)
  rownames(heat) <- md$label
  heat
}

# Solves x-axis plot range ensuring tip labels and heatmaps fit without clipping
.tiplab_xlim <- function(opts, md, tree_data, max_x, heat = 0) {
  frac <- .clamp(.tiplab_frac(opts, md) * X_EXPANSION, 0, 0.8)
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
#' A mid grey, not a pale one: this colour has to work as tip-label *text* as
#' well as a heatmap swatch, and anything lighter reads as invisible rather than
#' as de-emphasised.
#' @export
MISSING_COLOR <- "#9E9E9E"

# The shape reserved for the missing level, outside TREE_SHAPES so a mapping of
# six real values plus "not recorded" still has somewhere to put it.
TREE_MISSING_SHAPE <- 4

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
  ch <- trimws(as.character(v))
  ch[!nzchar(ch)] <- NA_character_
  present <- sort(unique(ch[!is.na(ch)]))
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

tree_scale <- function(values, palette, aesthetic, name = NULL) {
  numeric <- is.numeric(values) || inherits(values, "Date")
  viridis_pal <- is.null(palette) || palette %in% .viridis_scales
  opt <- if (is.null(palette) || !viridis_pal) "viridis" else palette

  if (numeric) {
    return(if (identical(aesthetic, "fill")) {
      if (viridis_pal) {
        scale_fill_viridis_c(option = opt, name = name)
      } else {
        scale_fill_distiller(palette = palette, name = name)
      }
    } else {
      if (viridis_pal) {
        scale_color_viridis_c(option = opt, name = name)
      } else {
        scale_color_distiller(palette = palette, name = name)
      }
    })
  }

  values <- mapped_values(values)
  cols <- tree_level_colors(levels(values), palette)
  guide <- guide_legend(ncol = tree_legend_ncol(length(cols)))

  if (identical(aesthetic, "fill")) {
    scale_fill_manual(
      values = cols,
      guide = guide,
      name = name,
      labels = .wrap_legend_labels,
      na.value = MISSING_COLOR
    )
  } else {
    scale_color_manual(
      values = cols,
      guide = guide,
      name = name,
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
.layer_values <- function(layer, md) {
  v <- md[[layer$field]]
  if (identical(layer$transform, "as_date")) as_date_safe(v) else mapped_values(v)
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
    md[[f]] <- .layer_values(layer, md)
  }
  md
}

layer_for_field <- function(opts, field) {
  hit <- Filter(function(l) identical(l$field, field), opts$layers %||% list())
  if (length(hit)) hit[[1]] else NULL
}

tree_tiplab_layer <- function(opts, layer = NULL) {
  if (!isTRUE(opts$tiplab_show)) {
    return(NULL)
  }

  mapped <- !is.null(layer)
  mapping <- if (mapped) {
    aes(label = .data[[opts$tiplab]], color = .data[[layer$field]])
  } else {
    aes(label = .data[[opts$tiplab]])
  }

  inward <- identical(opts$layout, "inward")

  params <- list(
    mapping = mapping,
    size = opts$tiplab_size,
    # Aligning draws a leader line from each tip out to the axis limit. In an
    # inward tree every one of those runs toward the centre, where they all
    # converge into a solid blot over the root — so the option that makes a
    # linear tree readable is the one that ruins this layout.
    align = isTRUE(opts$align) && !inward,
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

tree_branch_layer <- function(opts, branch_lengths) {
  if (!isTRUE(opts$branch_show)) {
    return(NULL)
  }

  cut <- quantile(
    branch_lengths,
    probs = opts$branch_cutoff / 100,
    na.rm = TRUE
  )

  geom_text2(
    mapping = aes(
      x = .data[["branch"]],
      label = round(.data[["branch.length"]], 2),
      subset = .data[["branch.length"]] > cut
    ),
    size = opts$branch_size * BRANCH_ABOVE_SHRINK,
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

TILE_GAP <- 0.03

tree_tile_layers <- function(
  opts,
  md,
  tiles = NULL,
  label_frac = 0,
  tree_span = 1
) {
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
  frac <- TILE_SPAN_EACH * squeeze
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
            label_frac + gap + frac / 2
          } else {
            gap + frac
          }
        ),
        tree_scale(
          .layer_values(tile, md),
          tile$palette,
          "fill",
          name = tile$title
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
    }
  )
}

.build_tree_ggtree <- function(tree, metadata, opts) {
  # Attach ggplot2 namespace if unattached (required for ggtreeExtra::geom_fruit)
  if (!"package:ggplot2" %in% search()) {
    base::attachNamespace("ggplot2")
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
  opts$heatmaps <- Filter(
    function(h) length(h$cols) > 0L,
    lapply(opts$heatmaps %||% list(), function(h) {
      h$cols <- intersect(h$cols, cols)
      h
    })
  )

  # Mapped columns are normalised in place, so every scale is built from the
  # same levels the geoms will actually draw.
  md <- .normalize_mapped_columns(opts, md)

  circular <- opts$layout %in% .circular_layouts
  label_reserve <- 0
  # ggtree's own name for the inward-facing radial layout is "inward_circular".
  # This used to translate it to plain "circular", which silently drew the
  # ordinary outward tree instead — picking Inward changed nothing at all.
  layout <- if (identical(opts$layout, "inward")) {
    "inward_circular"
  } else {
    opts$layout
  }

  # The inward layout's radius has to be solved before the plot is built, and
  # solving it needs the tree's depth — which only the built plot reports. One
  # throwaway pass in the default layout answers that; it draws nothing.
  inward_xlim <- if (identical(opts$layout, "inward")) {
    probe_x <- suppressWarnings(max(ggtree(tree)$data$x, na.rm = TRUE))
    c(tree_inward_xlim(opts, md, probe_x), 0)
  }

  build_base <- function(alpha = NULL) {
    args <- list(
      tree,
      color = opts$line_color,
      layout = layout,
      ladderize = TRUE,
      xlim = inward_xlim
    )
    if (!is.null(alpha)) {
      args$alpha <- alpha
    }
    do.call(ggtree, args)
  }

  # Node-label view dims the tree so the internal node numbers read over it.
  base <- if (isTRUE(opts$nodelabel_show)) build_base(0.2) else build_base()

  tree_data <- base$data
  max_x <- max(tree_data$x, na.rm = TRUE)
  branch_lengths <- tree_data$branch.length[tree_data$branch.length > 0]

  p <- base %<+% md

  lab_l <- layer_for(opts, "tiplab_color")
  pt_l <- layer_for(opts, "tippoint_color")
  shp_l <- layer_for(opts, "tippoint_shape")
  tile_ls <- Filter(
    function(l) identical(l$aesthetic, "tile"),
    opts$layers %||% list()
  )

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
  if (!circular) {
    # `opts$width_in` is the tree-and-labels budget, not the whole canvas: the
    # caller grows the canvas for the legend and the annotations rather than
    # taking their room out of the tree (see TREE_PANEL_IN in
    # visualization_tree.R). So the reserve is solved against it unmodified —
    # subtracting the legend here too would charge the tree for it twice, which
    # is what turned a few hundred isolates into a hairline.
    fit <- .tiplab_xlim(opts, md, tree_data, max_x, annot_total)
    label_reserve <- fit$reserve
  }

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
          .layer_values(lab_l, md),
          lab_l$palette,
          "color",
          name = lab_l$title
        ),
        new_scale_color()
      )
    },
    list(
      tree_branch_layer(opts, branch_lengths),
      tree_tippoint_layer(opts, pt_l, shp_l)
    ),
    if (!is.null(pt_l)) {
      list(
        tree_scale(
          .layer_values(pt_l, md),
          pt_l$palette,
          "color",
          name = pt_l$title
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
      list(scale_shape_manual(
        values = shp_values,
        name = shp_l$title,
        labels = .wrap_legend_labels,
        guide = guide_legend(ncol = tree_legend_ncol(length(shp_values)))
      ))
    },
    if (isTRUE(opts$nodelabel_show)) {
      list(geom_nodelab(aes(label = .data[["node"]])))
    },
    tree_tile_layers(opts, md, tile_ls, label_reserve / tree_span, tree_span)
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
        fontsize = 4
      )
  }

  # `fit` was solved before the layers were assembled, because the tile strips
  # needed the label reserve to place themselves.
  if (!circular) {
    p <- p + xlim(NA, fit$limit)
    if (length(opts$heatmaps %||% list())) {
      # Room above the last tip for the heatmap column headers, which are set
      # vertically and would otherwise be clipped by the panel. Replacing
      # ggtree's y scale is the point, so its announcement is not news.
      p <- suppressMessages(
        p +
          scale_y_continuous(
            expand = expansion(
              mult = c(0.02, heatmap_header_frac(opts, sum(tree_data$isTip)))
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
  p <- p +
    theme_tree(bgcolor = opts$bg) +
    theme(
      plot.margin = if (circular) margin(0, 0, 0, 0) else margin(6, 6, 6, 6),
      legend.position = if (circular) "bottom" else "right",
      legend.direction = opts$legend_orientation,
      legend.box = if (circular) "horizontal" else "vertical",
      # Align the guide box to the plot rather than to the panel, so it does
      # not drift as the panel's own width changes with the label reserve.
      legend.location = "plot",
      legend.justification = if (circular) "center" else "top",
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
  for (pan in heatmap_panels(opts, tree_span, label_reserve)$panels) {
    p <- gheatmap(
      p + new_scale_fill(),
      data = .heatmap_frame(pan, md),
      offset = if (circular) 0 else pan$offset,
      width = pan$width,
      legend_title = pan$title,
      # Headers above the matrix, reading upward. Below it they ran into the
      # tree scale bar and off the bottom of the panel, because a drug class
      # name set vertically is taller than the row of space under the last tip.
      # Above, the space the deleted title block used to hold is free.
      colnames_position = "top",
      colnames_angle = 90,
      hjust = 0,
      colnames_offset_y = 0.4,
      font.size = 2.6
    )
    if (identical(pan$kind, "amr")) {
      # gheatmap installs a default fill scale of its own, so replacing it is
      # the intended move — but ggplot2 announces every replacement, and this
      # one is not news. Deliberate, so silenced here rather than logged on
      # every draw.
      p <- suppressMessages(
        p +
          scale_fill_manual(
            values = setNames(AMR_HEATMAP_FILL, c(AMR_PRESENT, AMR_ABSENT)),
            name = pan$title,
            na.value = "grey90"
          )
      )
    }
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
