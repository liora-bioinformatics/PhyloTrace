# app/logic/tree_plot.R
#
# Render a phylo tree as a ggtree/ggplot object, wired to the Tree visualization
# controls. The (expensive) tree is computed elsewhere; this module only paints
# it, so control changes re-render cheaply.

box::use(
  ggtree[
    ggtree,
    `%<+%`,
    geom_tiplab,
    geom_tippoint,
    geom_nodepoint,
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
    scale_color_brewer,
    scale_fill_viridis_c,
    scale_fill_viridis_d,
    scale_fill_distiller,
    scale_fill_brewer,
  ],
  ape[root],
  stats[quantile],
)

# Palette names that belong to the viridis family (the rest are ColorBrewer).
.viridis_scales <- c(
  "viridis",
  "magma",
  "plasma",
  "inferno",
  "cividis",
  "turbo",
  "mako"
)

# Layouts that wrap the tree into a circle (labels need angular placement).
.circular_layouts <- c("circular", "inward")

# --- Data-fitted control defaults --------------------------------------------
#
# All of this follows from renderPlot(res = 192) in visualization_tree.R: the
# preview device is panel_px/192 inches wide and width * aspect_ratio inches
# tall, and ggplot2 text `size` is in millimetres. So for n tips the vertical
# room per tip ("row pitch") is
#
#   row_in = TIP_USABLE * (width_in * aspect) / n
#
# and a label sits in its row without touching its neighbours when its text box
# takes up TIP_ROW_FILL of that pitch. Reading it the other way round — pick the
# row pitch the labels need, get the aspect ratio that yields it — is what fits
# the plot to the data.
#
# TIP_ROW_IN and TIP_ROW_FILL are calibrated so n = 15 tips on a ~5.7in panel
# returns exactly the defaults this module shipped with (aspect 0.6, size 4mm):
# the fit reproduces the current look at the one dataset size the current look
# was right for, and scales away from it in both directions.
TIP_ROW_IN <- 0.228 # target inches of plot height per tip
TIP_USABLE <- 0.9 # of the device height, once margins / title / scale bar are out
TIP_ROW_FILL <- 0.77 # of the row pitch taken up by the text box

# Labels have width as well as height, and for this app that is usually the
# binding constraint: 36-character isolate names at the old fixed size 4 need
# 60% of a 5.7in panel — well past the 37.5% the tip-label reserve used to hand
# out, which is what clipped them. The tips may claim at most TIP_LABEL_FRAC of
# the panel; the rest belongs to the tree.
#
# TIP_CHAR_EM is the mean character advance, measured (grobWidth at a known
# fontsize) rather than assumed: the uppercase-and-digit accession names this
# app labels tips with run ~0.586em per character, against ~0.49em for ordinary
# lowercase text. Taking the wider of the two, with a little margin, keeps the
# estimate on the safe side — under-reserving clips labels at the panel edge,
# whereas over-reserving only costs a little tree width.
TIP_CHAR_EM <- 0.6
TIP_LABEL_FRAC <- 0.35

# Circular layouts spread the tips along a circumference rather than down a
# column, so the pitch is 2*pi*R/n; the tip circle's radius is ~0.35 of the
# panel width.
TIP_CIRC_K <- 2 * pi * 0.35

# Everything else drawn once per tip or per node is sized off the same row
# pitch, because it competes for the same room: a tip point wider than its row
# merges into its neighbours exactly as an oversized label does. The fractions
# are again read off the shipped defaults at n = 15 (tip points 4 — the same
# share as the label — and node points 2.5).
NODE_ROW_FILL <- 0.48

# The values the fitted controls ship with, and the calibration anchor itself:
# at ~15 tips on a ~5.7in panel the fit returns exactly these. Exported because
# visualization_tree.R builds its sliders from them, so the anchor and the UI
# cannot drift apart.
#' @export
TREE_FIT_DEFAULTS <- list(
  aspect = 0.6,
  tiplab_size = 4,
  branch_size = 4,
  tippoint_size = 4,
  nodepoint_size = 2.5,
  zoom = 0.95,
  h = -0.05
)

# as.ggplot(scale = zoom) shrinks the finished plot inside its canvas, so the
# 0.95 this shipped with spends 5% of the height on a blank border at the top
# and another 5% at the bottom — measurable, and the whole of the empty band
# above and below a tall tree. It was margin against content running off the
# edge, and on a linear layout nothing can: the tip labels are the only thing
# drawn past the tree, and .tiplab_xlim now reserves exactly the room they
# need. Circular layouts keep the margin, because their labels radiate outward
# with nothing reserving anything for them.
LINEAR_ZOOM <- 1
LINEAR_H <- 0

# The fit may shrink a size as far as its slider goes — density leaves it no
# choice — but may only grow one half again beyond its default. Four tips leave
# rows a centimetre and a half deep, and filling those honestly would put 12mm
# blobs and 23pt numbers on the page: consistent with the geometry, absurd to
# look at. Shrinking is a necessity, growing merely a nicety.
TIP_GROWTH <- 1.5

# Bounds of the controls being fitted (nj_aspect_ratio, nj_tiplab_size,
# nj_branch_size, nj_tippoint_size, nj_nodepoint_size in visualization_tree.R).
# The aspect floor is above the slider's own minimum: a handful of isolates
# should be a squat plot, not a hairline strip.
TIP_ASPECT_MIN <- 0.5
TIP_ASPECT_MAX <- 8
TIP_SIZE_MIN <- 0.5

# Below ~3.4pt a tip label is a grey smudge rather than text, and no aspect
# ratio inside TIP_ASPECT_MAX rescues it — that is a tree to draw without
# labels (see the Generate observer, which says so).
TIP_SIZE_FLOOR <- 1.2

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

# The layout controls that suit `n_tip` tips on a `width_in` inch wide device,
# given labels up to `label_chars` characters long. Pure, so it is testable
# without a session and cheap enough to call on every Generate.
#
# One quantity — the row pitch — decides all of it, which is why the sizes stay
# in proportion to each other at every tree size rather than drifting apart.
# Only `tiplab_size` also answers to the labels' width.
#
# `aspect` is always the *linear* fit: circular layouts are rendered square
# regardless (see the renderPlot height in visualization_tree.R), so the slider
# is left holding the value the tree would need if the user switched back.
# The sizes do follow the layout, since the circumference gives moderate trees
# considerably more room than a column of the same height would.
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

  # Font size from whichever of the two constraints binds first — the vertical
  # room one row leaves, or the horizontal room the longest label needs.
  pitch_in <- if (circular) {
    TIP_CIRC_K * w / n
  } else {
    TIP_USABLE * aspect * w / n
  }
  row_mm <- 25.4 * pitch_in
  by_row <- TIP_ROW_FILL * row_mm
  by_width <- 25.4 * TIP_LABEL_FRAC * w / (TIP_CHAR_EM * chars)
  size <- min(by_row, by_width)

  # Free to shrink, held near its default on the way up — see TIP_GROWTH. Both
  # the value and its ceiling are put on the sliders' 0.1 step (the ceiling
  # downwards, so rounding cannot lift a value back over it), because a fitted
  # value the slider cannot actually hold would be snapped by the browser and
  # echoed back as a different number — a change, and so a second draw.
  fit_size <- function(field, value) {
    cap <- floor(10 * TIP_GROWTH * TREE_FIT_DEFAULTS[[field]]) / 10
    .clamp(round(value, 1), TIP_SIZE_MIN, cap)
  }

  list(
    aspect = round(aspect, 1),
    tiplab_size = fit_size("tiplab_size", size),
    # Branch labels sit on the same rows and carry short numbers, so they take
    # the row's share without the width constraint the tip labels answer to.
    branch_size = fit_size("branch_size", by_row),
    tippoint_size = fit_size("tippoint_size", by_row),
    nodepoint_size = fit_size("nodepoint_size", NODE_ROW_FILL * row_mm),
    # A linear tree is drawn to the edges of its canvas; a circular one keeps
    # the border its radial labels have no other protection from.
    zoom = if (circular) TREE_FIT_DEFAULTS$zoom else LINEAR_ZOOM,
    h = if (circular) TREE_FIT_DEFAULTS$h else LINEAR_H,
    labels_legible = size >= TIP_SIZE_FLOOR
  )
}

# --- Fitting the variable mappings -------------------------------------------
#
# A metadata column is only worth mapping to colour, shape or a tile strip if
# its values actually separate the isolates into groups a reader can tell
# apart. Three ways a column fails that, all of them present in a real
# database: one distinct value (every tip identical — `organism` in a
# single-species scheme), one distinct value *per isolate* (`isolate` itself,
# 346 colours, which is what this shipped as the default mapping), and mostly
# empty (`source`, filled for 11 of 346).

# Largest ColorBrewer qualitative palette that is safe: Set1 and Pastel1 carry 9
# colours, Set2/Dark2/Accent only 8. Ask any of them for more and brewer warns
# ("Returning the palette you asked for with that many colors") and hands back
# short — every level past the end draws grey. Above this, a viridis scale,
# which is generated rather than tabulated and so has no ceiling.
MAX_QUAL_LEVELS <- 9L

# ggplot2's shape palette stops at six: "more than 6 becomes difficult to
# discriminate". Past it the extra levels get no shape at all, so those tips
# simply vanish from the plot rather than looking wrong.
#' @export
MAX_SHAPE_LEVELS <- 6L

# A column needs values for at least this share of the isolates to be offered as
# a default mapping; below it the plot is mostly "NA".
MIN_COVERAGE <- 0.5

# Distinct non-empty values in a metadata column.
#' @export
field_levels <- function(values) {
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(unique(v))
}

# Share of isolates the column actually has a value for.
.field_coverage <- function(values) {
  if (!length(values)) {
    return(0)
  }
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(v) / length(values)
}

# The metadata columns worth offering for a mapping, best first.
#
# Ordering the picker *is* the recommendation: the first entry becomes the
# default, and a user scanning the list meets the usable columns before the
# hopeless ones. Ranked by how well the column groups the data — a handful of
# well-populated groups first, then larger ones, then poorly covered ones —
# and columns that cannot group at all are dropped.
#' @export
mapping_fields <- function(metadata, max_levels = Inf) {
  fields <- setdiff(names(metadata), "isolate")
  n <- nrow(metadata)
  if (!length(fields) || !n) {
    return(character(0))
  }

  info <- lapply(fields, function(f) {
    list(
      field = f,
      levels = field_levels(metadata[[f]]),
      coverage = .field_coverage(metadata[[f]])
    )
  })
  # One value for everyone, or a different value for everyone: neither groups.
  info <- Filter(
    function(i) i$levels > 1 && i$levels < n && i$levels <= max_levels,
    info
  )
  if (!length(info)) {
    return(character(0))
  }

  rank <- vapply(
    info,
    function(i) {
      # Rank before order so the sort is a plain numeric one: well-covered
      # columns of a handful of groups first, then wider ones, then the sparse.
      band <- if (i$coverage < MIN_COVERAGE) {
        3L
      } else if (i$levels <= MAX_QUAL_LEVELS) {
        1L
      } else {
        2L
      }
      band + i$levels / (100 * max(i$levels, 1) + 1)
    },
    numeric(1)
  )
  vapply(info[order(rank)], function(i) i$field, character(1))
}

# Which palette family suits a column: a qualitative one while the levels stay
# inside the smallest brewer palette, a generated (viridis) one past that, and
# the numeric families for numbers. Returns names of `color_scales` groups in
# viz_helpers.R, best first.
#' @export
scale_categories_for <- function(values, numeric_categories) {
  if (is.numeric(values)) {
    return(numeric_categories)
  }
  if (field_levels(values) <= MAX_QUAL_LEVELS) {
    c("Qualitative", "Gradient")
  } else {
    c("Gradient", "Qualitative")
  }
}

# Entries per legend column. A mapping with forty levels draws a legend taller
# than the plot it explains, and ggplot will happily let it run off the canvas;
# wrapping into columns is what keeps it inside. Mirrors epi_legend_ncol in
# epi_plot.R, which does the same for the epidemic curve's strata.
#' @export
tree_legend_ncol <- function(n_levels, max_rows = 18L) {
  if (n_levels <= max_rows) {
    return(1L)
  }
  as.integer(min(4L, ceiling(n_levels / max_rows)))
}

# A round number at or just below `x`, in the 1 / 2 / 5 sequence.
#
# For the tree's scale bar, whose width ggtree prints verbatim: a tenth of the
# tree depth is a number like 1.36116098546807, and that is exactly what ended
# up under the plot. Scale bars carry round numbers.
#' @export
tree_nice_width <- function(x) {
  if (!is.finite(x) || x <= 0) {
    return(1)
  }
  mag <- 10^floor(log10(x))
  step <- c(1, 2, 5, 10)
  step[max(which(step * mag <= x))] * mag
}

# The percentile of branch length above which a branch gets a label, chosen so
# roughly `target` of them do. A tree of a few hundred isolates has as many
# branches again, and the shipped cutoff of 10 labelled 90% of them — a solid
# band of text over the tree. At ~15 tips this returns ~7, near the 10 the
# control shipped with, so the fit lands where the old default was reasonable.
# Branch numbers are drawn as plain text standing above the branch line. Both
# constants are in the geom's own units: `size` is ggplot2's mm-ish text size,
# `vjust` is a multiple of the text's height, so -0.35 lifts the baseline just
# over a third of a line above the branch — clear of it without floating.
BRANCH_ABOVE_SHRINK <- 0.72
BRANCH_VJUST <- -0.35

BRANCH_LABEL_TARGET <- 25L
#' @export
tree_branch_cutoff <- function(n_branches, target = BRANCH_LABEL_TARGET) {
  n <- max(as.integer(n_branches %||% 0L), 1L)
  round(.clamp(100 * (1 - target / n), 0, 99))
}

# Fraction of the tree's width one annotation strip may take.
#
# geom_fruit's `pwidth` and gheatmap's `width` are both multiples of the tree
# width, so the shipped 2 meant a single tile strip twice as wide as the tree
# it annotates. The annotations share a fixed budget instead, so the tree stays
# the thing the plot is of however many are switched on.
ANNOTATION_BUDGET <- 0.45
#' @export
tree_annotation_width <- function(n_strips) {
  n <- max(as.integer(n_strips %||% 1L), 1L)
  round(ANNOTATION_BUDGET / n, 2)
}

# Fraction of the panel width to keep clear to the right of the tips for their
# labels. Derived from the labels actually being drawn (the longest one, at the
# current font size) rather than the flat 0.375 this used to reserve
# unconditionally, which both clipped long isolate names and left a third of the
# panel empty for short ones. A NULL/unusable width_in — the browser has not
# reported the panel width yet — falls back to that old flat reserve, the same
# way epi_legend_ncol falls back to a typical panel.
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

# ggplot pads a continuous scale by 5% of the range at each end.
X_EXPANSION <- 1.1

# How far past the estimated end of the tip labels the annotation matrix starts.
HEATMAP_CLEARANCE <- 1.3

# Width of the heatmap matrix, as gheatmap wants it: a multiple of the tree's
# own width, shared across the selected columns.
.heatmap_width <- function(opts) {
  n <- length(opts$heatmap_select)
  if (!n) {
    return(0)
  }
  tree_annotation_width(1) * min(n, 6L) / 6L
}

# Where to end the x axis, and how much of it the tip labels take.
#
# Not simply max_x/(1 - frac): the panel does not start at the tips' origin. It
# spans from the root — pushed further left again by the root edge — out to this
# limit, and then ggplot expands both ends. Writing that out, with `heat` the
# annotation matrix's width as a multiple of the tree's own,
#
#   (max_x - x_min) * (1 + heat) + X_EXPANSION * range * frac = range
#
# and the reserve the labels get is the second term. Solving the two together
# is what keeps them apart at any axis width.
.tiplab_xlim <- function(opts, md, tree_data, max_x, heat = 0) {
  # The clearance is part of the fraction, not applied to the reserve
  # afterwards: inflating only the offset pushes the matrix past the limit
  # computed from the smaller fraction, and it is clipped away entirely.
  frac <- .clamp(
    .tiplab_frac(opts, md) * X_EXPANSION * HEATMAP_CLEARANCE,
    0,
    0.8
  )
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
    # Exactly the room the labels were given, so the matrix starts where they
    # end and the pair fill the axis they were solved for.
    reserve = range * frac
  )
}

# Per-tip metadata keyed by tip label, for `%<+%`. The tree's tip labels are the
# isolate names, so the join key is the `isolate` column (one row per isolate).
# The first column (`label`) must match the tree's tip labels.
#' @export
tree_tip_metadata <- function(tree, metadata) {
  data.frame(
    label = metadata$isolate,
    metadata,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# A color/fill scale for a mapped variable: continuous for numeric data,
# discrete otherwise. `aesthetic` is "color" or "fill".
tree_scale <- function(values, palette, aesthetic) {
  numeric <- is.numeric(values)
  viridis <- is.null(palette) || palette %in% .viridis_scales
  opt <- if (is.null(palette) || !viridis) "viridis" else palette
  # Discrete scales carry a legend with one entry per level, which is what runs
  # off the canvas when a column has dozens of them.
  guide <- if (numeric) {
    "colourbar"
  } else {
    guide_legend(ncol = tree_legend_ncol(field_levels(values)))
  }

  if (identical(aesthetic, "fill")) {
    if (numeric && viridis) {
      scale_fill_viridis_c(option = opt)
    } else if (numeric) {
      scale_fill_distiller(palette = palette)
    } else if (viridis) {
      scale_fill_viridis_d(option = opt, guide = guide)
    } else {
      scale_fill_brewer(palette = palette, guide = guide)
    }
  } else {
    if (numeric && viridis) {
      scale_color_viridis_c(option = opt)
    } else if (numeric) {
      scale_color_distiller(palette = palette)
    } else if (viridis) {
      scale_color_viridis_d(option = opt, guide = guide)
    } else {
      scale_color_brewer(palette = palette, guide = guide)
    }
  }
}

# Tip-label layer (text or boxed label), with optional color mapping.
tree_tiplab_layer <- function(opts) {
  if (!isTRUE(opts$tiplab_show)) {
    return(NULL)
  }

  mapped <- isTRUE(opts$mapping_show) && !is.null(opts$color_mapping)
  mapping <- if (mapped) {
    aes(label = .data[[opts$tiplab]], color = .data[[opts$color_mapping]])
  } else {
    aes(label = .data[[opts$tiplab]])
  }

  params <- list(
    mapping = mapping,
    size = opts$tiplab_size,
    align = isTRUE(opts$align),
    geom = "text"
  )

  # Only when nothing is mapped to colour. A fixed `color` parameter overrides
  # the aesthetic in ggplot2 rather than losing to it, so setting it
  # unconditionally meant "Map variable to color" drew every label in the fixed
  # colour and the mapping did nothing at all. Same rule the tip-point layer
  # below already followed.
  if (!mapped) {
    params$color <- opts$tiplab_color
  }

  do.call(geom_tiplab, params)
}

# Branch-distance / metadata labels at branch midpoints, filtered by cutoff.
tree_branch_layer <- function(opts, branch_lengths) {
  if (!isTRUE(opts$branch_show)) {
    return(NULL)
  }

  # The cutoff is a percentile of branch length. It used to be an absolute
  # allelic distance, which the 0-100 slider carrying it cannot express: cgMLST
  # distances on this scheme run into the thousands, so every branch cleared a
  # cutoff of 10 and switching branch labels on drew a label on all ~690 of
  # them. A percentile also makes the control fittable — see
  # tree_branch_cutoff().
  cut <- quantile(branch_lengths, probs = opts$branch_cutoff / 100, na.rm = TRUE)

  # Text standing above the branch, not a filled box centred on it. `x = branch`
  # is ggtree's branch midpoint and y is the branch's own row, so the default
  # vjust would centre the number *on* the horizontal line it describes, hiding
  # the line behind an opaque panel — which is why this needed a fill colour at
  # all. Lifting the baseline clear of the line instead means the branch stays
  # visible under its own number, and there is no panel left to colour.
  #
  # geom_text2 (ggtree) honours the `subset` aesthetic for cutoff filtering.
  geom_text2(
    mapping = aes(
      x = .data[["branch"]],
      label = round(.data[["branch.length"]], 2),
      subset = .data[["branch.length"]] > cut
    ),
    # Plain text on the background reads at a smaller size than the same number
    # boxed, and every pixel given back here is a pixel the tree keeps. Applied
    # to the fit's result rather than inside tree_auto_layout(), which is the
    # calibration anchor the layout tests pin.
    size = opts$branch_size * BRANCH_ABOVE_SHRINK,
    vjust = BRANCH_VJUST,
    color = opts$branch_color
  )
}

# Tip points, optionally driven by color and/or shape mappings.
tree_tippoint_layer <- function(opts) {
  if (!isTRUE(opts$tippoint_show)) {
    return(NULL)
  }

  aes_list <- list()
  if (isTRUE(opts$tipcolor_mapping_show) && !is.null(opts$tipcolor_mapping)) {
    aes_list$color <- as.name(opts$tipcolor_mapping)
  }
  if (isTRUE(opts$tipshape_mapping_show) && !is.null(opts$tipshape_mapping)) {
    aes_list$shape <- as.name(opts$tipshape_mapping)
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

# Internal node markers.
tree_nodepoint_layer <- function(opts) {
  if (!isTRUE(opts$nodepoint_show)) {
    return(NULL)
  }
  geom_nodepoint(
    alpha = opts$nodepoint_alpha,
    color = opts$nodepoint_color,
    shape = opts$nodepoint_shape,
    size = opts$nodepoint_size
  )
}

# Clade highlight rectangles for the selected parent nodes.
tree_clade_layers <- function(opts) {
  nodes <- suppressWarnings(as.integer(opts$parentnodes))
  nodes <- nodes[!is.na(nodes)]
  if (!length(nodes)) {
    return(NULL)
  }
  lapply(nodes, function(n) {
    geom_hilight(node = n, fill = opts$clade_color, type = "roundrect")
  })
}

# Up to five metadata tile strips drawn alongside the tree.
tree_tile_layers <- function(opts, md) {
  tiles <- opts$tiles
  if (is.null(tiles) || !length(tiles)) {
    return(NULL)
  }
  layers <- list()
  for (tile in tiles) {
    if (!isTRUE(tile$show) || is.null(tile$variable)) {
      next
    }
    layers <- c(
      layers,
      list(
        new_scale_fill(),
        geom_fruit(
          geom = geom_tile,
          mapping = aes(fill = .data[[tile$variable]]),
          alpha = tile$alpha,
          pwidth = tile$width,
          offset = tile$offset
        ),
        tree_scale(md[[tile$variable]], tile$scale, "fill")
      )
    )
  }
  if (length(layers)) layers else NULL
}

# Build the tree, muffling ggtree's internal ggplot2-deprecation warnings (the
# `size`-for-lines aesthetic and the `label.size` parameter come from ggtree's
# own geoms under ggplot2 4.x — nothing we can change here).
.muffled_tree_warnings <- "size.*aesthetic for lines|linewidth|label\\.size"

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

# Assemble the full ggtree plot. `opts` is a plain list of resolved control
# values; `metadata` is the make_metadata_table data.frame.
.build_tree_ggtree <- function(tree, metadata, opts) {
  # ggtreeExtra::geom_fruit resolves its geom by name on the search path, so
  # ggplot2 must be attached (box only imports into the module namespace).
  if (!"package:ggplot2" %in% search()) {
    base::attachNamespace("ggplot2")
  }

  # Optional rerooting on a chosen outgroup tip.
  if (!is.null(opts$root) && !identical(opts$root, "Automatic")) {
    og <- which(metadata$isolate == opts$root)
    if (length(og)) {
      tree <- root(tree, outgroup = og, resolve.root = TRUE)
    }
  }

  md <- tree_tip_metadata(tree, metadata)

  # Field-backed options can briefly hold placeholder names (before the selects
  # are repopulated from the metadata), so validate them against real columns.
  cols <- names(md)
  valid <- function(field) !is.null(field) && field %in% cols
  if (!valid(opts$tiplab)) {
    opts$tiplab <- "isolate"
  }
  opts$mapping_show <- isTRUE(opts$mapping_show) && valid(opts$color_mapping)
  opts$tipcolor_mapping_show <- isTRUE(opts$tipcolor_mapping_show) &&
    valid(opts$tipcolor_mapping)
  opts$tipshape_mapping_show <- isTRUE(opts$tipshape_mapping_show) &&
    valid(opts$tipshape_mapping)
  opts$heatmap_select <- intersect(opts$heatmap_select, cols)
  opts$tiles <- Filter(
    function(t) isTRUE(t$show) && !is.null(t$variable) && t$variable %in% cols,
    opts$tiles %||% list()
  )

  circular <- opts$layout %in% .circular_layouts
  label_reserve <- 0
  layout <- if (identical(opts$layout, "inward")) "circular" else opts$layout

  # Always ladderized. An unladderized tree of any size is a tangle no reader
  # gains anything from, so this was a control whose other setting was never the
  # right answer.
  base <- ggtree(
    tree,
    color = opts$line_color,
    layout = layout,
    ladderize = TRUE
  )
  # Node-label view dims the tree and overlays internal node numbers, helping
  # the user pick clades to highlight.
  if (isTRUE(opts$nodelabel_show)) {
    base <- ggtree(
      tree,
      color = opts$line_color,
      layout = layout,
      ladderize = TRUE,
      alpha = 0.2
    )
  }

  tree_data <- base$data
  max_x <- max(tree_data$x, na.rm = TRUE)
  branch_lengths <- tree_data$branch.length[tree_data$branch.length > 0]

  p <- base %<+% md

  # Order matters: clade highlights sit behind; each color mapping is closed
  # off with new_scale_color() so the next mapping starts a fresh scale.
  layers <- c(
    tree_clade_layers(opts),
    list(tree_tiplab_layer(opts)),
    if (isTRUE(opts$mapping_show)) {
      list(
        tree_scale(md[[opts$color_mapping]], opts$tiplab_scale, "color"),
        new_scale_color()
      )
    },
    list(
      tree_branch_layer(opts, branch_lengths),
      tree_nodepoint_layer(opts),
      tree_tippoint_layer(opts)
    ),
    if (isTRUE(opts$tipcolor_mapping_show)) {
      list(
        tree_scale(md[[opts$tipcolor_mapping]], opts$tippoint_scale, "color"),
        new_scale_color()
      )
    },
    if (isTRUE(opts$nodelabel_show)) {
      list(geom_nodelab(aes(label = .data[["node"]])))
    },
    tree_tile_layers(opts, md)
  )
  layers <- Filter(Negate(is.null), layers)
  for (layer in layers) {
    p <- p + layer
  }

  # Root edge and scale bar (the latter is meaningless on circular layouts).
  if (isTRUE(opts$rootedge_show)) {
    p <- p + geom_rootedge(rootedge = max_x * 0.05)
  }
  if (isTRUE(opts$treescale_show) && !circular) {
    p <- p +
      geom_treescale(
        x = max_x * 0.5,
        y = -1,
        # ggtree prints this width as the bar's label, so it has to be a number
        # worth reading: a tenth of the tree depth is 1.36116098546807.
        width = tree_nice_width(max_x * 0.1),
        color = opts$line_color,
        fontsize = 4
      )
  }

  # Room to the right of the tips for labels (linear layouts only), sized to the
  # labels this plot is actually drawing — see .tiplab_frac.
  if (!circular) {
    # The labels' share of the panel, plus room for the heatmap matrix when one
    # is drawn — the limit is fixed here, so anything gheatmap adds afterwards
    # has to already fit inside it or it is simply clipped.
    # Labels and any annotation matrix are solved for together. Sizing the
    # label reserve first and *then* widening the axis for the matrix is what
    # put the matrix over the labels: labels are drawn at a fixed physical size,
    # so stretching the axis afterwards leaves them spanning more data units
    # than were set aside for them.
    fit <- .tiplab_xlim(opts, md, tree_data, max_x, .heatmap_width(opts))
    label_reserve <- fit$reserve
    p <- p + xlim(NA, fit$limit)
  }

  p <- p +
    theme_tree(bgcolor = opts$bg) +
    theme(
      plot.margin = if (circular) margin(0, 0, 0, 0) else margin(6, 6, 6, 6),
      legend.direction = opts$legend_orientation,
      legend.position = c(opts$legend_x, opts$legend_y),
      legend.title = element_text(
        color = opts$line_color,
        size = opts$legend_size * 1.2
      ),
      legend.text = element_text(
        color = opts$line_color,
        size = opts$legend_size
      ),
      legend.key.size = unit(0.05 * opts$legend_size, "cm"),
      plot.background = element_rect(fill = opts$bg, color = opts$bg)
    )

  # Heatmap annotation matrix, to the right of the tip labels.
  if (isTRUE(opts$heatmap_show) && length(opts$heatmap_select)) {
    heat <- md[, opts$heatmap_select, drop = FALSE]
    # gheatmap matches its rows to the tree by *row name*, and this data frame
    # is built by tree_tip_metadata() with the default 1..n row names. Without
    # this every cell came out unmatched, which is why the heatmap drew nothing
    # and its legend read "NA".
    rownames(heat) <- md$label
    # A fresh fill scale for the matrix. The tile strips above map fill too, and
    # without this gheatmap's scale replaced theirs ("Scale for fill is already
    # present"), leaving every heatmap value outside the surviving scale — which
    # is what dropped all of them ("Removed 346 rows containing missing values").
    p <- gheatmap(
      p + new_scale_fill(),
      data = heat,
      # Starts past the tip labels rather than on top of them: `offset` is in
      # x-axis units from the tips, and at 0 the matrix was drawn over the
      # labels, column names and all.
      offset = if (circular) 0 else label_reserve,
      # Shares the annotation budget across the selected columns instead of
      # taking a fifth of the tree's width for each, which put a ten-column
      # heatmap at twice the width of the tree.
      width = .heatmap_width(opts),
      legend_title = "Heatmap",
      # Under the matrix, which is now clear of the tip labels — the collision
      # was the missing offset above, not where the names sit. Above the plot
      # they have no headroom to be drawn in and are clipped instead.
      colnames_angle = -90,
      colnames_offset_y = -1
    )
  }

  # Zoom / pan, then a background-filled canvas.
  out <- as.ggplot(p, scale = opts$zoom, hjust = opts$h, vjust = opts$v)
  ggdraw(out) +
    theme(plot.background = element_rect(fill = opts$bg, color = opts$bg))
}

# Largest raster dimension worth writing: an aspect ratio of 8 on a 5.5in canvas
# is a 44in tall plot, which at 300dpi would be a 13000px, ~350MB image.
MAX_PLOT_PX <- 10000

# Render the tree to a raster/vector file at the given aspect ratio.
#
# `width` is the canvas width in inches, and callers should pass the width of
# the *preview* device rather than take the default: font sizes are absolute
# (mm), so the same tiplab_size on a 10in canvas reads half as large, relative
# to the tree, as it does on the ~5.5in on-screen one — the export would not be
# what the user tuned. dpi carries the pixel count instead, which is what it is
# for.
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
  # Cap the raster by lowering the resolution, never by changing the canvas —
  # the canvas is the layout. Vector output has no pixels to cap.
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
