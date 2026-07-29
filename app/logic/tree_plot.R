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

# --- Layout & Geometry Constants ---------------------------------------------

# Vertical row geometry (in inches and ratios)
TIP_ROW_IN <- 0.228 # Target inches of plot height per tip
TIP_USABLE <- 0.9 # Share of plot height available excluding margins/title
TIP_ROW_FILL <- 0.77 # Fraction of row pitch occupied by tip label text box

# Horizontal label reservation geometry
TIP_CHAR_EM <- 0.6 # Character width estimate (em) for accession/isolate labels
TIP_LABEL_FRAC <- 0.35 # Maximum fraction of panel width reserved for tip labels
TIP_CIRC_K <- 2 * pi * 0.35 # Angular row pitch scaling factor for circular layouts

# Node element relative scaling
NODE_ROW_FILL <- 0.48 # Fraction of row pitch for node point diameter

#' Default Layout Parameters for Dynamic Sizing Controls
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
    nodepoint_size = fit_size("nodepoint_size", NODE_ROW_FILL * row_mm),
    zoom = if (circular) TREE_FIT_DEFAULTS$zoom else LINEAR_ZOOM,
    h = if (circular) TREE_FIT_DEFAULTS$h else LINEAR_H,
    labels_legible = size >= TIP_SIZE_FLOOR
  )
}

# --- Metadata Mapping Thresholds ---------------------------------------------

# Maximum categories before switching from discrete palettes to continuous/viridis
MAX_QUAL_LEVELS <- 9L

#' Maximum Supported Categories for Discrete Shapes
#' @export
MAX_SHAPE_LEVELS <- 6L

# Minimum non-empty values share required to propose a default mapping
MIN_COVERAGE <- 0.5

#' Count Unique Valid Levels
#'
#' @param values Vector of values to evaluate.
#' @return Integer count of non-NA, non-blank distinct values.
#' @export
field_levels <- function(values) {
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(unique(v))
}

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

#' Rank Metadata Fields for Visual Mapping
#'
#' Filters and ranks metadata columns based on coverage and grouping potential.
#'
#' @param metadata data.frame containing sample metadata.
#' @param max_levels Integer/Numeric. Upper threshold for distinct values.
#' @return Character vector of ranked column names.
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

  # Exclude non-grouping fields (single value, unique ID per row, or over max_levels)
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

#' Select Scale Category for Metadata Field
#'
#' @param values Vector of field data.
#' @param numeric_categories Character vector of scales for continuous data.
#' @return Recommended scale family vector.
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

ANNOTATION_BUDGET <- 0.45

#' Calculate Individual Tile Strip Width
#'
#' @param n_strips Integer. Number of active tile annotation layers.
#' @return Relative width fraction per strip.
#' @export
tree_annotation_width <- function(n_strips) {
  n <- max(as.integer(n_strips %||% 1L), 1L)
  round(ANNOTATION_BUDGET / n, 2)
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
HEATMAP_CLEARANCE <- 1.3

.heatmap_width <- function(opts) {
  n <- length(opts$heatmap_select)
  if (!n) {
    return(0)
  }
  tree_annotation_width(1) * min(n, 6L) / 6L
}

# Solves x-axis plot range ensuring tip labels and heatmaps fit without clipping
.tiplab_xlim <- function(opts, md, tree_data, max_x, heat = 0) {
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

tree_scale <- function(values, palette, aesthetic) {
  numeric <- is.numeric(values)
  viridis <- is.null(palette) || palette %in% .viridis_scales
  opt <- if (is.null(palette) || !viridis) "viridis" else palette

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

.muffled_tree_warnings <- "size.*aesthetic for lines|linewidth|label\\.size"

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

  base <- ggtree(
    tree,
    color = opts$line_color,
    layout = layout,
    ladderize = TRUE
  )

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

  # Assemble plot layers (order maintains visual hierarchy)
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

  if (isTRUE(opts$rootedge_show)) {
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

  if (!circular) {
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

  if (isTRUE(opts$heatmap_show) && length(opts$heatmap_select)) {
    heat <- md[, opts$heatmap_select, drop = FALSE]
    rownames(heat) <- md$label

    p <- gheatmap(
      p + new_scale_fill(),
      data = heat,
      offset = if (circular) 0 else label_reserve,
      width = .heatmap_width(opts),
      legend_title = "Heatmap",
      colnames_angle = -90,
      colnames_offset_y = -1
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
