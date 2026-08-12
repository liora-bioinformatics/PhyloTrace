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
    geom_label2,
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
    ggtitle,
    ggsave,
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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

# Per-tip metadata keyed by tip label, for `%<+%`. The tree's tip labels are the
# souche names, so the join key is the `isolate` column (one row per souche).
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

  if (identical(aesthetic, "fill")) {
    if (numeric && viridis) {
      scale_fill_viridis_c(option = opt)
    } else if (numeric) {
      scale_fill_distiller(palette = palette)
    } else if (viridis) {
      scale_fill_viridis_d(option = opt)
    } else {
      scale_fill_brewer(palette = palette)
    }
  } else {
    if (numeric && viridis) {
      scale_color_viridis_c(option = opt)
    } else if (numeric) {
      scale_color_distiller(palette = palette)
    } else if (viridis) {
      scale_color_viridis_d(option = opt)
    } else {
      scale_color_brewer(palette = palette)
    }
  }
}

# Tip-label layer (text or boxed label), with optional color mapping.
tree_tiplab_layer <- function(opts, circular) {
  if (!isTRUE(opts$tiplab_show)) {
    return(NULL)
  }

  mapping <- if (isTRUE(opts$mapping_show) && !is.null(opts$color_mapping)) {
    aes(label = .data[[opts$tiplab]], color = .data[[opts$color_mapping]])
  } else {
    aes(label = .data[[opts$tiplab]])
  }

  params <- list(
    mapping = mapping,
    size = opts$tiplab_size,
    alpha = opts$tiplab_alpha,
    fontface = opts$tiplab_fontface,
    align = isTRUE(opts$align),
    geom = if (isTRUE(opts$label_panel)) "label" else "text"
  )

  # Linear layouts nudge labels along x and may angle them; circular layouts use
  # hjust for inward/outward placement instead.
  if (circular) {
    params$hjust <- opts$tiplab_position
  } else {
    params$nudge_x <- opts$tiplab_position
    params$angle <- opts$tiplab_angle
  }

  if (isTRUE(opts$label_panel)) {
    params$label.padding <- unit(0.25, "lines")
    params$label.r <- unit(0.2, "lines")
    params$fill <- opts$tiplab_fill
  } else {
    params$color <- opts$tiplab_color
  }

  do.call(geom_tiplab, params)
}

# Branch-distance / metadata labels at branch midpoints, filtered by cutoff.
tree_branch_layer <- function(opts, branch_lengths) {
  if (!isTRUE(opts$branch_show)) {
    return(NULL)
  }

  if (identical(opts$branch_label, "Allelic Distance")) {
    mapping <- aes(
      x = .data[["branch"]],
      label = round(.data[["branch.length"]], 2),
      subset = .data[["branch.length"]] > opts$branch_cutoff
    )
  } else {
    cut <- quantile(
      branch_lengths,
      probs = opts$branch_cutoff / 100,
      na.rm = TRUE
    )
    mapping <- aes(
      x = .data[["branch"]],
      label = .data[[opts$branch_label]],
      subset = .data[["branch.length"]] > cut
    )
  }

  # geom_label2 (ggtree) honours the `subset` aesthetic for cutoff filtering.
  geom_label2(
    mapping = mapping,
    size = opts$branch_size,
    alpha = 0.65,
    color = opts$branch_color,
    fill = opts$branch_label_color
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
    geom_hilight(node = n, fill = opts$clade_color, type = opts$clade_type)
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
  if (
    isTRUE(opts$branch_show) &&
      !identical(opts$branch_label, "Allelic Distance") &&
      !valid(opts$branch_label)
  ) {
    opts$branch_label <- "Allelic Distance"
  }
  opts$heatmap_select <- intersect(opts$heatmap_select, cols)
  opts$tiles <- Filter(
    function(t) isTRUE(t$show) && !is.null(t$variable) && t$variable %in% cols,
    opts$tiles %||% list()
  )

  circular <- opts$layout %in% .circular_layouts
  layout <- if (identical(opts$layout, "inward")) "circular" else opts$layout

  base <- ggtree(
    tree,
    color = opts$line_color,
    layout = layout,
    ladderize = isTRUE(opts$ladderize)
  )
  # Node-label view dims the tree and overlays internal node numbers, helping
  # the user pick clades to highlight.
  if (isTRUE(opts$nodelabel_show)) {
    base <- ggtree(
      tree,
      color = opts$line_color,
      layout = layout,
      ladderize = isTRUE(opts$ladderize),
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
    list(tree_tiplab_layer(opts, circular)),
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
        width = max_x * 0.1,
        color = opts$line_color,
        fontsize = 4
      )
  }

  # Room to the right of the tips for labels (linear layouts only).
  if (!circular) {
    p <- p + xlim(NA, max_x * 1.6)
  }

  p <- p +
    ggtitle(label = opts$title, subtitle = opts$subtitle) +
    theme_tree(bgcolor = opts$bg) +
    theme(
      plot.margin = if (circular) margin(0, 0, 0, 0) else margin(6, 6, 6, 6),
      plot.title = element_text(
        color = opts$title_color,
        size = opts$title_size
      ),
      plot.subtitle = element_text(
        color = opts$title_color,
        size = opts$subtitle_size
      ),
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

  # Heatmap annotation matrix to the right of the tips.
  if (isTRUE(opts$heatmap_show) && length(opts$heatmap_select)) {
    p <- gheatmap(
      p,
      data = md[, opts$heatmap_select, drop = FALSE],
      offset = 0,
      width = 0.2 * length(opts$heatmap_select),
      legend_title = "Heatmap",
      colnames_angle = -90,
      colnames_offset_y = -1
    )
  }

  # Zoom / pan, then a background-filled canvas.
  out <- as.ggplot(p, scale = opts$zoom, hjust = opts$h, vjust = opts$v)
  ggdraw(out) +
    theme(plot.background = element_rect(fill = opts$bg, color = opts$bg))
}

# Render the tree to a raster/vector file at the given aspect ratio.
#' @export
save_tree_plot <- function(plot, file, filetype, aspect_ratio, dpi = 192) {
  width <- 10
  ggsave(
    filename = file,
    plot = plot,
    device = filetype,
    width = width,
    height = width * aspect_ratio,
    dpi = dpi,
    limitsize = FALSE
  )
}
