# app/logic/phylo.R
#
# Phylogenetic tree and Minimum Spanning Tree (MST) computation from cgMLST allele profiles.
#
# Pipeline:
# 1. Read per-isolate allele profiles from the `mlst` table (and optional staged imports).
# 2. Compute pairwise Hamming distance matrices using specified NA-handling policies.
# 3. Construct Neighbor-Joining (NJ) / UPGMA trees or interactive MST network graphs.

box::use(
  RSQLite[SQLite],
  DBI[
    dbConnect,
    dbGetQuery,
    dbDisconnect,
  ],
  tidyr[pivot_wider],
  dplyr[select, mutate],
  ape[nj, as.phylo],
  igraph[
    graph_from_adjacency_matrix,
    graph_from_data_frame,
    components,
    mst,
    set_vertex_attr,
  ],
  visNetwork[
    visNetwork,
    toVisNetworkData,
    visNodes,
    visEdges,
    visOptions,
    visInteraction,
    visLayout,
    visEvents,
    visGroups,
    visLegend,
    visSave,
  ],
  htmlwidgets[JS],
  viridisLite[viridis],
  grDevices[rainbow, col2rgb],
  stats[as.dist, hclust],
)

box::use(
  app / logic / db_staging[imported_profile_long, local_allele_map],
)

# --- 1. Allele Profile Extraction --------------------------------------------

#' Load Isolate Allele Profiles
#'
#' Queries the SQLite database for allele profiles (excluding synthetic reference entries)
#' and reshapes them into a matrix (isolates x loci). Staged imported peer profiles can
#' optionally be folded in and mapped into the local integer seqid code space[cite: 12].
#'
#' @param db_path File path to SQLite database[cite: 12].
#' @param isolates Vector of isolate IDs to filter, or NULL for all[cite: 12].
#' @param imported_sets Optional list of staged imported peer profile datasets[cite: 12].
#' @return Integer matrix of allele profiles with isolate names as row names[cite: 12].
#' @export
load_allele_profile <- function(
  db_path,
  isolates = NULL,
  imported_sets = NULL
) {
  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  long <- dbGetQuery(
    con,
    "SELECT souche AS isolate, gene, seqid FROM mlst WHERE souche != 'ref'"
  )

  if (length(imported_sets)) {
    long <- rbind(long, .imported_long(db_path, imported_sets, long$seqid))
  }

  if (nrow(long) == 0) {
    return(matrix(integer(0), nrow = 0, ncol = 0))
  }

  wide <- long |>
    select(isolate, gene, seqid) |>
    pivot_wider(names_from = gene, values_from = seqid)

  isolate <- wide$isolate
  mat <- as.matrix(wide[, setdiff(names(wide), "isolate"), drop = FALSE])
  storage.mode(mat) <- "integer"
  rownames(mat) <- isolate

  if (!is.null(isolates)) {
    mat <- mat[rownames(mat) %in% isolates, , drop = FALSE]
  }

  mat
}

# Helper: Format staged imported profiles into long format within local seqid code space
.imported_long <- function(db_path, imported_sets, local_seqids) {
  imp <- imported_profile_long(db_path, imported_sets)
  if (!nrow(imp)) {
    return(NULL)
  }

  map <- local_allele_map(db_path)
  imp$seqid <- map$seqid[match(
    paste(imp$gene, imp$hash),
    paste(map$gene, map$hash)
  )]

  # Assign novel integer seqids above local max for imported alleles unseen in local database
  novel <- is.na(imp$seqid)
  if (any(novel)) {
    base <- max(c(local_seqids, map$seqid), na.rm = TRUE)
    imp$seqid[novel] <- base +
      as.integer(factor(paste(imp$gene, imp$hash)[novel]))
  }

  data.frame(
    isolate = imp$isolate,
    gene = imp$gene,
    seqid = as.integer(imp$seqid),
    stringsAsFactors = FALSE
  )
}

# --- 2. Pairwise Distance Kernels --------------------------------------------

#' Standard Hamming Distance Kernel
#' @param x Vector of allele values[cite: 12].
#' @param y Vector of allele values[cite: 12].
#' @return Integer count of differing positions[cite: 12].
#' @export
hamming_dist <- function(x, y) {
  sum(x != y)
}

#' Missing-Value Pairwise Ignore Hamming Distance Kernel
#' @param x Vector of allele values[cite: 12].
#' @param y Vector of allele values[cite: 12].
#' @return Integer count of mismatches excluding positions where either value is NA[cite: 12].
#' @export
hamming_dist_ignore <- function(x, y) {
  sum((x != y) & !is.na(x) & !is.na(y))
}

#' NA-as-Category Hamming Distance Kernel
#' @param x Vector of allele values[cite: 12].
#' @param y Vector of allele values[cite: 12].
#' @return Integer count where NA vs value is a mismatch, but NA vs NA is a match[cite: 12].
#' @export
hamming_dist_category <- function(x, y) {
  sum((x != y | xor(is.na(x), is.na(y))) & !(is.na(x) & is.na(y)))
}

# --- 3. Distance Matrix Construction ----------------------------------------

#' Compute Distance Matrix Across Profiles
#'
#' Applies a distance metric function across all pairwise isolate profile combinations[cite: 12].
#'
#' @param profile Matrix of allele profiles[cite: 12].
#' @param hamming_method Distance function kernel to apply[cite: 12].
#' @return Symmetric distance matrix[cite: 12].
#' @export
compute_dist_matrix <- function(profile, hamming_method) {
  mat <- as.matrix(profile)
  n <- nrow(mat)
  dist_mat <- matrix(0, n, n)
  if (n < 2) {
    return(dist_mat)
  }
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      dist_mat[i, j] <- hamming_method(x = mat[i, ], y = mat[j, ])
      dist_mat[j, i] <- dist_mat[i, j]
    }
  }
  dist_mat
}

# --- 4. Phylogenetic Tree Construction --------------------------------------

#' Construct Phylogenetic Tree Object
#'
#' Builds an ape `phylo` object using Neighbor-Joining (NJ) or UPGMA[cite: 12].
#' NJ branch lengths are transformed via inverse hyperbolic sine (asinh)[cite: 12].
#'
#' @param dist_mat Distance matrix[cite: 12].
#' @param labels Tip label vector matching distance matrix ordering[cite: 12].
#' @param algo Clustering algorithm ("Neighbour-Joining" or "UPGMA")[cite: 12].
#' @return An ape `phylo` object[cite: 12].
#' @export
build_tree <- function(dist_mat, labels, algo) {
  d <- as.dist(dist_mat)

  tree <- if (identical(algo, "UPGMA")) {
    as.phylo(hclust(d, method = "average"))
  } else {
    nj_tree <- nj(d)
    el <- abs(nj_tree[["edge.length"]])
    nj_tree[["edge.length"]] <- log(el + sqrt(el^2 + 1))
    nj_tree
  }

  tree$tip.label <- labels
  tree
}

# --- 5. Internal Distance Preparation ---------------------------------------

# Helper: Load allele profile and prepare distance matrix according to NA policy
prepare_distance <- function(
  db_path,
  na_handling,
  isolates = NULL,
  imported_sets = NULL
) {
  profile <- load_allele_profile(db_path, isolates, imported_sets)
  if (nrow(profile) < 1) {
    return(NULL)
  }

  na_handling <- na_handling %||% "ignore_na"
  method <- switch(
    na_handling,
    ignore_na = hamming_dist_ignore,
    category = hamming_dist_category,
    omit = {
      keep <- colSums(is.na(profile)) == 0
      profile <- profile[, keep, drop = FALSE]
      hamming_dist
    },
    hamming_dist_ignore
  )

  list(
    profile = profile,
    method = method,
    dist = compute_dist_matrix(profile, method)
  )
}

# --- 6. Tree Orchestration --------------------------------------------------

#' Compute Phylogenetic Tree
#'
#' High-level wrapper to calculate distances and return a phylogenetic tree[cite: 12].
#'
#' @param db_path Database path[cite: 12].
#' @param na_handling Strategy for missing values ("ignore_na", "omit", or "category")[cite: 12].
#' @param algo Clustering algorithm ("Neighbour-Joining" or "UPGMA")[cite: 12].
#' @param isolates Optional list of isolate IDs[cite: 12].
#' @param imported_sets Optional list of staged imported peer profiles[cite: 12].
#' @return A `phylo` object, or NULL if insufficient isolates are provided[cite: 12].
#' @export
compute_phylo_tree <- function(
  db_path,
  na_handling,
  algo,
  isolates = NULL,
  imported_sets = NULL
) {
  prep <- prepare_distance(db_path, na_handling, isolates, imported_sets)
  if (is.null(prep) || nrow(prep$profile) < 3) {
    return(NULL)
  }

  build_tree(
    prep$dist,
    rownames(prep$profile),
    if (is.null(algo)) "Neighbour-Joining" else algo
  )
}

# --- 7. Minimum Spanning Tree (MST) Orchestration ----------------------------

#' Compute Minimum Spanning Tree Graph
#'
#' Builds an igraph MST representation from isolate allele profiles[cite: 12].
#' Zero-distance isolates are merged into single representative nodes[cite: 12].
#'
#' @param db_path Database path[cite: 12].
#' @param na_handling Strategy for handling missing values[cite: 12].
#' @param isolates Optional list of isolate IDs[cite: 12].
#' @param imported_sets Optional list of staged imported peer profiles[cite: 12].
#' @return An `igraph` object, or NULL if insufficient isolates exist[cite: 12].
#' @export
compute_mst <- function(
  db_path,
  na_handling,
  isolates = NULL,
  imported_sets = NULL
) {
  prep <- prepare_distance(db_path, na_handling, isolates, imported_sets)
  if (is.null(prep) || nrow(prep$profile) < 2) {
    return(NULL)
  }

  profile <- prep$profile
  labels <- rownames(profile)

  # Collapse zero-distance samples into groups (transitive: chained identical
  # profiles merge into one node).
  zero_adj <- prep$dist == 0
  diag(zero_adj) <- FALSE
  membership <- components(
    graph_from_adjacency_matrix(zero_adj, mode = "undirected", diag = FALSE)
  )$membership

  groups <- split(seq_along(labels), membership)
  rep_idx <- vapply(groups, `[`, integer(1), 1L)
  group_names <- vapply(
    groups,
    function(idx) {
      paste(labels[idx], collapse = "\n")
    },
    character(1)
  )
  group_sizes <- lengths(groups)

  rep_profile <- profile[rep_idx, , drop = FALSE]
  rep_dist <- compute_dist_matrix(rep_profile, prep$method)

  graph <- graph_from_adjacency_matrix(
    rep_dist,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )
  tree <- mst(graph)
  tree <- set_vertex_attr(tree, "name", value = group_names)
  tree <- set_vertex_attr(tree, "n", value = group_sizes)
  tree
}

# Helper: Extract node label text given metadata column
mst_node_labels <- function(node_ids, metadata, field) {
  if (is.null(field) || !field %in% names(metadata)) {
    field <- "isolate"
  }
  vapply(
    node_ids,
    function(id) {
      members <- strsplit(id, "\n", fixed = TRUE)[[1]]
      paste(metadata[match(members, metadata$isolate), field], collapse = "\n")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Shapes whose pixel size is driven by their (label) content rather than the
# `value`/`scaling` mechanism, so duplicate-count scaling does not apply.
.border_sized_shapes <- c("circle", "box", "text", "database")

# JavaScript HTML Canvas renderer for pie-chart MST nodes
ctxRendererJS <- JS(
  "({ctx, id, x, y, state: { selected, hover }, style, font, label, metadata}) => {
    var pieData = JSON.parse(metadata);
    var radius = style.size;
    var centerX = x;
    var centerY = y;
    var total = pieData.reduce((sum, slice) => sum + slice.value, 0)
    var startAngle = 0;
    const drawNode = () => {
    if (style.shadow) {
    ctx.shadowColor = style.shadowColor;
    ctx.shadowBlur = style.shadowSize;
    ctx.shadowOffsetX = style.shadowX;
    ctx.shadowOffsetY = style.shadowY;
    ctx.beginPath();
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
    ctx.fill();
    ctx.shadowColor = 'transparent';
    ctx.shadowBlur = 0;
    ctx.shadowOffsetX = 0;
    ctx.shadowOffsetY = 0;
    }
    pieData.forEach(slice => {
    var sliceAngle = 2 * Math.PI * (slice.value / total);
    ctx.beginPath();
    ctx.moveTo(centerX, centerY);
    ctx.arc(centerX, centerY, radius, startAngle, startAngle + sliceAngle);
    ctx.closePath();
    ctx.fillStyle = slice.color;
    ctx.fill();
    if (pieData.length > 1) {
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    ctx.stroke();
    }
    startAngle += sliceAngle;
    });
    ctx.beginPath();
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI);
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 1;
    ctx.stroke();
    };
    drawLabel = () => {
    var lines = label.split(`\n`);
    var lineHeight = font.size;
    ctx.font = `${font.size}px ${font.face}`;
    ctx.fillStyle = font.color;
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    lines.forEach((line, index) => {
    ctx.fillText(line, centerX,
    centerY + radius + (index + 1) * lineHeight);
    })
    }
    return {
    drawNode,
    drawExternalLabel: drawLabel,
    nodeDimensions: { width: 2 * radius, height: 2 * radius },
    };
    }"
)

# Generate metadata palette colors
mst_palette <- function(values, scale) {
  cats <- unique(values)
  n <- length(cats)
  cols <- if (identical(scale, "Rainbow")) rainbow(n) else viridis(n)
  data.frame(value = cats, color = cols, stringsAsFactors = FALSE)
}

# Generate JSON payload for HTML canvas pie chart rendering
mst_pie_metadata <- function(node_ids, metadata, col_var, var_cols) {
  vapply(
    node_ids,
    function(id) {
      members <- strsplit(id, "\n", fixed = TRUE)[[1]]
      values <- metadata[match(members, metadata$isolate), col_var]
      cats <- unique(values)
      slices <- vapply(
        cats,
        function(v) {
          in_cat <- if (is.na(v)) {
            is.na(values)
          } else {
            (!is.na(values) & values == v)
          }
          share <- sum(in_cat) / length(values) * 100
          color <- var_cols$color[match(v, var_cols$value)]
          sprintf('{"value":%s,"color":"%s"}', share, color)
        },
        character(1)
      )
      paste0("[", paste(slices, collapse = ","), "]")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# Calculate distance threshold cluster groupings
compute_clusters <- function(nodes, edges, threshold) {
  ids <- nodes$id
  qual <- edges[edges$weight <= threshold, c("from", "to"), drop = FALSE]
  g <- graph_from_data_frame(
    qual,
    directed = FALSE,
    vertices = data.frame(name = ids, stringsAsFactors = FALSE)
  )
  memb <- components(g)$membership
  memb <- memb[match(ids, names(memb))]
  sizes <- table(memb)

  groups <- ifelse(sizes[as.character(memb)] > 1, paste("Group", memb), "0")
  edge_group <- vapply(
    seq_len(nrow(edges)),
    function(i) {
      f <- match(as.character(edges$from[i]), ids)
      t <- match(as.character(edges$to[i]), ids)
      if (groups[f] != "0" && groups[f] == groups[t]) groups[f] else "0"
    },
    character(1)
  )

  list(groups = groups, edge_group = edge_group)
}

# Format legend configuration nodes for visNetwork
mst_legend_nodes <- function(var_cols, symbol_size, font_size, font_color) {
  lapply(seq_len(nrow(var_cols)), function(i) {
    list(
      label = as.character(var_cols$value[i]),
      shape = "dot",
      size = symbol_size,
      color = var_cols$color[i],
      font = list(size = font_size, color = font_color)
    )
  })
}

# Determine number of legend columns based on category count
legend_col <- function(n) {
  if (n <= 5) {
    1
  } else if (n <= 10) {
    2
  } else {
    3
  }
}

# Generate color palette for graph clusters
mst_cluster_palette <- function(n, scale) {
  if (n == 0) {
    return(character(0))
  }
  if (identical(scale, "Rainbow")) rainbow(n) else viridis(n)
}

# Generate dual-layer skeleton edge configurations for clusters
mst_skeleton_edges <- function(edges, edge_group, width, scale) {
  thin <- edges
  thin$width <- 2
  thin$color <- "black"

  thick <- edges
  thick$width <- width
  thick$color <- "rgba(0,0,0,0)"

  labels <- unique(edge_group[edge_group != "0"])
  palette <- mst_cluster_palette(length(labels), scale)
  for (i in seq_along(labels)) {
    rgb <- paste(col2rgb(palette[i]), collapse = ", ")
    thick$color[edge_group == labels[i]] <- paste0("rgba(", rgb, ", 0.5)")
  }
  rbind(thick, thin)
}

#' Build Interactive MST visNetwork Widget
#'
#' Converts an `igraph` MST object into an interactive HTML `visNetwork` widget[cite: 12].
#'
#' @param graph An `igraph` MST object[cite: 12].
#' @param metadata Isolate metadata data frame[cite: 12].
#' @param opts Visual display options list[cite: 12].
#' @return A `visNetwork` htmlwidget object[cite: 12].
#' @export
build_mst_visnetwork <- function(graph, metadata, opts) {
  data <- toVisNetworkData(graph)

  color_var <- isTRUE(opts$color_var) &&
    !is.null(opts$col_var) &&
    opts$col_var %in% names(metadata)

  # Variable coloring forces the pie renderer (shape "custom") and the labels
  # on, so each pie's constituent samples stay identifiable.
  show_label <- isTRUE(opts$show_label) || color_var
  shape <- if (color_var) "custom" else opts$shape

  data$nodes <- mutate(
    data$nodes,
    label = if (show_label) {
      mst_node_labels(data$nodes$id, metadata, opts$field)
    } else {
      ""
    },
    # `value` drives scaling within [min, max]; NULL leaves nodes at fixed size.
    value = if (isTRUE(opts$scale_nodes) && !shape %in% .border_sized_shapes) {
      data$nodes$n
    } else {
      NULL
    }
  )

  # Pie slice data per node: share of each category of `col_var`.
  var_cols <- NULL
  if (color_var) {
    members <- unlist(strsplit(data$nodes$id, "\n", fixed = TRUE))
    var_cols <- mst_palette(
      metadata[match(members, metadata$isolate), opts$col_var],
      opts$col_scale
    )
    data$nodes$metadata <- mst_pie_metadata(
      data$nodes$id,
      metadata,
      opts$col_var,
      var_cols
    )
  }

  data$edges <- mutate(
    data$edges,
    length = if (isFALSE(opts$scale_edges)) {
      35
    } else {
      log(data$edges$weight) * opts$edge_length_scale
    },
    label = as.character(data$edges$weight)
  )

  # Clustering: group nodes connected within the allelic-distance threshold,
  # then render as colored node groups (Area) or colored edge skeletons.
  clusters <- NULL
  skeleton <- FALSE
  if (isTRUE(opts$show_clusters)) {
    clusters <- compute_clusters(data$nodes, data$edges, opts$cluster_threshold)
    if (identical(opts$cluster_type, "Skeleton")) {
      skeleton <- TRUE
      data$edges <- mst_skeleton_edges(
        data$edges,
        clusters$edge_group,
        opts$cluster_width,
        opts$cluster_col_scale
      )
    } else {
      data$nodes$group <- clusters$groups
    }
  }

  background <- if (isTRUE(opts$transparent)) {
    "rgba(0,0,0,0)"
  } else {
    opts$background
  }

  vis <- visNetwork(data$nodes, data$edges, background = background) |>
    visNodes(
      size = opts$node_size,
      shape = shape,
      shadow = opts$shadow,
      color = opts$node_color,
      ctxRenderer = ctxRendererJS,
      scaling = list(min = 20, max = 40),
      font = list(color = opts$node_font_color, size = opts$node_font_size)
    ) |>
    visEdges(
      color = opts$edge_color,
      font = list(
        color = opts$edge_font_color,
        size = opts$edge_font_size,
        strokeWidth = 4,
        strokeColor = background
      ),
      smooth = !skeleton,
      physics = !skeleton
    ) |>
    visOptions(collapse = TRUE) |>
    visInteraction(hover = TRUE) |>
    visLayout(randomSeed = 1) |>
    # The physics stabilization runs on a hidden canvas after the data arrives;
    # signal the loading overlay to clear only once the network is laid out and
    # drawn, otherwise the spinner vanishes into several seconds of blank space.
    visEvents(
      stabilizationIterationsDone = paste0(
        "function(){document.querySelectorAll('.viz-plot-stage')",
        ".forEach(function(s){s.classList.remove('is-loading');});}"
      )
    )

  # Area clusters: color each multi-node group; singletons keep the node color.
  if (!is.null(clusters) && !skeleton) {
    labels <- unique(clusters$groups[clusters$groups != "0"])
    palette <- mst_cluster_palette(length(labels), opts$cluster_col_scale)
    for (i in seq_along(labels)) {
      vis <- visGroups(vis, groupname = labels[i], color = palette[i])
    }
  }

  # Legend mirrors the variable color categories.
  if (color_var) {
    vis <- visLegend(
      vis,
      useGroups = FALSE,
      zoom = TRUE,
      width = 0.2,
      position = opts$legend_ori,
      ncol = legend_col(nrow(var_cols)),
      addNodes = mst_legend_nodes(
        var_cols,
        opts$legend_symbol_size,
        opts$legend_font_size,
        opts$node_font_color
      )
    )
  }

  vis
}

#' Save MST HTML Visualization
#'
#' Exports a visNetwork widget object as a standalone HTML file[cite: 12].
#'
#' @param widget Interactive visNetwork widget[cite: 12].
#' @param file Output HTML file path[cite: 12].
#' @param background Canvas background color string[cite: 12].
#' @export
save_mst_html <- function(widget, file, background) {
  visSave(widget, file = file, background = background)
}

# Null-coalescing infix operator
`%||%` <- function(a, b) if (is.null(a)) b else a
