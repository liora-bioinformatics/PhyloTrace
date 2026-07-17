# app/logic/phylo.R
#
# Phylogenetic tree computation from cgMLST allele profiles.
#
# Pipeline: read the per-sample allele profile from the `mlst` table, build a
# pairwise Hamming distance matrix (with a configurable missing-value policy),
# and construct an NJ or UPGMA tree. NJ edge lengths are passed through the
# inverse hyperbolic sine to keep the tree renderable without clipping.

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

# --- 1. Allele profile -------------------------------------------------------

# Read the wide allele profile (rows = samples, cols = loci, value = allele id)
# from the `mlst` table. The synthetic `ref` core-genome strain is excluded.
# Returns an integer matrix with sample (souche) names as rownames; absent
# sample/locus combinations are NA. When `isolates` is non-NULL, the matrix is
# restricted to those sample names (the Visualization isolate preselection) —
# NULL means all isolates.
#
# `imported_sets` optionally folds in staged peer profiles (see db_staging.R).
# Those carry an allele *hash* rather than a `seqid`, so they are mapped onto the
# local seqid code space: an allele we already know reuses its seqid, and one we
# have never seen gets a fresh code above the local maximum. The codes are only
# ever an in-memory comparison currency — nothing is written back — and the
# distance kernels below compare with `!=`, so they neither know nor care whether
# a cell is a seqid or a synthetic code.
#
# With `imported_sets = NULL` this runs exactly the query it always has.
#' @export
load_allele_profile <- function(db_path, isolates = NULL, imported_sets = NULL) {
  con <- dbConnect(SQLite(), db_path, synchronous = NULL, busy_timeout = 5000)
  on.exit(dbDisconnect(con))

  long <- dbGetQuery(
    con,
    "SELECT souche, gene, seqid FROM mlst WHERE souche != 'ref'"
  )

  if (length(imported_sets)) {
    long <- rbind(long, .imported_long(db_path, imported_sets, long$seqid))
  }

  if (nrow(long) == 0) {
    return(matrix(integer(0), nrow = 0, ncol = 0))
  }

  wide <- long |>
    select(souche, gene, seqid) |>
    pivot_wider(names_from = gene, values_from = seqid)

  souche <- wide$souche
  mat <- as.matrix(wide[, setdiff(names(wide), "souche"), drop = FALSE])
  storage.mode(mat) <- "integer"
  rownames(mat) <- souche

  if (!is.null(isolates)) {
    mat <- mat[rownames(mat) %in% isolates, , drop = FALSE]
  }

  mat
}

# Staged profiles in the local seqid code space, shaped like the `mlst` rows.
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

  # An allele the database has never seen is a *novel* allele, not a missing one:
  # it must differ from every local allele at that locus, so it gets its own code
  # above the local id space. Two imported isolates sharing it share the code.
  novel <- is.na(imp$seqid)
  if (any(novel)) {
    base <- max(c(local_seqids, map$seqid), na.rm = TRUE)
    imp$seqid[novel] <- base +
      as.integer(factor(paste(imp$gene, imp$hash)[novel]))
  }

  data.frame(
    souche = imp$isolate,
    gene = imp$gene,
    seqid = as.integer(imp$seqid),
    stringsAsFactors = FALSE
  )
}

# --- 2. Pairwise Hamming kernels ---------------------------------------------

# Standard: count all differing positions.
#' @export
hamming_dist <- function(x, y) {
  sum(x != y)
}

# Ignore pairwise: only count positions where both values are present.
#' @export
hamming_dist_ignore <- function(x, y) {
  sum((x != y) & !is.na(x) & !is.na(y))
}

# NA as category: NA vs. a value counts as a mismatch; NA vs. NA does not.
#' @export
hamming_dist_category <- function(x, y) {
  sum((x != y | xor(is.na(x), is.na(y))) & !(is.na(x) & is.na(y)))
}

# --- 3. Distance matrix ------------------------------------------------------

# Apply the chosen kernel to every sample pair. The matrix is symmetric with a
# zero diagonal.
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

# --- 4. Tree construction ----------------------------------------------------

# Build a phylo tree from a square distance matrix. `algo` is one of
# "Neighbour-Joining" or "UPGMA". NJ edge lengths are mapped through asinh.
#' @export
build_tree <- function(dist_mat, labels, algo) {
  d <- as.dist(dist_mat)

  tree <- if (identical(algo, "UPGMA")) {
    as.phylo(hclust(d, method = "average"))
  } else {
    nj_tree <- nj(d)
    # NJ can produce negative branch lengths. Apply asinh (in log form) so the
    # full real line maps to non-negative values, preserving zero and relative
    # structure while compressing large magnitudes.
    el <- abs(nj_tree[["edge.length"]])
    nj_tree[["edge.length"]] <- log(el + sqrt(el^2 + 1))
    nj_tree
  }

  tree$tip.label <- labels
  tree
}

# --- 5. Shared distance preparation ------------------------------------------

# Load the profile for the current database and compute its distance matrix
# under the chosen missing-value policy. Returns a list with the (possibly
# omit-filtered) `profile`, the `method` kernel used, and the `dist` matrix, or
# NULL when the database holds no isolates.
#   "omit": drop any locus with a missing value across the included samples,
#   then compare with the standard kernel. The other policies use NA-aware
#   kernels on the full profile.
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

# --- 6. Orchestration: tree --------------------------------------------------

# Compute a phylo tree for the currently loaded database.
#   na_handling - one of "ignore_na", "omit", "category" (see visualization UI).
#   algo        - "Neighbour-Joining" or "UPGMA".
# Returns a phylo object, or NULL when there are too few samples to build a tree.
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

# --- 7. Orchestration: MST ---------------------------------------------------

# Compute a Minimum Spanning Tree (igraph object) for the current database.
#
# Samples with a pairwise Hamming distance of 0 (identical allelic profiles)
# are first collapsed into a single node via the connected components of the
# zero-distance graph. Each merged node carries the constituent sample names
# (newline-joined) in its `name` attribute and the sample count in `n`. A fresh
# distance matrix is computed on one representative per group, turned into a
# weighted graph (edge weight = allelic distance), and reduced to its MST.
#
# Returns an igraph object, or NULL when there are too few samples.
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

  # Recompute distances on one representative per group, then build the MST.
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

# Per-node label text for a chosen metadata field. Each MST node id is the
# newline-joined sample names it represents; the label joins those samples'
# values of `field` (from make_metadata_table) the same way. With field
# "isolate" this returns the node id unchanged.
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

# Custom canvas renderer: vis.js calls this for nodes with shape "custom" to
# paint each node as a pie chart (slice data is the JSON `metadata` field) with
# the label drawn below it. Engaged only when variable coloring is active.
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

# Map a set of category values to colors. NA is treated as its own category.
mst_palette <- function(values, scale) {
  cats <- unique(values)
  n <- length(cats)
  cols <- if (identical(scale, "Rainbow")) rainbow(n) else viridis(n)
  data.frame(value = cats, color = cols, stringsAsFactors = FALSE)
}

# Per-node pie slice JSON: each node's share of every category of `col_var`
# across its constituent samples, colored via `var_cols`.
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
          # NA-safe membership: an NA category matches NA values, otherwise a
          # present value matches by equality (avoids NA poisoning the sum).
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

# Flood-fill clusters of nodes connected through edges within `threshold`.
# Returns per-node group labels ("Group N", "0" when unclustered) and the
# matching label per edge (only for edges inside a >1-node cluster).
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

# Legend node entries (one per category) for visLegend(addNodes=).
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

# Legend column count scales with the number of categories.
legend_col <- function(n) {
  if (n <= 5) {
    1
  } else if (n <= 10) {
    2
  } else {
    3
  }
}

# Cluster color vector (one entry per cluster).
mst_cluster_palette <- function(n, scale) {
  if (n == 0) {
    return(character(0))
  }
  if (identical(scale, "Rainbow")) rainbow(n) else viridis(n)
}

# Skeleton clustering: overlay a thick translucent colored edge per cluster on
# top of a thin black base layer.
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

# Build an interactive visNetwork widget from an MST igraph object. `opts` is a
# plain list of resolved control values (no reactives), keeping this pure and
# testable. Node size optionally scales with the number of samples a node
# represents; edge length optionally scales with allelic distance.
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

# Serialise a built MST widget to a self-contained HTML file.
#' @export
save_mst_html <- function(widget, file, background) {
  visSave(widget, file = file, background = background)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
