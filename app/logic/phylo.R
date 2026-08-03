# app/logic/phylo.R
#
# Phylogenetic tree and Minimum Spanning Tree (MST) computation from cgMLST allele profiles.
#
# Pipeline:
# 1. Read per-isolate allele profiles from the `mlst` table (and optional staged imports).
# 2. Compute pairwise Hamming distance matrices using specified NA-handling policies.
# 3. Construct Neighbor-Joining (NJ) / UPGMA trees, or the MST graph that
#    app/logic/mst_plot.R draws.

box::use(
  DBI[
    dbDisconnect,
    dbGetQuery,
  ],
  ape[as.phylo, nj],
  dplyr[select],
  igraph[
    components,
    graph_from_adjacency_matrix,
    mst,
    set_vertex_attr,
  ],
  stats[as.dist, hclust],
  tidyr[pivot_wider],
)

box::use(
  app / logic / db_connect[connect],
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
  con <- connect(db_path, synchronous = NULL)
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
#' Branch lengths are left in the units of `dist_mat` (raw allelic distance),
#' the same units the MST draws its edge weights in — a plot that shows one
#' isolate as "3272 alleles apart" and another view of the same pair a
#' different number for the same data is a bug, not a rendering choice[cite: 12].
#'
#' NJ's least-squares estimate can come out slightly negative for a very short
#' branch, which has no biological reading (a negative number of allele
#' differences), so those are clamped to zero — the same fix other NJ viewers
#' apply — rather than compressed. A branch that is genuinely far longer than
#' its neighbours (an outgroup, a divergent reference) stays exactly as long
#' as the data says: which of those branches is legible enough to carry a
#' printed number is a rendering decision, handled downstream in
#' `tree_plot.R` (`tree_branch_keep`) from the drawn geometry rather than by
#' silently rescaling what a branch means[cite: 12].
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
    nj_tree[["edge.length"]] <- pmax(nj_tree[["edge.length"]], 0)
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

# Everything about *drawing* an MST — layout, colour, clustering, legend — is
# app/logic/mst_plot.R. This module stops at the graph: it is the expensive half
# (a Generate away) and the half that has nothing to do with how the result
# looks, and keeping the two apart is what lets a control change redraw without
# recomputing a distance matrix.

# Null-coalescing infix operator
`%||%` <- function(a, b) if (is.null(a)) b else a
