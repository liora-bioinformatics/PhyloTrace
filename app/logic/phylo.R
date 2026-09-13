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
  Matrix[sparseMatrix, tcrossprod],
  ape[as.phylo, nj],
  igraph[
    components,
    graph_from_adjacency_matrix,
    mst,
    set_vertex_attr,
  ],
  rlang[`%||%`],
  stats[as.dist, hclust],
)

box::use(
  app / logic / db_connect[connect],
  app / logic / db_staging[imported_profile_long, local_allele_map],
  app / logic / logging[log_event],
)

# --- 1. Allele Profile Extraction --------------------------------------------

#' Load Isolate Allele Profiles
#'
#' Queries the SQLite database for allele profiles (excluding synthetic reference entries)
#' and reshapes them into a matrix (isolates x loci). Staged imported peer profiles can
#' optionally be folded in and mapped into the local integer seqid code space.
#'
#' `isolates = NULL` means "every isolate in the database", resolved from
#' `mlst` at call time. That is a fine default for a library function, but it
#' makes the result only as current as the file: an isolate another process
#' wrote a moment ago is in it. A caller that has already shown the user a set
#' of isolates must therefore pass that set explicitly rather than rely on
#' NULL, or the two will disagree - which is exactly what happened when a tree
#' generated during a typing run came back with tips for half-written isolates
#' that the sidebar, the selection modal and the tip labels knew nothing
#' about. See `engine_isolates` in app/view/visualization_plot.R.
#'
#' @param db_path File path to SQLite database.
#' @param isolates Vector of isolate IDs to filter, or NULL for every isolate
#'   the database holds at call time (see above).
#' @param imported_sets Optional list of staged imported peer profile datasets.
#' @return Integer matrix of allele profiles with isolate names as row names.
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

  # Filled by index rather than pivoted: rows and columns keep their order of
  # first appearance, as pivot_wider() gave them, at a fraction of the cost on a
  # whole-genome scheme.
  isolate_ids <- unique(long$isolate)
  genes <- unique(long$gene)
  row_i <- match(long$isolate, isolate_ids)
  col_i <- match(long$gene, genes)
  keep <- !.ambiguous_cells(long, row_i, col_i, length(genes))

  mat <- matrix(
    NA_integer_,
    length(isolate_ids),
    length(genes),
    dimnames = list(isolate_ids, genes)
  )
  mat[cbind(row_i[keep], col_i[keep])] <- as.integer(long$seqid[keep])

  if (!is.null(isolates)) {
    mat <- mat[rownames(mat) %in% isolates, , drop = FALSE]
  }

  mat
}

# Helper: Flag the rows of a locus that carries more than one allele for the same
# isolate, so that cell is scored missing. pyMLST's `mlst` table has no unique key
# on (souche, gene), so a genome with a duplicated or paralogous locus
# legitimately contributes two allele rows for it. Two alleles at one locus is an
# ambiguous call, not a profile value, and NA is what every na_handling policy
# downstream is built to absorb. Letting either row win would silently pick one.
.ambiguous_cells <- function(long, row_i, col_i, n_genes) {
  cell <- (row_i - 1) * n_genes + col_i
  dup <- duplicated(cell)
  if (!any(dup)) {
    return(logical(length(cell)))
  }

  ambiguous <- cell %in% cell[dup]
  first <- which(ambiguous & !dup)
  sample_i <- first[seq_len(min(3L, length(first)))]
  log_event(
    "PHYLO",
    "Ambiguous loci scored missing",
    sprintf(
      "%d locus/isolate pair(s) across %d isolate(s) | e.g. %s",
      length(first),
      length(unique(long$isolate[first])),
      paste(
        sprintf("%s@%s", long$gene[sample_i], long$isolate[sample_i]),
        collapse = ", "
      )
    )
  )

  ambiguous
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

# These per-pair kernels are the reference definitions of each missing-value
# policy. `compute_dist_matrix()` computes the same counts for all pairs at once.

#' Standard Hamming Distance Kernel
#' @param x Vector of allele values.
#' @param y Vector of allele values.
#' @return Integer count of differing positions.
#' @export
hamming_dist <- function(x, y) {
  sum(x != y)
}

#' Missing-Value Pairwise Ignore Hamming Distance Kernel
#' @param x Vector of allele values.
#' @param y Vector of allele values.
#' @return Integer count of mismatches excluding positions where either value is NA.
#' @export
hamming_dist_ignore <- function(x, y) {
  sum((x != y) & !is.na(x) & !is.na(y))
}

#' NA-as-Category Hamming Distance Kernel
#' @param x Vector of allele values.
#' @param y Vector of allele values.
#' @return Integer count where NA vs value is a mismatch, but NA vs NA is a match.
#' @export
hamming_dist_category <- function(x, y) {
  sum((x != y | xor(is.na(x), is.na(y))) & !(is.na(x) & is.na(y)))
}

# --- 3. Distance Matrix Construction ----------------------------------------

#' Compute Distance Matrix Across Profiles
#'
#' Counts, for every pair of isolates, the loci at which their allele calls
#' differ under a missing-value policy - exactly what the matching reference
#' kernel above gives per pair, computed for all pairs at once with sparse
#' linear algebra instead of one R call per pair. The pair count still grows with
#' the square of the isolate count; the cost per pair no longer involves R.
#'
#' Each called cell becomes a 1 in an isolates x (locus, allele) indicator
#' matrix, whose cross product counts the loci at which two isolates carry the
#' same allele. From that:
#' - `ignore_na`: loci called in both, minus shared alleles
#'   (`hamming_dist_ignore`);
#' - `category`: all loci, minus shared alleles, minus loci missing in both
#'   (`hamming_dist_category`);
#' - `omit`: `ignore_na` over the loci called in every row of `profile`
#'   (`hamming_dist` on those loci), so the result depends on which isolates
#'   the profile holds.
#' Any other value is treated as `ignore_na`.
#'
#' @param profile Integer matrix of allele profiles, isolates in rows.
#' @param na_handling Missing-value policy ("ignore_na", "category" or "omit").
#' @return Integer matrix of pairwise distances, labelled by isolate on both axes.
#' @export
compute_dist_matrix <- function(profile, na_handling = "ignore_na") {
  mat <- as.matrix(profile)
  n <- nrow(mat)
  labels <- rownames(mat)
  if (identical(na_handling, "omit")) {
    mat <- mat[, colSums(is.na(mat)) == 0, drop = FALSE]
  }

  called <- !is.na(mat)
  shared <- matrix(0, n, n)
  if (any(called)) {
    idx <- which(called)
    vals <- as.double(mat[idx])
    low <- min(vals)
    # Offset by locus so an allele code reused by two loci is never mistaken
    # for a shared allele.
    allele <- ((idx - 1) %/% n) * (max(vals) - low + 1) + (vals - low)
    codes <- unique(allele)
    indicator <- sparseMatrix(
      i = (idx - 1) %% n + 1,
      j = match(allele, codes),
      x = 1,
      dims = c(n, length(codes))
    )
    shared <- as.matrix(tcrossprod(indicator))
  }

  dist_mat <- if (identical(na_handling, "category")) {
    ncol(mat) - shared - tcrossprod((!called) * 1)
  } else {
    tcrossprod(called * 1) - shared
  }
  storage.mode(dist_mat) <- "integer"
  dimnames(dist_mat) <- list(labels, labels)
  dist_mat
}

# --- 4. Phylogenetic Tree Construction --------------------------------------

#' Construct Phylogenetic Tree Object
#'
#' Builds an ape `phylo` object using Neighbor-Joining (NJ) or UPGMA.
#' Branch lengths are left in the units of `dist_mat` (raw allelic distance),
#' the same units the MST draws its edge weights in — a plot that shows one
#' isolate as "3272 alleles apart" and another view of the same pair a
#' different number for the same data is a bug, not a rendering choice.
#'
#' NJ's least-squares estimate can come out slightly negative for a very short
#' branch, which has no biological reading (a negative number of allele
#' differences), so those are clamped to zero — the same fix other NJ viewers
#' apply — rather than compressed. A branch that is genuinely far longer than
#' its neighbours (an outgroup, a divergent reference) stays exactly as long
#' as the data says. Whether it is drawn broken, and which branches are legible
#' enough to carry a printed number, are rendering decisions made downstream in
#' `tree_plot.R` (`tree_shorten_branches`, `tree_branch_keep`) and marked on the
#' figure, rather than a silent rescaling of what a branch means.
#'
#' @param dist_mat Distance matrix.
#' @param labels Tip label vector matching distance matrix ordering.
#' @param algo Clustering algorithm ("Neighbour-Joining" or "UPGMA").
#' @return An ape `phylo` object.
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

# --- 5. Distance Computation ------------------------------------------------

#' Compute the Distance Matrix for a Set of Isolates
#'
#' Loads the allele profiles and applies [compute_dist_matrix()]. The Tree and
#' MST engines reach this through the session cache in app/logic/dist_cache.R.
#'
#' @param db_path Database path.
#' @param na_handling Strategy for missing values ("ignore_na", "omit", or
#'   "category"); NULL means "ignore_na".
#' @param isolates Optional vector of isolate IDs (see [load_allele_profile()]).
#' @param imported_sets Optional staged imported peer profile sets.
#' @return Labelled integer distance matrix, or NULL when no profile matches.
#' @export
compute_distance <- function(
  db_path,
  na_handling,
  isolates = NULL,
  imported_sets = NULL
) {
  profile <- load_allele_profile(db_path, isolates, imported_sets)
  if (nrow(profile) < 1) {
    return(NULL)
  }
  compute_dist_matrix(profile, na_handling %||% "ignore_na")
}

# --- 6. Tree Orchestration --------------------------------------------------

#' Build a Phylogenetic Tree From a Distance Matrix
#'
#' @param dist_mat Labelled distance matrix, as from [compute_distance()].
#' @param algo Clustering algorithm ("Neighbour-Joining" or "UPGMA"); NULL
#'   means Neighbour-Joining.
#' @return A `phylo` object, or NULL for fewer than 3 isolates.
#' @export
tree_from_distance <- function(dist_mat, algo = NULL) {
  if (is.null(dist_mat) || nrow(dist_mat) < 3) {
    return(NULL)
  }
  build_tree(dist_mat, rownames(dist_mat), algo %||% "Neighbour-Joining")
}

#' Compute Phylogenetic Tree
#'
#' High-level wrapper to calculate distances and return a phylogenetic tree.
#'
#' @param db_path Database path.
#' @param na_handling Strategy for missing values ("ignore_na", "omit", or "category").
#' @param algo Clustering algorithm ("Neighbour-Joining" or "UPGMA").
#' @param isolates Optional list of isolate IDs.
#' @param imported_sets Optional list of staged imported peer profiles.
#' @return A `phylo` object, or NULL if insufficient isolates are provided.
#' @export
compute_phylo_tree <- function(
  db_path,
  na_handling,
  algo,
  isolates = NULL,
  imported_sets = NULL
) {
  tree_from_distance(
    compute_distance(db_path, na_handling, isolates, imported_sets),
    algo
  )
}

# --- 7. Minimum Spanning Tree (MST) Orchestration ----------------------------

#' Compute Minimum Spanning Tree Graph
#'
#' High-level wrapper to calculate distances and return the MST graph.
#'
#' @param db_path Database path.
#' @param na_handling Strategy for handling missing values.
#' @param isolates Optional list of isolate IDs.
#' @param imported_sets Optional list of staged imported peer profiles.
#' @return An `igraph` object, or NULL if insufficient isolates exist.
#' @export
compute_mst <- function(
  db_path,
  na_handling,
  isolates = NULL,
  imported_sets = NULL
) {
  mst_from_distance(
    compute_distance(db_path, na_handling, isolates, imported_sets)
  )
}

#' Build a Minimum Spanning Tree Graph From a Distance Matrix
#'
#' Zero-distance isolates are merged into single representative nodes.
#'
#' @param dist_mat Labelled distance matrix, as from [compute_distance()].
#' @return An `igraph` object, or NULL for fewer than 2 isolates.
#' @export
mst_from_distance <- function(dist_mat) {
  if (is.null(dist_mat) || nrow(dist_mat) < 2) {
    return(NULL)
  }

  labels <- rownames(dist_mat)

  # Collapse zero-distance samples into groups (transitive: chained identical
  # profiles merge into one node).
  zero_adj <- dist_mat == 0
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

  # The representatives' distances to each other are already in the matrix;
  # slicing it gives exactly what recomputing them from their profiles would.
  rep_dist <- dist_mat[rep_idx, rep_idx, drop = FALSE]

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
