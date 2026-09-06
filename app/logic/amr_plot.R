# app/logic/amr_plot.R
#
# Data transformation and plot construction for AMR screening outputs.
# Generates ggplot2-compatible objects for heatmap and prevalence visualizations.

box::use(
  circlize[colorRamp2],
  ComplexHeatmap,
  DBI[dbDisconnect, dbGetQuery, dbListTables],
  ggplot2[
    .data,
    aes,
    coord_flip,
    element_blank,
    element_rect,
    element_text,
    expansion,
    geom_col,
    ggplot,
    ggsave,
    labs,
    scale_fill_manual,
    scale_x_discrete,
    scale_y_continuous,
    theme,
    theme_minimal,
  ],
  ggplotify[as.ggplot],
  grDevices[colorRampPalette],
  grid[
    gpar,
    grid.grabExpr,
    grid.text,
    popViewport,
    pushViewport,
    unit,
    viewport
  ],
  RColorBrewer[brewer.pal.info],
  rlang[`%||%`],
  stats[dist, hclust, setNames],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / epi_plot[epi_fit_scale, epi_palette, epi_scale_choices],
  app / logic / field_labels[amr_class_label],
)

# --- Constants & Vocabulary --------------------------------------------------

#' @export
AMR_ELEMENT_TYPES <- c(
  Resistance = "AMR",
  Virulence = "VIRULENCE",
  Stress = "STRESS"
)

#' @export
AMR_SECTIONS <- c(
  Matches = "matches",
  Partials = "partials",
  Virulence = "virulence"
)

#' Which curation the gene heatmap files a gene under.
#'
#' Two vocabularies describe the same screen and they do not agree. AMRFinderPlus
#' stamps a broad functional class on every hit, so every beta-lactamase and
#' every PBP mutation alike comes back "Beta-lactam"; abritamr re-files those
#' same hits under the finer, clinically curated headings — AmpC, Carbapenemase,
#' one per aminoglycoside — that the rest of the app already shows, because the
#' database browser, the drug-class view and the tree's heatmap picker all read
#' `amr_summary` rather than `amr_results`.
#'
#' The rollup leads so those agree: a gene the browser calls a carbapenemase
#' reading "Beta-lactam" in the heatmap beside it is the mismatch this exists to
#' settle. AMRFinder's stays on offer because its blocks are wider, and a panel
#' split into thirteen single-column classes is harder to read than one split
#' into nine — it is also the only vocabulary covering a hit abritamr never
#' summarised.
#' @export
AMR_CLASS_VOCABULARIES <- c(
  `abritamr (curated)` = "rollup",
  AMRFinderPlus = "amrfinder"
)

#' @export
AMR_CLASS_VOCABULARY_DEFAULT <- "amrfinder"

#' The gene heatmap's cell states, coarse-to-fine. Not a plain presence/absence
#' pair: AMRFinderPlus already curates a per-gene identity/coverage cutoff for
#' every call it makes and records which side of it a hit landed on (the
#' `method` column in `amr_results`), so "present" is treated as a confidence
#' tier rather than collapsed to a single state. See `.method_confidence()`.
#' @export
AMR_CONFIDENCE_STATES <- c("Absent", "Putative", "Partial", "Strong", "Perfect")

#' @export
AMR_UNCLASSIFIED <- "Unclassified"

#' Label for isolates a mapped variable has no value for.
#' @export
AMR_MISSING_LABEL <- "NA"

#' Order a mapped variable's categories for its strip and legend.
#'
#' Alphabetical, except that the "no value recorded" category is always last:
#' it sorts wherever its label happens to fall, which put the one category
#' carrying no information ahead of the values that do.
#'
#' @param x Character vector of category labels.
#' @return Sorted unique labels, `AMR_MISSING_LABEL` last when present.
#' @export
amr_strip_levels <- function(x) {
  lv <- sort(unique(as.character(x)))
  c(setdiff(lv, AMR_MISSING_LABEL), intersect(lv, AMR_MISSING_LABEL))
}

#' @export
AMR_CLUSTER_METHODS <- c(
  `Ward D2` = "ward.D2",
  Average = "average",
  Complete = "complete",
  Single = "single",
  Centroid = "centroid"
)

#' @export
AMR_CLUSTER_DISTANCES <- c(
  `Jaccard (binary)` = "binary",
  Euclidean = "euclidean",
  Manhattan = "manhattan"
)

#' Distance the presence/absence matrix is clustered with by default.
#'
#' Jaccard, on both axes. A cell here is "was this gene called in this isolate",
#' so a *joint absence* says nothing — two isolates that both lack a rare
#' carbapenemase are not thereby alike — and Jaccard is the one measure in the
#' list that ignores them. It is also the only one invariant to how many genes
#' the panel happens to carry: Euclidean and Manhattan on 0/1 both count
#' matching zeros into the denominator, so widening the screen from resistance
#' to resistance-plus-virulence quietly pulls every isolate closer together.
#'
#' Level on the isolate axis, decisive on the gene axis — which is the axis the
#' argument above predicts, since two genes are jointly absent from most
#' isolates far more often than two isolates are jointly clean.
#' @export
AMR_CLUSTER_DISTANCE_DEFAULT <- "binary"

#' Linkage the presence/absence matrix is clustered with by default.
#'
#' Ward's, on both axes. A heatmap dendrogram is not a phylogeny: its job is to
#' order the rows so that like profiles form a visible block, and Ward is the
#' linkage that optimises for exactly that. Average linkage (UPGMA) reproduces
#' the distance matrix more faithfully — it maximises cophenetic correlation by
#' construction, 0.955 against Ward's 0.735 on the P. aeruginosa set — but on
#' this kind of data it chains: cutting its tree into four groups on that set
#' left one cluster holding 99.6% of the isolates and three singletons beside
#' it, which draws as a ladder rather than as blocks.
#'
#' By best mean silhouette width (Jaccard space, k from 2 to 12):
#'
#'   axis        ward.D2   average   complete   single   centroid
#'   isolates      0.978     0.927      0.827    0.827      0.966
#'   genes         0.660     0.610      0.593    0.618      0.516
#'
#' Ward leads on both axes. Centroid is offered but never chosen automatically:
#' it produced non-monotonic merge heights on both databases, which ComplexHeatmap
#' draws as branches that double back on themselves.
#' @export
AMR_CLUSTER_METHOD_DEFAULT <- "ward.D2"

#' @export
amr_scale_choices <- function(n) epi_scale_choices(n)

#' @export
amr_fit_scale <- function(scale, n) epi_fit_scale(scale, n)

#' @export
amr_palette <- function(cats, scale) epi_palette(cats, scale)

#' Resolves the Prevalence chart's bar scale to just two states.
#'
#' Every other scale picker in this module (see `amr_fit_scale()`) falls
#' through a ladder of qualitative palettes before it reaches a gradient one.
#' This control does not: the reader's own pick stays in force while it can
#' still name every group distinctly, the Dark2 default takes over the moment
#' it cannot but Dark2 itself still could, and the continuous viridis scale is
#' the only place left to land once neither can - never some other
#' qualitative palette in between.
#'
#' @param scale Requested palette name.
#' @param n_groups How many distinct groups the bars must be colored by.
#' @return `scale` unchanged when it still fits, else "Dark2", else "viridis".
#' @export
amr_bar_scale_fit <- function(scale, n_groups) {
  n_groups <- max(as.integer(n_groups %||% 1L), 1L)
  fits <- function(s) {
    if (is.null(s) || !nzchar(s %||% "")) {
      return(FALSE)
    }
    if (s %in% rownames(brewer.pal.info)) {
      brewer.pal.info[s, "maxcolors"] >= n_groups
    } else {
      TRUE
    }
  }
  if (fits(scale)) {
    scale
  } else if (fits("Dark2")) {
    "Dark2"
  } else {
    "viridis"
  }
}

# --- Database Interface ------------------------------------------------------

.EMPTY_HITS <- data.frame(
  isolate = character(0),
  gene_symbol = character(0),
  element_type = character(0),
  element_subtype = character(0),
  class = character(0),
  subclass = character(0),
  method = character(0),
  pct_identity = numeric(0),
  pct_coverage = numeric(0),
  stringsAsFactors = FALSE
)

.EMPTY_SECTIONS <- data.frame(
  isolate = character(0),
  section = character(0),
  drug_class = character(0),
  genes = character(0),
  stringsAsFactors = FALSE
)

.usable_path <- function(db_path) {
  !is.null(db_path) &&
    length(db_path) == 1 &&
    !is.na(db_path) &&
    nzchar(db_path) &&
    file.exists(db_path)
}

#' Checks if the target SQLite database contains AMR screening data.
#' @export
has_amr_data <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(FALSE)
  }
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)
  if (!"amr_results" %in% tables) {
    return(FALSE)
  }
  nrow(dbGetQuery(con, "SELECT 1 FROM amr_results LIMIT 1")) > 0
}

#' Reads raw AMR/virulence/stress gene hits from the database.
#' Returns an empty schema frame if data is missing.
#' @export
load_amr_hits <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(.EMPTY_HITS)
  }
  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_results" %in% dbListTables(con)) {
    return(.EMPTY_HITS)
  }
  hits <- dbGetQuery(
    con,
    "SELECT isolate, gene_symbol, element_type, element_subtype, class, subclass,
            method, pct_identity, pct_coverage
       FROM amr_results
      ORDER BY id"
  )
  if (!nrow(hits)) {
    return(.EMPTY_HITS)
  }
  hits$gene_symbol <- as.character(hits$gene_symbol)
  hits$element_type <- toupper(as.character(hits$element_type %||% NA))
  hits[!is.na(hits$gene_symbol) & nzchar(hits$gene_symbol), , drop = FALSE]
}

#' Reads curated drug-class and virulence summary sections (tidy format).
#' Returns an empty schema frame if data is missing.
#' @export
load_amr_sections <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(.EMPTY_SECTIONS)
  }
  con <- connect(db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_summary" %in% dbListTables(con)) {
    return(.EMPTY_SECTIONS)
  }
  sections <- dbGetQuery(
    con,
    "SELECT isolate, section, drug_class, genes FROM amr_summary ORDER BY id"
  )
  if (!nrow(sections)) {
    return(.EMPTY_SECTIONS)
  }
  sections$drug_class <- as.character(sections$drug_class)
  sections[
    !is.na(sections$drug_class) & nzchar(sections$drug_class),
    ,
    drop = FALSE
  ]
}

# --- Data Transformation & Filtering ----------------------------------------

#' Filters gene hits based on element types and threshold limits (identity/coverage).
#' Retains NA metrics (e.g., point mutations).
#' @export
filter_amr_hits <- function(
  hits,
  element_types = NULL,
  min_identity = 0,
  min_coverage = 0
) {
  if (is.null(hits) || !nrow(hits)) {
    return(.EMPTY_HITS)
  }
  keep <- rep(TRUE, nrow(hits))
  if (!is.null(element_types) && length(element_types)) {
    keep <- keep & hits$element_type %in% toupper(element_types)
  }
  if (isTRUE(min_identity > 0)) {
    keep <- keep &
      (is.na(hits$pct_identity) | hits$pct_identity >= min_identity)
  }
  if (isTRUE(min_coverage > 0)) {
    keep <- keep &
      (is.na(hits$pct_coverage) | hits$pct_coverage >= min_coverage)
  }
  hits[keep, , drop = FALSE]
}

#' Fits a threshold slider's range to the values a screen actually reported,
#' rounded out to whole percent so genuine data is never excluded by rounding.
#' `value` is the fitted floor, i.e. "no filter" for the new range - the same
#' role 0 played for a flat 0-100 span. Falls back to that flat span when there
#' is nothing numeric to fit (no screen loaded, or every hit's metric is NA,
#' e.g. point mutations only).
#' @export
amr_threshold_bounds <- function(values) {
  rng <- suppressWarnings(range(values, na.rm = TRUE))
  if (!all(is.finite(rng))) {
    rng <- c(0, 100)
  }
  list(min = floor(rng[1]), max = ceiling(rng[2]), value = floor(rng[1]))
}

#' Fits the Prevalence "Show top" slider's range to how many items there
#' actually are to rank - a screen of eight genes offers nothing past eight,
#' and a database of several hundred should not stay capped at some flat 100
#' the way the old slider was. `value` keeps the caller's own current setting
#' where it still fits, so switching "Count by" or the drug-class vocabulary
#' never clobbers a deliberate choice - it only clamps one the new range can
#' no longer hold.
#' @param n_items Distinct genes or drug classes the current level counts.
#' @param default Value to open on when `current` is NULL.
#' @param current The slider's own value before this fit, or NULL to use
#'   `default`.
#' @return List with `min`, `max`, `value` and `step`.
#' @export
amr_top_n_bounds <- function(n_items, default = 30L, current = NULL) {
  n_items <- max(as.integer(n_items %||% 0L), 1L)
  min_v <- min(5L, n_items)
  step <- if (n_items > 20L) 5L else 1L
  want <- as.integer(current %||% default %||% 30L)
  list(
    min = min_v,
    max = n_items,
    value = min(max(want, min_v), n_items),
    step = step
  )
}

# Gene symbol -> the class abritamr filed it under, read off the rollup rows.
# abritamr carries its quality flags on the gene name ("blaOXA-2*") where the
# hit table does not, so the key is the stripped symbol. A gene filed under more
# than one class across the isolates takes the one it was filed under most
# often, alphabetically on a tie, so two runs over the same data group it the
# same way.
.rollup_class_map <- function(sections) {
  if (is.null(sections) || !nrow(sections)) {
    return(character(0))
  }
  gene <- sub("[*^]+$", "", as.character(sections$genes))
  class <- trimws(as.character(sections$drug_class))
  keep <- !is.na(gene) & nzchar(gene) & !is.na(class) & nzchar(class)
  if (!any(keep)) {
    return(character(0))
  }
  tab <- table(gene[keep], class[keep])
  setNames(
    colnames(tab)[apply(tab, 1, which.max)],
    rownames(tab)
  )
}

.gene_group <- function(element_type, class) {
  cls <- amr_class_label(class %||% NA)
  if (!is.na(cls) && nzchar(cls)) {
    return(cls)
  }
  et <- as.character(element_type %||% NA)
  if (!is.na(et) && nzchar(et)) {
    label <- names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)]
    return(label %||% et)
  }
  AMR_UNCLASSIFIED
}

#' Generates a gene metadata mapping table containing element types and functional groups.
#'
#' @param hits Gene hits from `load_amr_hits()`.
#' @param genes Gene symbols to describe, in the order they are wanted.
#' @param sections abritamr rollup rows, needed only for the "rollup"
#'   vocabulary; without them every gene falls back to AMRFinder's class.
#' @param vocabulary One of `AMR_CLASS_VOCABULARIES`.
#' @return Data frame of `gene`, `element_type` and `group`.
#' @export
amr_gene_meta <- function(
  hits,
  genes,
  sections = NULL,
  vocabulary = AMR_CLASS_VOCABULARY_DEFAULT
) {
  genes <- as.character(genes)
  if (!length(genes)) {
    return(data.frame(
      gene = character(0),
      element_type = character(0),
      group = character(0),
      stringsAsFactors = FALSE
    ))
  }
  idx <- match(genes, hits$gene_symbol)
  element_type <- hits$element_type[idx]
  class <- hits$class[idx]
  # The curated class wherever the rollup covers the gene. AMRFinder's own is
  # the fallback rather than an error: a partial hit to a gene family can reach
  # `amr_results` without abritamr ever summarising it.
  if (identical(vocabulary, "rollup")) {
    curated <- unname(.rollup_class_map(sections)[genes])
    class <- ifelse(is.na(curated), class, curated)
  }
  data.frame(
    gene = genes,
    element_type = ifelse(is.na(element_type), AMR_UNCLASSIFIED, element_type),
    group = vapply(
      seq_along(genes),
      function(i) .gene_group(element_type[i], class[i]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' Formats gene list into grouped select input choices structured by element type and drug class.
#'
#' Takes the vocabulary the heatmap is drawn in so the headings a reader
#' searches under are the ones the plot files the gene under.
#'
#' @inheritParams amr_gene_meta
#' @return Named list of gene vectors, one per heading.
#' @export
amr_gene_choices <- function(
  hits,
  sections = NULL,
  vocabulary = AMR_CLASS_VOCABULARY_DEFAULT
) {
  if (is.null(hits) || !nrow(hits)) {
    return(list())
  }
  genes <- sort(unique(hits$gene_symbol))
  meta <- amr_gene_meta(hits, genes, sections, vocabulary)

  type_label <- vapply(
    meta$element_type,
    function(et) names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||% et,
    character(1),
    USE.NAMES = FALSE
  )
  label <- ifelse(
    meta$group == type_label,
    type_label,
    paste(type_label, meta$group, sep = " - ")
  )

  split(meta$gene, factor(label, levels = sort(unique(label))))
}

# --- Matrix Generators --------------------------------------------------------

# AMRFinderPlus's own documented method hierarchy, ranked coarse-to-fine
# (https://github.com/ncbi/amr/wiki/Methods) and aligned to
# AMR_CONFIDENCE_STATES: ALLELE/EXACT are exact, full-length matches; BLAST
# cleared the gene's curated identity *and* coverage cutoff; PARTIAL and
# PARTIAL_CONTIG_END cleared identity but fell short on coverage; HMM is a
# motif match with no alignment behind it at all. Curated per gene by
# AMRFinderPlus itself, not a single global identity/coverage percentage of
# ours.
.METHOD_CONFIDENCE <- c(
  ALLELE = 4L,
  ALLELEX = 4L,
  EXACT = 4L,
  EXACTX = 4L,
  BLAST = 3L,
  BLASTX = 3L,
  INTERNAL_STOP = 2L,
  INTERNAL_STOPX = 2L,
  PARTIAL = 2L,
  PARTIALX = 2L,
  PARTIAL_CONTIG_END = 2L,
  PARTIAL_CONTIG_ENDX = 2L,
  HMM = 1L,
  HMM_PARTIAL = 1L
)

#' Ranks a hit's `method` against `AMR_CONFIDENCE_STATES` (1-4; 0 is reserved
#' for "no hit", which this never returns). A method this table does not
#' recognise, or none at all, is treated as a confident BLAST-grade call
#' rather than dropped to the floor - the safer assumption for a hit this
#' module did not curate itself.
#' @export
amr_method_confidence <- function(method) {
  rank <- unname(.METHOD_CONFIDENCE[toupper(trimws(as.character(method)))])
  ifelse(is.na(rank), 3L, rank)
}

#' Constructs a binary (0/1) presence/absence matrix (isolates x genes).
#' Attaches gene metadata attributes to support heatmap annotations, plus a
#' same-shape "confidence" attribute (0 = Absent, else `amr_method_confidence`
#' of the strongest hit in that cell) for coloring by call confidence rather
#' than plain presence — see `amr_confidence_palette()`.
#' @export
amr_presence_matrix <- function(
  hits,
  isolates,
  genes = NULL,
  drop_empty = TRUE,
  sections = NULL,
  vocabulary = AMR_CLASS_VOCABULARY_DEFAULT
) {
  isolates <- unique(as.character(isolates))
  isolates <- isolates[!is.na(isolates) & nzchar(isolates)]

  present <- if (is.null(hits) || !nrow(hits)) {
    .EMPTY_HITS
  } else {
    hits[hits$isolate %in% isolates, , drop = FALSE]
  }

  genes <- if (is.null(genes)) {
    sort(unique(present$gene_symbol))
  } else {
    unique(as.character(genes))
  }
  genes <- genes[!is.na(genes) & nzchar(genes)]

  mat <- matrix(
    0L,
    nrow = length(isolates),
    ncol = length(genes),
    dimnames = list(isolates, genes)
  )
  confidence <- matrix(
    0L,
    nrow = length(isolates),
    ncol = length(genes),
    dimnames = list(isolates, genes)
  )
  if (nrow(present) && length(genes) && length(isolates)) {
    present <- present[present$gene_symbol %in% genes, , drop = FALSE]
    if (nrow(present)) {
      idx <- cbind(
        match(present$isolate, isolates),
        match(present$gene_symbol, genes)
      )
      mat[idx] <- 1L
      # Two hits can land in the same cell (e.g. two copies of a gene) with
      # different confidence; the strongest one wins.
      rank <- amr_method_confidence(present$method)
      for (i in seq_len(nrow(idx))) {
        cell <- confidence[idx[i, 1L], idx[i, 2L]]
        confidence[idx[i, 1L], idx[i, 2L]] <- max(cell, rank[i])
      }
    }
  }

  if (isTRUE(drop_empty) && ncol(mat)) {
    keep <- colSums(mat) > 0L
    mat <- mat[, keep, drop = FALSE]
    confidence <- confidence[, keep, drop = FALSE]
    genes <- genes[keep]
  }

  attr(mat, "genes") <- amr_gene_meta(
    hits %||% .EMPTY_HITS,
    genes,
    sections,
    vocabulary
  )
  attr(mat, "confidence") <- confidence
  mat
}

#' The gene heatmap's confidence tiers as a wide, isolate-keyed frame.
#'
#' One factor column per gene, levelled on `AMR_CONFIDENCE_STATES`, holding the
#' tier of the strongest hit that isolate has for the gene (`Absent` where it
#' has none). This is the shape the tree engine's heatmap reads; it is built
#' from the same hits and the same `amr_method_confidence()` ranking as this
#' module's own gene heatmap, so a gene reads at the identical tier in both.
#'
#' @param hits Gene hits from `load_amr_hits()`.
#' @param isolates Isolate ids to place, in the row order wanted.
#' @param genes Gene symbols to place, in column order; `NULL` takes every gene
#'   present across `isolates`.
#' @return Data frame: an `isolate` character column then one factor column per
#'   gene. Zero gene columns when nothing matches.
#' @export
amr_confidence_frame <- function(hits, isolates, genes = NULL) {
  isolates <- unique(as.character(isolates))
  isolates <- isolates[!is.na(isolates) & nzchar(isolates)]
  mat <- amr_presence_matrix(hits, isolates, genes, drop_empty = FALSE)
  conf <- attr(mat, "confidence")

  out <- data.frame(
    isolate = isolates,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (g in colnames(mat)) {
    out[[g]] <- factor(
      AMR_CONFIDENCE_STATES[conf[, g] + 1L],
      levels = AMR_CONFIDENCE_STATES
    )
  }
  out
}

# --- Prevalence Calculations --------------------------------------------------

#' Computes gene or drug-class occurrence counts across selected isolates.
#'
#' Returns top `top_n` items ordered by prevalence.
#'
#' @param hits Gene hits, for `level = "gene"` and for `level = "class"` under
#'   the "amrfinder" vocabulary.
#' @param sections abritamr rollup rows, for `level = "class"` under the
#'   "rollup" vocabulary.
#' @param isolates Character vector of isolates the count covers.
#' @param level Either "gene" or "class".
#' @param top_n Integer. Items to keep; 0 or less keeps every one.
#' @param keep_sections Call sections to count, or NULL for every one. Applies
#'   at class level under the "rollup" vocabulary only — AMRFinderPlus's own
#'   class field carries no section, and a gene hit carries neither.
#' @param vocabulary One of `AMR_CLASS_VOCABULARIES`. Read at class level
#'   only: "rollup" counts off abritamr's own summary, unchanged; "amrfinder"
#'   counts off `hits` instead, filed under the class the gene heatmap's own
#'   AMRFinderPlus vocabulary would file the same gene under (see
#'   `amr_gene_meta()`), so switching vocabularies here and on the heatmap
#'   files a gene under the same heading in both places.
#' @return A data frame of `item`, `group`, `n` and `frac`.
#' @export
amr_prevalence <- function(
  hits,
  sections,
  isolates,
  level = "gene",
  top_n = 30L,
  keep_sections = NULL,
  vocabulary = AMR_CLASS_VOCABULARY_DEFAULT
) {
  isolates <- unique(as.character(isolates))
  n_iso <- length(isolates)
  empty <- data.frame(
    item = character(0),
    group = character(0),
    n = integer(0),
    frac = numeric(0),
    stringsAsFactors = FALSE
  )

  if (identical(level, "class") && identical(vocabulary, "amrfinder")) {
    rows <- if (is.null(hits) || !nrow(hits)) {
      .EMPTY_HITS
    } else {
      hits[hits$isolate %in% isolates, , drop = FALSE]
    }
    if (!nrow(rows)) {
      return(empty)
    }
    # Filed the same way the gene heatmap's own AMRFinderPlus vocabulary files
    # a column: straight off amr_results' own class field, no rollup override
    # (see amr_gene_meta()) - so a reader who switches vocabularies here and
    # there sees the same class names in both places.
    genes <- unique(rows$gene_symbol)
    meta <- amr_gene_meta(
      rows,
      genes,
      sections = NULL,
      vocabulary = "amrfinder"
    )
    class_of <- setNames(meta$group, meta$gene)
    element_of <- setNames(meta$element_type, meta$gene)
    item <- unname(class_of[rows$gene_symbol])
    et_label <- vapply(
      element_of,
      function(et) {
        names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||%
          AMR_UNCLASSIFIED
      },
      character(1)
    )
    group <- vapply(
      unique(item),
      function(cl) {
        ets <- unique(et_label[names(class_of)[class_of == cl]])
        if ("Virulence" %in% ets) "Virulence" else ets[1]
      },
      character(1),
      USE.NAMES = FALSE
    )
    group <- setNames(group, unique(item))
    isolate <- rows$isolate
  } else if (identical(level, "class")) {
    rows <- if (is.null(sections) || !nrow(sections)) {
      .EMPTY_SECTIONS
    } else {
      sections[sections$isolate %in% isolates, , drop = FALSE]
    }
    # The call-section filter bites here as well as on the drug-class matrix:
    # the bars are counted off `amr_summary`, so switching Partials off has to
    # take those calls out of the count rather than only out of the heatmap.
    if (!is.null(keep_sections) && length(keep_sections)) {
      rows <- rows[rows$section %in% keep_sections, , drop = FALSE]
    }
    if (!nrow(rows)) {
      return(empty)
    }
    item <- rows$drug_class
    group <- vapply(
      unique(item),
      function(cl) {
        secs <- rows$section[item == cl]
        if (any(secs == "virulence")) {
          "Virulence"
        } else if (any(secs == "matches")) {
          "Matches"
        } else {
          "Partials"
        }
      },
      character(1),
      USE.NAMES = FALSE
    )
    group <- setNames(group, unique(item))
    isolate <- rows$isolate
  } else {
    rows <- if (is.null(hits) || !nrow(hits)) {
      .EMPTY_HITS
    } else {
      hits[hits$isolate %in% isolates, , drop = FALSE]
    }
    if (!nrow(rows)) {
      return(empty)
    }
    item <- rows$gene_symbol
    group <- vapply(
      unique(item),
      function(g) {
        et <- rows$element_type[item == g][1]
        names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||%
          AMR_UNCLASSIFIED
      },
      character(1),
      USE.NAMES = FALSE
    )
    group <- setNames(group, unique(item))
    isolate <- rows$isolate
  }

  counts <- vapply(
    names(group),
    function(it) length(unique(isolate[item == it])),
    integer(1),
    USE.NAMES = FALSE
  )

  out <- data.frame(
    item = names(group),
    group = unname(group),
    n = counts,
    frac = if (n_iso > 0) counts / n_iso else rep(NA_real_, length(counts)),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$n, out$item), , drop = FALSE]
  top_n <- as.integer(top_n %||% 30L)
  if (top_n > 0L && nrow(out) > top_n) {
    out <- out[seq_len(top_n), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# --- Plot Helpers & Annotations ----------------------------------------------

#' Calculates scalable font size dynamically based on label count.
#' @export
amr_fit_fontsize <- function(n) {
  breaks <- c(10, 20, 30, 50, 80, 120, 160, 200)
  sizes <- c(14, 12, 11, 10, 9, 8, 7, 6, 5)
  sizes[[sum(n >= breaks) + 1L]]
}

# --- Automatic layout --------------------------------------------------------
#
# The sidebar used to carry six sliders for this — row and column label size, a
# block-title size, a legend size, a cell border width and an aspect ratio — and
# every one of them had a correct answer the module could work out for itself
# from the shape of the matrix. They are gone; this is what replaced them, the
# same arrangement as `tree_plot$tree_auto_layout()`.
#
# The whole fit hangs off one quantity: how tall one isolate's row ends up
# being. Everything else is either fitted to that pitch (the row labels), to the
# cell width the column count leaves (the column labels, the block titles, the
# cell borders), or to the page (the legend).

# Share of the canvas width the matrix body gets. The rest goes to the row
# dendrogram, the annotation strips, the row labels and the legend column, all
# of which sit outside the body and none of which scale with the column count.
AMR_BODY_FRAC <- 0.62

# Inches of body height one isolate should get. Labelled rows need room for the
# name; unlabelled ones only need the band to be a band.
AMR_ROW_IN_LABELLED <- 0.13
AMR_ROW_IN_PLAIN <- 0.055

# Ceiling on that pitch, so a six-isolate screen is not drawn as six fat
# stripes with a legend beside it.
AMR_ROW_IN_MAX <- 0.30

# A tall matrix is the point — "many isolates" should mean a taller picture, not
# a wider one — but a page that has to be scrolled through four screens reads
# worse than a tight one that does not, so the growth stops here.
AMR_ASPECT_MIN <- 0.65
AMR_ASPECT_MAX <- 2

# The raw fit above reads taller than readers actually want by default — the
# row-pitch ceiling alone made most real screens (even a middling few dozen
# isolates) come in noticeably taller than wide. Trimmed before the bounds
# above apply, so a reader who prefers the fuller height can still drag the
# slider back up to AMR_ASPECT_MAX.
AMR_ASPECT_FIT_SCALE <- 0.7

# Type sizes, in points. The floor is where a label stops being readable at all;
# past it the labels are better turned off than shrunk further.
AMR_FONT_MIN <- 4
AMR_FONT_MAX <- 13

# Fraction of the pitch a label's type size may take, leaving the rest as the
# gap between one label and the next.
AMR_LABEL_FILL <- 0.72

# Mean character width as a fraction of the type size, for reserving the room a
# rotated column label or a block title needs.
AMR_CHAR_EM <- 0.6

# Below this cell size a drawn border is a larger share of the cell than the
# fill is, so the matrix reads as a grid with colour in it rather than as a
# heatmap. Borders come off.
AMR_GRID_MIN_IN <- 0.045

# The drug-class colour strip's own drawn height, in inches — fixed rather
# than solved for, since it carries no text of its own to size against. Set
# explicitly on the annotation itself (see .class_annotation) so the room
# amr_auto_layout() reserves for it and the room ComplexHeatmap actually
# draws it into are guaranteed to be the same number.
AMR_CLASS_STRIP_IN <- 0.16

# Clearance between a class title and whatever sits directly under it — the
# block's own dendrogram, or the matrix itself where the dendrogram is off.
# Budgeted into title_in as well as drawn (see .decorate_class_dend), so the
# gap comes out of the title's own room rather than pushing it into the gene
# names above.
AMR_TITLE_GAP_IN <- 0.03

# Clearance under the gene names, between the longest of them and the
# element-type row below. AMR_CHAR_EM is an average over the alphabet, so a
# name made of wide characters overruns the room budgeted for it by a little;
# this is what keeps that overrun off the label underneath.
AMR_COL_LABEL_GAP_IN <- 0.08

# Where each label stops being worth drawing at all. Below its own floor a
# label is a smear rather than a word, and the rule across every one of them is
# the same: fit the type to the room the label actually has, and where even the
# floor will not fit, drop the label rather than set it smaller. Nothing in an
# AMR plot is ever drawn below the floor for its role — what differs between
# the roles is only what stands in for a dropped label (the class strip and its
# key for a block title, the class strip's legend heading for an element-type
# name, nothing at all for a gene name, which the reader can always switch back
# on) and how far each can be trusted to shrink before that point.
AMR_TITLE_MIN_PT <- 7
AMR_ELEMENT_MIN_PT <- 7
AMR_COL_MIN_PT <- 5
AMR_ROW_MIN_PT <- 5
AMR_ANNO_NAME_MIN_PT <- 5

# Ceiling on the element-type row, and the share of the canvas width that
# picks a size below it. One label under a whole panel has room to grow far
# past anything worth reading it at, so what caps it is the page rather than
# the panel: set much larger than the legend titles beside it, an element name
# reads as a second plot title rather than as the axis label it is.
AMR_ELEMENT_MAX_PT <- 12
AMR_ELEMENT_PT_PER_IN <- 0.9

# Width of one mapped-variable strip beside the rows. Budgeted out of the body
# in amr_auto_layout() and handed to the annotation as its simple_anno_size, so
# the room taken and the room reserved are the same number — the same pinning
# AMR_CLASS_STRIP_IN does on the other axis.
AMR_STRIP_IN <- 0.12

# Clearance between the matrix (isolate names included) and the legend column.
# ComplexHeatmap measures the row names on whatever device is current when the
# heatmap is laid out and they are redrawn on the device the figure is finally
# rendered to, whose font metrics are a few percent wider - so a name measured
# to end exactly where the legend begins is drawn a character or two into it.
# ComplexHeatmap's own 2mm default is inside that error; this is not.
AMR_LEGEND_PAD_MM <- 6

# Share of the room between two block titles that the type may take, leaving
# the rest as clearance. A rotated title's footprint across the page is its
# line height rather than its length, so this is measured against the type
# size directly and not through AMR_CHAR_EM.
AMR_TITLE_FILL <- 0.85

# Legend geometry. A key row is its swatch plus the gap under it, both scaled
# with the type size — ComplexHeatmap's own fixed ~4mm swatch would otherwise
# keep a forty-key list exactly as tall however small its labels were made —
# and both stopping at a floor, below which a swatch stops reading as a colour
# at all.
#
# Sizing this against the height available is load-bearing rather than
# cosmetic. packLegend() sets one legend beside another when the column fills
# up, but it will not break up a *single* legend that is taller than the page:
# that one is drawn straight off the bottom edge, keys and all. It also
# measures itself against the whole device while the column is drawn from the
# top of the matrix *body* downwards, so the room it really has is short by
# everything stacked above the body. tree_plot.R solves the same problem the
# same way — see tree_legend_room().
AMR_LEGEND_MIN_PT <- 5.5

.legend_grid_mm <- function(size) max(size * 0.4, 2.5)

.legend_gap_mm <- function(size) max(size * 0.15, 0.6)

.legend_row_in <- function(size) {
  (.legend_grid_mm(size) + .legend_gap_mm(size)) / 25.4
}

# Key columns one legend may wrap into once its type is already at the floor.
# Kept short deliberately: a second column of drug-class names is inches of
# canvas the matrix itself would otherwise have had.
AMR_LEGEND_MAX_COLS <- 3L

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

.pt_in <- function(pt) pt / 72

# The room the narrowest pair of neighbouring block titles has between their
# centres, in inches.
#
# The binding constraint on a rotated title is not the width of its own block
# but the distance to the *next* title, and two blocks' titles are centred
# half of each block apart: a single-column class beside a ten-column one has
# the whole of that neighbour's half to lean into, and only two narrow blocks
# side by side actually collide. Measuring the narrowest block instead (which
# this did at first) shrank every title on the plot to fit the one class that
# happened to carry a single gene.
.title_pitch <- function(cols, cell_w) {
  cols <- as.numeric(cols)
  cols <- cols[is.finite(cols)]
  if (!length(cols)) {
    return(Inf)
  }
  if (length(cols) == 1L) {
    return(cols[[1L]] * cell_w)
  }
  min((cols[-length(cols)] + cols[-1L]) / 2) * cell_w
}

#' Key columns a legend's own keys wrap into so it does not run past the page.
#'
#' The last resort, once the type size is already at its floor: a second column
#' of keys costs the matrix width, so `amr_auto_layout()` shrinks the type
#' first and only overflows into this when even the floor will not fit.
#'
#' @param n_keys Integer. Keys the legend lists.
#' @param max_rows Integer. Key rows the legend column has room for.
#' @return Integer, 1 to `AMR_LEGEND_MAX_COLS`.
#' @export
amr_legend_ncol <- function(n_keys, max_rows) {
  max_rows <- max(as.integer(max_rows %||% 1L), 1L)
  n_keys <- max(as.integer(n_keys %||% 1L), 1L)
  if (n_keys <= max_rows) {
    return(1L)
  }
  as.integer(min(AMR_LEGEND_MAX_COLS, ceiling(n_keys / max_rows)))
}

# The prevalence chart's own shape. One bar per item and no matrix to solve
# against, so the page grows with the bar count rather than with the isolates,
# and stops where a chart nobody can scroll begins.
AMR_PREVALENCE_BASE <- 0.22
AMR_PREVALENCE_PER_BAR <- 0.035
AMR_PREVALENCE_MIN <- 0.35
AMR_PREVALENCE_MAX <- 1.6

# What the axis title, the legend row and the plot's margins take off the top
# and bottom of that page before the bars get their share.
AMR_PREVALENCE_OVERHEAD_IN <- 0.9

#' Fits the prevalence chart's height and type to the bars it draws.
#'
#' The same rule as `amr_auto_layout()` on a much simpler shape: the page grows
#' with the row count up to a ceiling, and the labels are set to the row pitch
#' that leaves rather than to a step table of counts, which was blind to how
#' tall the chart had actually come out.
#'
#' Unlike every other label in this module a bar label is never dropped: a bar
#' chart whose bars are not named says nothing at all, and there is no strip,
#' key or heading elsewhere on the page carrying the same names. What keeps it
#' readable instead is the page growing with the bar count - within the
#' `top_n` the reader can ask for, that is enough on its own, and `legible`
#' reports the case it would not be.
#'
#' @param n_items Integer. Bars the chart draws.
#' @param width_in Numeric. Canvas width in inches.
#' @return A list with `aspect`, `fontsize_row`, `fontsize_legend` and
#'   `legible`.
#' @export
amr_prevalence_layout <- function(n_items, width_in = 9) {
  n <- max(as.integer(n_items %||% 1L), 1L)
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    9
  } else {
    as.numeric(width_in)
  }
  aspect <- .clamp(
    AMR_PREVALENCE_BASE + n * AMR_PREVALENCE_PER_BAR,
    AMR_PREVALENCE_MIN,
    AMR_PREVALENCE_MAX
  )
  pitch <- max(w * aspect - AMR_PREVALENCE_OVERHEAD_IN, 0.2) / n
  pt <- 72 * pitch * AMR_LABEL_FILL
  fontsize_row <- .clamp(pt, AMR_FONT_MIN, AMR_FONT_MAX)
  # The legend key and the axis title scale off the same bar pitch rather
  # than a flat 11pt, so they never read oversized beside bar labels a
  # crowded screen has already shrunk down - the same floor and ceiling
  # amr_auto_layout() fits the gene heatmap's own legend to.
  fontsize_legend <- .clamp(fontsize_row * 1.3, AMR_LEGEND_MIN_PT, 11)
  list(
    aspect = round(aspect, 2),
    fontsize_row = round(fontsize_row, 1),
    fontsize_legend = round(fontsize_legend, 1),
    legible = pt >= AMR_ROW_MIN_PT
  )
}

#' Fit every size in an AMR heatmap to the shape of its matrix.
#'
#' @param n_rows Integer. Isolates the heatmap draws.
#' @param n_cols Integer. Genes or drug classes it draws.
#' @param width_in Numeric. Canvas width in inches.
#' @param show_row_names Logical. Whether isolate names are drawn.
#' @param show_col_names Logical. Whether gene names are drawn.
#' @param show_element_names Logical. Whether the element-type row is drawn.
#' @param row_label_chars Numeric. Longest isolate name, in characters.
#' @param col_label_chars Numeric. Longest column name, in characters.
#' @param block_titles Character vector of column-block titles, or NULL.
#' @param block_cols Integer vector of columns per block, aligned to
#'   `block_titles`.
#' @param element_titles Character vector of element-type panel labels, or
#'   NULL. Separate from `block_titles` because the two only coincide under one
#'   of the two groupings, and each is fitted to its own room.
#' @param element_cols Integer vector of columns per panel, aligned to
#'   `element_titles`.
#' @param dend_cm Numeric. Dendrogram depth the reader asked for, in cm.
#' @param n_strips Integer. Annotation strips drawn beside the rows.
#' @param aspect Numeric, or NULL to fit one. A ratio given here is used as-is
#'   and every size that depends on the row pitch is solved against it, so a
#'   reader who makes the figure taller gets larger isolate labels rather than
#'   the same labels in more whitespace.
#' @return A list of fitted sizes and rotations, plus one legibility verdict
#'   per text role (`legible` for the isolate names, `cols_legible`,
#'   `titles_legible`, `elements_legible`, `anno_names_legible`). A size is
#'   never returned below the floor for its role; the verdict is what says the
#'   floor did not fit, so the caller drops the label rather than drawing it.
#' @export
amr_auto_layout <- function(
  n_rows,
  n_cols,
  width_in = 9,
  show_row_names = FALSE,
  show_col_names = TRUE,
  show_element_names = TRUE,
  row_label_chars = 12,
  col_label_chars = 12,
  block_titles = NULL,
  block_cols = NULL,
  element_titles = NULL,
  element_cols = NULL,
  dend_cm = 1.5,
  n_strips = 0L,
  # "class" or "cluster" (see .column_layout). What it decides here is whether
  # the drug classes are named as text over each block or coloured into a strip
  # with a key - two quite different demands on the page. The element-type row
  # below the body is drawn either way and is fitted separately (see
  # element_titles), so it no longer rides on this.
  column_grouping = "cluster",
  aspect = NULL
) {
  n_rows <- max(as.integer(n_rows %||% 1L), 1L)
  n_cols <- max(as.integer(n_cols %||% 1L), 1L)
  w <- if (is.null(width_in) || !is.finite(width_in) || width_in <= 0) {
    9
  } else {
    as.numeric(width_in)
  }

  # The body loses width to whatever sits beside it, and the strips are the one
  # part of that which the reader controls.
  body_w <- w * AMR_BODY_FRAC - n_strips * AMR_STRIP_IN
  body_w <- max(body_w, w * 0.25)
  cell_w <- body_w / n_cols

  # Column labels are always drawn rotated (a horizontal gene name is wider
  # than any cell it could sit over), so their size is the cell width and the
  # room they need is vertical.
  fontsize_col <- .clamp(
    72 * cell_w * AMR_LABEL_FILL,
    AMR_FONT_MIN,
    AMR_FONT_MAX
  )
  fontsize_title <- .clamp(fontsize_col + 3, 8, 16)

  rot <- .title_rotation(block_titles, block_cols, cell_w, fontsize_title)
  # Rotated, a title's footprint on the page is its own line height rather than
  # its length - but that footprint still has to clear the next title along, or
  # the two run through each other exactly as unrotated ones did. fontsize_col
  # + 3 above answers only to the *average* column, so a screen split into many
  # narrow drug classes needs the size brought down to what the tightest pair
  # of neighbours can actually carry (see .title_pitch).
  if (identical(rot, 90)) {
    fontsize_title <- min(
      fontsize_title,
      72 * .title_pitch(block_cols, cell_w) * AMR_TITLE_FILL
    )
  }
  # Past a point no size fits: forty drug classes over two hundred genes leaves
  # each title a third of a millimetre of page to sit in. Where that happens
  # the titles come off the plot entirely and the class strip takes their place
  # (see .class_strip_drawn), so the floor below is a floor on what is *drawn*
  # rather than a size anything is squeezed down to — a title set at four
  # points was a smudge that still cost the page a full row.
  titles_legible <- fontsize_title >= AMR_TITLE_MIN_PT
  cols_legible <- fontsize_col >= AMR_COL_MIN_PT
  fontsize_title <- max(fontsize_title, AMR_TITLE_MIN_PT)

  # The element-type row. One label under a whole panel, not one per drug-class
  # block — it only ever shared fontsize_title by accident, and that size is
  # solved against the *tightest* class block on the screen, which is what set
  # "Resistance" in four-point type under a seven-inch panel. Fitted to its own
  # panel's width instead, and, like a block title, turned on its side rather
  # than shrunk past reading when a narrow panel cannot carry it flat.
  elem_cap <- .clamp(
    w * AMR_ELEMENT_PT_PER_IN,
    AMR_ELEMENT_MIN_PT,
    AMR_ELEMENT_MAX_PT
  )
  elem_flat <- .element_fontsize(element_titles, element_cols, body_w, elem_cap)
  elem_rot <- if (elem_flat >= AMR_ELEMENT_MIN_PT) 0 else 90
  fontsize_element <- if (identical(elem_rot, 0)) {
    elem_flat
  } else {
    # Turned, the label's footprint across the page is its line height and it
    # has its panel's whole width to sit in — the length that would not fit
    # flat now runs down the page instead, where elem_in reserves for it.
    min(72 * .panel_width(element_cols, body_w) * AMR_TITLE_FILL, elem_cap)
  }
  elements_legible <- fontsize_element >= AMR_ELEMENT_MIN_PT
  fontsize_element <- max(fontsize_element, AMR_ELEMENT_MIN_PT)

  # Everything stacked above and below the body: the rotated column labels, the
  # column dendrogram, the block titles, the element-type row and the plot's
  # own margins. Subtracted from the canvas before the rows get their share, so
  # a screen with long gene names does not lose the room to draw them.
  #
  # Each part is budgeted if and only if it is drawn, and those three
  # conditions are the same ones the builders read (see .class_strip_drawn and
  # .gene_panel) rather than a second set that could drift from them. Room set
  # aside for a label nobody draws is a band of white under the matrix; a label
  # drawn into room nobody set aside runs through whatever sits below it.
  # The switches decide the budget; the verdicts above decide only whether the
  # fit seeds a switch off in the first place (see refit_labels() in
  # visualization_amr.R). Budgeting against a verdict instead would hand a
  # reader who overrules one a label with no room to sit in — which is the
  # zero-height band the gene names used to be drawn into.
  has_blocks <- length(block_titles %||% character(0)) > 0
  class_titles_drawn <- has_blocks &&
    identical(column_grouping, "class") &&
    titles_legible
  strip_drawn <- has_blocks && !class_titles_drawn
  element_row_drawn <- !isFALSE(show_element_names)

  title_in <- AMR_TITLE_GAP_IN +
    if (identical(rot, 90)) {
      .pt_in(fontsize_title) * AMR_CHAR_EM * max(nchar(block_titles %||% ""), 0)
    } else {
      .pt_in(fontsize_title) * 2
    }
  # Turned, the element label runs down the page rather than across it, so what
  # it costs is its length rather than two lines of type.
  elem_in <- if (!element_row_drawn) {
    0
  } else if (identical(elem_rot, 90)) {
    AMR_TITLE_GAP_IN +
      .pt_in(fontsize_element) *
        AMR_CHAR_EM *
        max(nchar(element_titles %||% ""), 1)
  } else {
    .pt_in(fontsize_element) * 2
  }
  # Answers to the switch, not to `cols_legible`: the fit only ever *seeds* the
  # switch off over a crowded shape (see refit_labels() in
  # visualization_amr.R), and a reader who turns the names back on there still
  # gets names — reserving nothing for them left ComplexHeatmap a zero-height
  # band to draw them in, so they ran off the foot of the page and straight
  # through the element-type row below (which is laid out under the room the
  # names were given, not under the room they actually take).
  col_label_in <- if (isFALSE(show_col_names)) {
    0
  } else {
    .pt_in(fontsize_col) *
      AMR_CHAR_EM *
      max(col_label_chars, 1) +
      AMR_COL_LABEL_GAP_IN
  }
  overhead <- col_label_in +
    max(dend_cm, 0) / 2.54 +
    (if (class_titles_drawn) title_in else 0) +
    (if (strip_drawn) AMR_CLASS_STRIP_IN else 0) +
    elem_in +
    0.5

  # Square cells where the matrix is small enough to allow it, the target pitch
  # where it is not.
  row_in <- if (isTRUE(show_row_names)) {
    AMR_ROW_IN_LABELLED
  } else {
    AMR_ROW_IN_PLAIN
  }
  row_h <- .clamp(cell_w, row_in, AMR_ROW_IN_MAX)

  # The reader's ratio wins where they have set one; otherwise the row count
  # picks it, within bounds that stop a six-isolate screen being a letterbox and
  # a six-hundred-isolate one being four screens of scrolling.
  aspect <- if (is.null(aspect) || !is.finite(aspect) || aspect <= 0) {
    raw <- (n_rows * row_h + overhead) / w
    .clamp(raw * AMR_ASPECT_FIT_SCALE, AMR_ASPECT_MIN, AMR_ASPECT_MAX)
  } else {
    as.numeric(aspect)
  }
  # What the clamp actually left for the body, which is the pitch the row
  # labels have to fit inside — not the pitch that was asked for.
  pitch <- max(aspect * w - overhead, 0.2) / n_rows
  # Read off the room rather than off the clamped size: an isolate name whose
  # pitch is worth two points is still *set* at the floor, since a name drawn
  # at all has to be drawn at a size, and the verdict on whether to draw one at
  # all (`legible`, which the view reports on) has to be able to tell that
  # apart from a name that genuinely fits at the floor. A name wider than the
  # margin kept for it is the other way this fails.
  row_pt <- min(
    72 * pitch * AMR_LABEL_FILL,
    72 * (w - body_w) * 0.45 / (AMR_CHAR_EM * max(row_label_chars, 1))
  )
  rows_legible <- row_pt >= AMR_ROW_MIN_PT
  fontsize_row <- .clamp(row_pt, AMR_FONT_MIN, AMR_FONT_MAX)

  # A mapped variable's name, turned on its side over a strip AMR_STRIP_IN
  # wide: that width is its line height, and a strip too narrow to carry the
  # floor goes unnamed rather than smudged - its own legend, which is titled
  # with the same variable name, is what a reader has left to read it from.
  fontsize_anno <- 72 * AMR_STRIP_IN * AMR_LABEL_FILL
  anno_names_legible <- n_strips > 0L && fontsize_anno >= AMR_ANNO_NAME_MIN_PT
  fontsize_anno <- .clamp(fontsize_anno, AMR_ANNO_NAME_MIN_PT, AMR_FONT_MAX)

  # The legend column belongs to the page rather than to the matrix, so its
  # type does not answer to cell_w the way everything above does. What it does
  # answer to is the height it is drawn into: the whole stack has to fit
  # between the top of the body and the bottom of the page, because that is
  # where align_heatmap_legend = "heatmap_top" starts it (see amr_as_ggplot).
  #
  # Every legend is counted, not just the longest, since ComplexHeatmap stacks
  # them into one column before it starts a second: the fill legend's own
  # states, one row per class block plus a title for the panel it heads, and an
  # allowance per mapped variable, whose categories are not tabulated yet at
  # this point. The class strip is what makes the stack long — forty drug
  # classes against the four or five states the fill legend ever lists.
  legend_h <- max(aspect * w - overhead, 1)
  legend_keys <- length(block_titles) +
    length(AMR_ELEMENT_TYPES) +
    length(AMR_CONFIDENCE_STATES) +
    2L +
    n_strips * 10L
  # Solved by trying sizes rather than by rearranging for one, because the
  # swatch and the gap each stop at a floor and there is no closed form the
  # other side of them.
  sizes <- seq(11, AMR_LEGEND_MIN_PT, by = -0.1)
  fits <- vapply(
    sizes,
    function(s) legend_keys * .legend_row_in(s) <= legend_h,
    logical(1)
  )
  fontsize_legend <- min(
    w * 1.05,
    if (any(fits)) sizes[[which(fits)[[1]]]] else AMR_LEGEND_MIN_PT
  )
  fontsize_legend <- .clamp(fontsize_legend, AMR_LEGEND_MIN_PT, 11)
  # What that size leaves room for, so a legend longer still — a mapped
  # variable with a category per isolate, say — can wrap its own keys rather
  # than run off the page. See amr_legend_ncol().
  legend_rows <- max(floor(legend_h / .legend_row_in(fontsize_legend)) - 2, 3)

  list(
    aspect = round(aspect, 2),
    fontsize_row = round(fontsize_row, 1),
    fontsize_col = round(fontsize_col, 1),
    fontsize_title = round(fontsize_title, 1),
    fontsize_element = round(fontsize_element, 1),
    fontsize_anno = round(fontsize_anno, 1),
    fontsize_legend = round(fontsize_legend, 1),
    legend_rows = as.integer(legend_rows),
    # The height the legend column is drawn into, which the builders pack it
    # against so it wraps rather than overrunning the page. See .pack_legends.
    legend_height_in = round(legend_h, 3),
    # The room the labels were budgeted above, handed to ComplexHeatmap as the
    # room it may use for them. Its own defaults are a flat 6cm either way, so
    # a name longer than that was quietly cut off at the edge of the page while
    # `overhead` here had already set aside the whole of it — a band of white
    # above the matrix and a clipped label under it, from the same disagreement.
    col_label_in = round(col_label_in, 3),
    row_label_in = round(max(w - body_w, 0.1) * 0.45, 3),
    # The room reserved for a class block's own title text alone, without the
    # dendrogram below it - handed back so .gene_panel can size the combined
    # title+dendrogram region it draws by hand (see .class_dend_reserve()) to
    # exactly what this fit already budgeted for it.
    title_in = round(title_in, 3),
    grid_width = if (min(cell_w, pitch) < AMR_GRID_MIN_IN) 0 else 0.5,
    title_rot = rot,
    element_rot = elem_rot,
    # Whether the row labels came out at a size worth drawing. The view uses
    # this to say so rather than to override the switch: isolate names are off
    # until a reader asks for them, so there is no initial draw for the fit to
    # keep clean, and a reader who has asked is better answered with the reason
    # than with a switch that silently undoes itself.
    legible = rows_legible,
    # The same verdict for every label the plot draws without being asked.
    # Each is acted on rather than only reported: the gene names are seeded off
    # (refit_labels() in visualization_amr.R, so a reader can still overrule
    # it), and the block titles, the element-type row and the strip names are
    # dropped outright by the builders, each falling back to whatever names the
    # same thing in the legend column.
    titles_legible = titles_legible,
    cols_legible = cols_legible,
    elements_legible = elements_legible,
    anno_names_legible = anno_names_legible,
    row_pitch_in = round(pitch, 4),
    cell_width_in = round(cell_w, 4)
  )
}

# The width one column block gets on the page, and the narrowest of them —
# which is the one that decides whether a label fits, since every element-type
# label is set at one size.
.panel_width <- function(cols, body_w) {
  cols <- as.numeric(cols %||% numeric(0))
  if (!length(cols) || sum(cols) <= 0) {
    return(body_w)
  }
  min(body_w * cols / sum(cols))
}

# The largest type, up to `cap`, that fits every element-type label flat inside
# its own panel. Returns the cap where there is nothing to label, so a screen
# of one unnamed panel is never forced into the rotated branch by an empty
# title.
.element_fontsize <- function(titles, cols, body_w, cap) {
  titles <- as.character(titles %||% character(0))
  cols <- as.numeric(cols %||% numeric(0))
  if (!length(titles) || length(titles) != length(cols) || sum(cols) <= 0) {
    return(cap)
  }
  widths <- body_w * cols / sum(cols)
  min(
    cap,
    72 * widths * AMR_TITLE_FILL / (AMR_CHAR_EM * pmax(nchar(titles), 1))
  )
}

# Whether the column-block titles have to be turned on their side.
#
# A block title is centred over its own block, and nothing clips or staggers it:
# where two blocks are narrower than their names, ComplexHeatmap draws both in
# full and they run through each other. Grouping by drug class is where this
# bites — "FLUOROQUINOLONE" over a single-column block is eight times wider than
# the block — so the check is per block rather than a rule about which grouping
# is in force.
.title_rotation <- function(titles, cols, cell_w, fontsize) {
  if (!length(titles) || !length(cols) || length(titles) != length(cols)) {
    return(0)
  }
  needed <- nchar(as.character(titles)) * AMR_CHAR_EM * .pt_in(fontsize)
  available <- as.numeric(cols) * cell_w
  if (any(needed > available)) 90 else 0
}

# Pre-computes explicit dendrogram objects for character matrices.
# Replaces NaN distances (e.g., all-zero profiles) with 0 to prevent hclust errors.
.dendrogram <- function(mat, enable, distance, method) {
  if (!isTRUE(enable) || nrow(mat) < 3) {
    return(FALSE)
  }
  d <- dist(mat, method = distance)
  d[!is.finite(d)] <- 0
  tryCatch(hclust(d, method = method), error = function(e) FALSE)
}

.column_dendrogram <- function(mat, enable, distance, method) {
  if (!isTRUE(enable) || ncol(mat) < 3) {
    return(FALSE)
  }
  .dendrogram(t(mat), TRUE, distance, method)
}

# A distance function for ComplexHeatmap to cluster a column-split slice with,
# rather than a precomputed tree: the class-grouped branch of .column_layout
# needs one dendrogram per drug-class block, cut fresh from whichever genes
# land in it, and ComplexHeatmap only computes that itself when handed a
# distance to work with rather than a finished tree (see .column_layout).
#
# Handed a plain distance name, it would compute that distance against the
# matrix it is drawing - here, the confidence-tier labels ("Absent",
# "Perfect", ...), which are not numeric and cannot be clustered - so this
# closes over the real presence matrix and looks the slice's genes up in that
# instead, by the column names ComplexHeatmap still passes through per slice.
.column_dist_fn <- function(mat, distance) {
  function(m) {
    d <- dist(t(mat[, rownames(m), drop = FALSE]), method = distance)
    d[!is.finite(d)] <- 0
    d
  }
}

# One mapped variable as a strip beside the rows. Discrete variables get a
# tabulated palette keyed on their categories; a continuous one gets a ramp
# across its own range, because a colour per distinct value is what a collection
# date looked like before the mapping layers arrived.
#
# Isolates the variable is empty for are labelled "NA" rather than dropped: the
# row still exists in the matrix, and a gap in the strip beside a present row is
# read as a rendering fault. That category is listed last whatever it sorts as,
# so the one value carrying no information does not head the legend.
.strip_spec <- function(layer, mat) {
  vals <- layer$values[rownames(mat)]
  label <- layer$label %||% layer$field
  if (isTRUE(layer$continuous)) {
    num <- suppressWarnings(as.numeric(vals))
    if (any(is.finite(num))) {
      rng <- range(num[is.finite(num)])
      # A constant column has no range to ramp over, so it falls through to the
      # discrete branch rather than producing a degenerate colorRamp2.
      if (rng[[1]] < rng[[2]]) {
        stops <- seq(rng[[1]], rng[[2]], length.out = 5L)
        cols <- unname(amr_palette(
          as.character(seq_along(stops)),
          amr_fit_scale(layer$palette, length(stops))
        ))
        return(list(
          label = label,
          values = num,
          col = colorRamp2(stops, cols)
        ))
      }
    }
  }
  chr <- as.character(vals)
  chr[is.na(chr) | !nzchar(chr)] <- AMR_MISSING_LABEL
  cats <- amr_strip_levels(chr)
  list(
    label = label,
    values = chr,
    col = amr_palette(cats, amr_fit_scale(layer$palette, length(cats)))
  )
}

# One keyed legend, at the geometry the fit solved for (see .legend_gp).
.discrete_legend <- function(cols, title, legend_gp) {
  ComplexHeatmap$Legend(
    at = names(cols),
    legend_gp = gpar(fill = unname(cols)),
    title = title,
    labels_gp = legend_gp$labels,
    title_gp = legend_gp$title,
    grid_height = legend_gp$grid,
    grid_width = legend_gp$grid,
    row_gap = legend_gp$row_gap,
    ncol = amr_legend_ncol(length(cols), legend_gp$max_rows)
  )
}

# The whole legend column, in the order the caller built it, packed against the
# height it will actually be drawn into.
#
# Packing it here rather than leaving it to ComplexHeatmap is what keeps it on
# the page. ComplexHeatmap packs against the *device* while drawing the column
# from the top of the matrix body downwards, so it believes it has the whole
# canvas and overruns the bottom by everything stacked above the body; and it
# never reorders, so whichever legend it decides to draw last is the one that
# falls off. Given the true height it wraps into a second column instead, and
# "Gene call" - built first by both builders - stays at the head of the first.
.pack_legends <- function(legends, legend_gp, height_in) {
  legends <- Filter(Negate(is.null), legends)
  if (!length(legends)) {
    return(list())
  }
  packed <- ComplexHeatmap$packLegend(
    list = legends,
    direction = "vertical",
    max_height = unit(max(height_in %||% 8, 2), "in"),
    gap = unit(max(legend_gp$size * 0.25, 1.2), "mm")
  )
  list(packed)
}

# One Legend for one mapped variable's strip, built by hand rather than left
# to ComplexHeatmap's own per-annotation legend — see the comment on
# build_amr_heatmap's extra_legends for why the whole column has to be
# assembled explicitly rather than collected automatically.
.strip_legend <- function(spec, label, legend_gp) {
  if (is.function(spec$col)) {
    return(ComplexHeatmap$Legend(
      col_fun = spec$col,
      title = label,
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    ))
  }
  .discrete_legend(spec$col, label, legend_gp)
}

# Every mapped variable as one rowAnnotation, plus the Legend for each one.
# Several strips have to travel in a single annotation object rather than as
# several: ComplexHeatmap takes exactly one `left_annotation`, and stacking
# them any other way puts the second on the opposite side of the matrix from
# the first. The legends travel separately (`show_legend = FALSE` here) so the
# caller can place them itself — see build_amr_heatmap's extra_legends.
#
# @return A list with `anno` (a rowAnnotation, or NULL) and `legends` (a list
#   of Legend objects, one per mapped variable, empty when `anno` is NULL).
.row_annotation <- function(mat, layers, text_color, legend_gp, opts = list()) {
  layers <- Filter(
    function(l) length(l$values) && nzchar(l$label %||% l$field %||% ""),
    layers %||% list()
  )
  if (!length(layers)) {
    return(list(anno = NULL, legends = list()))
  }
  specs <- lapply(layers, .strip_spec, mat = mat)
  # Two mappings of the same variable would collide on the name ComplexHeatmap
  # keys the strip by, and the second would silently replace the first.
  labels <- make.unique(vapply(specs, function(x) x$label, character(1)))

  args <- list(
    # Turned on its side over a strip a tenth of an inch wide, the name's own
    # line height is what has to fit that width - not the legend's size, which
    # answers to the page and had the names spilling across their neighbours.
    # Where even the floor will not fit it goes unnamed (the strip's key, which
    # carries the same variable name as its title, is what names it then).
    show_annotation_name = !isFALSE(opts$anno_names_legible),
    annotation_name_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_anno %||% legend_gp$size
    ),
    annotation_name_side = "top",
    annotation_name_rot = 90,
    # Pinned to the width amr_auto_layout() takes out of the body for each
    # strip, rather than left at ComplexHeatmap's own 5mm - the two drifting
    # apart is a body narrower than the fit solved every column width against.
    simple_anno_size = unit(AMR_STRIP_IN, "in"),
    col = setNames(lapply(specs, function(x) x$col), labels),
    show_legend = FALSE
  )
  for (i in seq_along(specs)) {
    args[[labels[[i]]]] <- specs[[i]]$values
  }
  list(
    anno = do.call(ComplexHeatmap$rowAnnotation, args),
    legends = Map(
      .strip_legend,
      specs,
      labels,
      MoreArgs = list(legend_gp = legend_gp)
    )
  )
}

# One palette over every class in the screen, whichever panel each lands in.
#
# Tabulated per panel instead, it restarted at the first colour in every panel,
# so the resistance panel's leading class and the stress panel's leading class
# drew the same swatch — "Aminoglycoside" and "Mercury" were both the palette's
# colour 1. The keys are split per panel (see `.class_annotation`), which is
# exactly the arrangement in which a repeated colour is hardest to catch.
.class_colors <- function(meta, scale) {
  cats <- sort(unique(as.character(meta$group)))
  if (!length(cats)) {
    return(character(0))
  }
  amr_palette(cats, amr_fit_scale(scale, length(cats)))
}

# The element type a panel holds, as the reader sees it. One panel is one type
# (see `amr_column_blocks`), so the panel's own columns name the whole of it.
.panel_label <- function(meta) {
  types <- .element_order(meta)
  if (!length(types)) {
    return("Drug class")
  }
  names(AMR_ELEMENT_TYPES)[match(types[[1]], AMR_ELEMENT_TYPES)] %||% types[[1]]
}

# Whether the drug classes are identified by a colour strip and its key rather
# than by a title over each block. The two are alternatives, never both and
# never neither, so both the panel that draws them and the fit that budgets for
# them ask this one question rather than re-deriving the answer apiece.
#
# Clustered, there are no blocks left to title. Grouped by class there are, but
# a screen split into forty of them leaves each title a third of a millimetre
# of page: `titles_legible` says so, and the strip - which costs a sixth of an
# inch whatever the class count - is what a reader is left to read the classes
# off instead.
.class_strip_drawn <- function(opts) {
  !identical(opts$column_grouping %||% "class", "class") ||
    isFALSE(opts$titles_legible)
}

# The class strip over one panel's columns. Its legend is built separately by
# .class_legend() and placed by the caller (see build_amr_heatmap's
# extra_legends), so this never carries one of its own.
#
# Colours still come from one tabulation over the whole screen (see
# `.class_colors`) and are only subset here, so no two classes share a swatch
# across the legends that now sit above each other.
.class_annotation <- function(groups, cols, legend_gp, name) {
  cats <- intersect(names(cols), unique(as.character(groups)))
  args <- list(
    col = setNames(list(cols[cats]), name),
    show_annotation_name = FALSE,
    show_legend = FALSE,
    # Fixed rather than left to ComplexHeatmap's own default, and matched to
    # AMR_CLASS_STRIP_IN - the constant amr_auto_layout() budgets against, so
    # the two never disagree about how tall this actually draws. `height`
    # alone is not enough here: with one simple annotation and nothing else
    # in the track, ComplexHeatmap sizes it from `simple_anno_size` and
    # ignores the total `height` handed in.
    simple_anno_size = unit(AMR_CLASS_STRIP_IN, "in")
  )
  args[[name]] <- factor(as.character(groups), levels = cats)
  do.call(ComplexHeatmap$HeatmapAnnotation, args)
}

# The drug-class name printed over each block, and that block's own column
# dendrogram — drawn together, by hand, into a reserved blank strip rather
# than through ComplexHeatmap's own column_title/column_dend.
#
# Neither of ComplexHeatmap's native slots can do this alone. column_title is
# already the panel's element-type name at the bottom (see .gene_panel), so a
# class name needs a mechanism of its own to land at the top at all — an
# `anno_block` can print it, but ComplexHeatmap always draws top_annotation
# *below* the column dendrogram, whichever annotation is in it, so a title
# living there is stuck between the dendrogram and the matrix rather than
# above the dendrogram the way a reader expects a heading to sit. The fix is
# to take the dendrogram over too: `show_column_dend = FALSE` on the panel,
# a blank `anno_empty` reserved here in its place sized to hold both, and
# .decorate_class_dend() draws the real title text and the real per-block
# dendrogram into that reserved room once the panel has been drawn (see
# amr_as_ggplot) — column_dend() reads the same clustering ComplexHeatmap
# would have drawn itself, so the tree is identical, just relocated.
#
# @param height A grid `unit` — the combined title-plus-dendrogram room the
#   fit budgeted (`title_in` inches over `dend_size` cm; see .gene_panel).
# @param name The annotation's own name, unique within the whole HeatmapList
#   so `decorate_annotation()` in a multi-panel screen never targets the
#   wrong panel's reserved strip.
.class_dend_reserve <- function(height, name) {
  args <- list(show_annotation_name = FALSE)
  args[[name]] <- ComplexHeatmap$anno_empty(border = FALSE, height = height)
  do.call(ComplexHeatmap$HeatmapAnnotation, args)
}

# Draws one class block's title and its own dendrogram into the room
# .class_dend_reserve() set aside for it — called once per block, after the
# panel it belongs to has been drawn (see amr_as_ggplot's decorations loop).
#
# Neither piece is drawn by pushing a child viewport offset within the
# reserved region (giving that child its own `y`, `height` and `just`) - a
# pushed viewport's `y`/`just` are silently ignored inside a
# decorate_annotation() callback, and the child always collapses to the
# reserved region's own bottom edge regardless of what position was asked
# for (reproduced in isolation against a bare ComplexHeatmap annotation, so
# this is not specific to how this module calls it). Two workarounds that
# don't hit it instead:
#   - the title is `grid.text()`ed directly against the *reserved region's*
#     own coordinates (no child viewport at all) - a plain grob's own
#     `y`/`just` position correctly even though the same values on a pushed
#     viewport do not.
#   - the dendrogram still needs a `native`-scaled viewport for
#     grid.dendrogram() to draw into, so instead of pushing one sized to just
#     the bottom portion, a *full-size* child is pushed (untouched `y`/
#     `height` default to the whole region, which is unaffected by the bug)
#     and its `yscale` is stretched past the tree's own max height by
#     total/dend_cm, so the tree's real data range only ever reaches the
#     bottom dend_cm fraction of that full-size viewport - visually
#     identical to a viewport confined to that fraction, without needing one.
#
# Rotated, the title starts a hair above the dendrogram below it (or above the
# matrix, where the dendrogram is off) and grows upward, the same way
# ComplexHeatmap's own column_title does when rotated — matched here because a
# reader comparing classes of very different name length reads the ragged tops
# as "these titles are the same size", not as misalignment. Unrotated, there
# is no such comparison to preserve - a short word centres itself in the room
# instead.
.decorate_class_dend <- function(
  annotation,
  slice,
  label,
  dend,
  title_in,
  dend_cm,
  text_color,
  size,
  rot
) {
  ComplexHeatmap$decorate_annotation(
    annotation,
    {
      base <- unit(dend_cm, "cm") + unit(AMR_TITLE_GAP_IN, "in")
      if (identical(rot, 90)) {
        # Justification on rotated text is read in the text's own turned
        # frame, so it is the *horizontal* half that decides where the label
        # sits on the page here: "left" starts it at the anchor and grows it
        # upward, where "center" would straddle the anchor and lay half the
        # word over the dendrogram (or the matrix) below.
        grid.text(
          label,
          x = unit(0.5, "npc"),
          y = base,
          just = c("left", "center"),
          rot = rot,
          gp = gpar(col = text_color, fontsize = size)
        )
      } else {
        grid.text(
          label,
          x = unit(0.5, "npc"),
          y = base + (unit(title_in - AMR_TITLE_GAP_IN, "in")) * 0.5,
          rot = rot,
          gp = gpar(col = text_color, fontsize = size)
        )
      }
      # A one-column block clusters against nothing and column_dend() hands
      # back a degenerate, zero-height "dendrogram" for it - drawing that is
      # skipped rather than fed to grid.dendrogram(), which errors on a
      # height scale of zero width.
      n <- length(labels(dend))
      heights <- ComplexHeatmap$dend_heights(dend)
      if (n > 1L && max(heights, 0) > 0 && dend_cm > 0) {
        dend_in <- dend_cm / 2.54
        total_in <- title_in + dend_in
        pushViewport(viewport(
          xscale = c(0, n),
          yscale = c(0, max(heights) * total_in / dend_in)
        ))
        # Matched to draw_dend()'s own "top"-side branch (ComplexHeatmap's
        # internal dendrogram drawer: `xscale = c(0, nobs)` / `facing =
        # "bottom"` for a dendrogram sitting above its body) rather than
        # guessed: `xscale = c(0.5, n + 0.5)` drew every leaf half a column
        # short of where the matrix underneath actually put it, and `facing
        # = "top"` hung the tree from its leaves instead of its root - the
        # "upside down" a reader saw was this, not a bug in grid.dendrogram
        # itself.
        ComplexHeatmap$grid.dendrogram(
          dend,
          facing = "bottom",
          gp = gpar(col = text_color)
        )
        popViewport()
      }
    },
    slice = slice
  )
}

# The key for one panel's class strip, headed by the panel's own element type:
# the stress panel's entries are not drug classes at all — filing "Mercury"
# and "Quaternary ammonium" under a heading reading "Drug class" was wrong,
# not merely long.
#
# A panel holding no per-gene classes (a virulence screen carries no drug
# class) has a single, panel-wide class equal to the panel's own name; a key
# reading "Virulence: Virulence" would explain nothing the block title has
# not, so that one comes back NULL and the strip runs without a legend.
.class_legend <- function(groups, cols, legend_gp, name) {
  cats <- intersect(names(cols), unique(as.character(groups)))
  if (identical(cats, name)) {
    return(NULL)
  }
  .discrete_legend(cols[cats], name, legend_gp)
}

.legend_gp <- function(text_color, size, max_rows = 40L) {
  list(
    size = size,
    labels = gpar(col = text_color, fontsize = size),
    title = gpar(col = text_color, fontsize = size + 2),
    # The same swatch and row gap amr_auto_layout() solved the type size
    # against, so the height it budgeted for is the height actually drawn.
    grid = unit(.legend_grid_mm(size), "mm"),
    row_gap = unit(.legend_gap_mm(size), "mm"),
    max_rows = max(as.integer(max_rows), 1L)
  )
}

# How one panel's columns are arranged *inside* the panel. Element type is not
# among the options: a screen covering more than one element type is drawn as
# one panel per type (see `.element_blocks`), so that separation is structural
# rather than a grouping the reader can turn off. "element" survives as an
# accepted value only because saved analyses carry it.
#
# Grouped by class, genes still cluster - just within their own block rather
# than across all of them. ComplexHeatmap does this natively given a logical
# `cluster_columns` alongside `column_split` (a single precomputed dendrogram,
# the way the "cluster" branch below hands it, cannot be split this way - it
# has to compute one tree per slice itself), which is also why this branch
# returns the distance/method to cluster with rather than a finished tree.
.column_layout <- function(mat, grouping, meta, distance, method) {
  none <- list(split = NULL, cluster = FALSE)
  if (!ncol(mat)) {
    return(none)
  }
  switch(
    grouping %||% "class",
    cluster = list(
      split = NULL,
      cluster = .column_dendrogram(mat, TRUE, distance, method)
    ),
    class = list(
      split = factor(meta$group, levels = sort(unique(meta$group))),
      cluster = ncol(mat) >= 3
    ),
    none
  )
}

# The element types present, in the vocabulary's own order rather than
# alphabetically, so Resistance always leads and Unclassified always trails.
.element_order <- function(meta) {
  intersect(
    c(unname(AMR_ELEMENT_TYPES), AMR_UNCLASSIFIED),
    unique(as.character(meta$element_type))
  )
}

#' The column blocks a heatmap will be drawn in, and how wide each one is.
#'
#' Resistance, virulence and stress genes answer different questions about the
#' same isolate, so a screen covering more than one of them is drawn as one
#' panel per type side by side, each clustered on its own. Reading a virulence
#' gene's co-occurrence off a dendrogram that also had to accommodate thirty
#' beta-lactamases is the thing that arrangement prevents.
#'
#' Exported because the layout fit needs the block titles and widths before the
#' heatmap exists, to decide whether the titles fit horizontally.
#'
#' @param mat A presence matrix from `amr_presence_matrix()`.
#' @param grouping How columns are arranged inside one panel.
#' @return A list with `titles` and `cols`, one entry per block.
#' @export
amr_column_blocks <- function(mat, grouping = "class") {
  meta <- attr(mat, "genes")
  if (is.null(meta) || !nrow(meta) || !ncol(mat)) {
    return(list(titles = character(0), cols = integer(0)))
  }
  # Grouped by drug class, the titles a reader actually sees are the class names
  # inside each panel — those are the ones that collide, so those are the ones
  # the fit is asked about.
  if (identical(grouping, "class")) {
    types <- .element_order(meta)
    groups <- unlist(lapply(types, function(et) {
      sort(unique(meta$group[meta$element_type == et]))
    }))
    cols <- vapply(
      seq_along(groups),
      function(i) sum(meta$group == groups[[i]]),
      integer(1)
    )
    return(list(titles = as.character(groups), cols = cols))
  }
  amr_element_blocks(mat, grouping)
}

#' The element-type panels a heatmap will be drawn in, and how wide each is.
#'
#' The other half of what the fit needs before the heatmap exists: the
#' element-type row under the body is one label per panel, so the room each has
#' is its own panel's width — quite unrelated to the drug-class blocks
#' `amr_column_blocks()` measures, which is why the two are asked for
#' separately and sized separately.
#'
#' @param mat A presence matrix from `amr_presence_matrix()`.
#' @param grouping Unused; accepted so the signature matches
#'   `amr_column_blocks()`, which falls back to this when not grouping by
#'   class — a gene matrix is split by element type structurally, regardless
#'   of grouping.
#' @return A list with `titles` and `cols`, one entry per panel.
#' @export
amr_element_blocks <- function(mat, grouping = "class") {
  none <- list(titles = character(0), cols = integer(0))
  if (!ncol(mat)) {
    return(none)
  }
  meta <- attr(mat, "genes")
  if (is.null(meta) || !nrow(meta)) {
    return(none)
  }
  types <- .element_order(meta)
  list(
    titles = vapply(
      types,
      function(et) {
        names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||% et
      },
      character(1),
      USE.NAMES = FALSE
    ),
    cols = vapply(types, function(et) sum(meta$element_type == et), integer(1))
  )
}

# --- Heatmap Builders --------------------------------------------------------

#' Colors for the gene heatmap's four named confidence tiers, running through
#' the reader's own absent/partial/strong/perfect colors.
#'
#' Putative (an HMM motif match with no alignment behind it - the weakest tier
#' AMRFinderPlus reports, and rare enough in practice that a fifth picker
#' would be more sidebar than the information is worth) is not separately
#' configurable: it is the midpoint blend between `absent` and `partial`,
#' since it sits below Partial on the same confidence ladder.
#' @param absent,partial,strong,present Hex colors, low to high confidence.
#' @return Named character vector of colors, keyed by `AMR_CONFIDENCE_STATES`.
#' @export
amr_confidence_palette <- function(absent, partial, strong, present) {
  putative <- colorRampPalette(c(absent, partial))(3)[[2]]
  setNames(
    c(absent, putative, partial, strong, present),
    AMR_CONFIDENCE_STATES
  )
}

# One panel of the gene heatmap: the columns of a single element type, with
# their own column arrangement and their own dendrogram.
#
# Only the first panel carries the row dendrogram and the annotation strips, and
# only the last carries the isolate names and the fill legend — repeated on
# every panel they are the same information three times, and ComplexHeatmap
# takes the row order from the first panel regardless, so the others must not
# cluster. Each panel does key its own drug classes, which is the one thing that
# genuinely differs between them.
.gene_panel <- function(
  mat,
  confidence,
  meta,
  opts,
  legend_gp,
  title,
  first,
  last,
  row_cluster,
  anno,
  class_cols
) {
  text_color <- opts$text_color %||% "#000000"
  display <- matrix(
    AMR_CONFIDENCE_STATES[confidence + 1L],
    nrow = nrow(mat),
    ncol = ncol(mat),
    dimnames = dimnames(mat)
  )
  col_distance <- opts$col_cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT
  col_method <- opts$col_cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
  # Defaulted once here rather than read as opts$column_grouping throughout,
  # so this and .column_layout's own %||% "class" fallback can never disagree
  # about which branch an unset grouping falls into.
  grouping <- opts$column_grouping %||% "class"
  layout <- .column_layout(mat, grouping, meta, col_distance, col_method)
  dend_cm <- max(opts$dend_size %||% 1.5, 0)
  elem_label <- title %||% .panel_label(meta)

  # A panel with a single class equal to its own element type (a virulence
  # screen, or the stress genes with no more specific class - see
  # .class_legend for the same case) draws no class titles: they would only
  # repeat the bottom element-type label. Nor does a screen whose classes came
  # out too narrow to name (see .class_strip_drawn) - there the strip and its
  # key take the titles' place. Computed up front because it also decides
  # whether ComplexHeatmap's own column dendrogram is used as-is or taken over
  # by .class_dend_reserve()/.decorate_class_dend() below.
  cats <- if (!is.null(layout$split)) levels(layout$split) else character(0)
  class_titles_needed <- !.class_strip_drawn(opts) &&
    length(cats) &&
    !identical(cats, elem_label) &&
    ncol(mat) > 0
  reserve_name <- paste0("class_dend_", gsub("[^A-Za-z0-9]+", "_", elem_label))
  reserve_height <- unit(opts$title_in %||% 1, "in") + unit(dend_cm, "cm")
  panel_name <- if (first) "Gene" else paste0("Gene.", title)

  args <- list(
    display,
    name = panel_name,
    col = amr_confidence_palette(
      opts$absent_color %||% "#EFEFEF",
      opts$partial_color %||% "#E5C494",
      opts$strong_color %||% "#8C6E3D",
      opts$present_color %||% "#000000"
    ),
    rect_gp = gpar(
      col = opts$grid_color %||% "#FFFFFF",
      lwd = opts$grid_width %||% 0.5
    ),
    # The element-type row's own size, not the class titles' - those are solved
    # against the tightest drug-class block on the screen, which has nothing to
    # do with the width of the panel this label sits under (see
    # .element_fontsize).
    column_title_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_element %||% 12
    ),
    # Flat wherever the panel is wide enough to carry the name, and turned on
    # its side where it is not - the same choice, for the same reason, that
    # title_rot makes for the class titles above, just answering to the panel's
    # width rather than to a class block's.
    column_title_rot = opts$element_rot %||% 0,
    row_title = NULL,
    row_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_row %||% amr_fit_fontsize(nrow(mat))
    ),
    column_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_col %||% amr_fit_fontsize(ncol(mat))
    ),
    show_row_names = last && !isFALSE(opts$show_row_names),
    show_column_names = !isFALSE(opts$show_col_names),
    # The room the fit set aside for them, rather than ComplexHeatmap's flat
    # 6cm, which cut off any name longer than that while the fit had already
    # reserved the whole of it above the matrix.
    column_names_max_height = unit(opts$col_label_in %||% 2.4, "in"),
    row_names_max_width = unit(opts$row_label_in %||% 2.4, "in"),
    cluster_rows = if (first) row_cluster else FALSE,
    show_row_dend = first && dend_cm > 0 && !isFALSE(row_cluster),
    # Legends are merged into one list in panel order, so the fill key rides
    # with whichever panel draws it: on the last, it follows every panel's class
    # key instead of splitting them apart.
    cluster_columns = layout$cluster,
    # Only read when cluster_columns is TRUE rather than a precomputed tree
    # (the class-grouped branch of .column_layout) - ComplexHeatmap clusters
    # each split slice itself, against this rather than against the display
    # matrix it draws (see .column_dist_fn). Ignored otherwise.
    clustering_distance_columns = .column_dist_fn(mat, col_distance),
    clustering_method_columns = col_method,
    # ComplexHeatmap's own dendrogram is only used where nothing is titling
    # the blocks above it - a class-split panel takes it over instead, so its
    # own title can sit above the tree rather than below it (see
    # .class_dend_reserve()/.decorate_class_dend()); the clustering itself
    # still runs unconditionally (cluster_columns above), so the tree
    # column_dend() reads back afterwards is identical either way.
    show_column_dend = dend_cm > 0 && !class_titles_needed,
    column_split = layout$split,
    # A block's position is its drug class, curated above - not something a
    # similarity score should be allowed to move. This only bears on the
    # class-grouped branch; there is nothing to reorder in the other two.
    cluster_column_slices = FALSE,
    row_dend_width = unit(max(dend_cm, 0.1), "cm"),
    column_dend_height = unit(max(dend_cm, 0.1), "cm"),
    row_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    column_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    # Which of these draws is decided by .class_strip_drawn, never by a switch
    # of the reader's: grouped by class with room to name them, the columns are
    # already split into blocks, so a title (plus its own dendrogram, taken
    # over above) over each one says which class it is and a colour strip
    # repeating that would be redundant; clustered, or split so finely that no
    # legible title fits, there is nothing left to title and the strip (with
    # .class_legend's key) carries the same information instead. Never both at
    # once, and never neither.
    top_annotation = if (class_titles_needed) {
      .class_dend_reserve(reserve_height, reserve_name)
    } else if (.class_strip_drawn(opts) && ncol(mat) > 0) {
      .class_annotation(meta$group, class_cols, legend_gp, .panel_label(meta))
    },
    left_annotation = if (first) anno,
    # No panel draws the fill legend either: like the class keys and the
    # mapped-variable strips, it is built by hand and placed with them (see
    # build_amr_heatmap's extra_legends), which is what lets the whole column
    # be ordered and measured as one thing.
    show_heatmap_legend = FALSE,
    # The element type this panel covers, always one label regardless of how
    # its columns are arranged, and always at the bottom - freeing the top for
    # the per-class titles above, which used to share this same slot and could
    # only ever occupy it when the columns were actually split by class.
    column_title_side = "bottom"
  )
  # `args["column_title"] <-`, not `args$column_title <-`: the latter deletes
  # the element instead of storing NULL in it, which is indistinguishable from
  # omitting it - and omitted, with column_split set, ComplexHeatmap would
  # auto-title each class block at the bottom itself, duplicating the text row
  # above.
  if (isFALSE(opts$show_element_names)) {
    args["column_title"] <- list(NULL)
  } else {
    args$column_title <- elem_label
  }
  ht <- do.call(ComplexHeatmap$Heatmap, args)
  # One record per panel, not per class block: the per-block dendrograms
  # themselves are not read off here. ComplexHeatmap only finalises a
  # heatmap's column order/dendrograms once it is actually drawn (column_dend()
  # warns as much - "you might have different results if you repeatedly
  # execute this function" - and a slice count read before that point has
  # already been observed to disagree with what draw() goes on to lay out),
  # so amr_as_ggplot() re-reads column_dend() itself, by this panel's own
  # `name`, right after the real draw() call. This just carries what that
  # later step cannot recover on its own - which reserved strip is this
  # panel's, and how it should be styled. See build_amr_heatmap for where
  # several panels' records are merged before the shared draw() call.
  if (class_titles_needed) {
    attr(ht, "class_decorations") <- list(list(
      panel_name = panel_name,
      annotation = reserve_name,
      # The block labels, in the order column_split draws them - taken from
      # here rather than from column_dend()'s own names(), which collapses
      # to an unnamed, un-listed single dendrogram (not a length-1 list of
      # one) for a panel with only one class, the same way a single-class
      # column_title falls back to its own name rather than a "labels"
      # vector. amr_as_ggplot's decorate loop matches these positionally
      # against column_dend()'s list instead of trusting its names either
      # way.
      cats = cats,
      title_in = opts$title_in %||% 1,
      dend_cm = dend_cm,
      text_color = text_color,
      size = opts$fontsize_title %||% 12,
      rot = opts$title_rot %||% 0
    ))
  }
  ht
}

#' Builds the gene presence/absence heatmap.
#'
#' One `Heatmap` when the screen covers a single element type, a `HeatmapList`
#' of side-by-side panels when it covers several — see `.gene_panel`. Both draw
#' through the same `amr_as_ggplot()`.
#'
#' @param mat A presence matrix from `amr_presence_matrix()`.
#' @param opts Named list of display options.
#' @return A ComplexHeatmap `Heatmap` or `HeatmapList`.
#' @export
build_amr_heatmap <- function(mat, opts = list()) {
  meta <- attr(mat, "genes")
  # Falls back to treating every present cell as a confident (BLAST-grade)
  # call for a matrix built by hand rather than by amr_presence_matrix() -
  # the same fallback amr_method_confidence() uses for an unrecognised method.
  confidence <- attr(mat, "confidence") %||% (mat * 3L)
  text_color <- opts$text_color %||% "#000000"
  legend_gp <- .legend_gp(
    text_color,
    opts$fontsize_legend %||% 9,
    opts$legend_rows %||% 40L
  )

  # The row clustering is computed once, over the whole matrix, and handed to
  # the leading panel. Per-panel row dendrograms would each order the isolates
  # differently, and the panels have to share one row order to be read across.
  row_cluster <- .dendrogram(
    mat,
    opts$cluster_rows %||% TRUE,
    opts$cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
    opts$cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
  )
  row_anno <- .row_annotation(
    mat,
    opts$anno_layers,
    text_color,
    legend_gp,
    opts
  )
  anno <- row_anno$anno

  # Tabulated once over the whole screen, before the columns are split into
  # panels, so a class keeps its colour across them.
  class_cols <- if (is.null(meta) || !nrow(meta)) {
    character(0)
  } else {
    .class_colors(meta, opts$class_scale)
  }

  types <- if (is.null(meta) || !nrow(meta)) {
    character(0)
  } else {
    .element_order(meta)
  }

  # The whole legend column, in the order a reader meets each part of it
  # reading the plot: what a cell means first, then the class each block
  # belongs to, then the variables mapped beside the rows.
  #
  # Built here rather than left for ComplexHeatmap to collect: automatic
  # collection walks the panels in order and slots each one's own fill legend
  # in after that panel's other legends, which buried "Gene call" wherever the
  # last panel happened to fall, and it measures the column against the whole
  # device while drawing it from the top of the body down, which ran the tail
  # of it off the bottom of the page. Assembled and packed here (see
  # .pack_legends) both are decided rather than inherited.
  #
  # Only the states the screen actually reached are keyed, which is what
  # ComplexHeatmap's own fill legend did: a screen with no HMM-only call has
  # no "Putative" cell to explain.
  fill_palette <- amr_confidence_palette(
    opts$absent_color %||% "#EFEFEF",
    opts$partial_color %||% "#E5C494",
    opts$strong_color %||% "#8C6E3D",
    opts$present_color %||% "#000000"
  )
  seen <- AMR_CONFIDENCE_STATES[sort(unique(as.vector(confidence))) + 1L]
  fill_legend <- if (length(seen)) {
    .discrete_legend(fill_palette[seen], "Gene call", legend_gp)
  }

  # The strip's own key, drawn only where the strip itself is (see
  # .gene_panel's top_annotation) - a class-split screen already has its
  # classes named as text over each block, so a key repeating them would
  # explain nothing the plot has not already said.
  class_legends <- if (.class_strip_drawn(opts) && length(types)) {
    Filter(
      Negate(is.null),
      lapply(types, function(et) {
        .class_legend(
          meta$group[meta$element_type == et],
          class_cols,
          legend_gp,
          names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||% et
        )
      })
    )
  } else {
    list()
  }
  extra_legends <- .pack_legends(
    c(list(fill_legend), class_legends, row_anno$legends),
    legend_gp,
    opts$legend_height_in
  )

  # A HeatmapList built with `+` is a new S4 object - none of its panels' own
  # R-level attributes survive the combination, so each panel's class
  # decorations (see .gene_panel) are read off before that happens and
  # reattached to whatever the combined result turns out to be.
  if (length(types) < 2L) {
    panels <- list(.gene_panel(
      mat,
      confidence,
      meta,
      opts,
      legend_gp,
      title = NULL,
      first = TRUE,
      last = TRUE,
      row_cluster = row_cluster,
      anno = anno,
      class_cols = class_cols
    ))
  } else {
    panels <- lapply(seq_along(types), function(i) {
      keep <- meta$element_type == types[[i]]
      label <- names(AMR_ELEMENT_TYPES)[match(
        types[[i]],
        AMR_ELEMENT_TYPES
      )] %||%
        types[[i]]
      .gene_panel(
        mat[, keep, drop = FALSE],
        confidence[, keep, drop = FALSE],
        meta[keep, , drop = FALSE],
        opts,
        legend_gp,
        title = label,
        first = i == 1L,
        last = i == length(types),
        row_cluster = row_cluster,
        anno = anno,
        class_cols = class_cols
      )
    })
  }
  ht <- Reduce(`+`, panels)
  attr(ht, "extra_legends") <- extra_legends
  attr(ht, "class_decorations") <- do.call(
    c,
    lapply(panels, function(p) attr(p, "class_decorations"))
  )
  ht
}

#' Converts a ComplexHeatmap object or list into a standard ggplot2 object.
#'
#' `width_in`/`height_in` are the size the result will finally be drawn at, and
#' they matter more than they look. ComplexHeatmap makes layout decisions
#' against the *current device* — `packLegend()` takes its `max_height` from
#' `par("din")` — and `grid.grabExpr()` opens a 7x7 inch device unless told
#' otherwise. Grabbed on that default, a legend column taller than seven inches
#' wraps into a second column beside the first, whatever the real canvas is:
#' which is exactly what a tall heatmap with several annotation strips produces,
#' and it wrapped no matter how tall the figure was made. Handing the grab
#' device the true size is what makes the legends use the height they have.
#'
#' @param ht A `Heatmap` or `HeatmapList`.
#' @param background Hex colour for the whole frame.
#' @param gap_mm Numeric. Gutter between side-by-side panels, in millimetres.
#' @param width_in,height_in Numeric. The canvas the result is bound for.
#' @return A ggplot object.
#' @export
amr_as_ggplot <- function(
  ht,
  background = "#FFFFFF",
  gap_mm = 4,
  width_in = 9,
  height_in = 9
) {
  opt <- ComplexHeatmap$ht_opt
  opt$message <- FALSE
  opt$HEATMAP_LEGEND_PADDING <- unit(AMR_LEGEND_PAD_MM, "mm")
  grob <- grid.grabExpr(
    {
      # Captured rather than discarded: draw() is what finalises a heatmap's
      # column order and dendrograms (see .decorate_class_dend's caller
      # below), and that finished state lives on the object draw() returns,
      # not on the `ht` this function was handed.
      ht_drawn <- ComplexHeatmap$draw(
        ht,
        # `merge_legends`, plural. The formal sits after `...` in draw()'s
        # signature, so R will not partial-match it: the singular spelling
        # this carried for a long time fell into `...` and was discarded,
        # which is why the annotation legends and the heatmap legends were
        # drawn as two adjacent columns rather than as one list.
        merge_legends = TRUE,
        # The fill legend (Gene call) is the only one left for
        # ComplexHeatmap to collect on its own - every class and
        # mapped-variable key was built by hand and is handed in here
        # instead, in the fixed order build_amr_heatmap() chose. Passed this
        # way rather than left automatic, they are guaranteed to land *after*
        # the fill legend
        # rather than wherever their own panel happens to fall in the
        # drawing order - which is what kept pushing it out of first place,
        # and past the bottom of the page, once a screen had enough classes
        # to fill more than one legend column.
        heatmap_legend_list = attr(ht, "extra_legends") %||% list(),
        background = "transparent",
        # The gutter between one element type's panel and the next. Wide
        # enough to read as a break, narrow enough that the panels still
        # read as one matrix.
        ht_gap = unit(max(gap_mm, 0), "mm"),
        # Legends sit at the top of the matrix rather than centred against
        # it. A screen of two hundred isolates is several screens tall, and
        # a vertically centred legend lands halfway down it, out of sight of
        # the plot it explains.
        align_heatmap_legend = "heatmap_top",
        align_annotation_legend = "heatmap_top"
      )
      # Each class block's title and its own dendrogram, taken over from
      # ComplexHeatmap and drawn by hand into the room build_amr_heatmap()
      # reserved for them - see .gene_panel's top_annotation and
      # .decorate_class_dend(). Has to happen after draw() above, against
      # ht_drawn rather than ht: the regions being decorated into only exist
      # as named viewports once the heatmap has actually been laid out, and
      # column_dend() only settles into its final per-block order at the
      # same point.
      for (d in attr(ht, "class_decorations") %||% list()) {
        cd <- suppressWarnings(
          ComplexHeatmap$column_dend(ht_drawn, name = d$panel_name)
        )
        # A panel with only one class is not split into a named list at all
        # - column_dend() hands back the bare dendrogram itself instead of a
        # length-one list holding it (see .gene_panel's class_decorations).
        if (inherits(cd, "dendrogram")) {
          cd <- list(cd)
        }
        for (k in seq_along(d$cats)) {
          .decorate_class_dend(
            d$annotation,
            k,
            d$cats[[k]],
            cd[[k]],
            d$title_in,
            d$dend_cm,
            d$text_color,
            d$size,
            d$rot
          )
        }
      }
    },
    width = max(width_in, 1),
    height = max(height_in, 1)
  )
  as.ggplot(grob) +
    theme(plot.background = element_rect(fill = background, colour = NA))
}

# --- Prevalence Chart --------------------------------------------------------

#' Builds a horizontal ggplot2 bar chart for AMR gene/class prevalence.
#' @export
build_amr_prevalence <- function(df, opts = list()) {
  text_color <- opts$text_color %||% "#000000"
  background <- opts$background %||% "#FFFFFF"
  n_iso <- opts$n_isolates %||% NA_integer_
  row_size <- opts$fontsize_row %||% amr_fit_fontsize(nrow(df))
  legend_size <- opts$fontsize_legend %||% 11

  cats <- sort(unique(df$group))
  cols <- amr_palette(cats, amr_bar_scale_fit(opts$bar_scale, length(cats)))

  df$item <- factor(df$item, levels = rev(df$item))

  ggplot(df, aes(x = .data$item, y = .data$n, fill = .data$group)) +
    geom_col(width = 0.75) +
    coord_flip() +
    scale_fill_manual(values = cols, name = NULL, drop = FALSE) +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = NULL,
      y = if (is.na(n_iso)) {
        "Isolates"
      } else {
        sprintf("Isolates (of %d)", n_iso)
      }
    ) +
    theme_minimal(base_size = legend_size) +
    theme(
      text = element_text(colour = text_color),
      axis.text = element_text(colour = text_color, size = row_size),
      axis.title = element_text(colour = text_color, size = legend_size),
      legend.text = element_text(colour = text_color, size = legend_size),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = background, colour = NA),
      panel.background = element_rect(fill = background, colour = NA)
    )
}

# --- Export Utilities --------------------------------------------------------

#' Renders a plot object to a PNG file scaled for display.
#' @export
render_amr_png <- function(
  plot,
  file,
  width_px,
  height_px,
  res = 96,
  scale = 1
) {
  ggsave(
    filename = file,
    plot = plot,
    device = "png",
    width = width_px / res,
    height = height_px / res,
    dpi = res * scale,
    limitsize = FALSE
  )
}

# File export goes through app/logic/viz_export.R's save_plot_export(), which
# owns the device settings for every plot type; what stays here is the on-screen
# render above, which is sized in pixels rather than in physical units.
