# app/logic/amr_plot.R
#
# AMR-screening data reshaping and plot building for app/view/visualization_amr.R.
# Follows the split every other engine uses (app/logic/phylo.R for the MST,
# app/logic/tree_plot.R for the Tree, app/logic/epi_plot.R for the Epi curve):
# the database reads, the matrix reshaping and the plot construction live here so
# the view module only wires reactives to them, and the pure parts stay
# unit-testable without Shiny.
#
# Two tables feed this, both written by app/logic/amr.R at typing time and both
# keyed on `souche` (the same isolate key as `mlst` / `metadata`):
#
#   * amr_results  - one row per detected element. The granular source: gene
#                    symbol, element type (AMR / VIRULENCE / STRESS), AMRFinder's
#                    drug class and subclass, and the % identity / coverage of
#                    the hit. Drives the gene heatmap and the gene-level
#                    prevalence bars.
#   * amr_summary  - abritamr's curated per-isolate rollup, tidy (souche,
#                    section, drug_class, genes) with section in
#                    matches / partials / virulence. Drives the drug-class matrix
#                    and the class-level prevalence bars. `matches` is a
#                    confident call, `partials` a partial one - that distinction
#                    is the whole reason the class view is worth having next to
#                    the gene heatmap.
#
# Both tables are created lazily (a database typed before AMR screening existed,
# or with screening unavailable for its species, simply has neither), so every
# reader here returns a correctly-shaped empty frame rather than failing.
#
# The two matrix views are ComplexHeatmap heatmaps and the prevalence view is a
# plain ggplot2 bar chart, but all three reach the view module as *ggplot*
# objects (see `amr_as_ggplot`), so there is one on-screen render path, one
# export path and one thumbnail path - exactly the arrangement epi_plot.R has.

box::use(
  ComplexHeatmap,
  DBI[dbConnect, dbDisconnect, dbGetQuery, dbListTables],
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
  grid[gpar, grid.grabExpr, unit],
  RSQLite[SQLite],
  stats[dist, hclust, setNames],
)
box::use(
  # The palette resolver and the cardinality-aware scale filter are not epi
  # curve specific - they map any set of category levels onto a ColorBrewer /
  # viridis scale and drop the palettes too narrow to carry them. Reused rather
  # than re-implemented; see their comments in epi_plot.R for the reasoning.
  app / logic / epi_plot[epi_fit_scale, epi_palette, epi_scale_choices],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- vocabulary --------------------------------------------------------------

# AMRFinderPlus element types, in the order they should read on a plot:
# acquired resistance first, then virulence, then the stress/metal/biocide
# elements `--plus` adds. Exported so the view's filter offers exactly these
# rather than whatever the loaded database happens to contain.
#' @export
AMR_ELEMENT_TYPES <- c(
  Resistance = "AMR",
  Virulence = "VIRULENCE",
  Stress = "STRESS"
)

# abritamr's three summary sections. `matches` is a confident call, `partials` a
# partial one; `virulence` is a separate axis rather than a confidence level.
#' @export
AMR_SECTIONS <- c(
  Matches = "matches",
  Partials = "partials",
  Virulence = "virulence"
)

# Cell states of the drug-class matrix, ordered by increasing confidence. The
# matrix stores the rank; these are what the heatmap draws and legends.
#' @export
AMR_CLASS_STATES <- c("Absent", "Partial", "Match")

# Cell states of the gene heatmap.
#' @export
AMR_PRESENCE_STATES <- c("Absent", "Present")

# Group label for genes AMRFinderPlus reported without a drug class.
#' @export
AMR_UNCLASSIFIED <- "Unclassified"

# Linkage methods offered for the dendrograms. "average" is master's default and
# stays the default here.
#' @export
AMR_CLUSTER_METHODS <- c(
  Average = "average",
  Complete = "complete",
  Single = "single",
  `Ward D2` = "ward.D2",
  Centroid = "centroid"
)

# Distance measures. Binary (Jaccard) is the right default for presence/absence
# and is what master clustered on.
#' @export
AMR_CLUSTER_DISTANCES <- c(
  Binary = "binary",
  Euclidean = "euclidean",
  Manhattan = "manhattan"
)

# Re-exported so the view can restrict a colour-scale picker to the palettes
# that can actually carry the number of categories currently mapped, without
# importing epi_plot itself.
#' @export
amr_scale_choices <- function(n) epi_scale_choices(n)

#' @export
amr_fit_scale <- function(scale, n) epi_fit_scale(scale, n)

#' @export
amr_palette <- function(cats, scale) epi_palette(cats, scale)

# --- database reads ----------------------------------------------------------

.EMPTY_HITS <- data.frame(
  souche = character(0),
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
  souche = character(0),
  section = character(0),
  drug_class = character(0),
  genes = character(0),
  stringsAsFactors = FALSE
)

# A readable database path, or FALSE. Every reader below opens its own
# connection, so this is the one place the argument is validated.
.usable_path <- function(db_path) {
  !is.null(db_path) &&
    length(db_path) == 1 &&
    !is.na(db_path) &&
    nzchar(db_path) &&
    file.exists(db_path)
}

#' Whether this database holds any AMR screening at all. Cheap (a table listing,
#' no row read), so the view can gate its empty state on it before doing work.
#' @export
has_amr_data <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(FALSE)
  }
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)
  if (!"amr_results" %in% tables) {
    return(FALSE)
  }
  nrow(dbGetQuery(con, "SELECT 1 FROM amr_results LIMIT 1")) > 0
}

#' Every detected AMR / virulence / stress element, one row each.
#'
#' The granular half of the screen, straight out of `amr_results`. Returns an
#' empty frame of the right shape when the database has no such table or it
#' holds no rows, so callers never have to special-case a database that was
#' typed without screening.
#' @export
load_amr_hits <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(.EMPTY_HITS)
  }
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_results" %in% dbListTables(con)) {
    return(.EMPTY_HITS)
  }
  hits <- dbGetQuery(
    con,
    "SELECT souche, gene_symbol, element_type, element_subtype, class, subclass,
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

#' abritamr's curated per-isolate rollup, long.
#'
#' `load_amr()` in database_functions.R pivots this same table wide (one column
#' per drug class, genes comma-joined) for the browse table. The plots need the
#' tidy shape it was stored in, so this reads the table directly rather than
#' un-pivoting that result.
#' @export
load_amr_sections <- function(db_path) {
  if (!.usable_path(db_path)) {
    return(.EMPTY_SECTIONS)
  }
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  if (!"amr_summary" %in% dbListTables(con)) {
    return(.EMPTY_SECTIONS)
  }
  sections <- dbGetQuery(
    con,
    "SELECT souche, section, drug_class, genes FROM amr_summary ORDER BY id"
  )
  if (!nrow(sections)) {
    return(.EMPTY_SECTIONS)
  }
  sections$drug_class <- as.character(sections$drug_class)
  sections[
    !is.na(sections$drug_class) & nzchar(sections$drug_class), ,
    drop = FALSE
  ]
}

# --- filtering ---------------------------------------------------------------

#' Restrict hits to the element types kept and to hits that clear the identity
#' and coverage floors.
#'
#' AMRFinderPlus reports partial and low-identity hits alongside confident ones;
#' `amr_results` keeps the percentages so the reader can decide. Rows whose
#' percentages are NA (point mutations report neither) are kept - a floor is a
#' filter on what was measured, not a demand that everything be measured.
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
    keep <- keep & (is.na(hits$pct_identity) | hits$pct_identity >= min_identity)
  }
  if (isTRUE(min_coverage > 0)) {
    keep <- keep & (is.na(hits$pct_coverage) | hits$pct_coverage >= min_coverage)
  }
  hits[keep, , drop = FALSE]
}

# The drug class a gene belongs to, as one readable label. AMRFinderPlus leaves
# `class` empty for virulence and for some stress elements, which is not an
# error - those genes group under their element type instead.
.gene_group <- function(element_type, class) {
  cls <- trimws(as.character(class %||% NA))
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

#' Per-gene metadata: element type and drug-class group, one row per gene, in
#' the order given.
#'
#' This is the replacement for master's `get.gsMeta()`, which rebuilt the same
#' gene -> class map out of a separate `AMR_Profile.rds` sidecar. Here it comes
#' straight off the rows the genes were detected in.
#' @export
amr_gene_meta <- function(hits, genes) {
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

#' Gene picker choices, grouped by drug class within element type.
#'
#' The same structure master built by hand as `choices_amr` / `choices_vir` /
#' `choices_noclass`, except the grouping comes from `amr_results` rather than
#' from the classification frames of an RDS sidecar. Groups and the genes inside
#' them are sorted; understood by both `selectInput()` and `pickerInput()`.
#' @export
amr_gene_choices <- function(hits) {
  if (is.null(hits) || !nrow(hits)) {
    return(list())
  }
  genes <- sort(unique(hits$gene_symbol))
  meta <- amr_gene_meta(hits, genes)

  # Element type leads the group label so the picker keeps resistance,
  # virulence and stress genes visually apart even when two of them happen to
  # carry the same class name.
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

# --- matrices ----------------------------------------------------------------

#' Isolates x genes presence/absence, as a 0/1 integer matrix.
#'
#' Rows are exactly `isolates`, in the order given, so an isolate that was
#' screened and had no hits keeps its (all-zero) row - "we looked and found
#' nothing" is a result, and dropping it would silently shrink the plot.
#'
#' Columns default to every gene these isolates carry, so leaving `genes` unset
#' already excludes anything only ever detected elsewhere. `drop_empty` (TRUE by
#' default) is what matters when the caller *does* pass a gene list - the view's
#' picker offers every gene in the database, so narrowing the isolate selection
#' would otherwise leave all-zero columns behind. Those carry no information,
#' and - because binary distance between two all-zero vectors is 0/0 - they are
#' also what would put NaNs into the column dendrogram.
#'
#' The per-gene metadata (element type, drug-class group) rides along in the
#' `"genes"` attribute, in column order, for the heatmap's column split and its
#' class annotation.
#' @export
amr_presence_matrix <- function(
  hits,
  isolates,
  genes = NULL,
  drop_empty = TRUE
) {
  isolates <- unique(as.character(isolates))
  isolates <- isolates[!is.na(isolates) & nzchar(isolates)]

  present <- if (is.null(hits) || !nrow(hits)) {
    .EMPTY_HITS
  } else {
    hits[hits$souche %in% isolates, , drop = FALSE]
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
  if (nrow(present) && length(genes) && length(isolates)) {
    present <- present[present$gene_symbol %in% genes, , drop = FALSE]
    if (nrow(present)) {
      mat[cbind(
        match(present$souche, isolates),
        match(present$gene_symbol, genes)
      )] <- 1L
    }
  }

  if (isTRUE(drop_empty) && ncol(mat)) {
    keep <- colSums(mat) > 0L
    mat <- mat[, keep, drop = FALSE]
    genes <- genes[keep]
  }

  attr(mat, "genes") <- amr_gene_meta(hits %||% .EMPTY_HITS, genes)
  mat
}

# Confidence rank of an abritamr section. Virulence is not a weaker call than a
# match, it is a different question - so it ranks as a detection, and the
# virulence groups are told apart by the column flag instead.
.section_rank <- function(section) {
  ifelse(section == "partials", 1L, ifelse(is.na(section), 0L, 2L))
}

#' Isolates x drug-class matrix, cells ranked by call confidence.
#'
#' 0 = nothing reported, 1 = a partial hit only, 2 = a confident (`matches`) hit
#' or a virulence group. Where an isolate has both a partial and a confident hit
#' for the same class, the confident one wins - the cell answers "what is the
#' best evidence here", and a partial alongside a match adds nothing.
#'
#' The `"virulence"` attribute flags the columns that only ever came from the
#' virulence section, so the heatmap can split them off from the resistance
#' classes.
#' @export
amr_class_matrix <- function(
  sections,
  isolates,
  keep_sections = NULL,
  drop_empty = TRUE
) {
  isolates <- unique(as.character(isolates))
  isolates <- isolates[!is.na(isolates) & nzchar(isolates)]

  rows <- if (is.null(sections) || !nrow(sections)) {
    .EMPTY_SECTIONS
  } else {
    sections[sections$souche %in% isolates, , drop = FALSE]
  }
  if (!is.null(keep_sections) && length(keep_sections)) {
    rows <- rows[rows$section %in% keep_sections, , drop = FALSE]
  }

  classes <- sort(unique(rows$drug_class))
  mat <- matrix(
    0L,
    nrow = length(isolates),
    ncol = length(classes),
    dimnames = list(isolates, classes)
  )
  if (nrow(rows) && length(classes) && length(isolates)) {
    ri <- match(rows$souche, isolates)
    ci <- match(rows$drug_class, classes)
    rank <- .section_rank(rows$section)
    # pmax against what is already there: an isolate can appear in several
    # sections for one class, and the strongest call is the one that shows.
    for (i in seq_along(ri)) {
      mat[ri[i], ci[i]] <- max(mat[ri[i], ci[i]], rank[i])
    }
  }

  if (isTRUE(drop_empty) && ncol(mat)) {
    keep <- colSums(mat) > 0L
    mat <- mat[, keep, drop = FALSE]
    classes <- classes[keep]
  }

  virulence <- vapply(
    classes,
    function(cl) {
      secs <- unique(rows$section[rows$drug_class == cl])
      length(secs) > 0 && all(secs == "virulence")
    },
    logical(1),
    USE.NAMES = FALSE
  )
  attr(mat, "virulence") <- virulence
  mat
}

# --- prevalence --------------------------------------------------------------

#' How many of the selected isolates carry each gene (or each drug class).
#'
#' `level = "gene"` counts distinct isolates per gene symbol from `hits`, grouped
#' by element type. `level = "class"` counts distinct isolates per drug class
#' from `sections`, grouped by the strongest section that class was called in.
#' Ranked by count and truncated to `top_n`, because a full screen easily
#' reports several hundred genes and a bar chart of all of them is unreadable.
#' @export
amr_prevalence <- function(
  hits,
  sections,
  isolates,
  level = "gene",
  top_n = 30L
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

  if (identical(level, "class")) {
    rows <- if (is.null(sections) || !nrow(sections)) {
      .EMPTY_SECTIONS
    } else {
      sections[sections$souche %in% isolates, , drop = FALSE]
    }
    if (!nrow(rows)) {
      return(empty)
    }
    item <- rows$drug_class
    # The strongest section a class was called in labels its bar, so a class
    # only ever seen as a partial does not read as a confident finding.
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
    souche <- rows$souche
  } else {
    rows <- if (is.null(hits) || !nrow(hits)) {
      .EMPTY_HITS
    } else {
      hits[hits$souche %in% isolates, , drop = FALSE]
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
    souche <- rows$souche
  }

  counts <- vapply(
    names(group),
    function(it) length(unique(souche[item == it])),
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

# --- sizing ------------------------------------------------------------------

#' Label size that keeps `n` row or column labels legible without overlapping.
#'
#' Master's step function, kept as the default. Unlike master, the view offers an
#' override - the steps stop helping somewhere past a couple of hundred labels,
#' and at that point the reader wants to make the call themselves.
#' @export
amr_fit_fontsize <- function(n) {
  breaks <- c(10, 20, 30, 50, 80, 120, 160, 200)
  sizes <- c(14, 12, 11, 10, 9, 8, 7, 6, 5)
  sizes[[sum(n >= breaks) + 1L]]
}

# --- heatmaps ----------------------------------------------------------------

# A dendrogram for one margin, or FALSE when clustering is off or impossible.
#
# The dendrograms are computed here rather than left to ComplexHeatmap because
# both heatmaps hand it a *character* matrix (so the legend is a discrete
# Present/Absent or Absent/Partial/Match key rather than a meaningless 0-1
# ramp), and ComplexHeatmap can only cluster a numeric one. Binary distance
# between two all-zero rows is 0/0 = NaN, which hclust refuses; two isolates
# with nothing detected are identical, so those become 0.
.dendrogram <- function(mat, enable, distance, method) {
  if (!isTRUE(enable) || nrow(mat) < 3) {
    return(FALSE)
  }
  d <- dist(mat, method = distance)
  d[!is.finite(d)] <- 0
  tryCatch(hclust(d, method = method), error = function(e) FALSE)
}

# Column-side dendrogram; same guard, transposed.
.column_dendrogram <- function(mat, enable, distance, method) {
  if (!isTRUE(enable) || ncol(mat) < 3) {
    return(FALSE)
  }
  .dendrogram(t(mat), TRUE, distance, method)
}

# The row colour strip: an isolate-level metadata field drawn beside the
# heatmap. `values` is a named vector keyed by isolate (the view resolves it
# from viz_metadata); isolates the field has nothing for read as "NA" rather
# than being dropped.
.row_annotation <- function(mat, values, label, scale, text_color, legend_gp) {
  if (is.null(values) || !length(values) || !nzchar(label %||% "")) {
    return(NULL)
  }
  vals <- as.character(values[rownames(mat)])
  vals[is.na(vals) | !nzchar(vals)] <- "NA"
  cats <- sort(unique(vals))
  cols <- amr_palette(cats, amr_fit_scale(scale, length(cats)))

  args <- list(
    show_annotation_name = TRUE,
    annotation_label = label,
    annotation_name_gp = gpar(col = text_color),
    annotation_legend_param = list(
      title = label,
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    ),
    col = list(cols)
  )
  names(args$col) <- label
  args[[label]] <- vals
  do.call(ComplexHeatmap$rowAnnotation, args)
}

# The drug-class colour strip above the heatmap's columns.
.class_annotation <- function(groups, scale, text_color, legend_gp) {
  cats <- sort(unique(groups))
  cols <- amr_palette(cats, amr_fit_scale(scale, length(cats)))
  ComplexHeatmap$HeatmapAnnotation(
    Class = groups,
    col = list(Class = cols),
    show_annotation_name = FALSE,
    annotation_legend_param = list(
      title = "Drug class",
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    )
  )
}

# Font gpars shared by every legend and title on a heatmap.
.legend_gp <- function(text_color, size) {
  list(
    labels = gpar(col = text_color, fontsize = size),
    title = gpar(col = text_color, fontsize = size + 2)
  )
}

# How the columns are arranged. Splitting and clustering are mutually exclusive
# on the column axis: ComplexHeatmap refuses a supplied dendrogram alongside a
# categorical split (it has no way to reconcile one tree with several slices),
# so the view offers this as a single "Group columns by" choice rather than two
# controls that would silently fight. Returns the split factor (or NULL) and the
# cluster argument.
.column_layout <- function(mat, grouping, meta, distance, method) {
  none <- list(split = NULL, cluster = FALSE)
  if (!ncol(mat)) {
    return(none)
  }
  switch(
    grouping %||% "element",
    cluster = list(
      split = NULL,
      cluster = .column_dendrogram(mat, TRUE, distance, method)
    ),
    element = list(
      split = factor(
        meta$element_type,
        levels = intersect(
          c(AMR_ELEMENT_TYPES, AMR_UNCLASSIFIED),
          unique(meta$element_type)
        )
      ),
      cluster = FALSE
    ),
    class = list(
      split = factor(meta$group, levels = sort(unique(meta$group))),
      cluster = FALSE
    ),
    none
  )
}

#' The gene heatmap: isolates x genes, presence/absence.
#'
#' The direct successor to master's `gs_plot`. Returns a ComplexHeatmap object;
#' pass it through `amr_as_ggplot()` to render or export it.
#'
#' `opts` (all optional): present_color, absent_color, text_color, grid_color,
#' grid_width, column_grouping (element / class / cluster / none), cluster_rows,
#' cluster_distance, cluster_method, dend_row, dend_col (cm), fontsize_row,
#' fontsize_col, fontsize_title, fontsize_legend, show_row_names, dend_color,
#' show_class_anno, class_scale, anno_values, anno_label, anno_scale.
#' @export
build_amr_heatmap <- function(mat, opts = list()) {
  meta <- attr(mat, "genes")
  text_color <- opts$text_color %||% "#000000"
  legend_gp <- .legend_gp(text_color, opts$fontsize_legend %||% 9)

  display <- matrix(
    AMR_PRESENCE_STATES[mat + 1L],
    nrow = nrow(mat),
    ncol = ncol(mat),
    dimnames = dimnames(mat)
  )

  layout <- .column_layout(
    mat,
    opts$column_grouping,
    meta,
    opts$cluster_distance %||% "binary",
    opts$cluster_method %||% "average"
  )

  ComplexHeatmap$Heatmap(
    display,
    name = "Gene",
    col = setNames(
      c(opts$absent_color %||% "#EFEFEF", opts$present_color %||% "#66C2A5"),
      AMR_PRESENCE_STATES
    ),
    rect_gp = gpar(
      col = opts$grid_color %||% "#FFFFFF",
      lwd = opts$grid_width %||% 1
    ),
    column_title_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_title %||% 14
    ),
    row_title = NULL,
    row_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_row %||% amr_fit_fontsize(nrow(mat))
    ),
    column_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_col %||% amr_fit_fontsize(ncol(mat))
    ),
    show_row_names = !isFALSE(opts$show_row_names),
    cluster_rows = .dendrogram(
      mat,
      opts$cluster_rows %||% TRUE,
      opts$cluster_distance %||% "binary",
      opts$cluster_method %||% "average"
    ),
    cluster_columns = layout$cluster,
    column_split = layout$split,
    row_dend_width = unit(opts$dend_row %||% 2, "cm"),
    column_dend_height = unit(opts$dend_col %||% 2, "cm"),
    row_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    column_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    top_annotation = if (isTRUE(opts$show_class_anno) && ncol(mat)) {
      .class_annotation(meta$group, opts$class_scale, text_color, legend_gp)
    },
    left_annotation = .row_annotation(
      mat,
      opts$anno_values,
      opts$anno_label,
      opts$anno_scale,
      text_color,
      legend_gp
    ),
    heatmap_legend_param = list(
      title = "Gene",
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    )
  )
}

#' The drug-class matrix: isolates x drug class, cells by call confidence.
#'
#' Coarser than the gene heatmap and readable with far more isolates, and it is
#' the only view that shows abritamr's partial/confident distinction. Same
#' `opts` as `build_amr_heatmap()`, plus partial_color; the class columns carry
#' no per-gene metadata, so column_grouping only offers cluster / none here (a
#' virulence split is applied automatically when both kinds of column exist).
#' @export
build_amr_class_heatmap <- function(mat, opts = list()) {
  text_color <- opts$text_color %||% "#000000"
  legend_gp <- .legend_gp(text_color, opts$fontsize_legend %||% 9)
  virulence <- attr(mat, "virulence") %||% rep(FALSE, ncol(mat))

  display <- matrix(
    AMR_CLASS_STATES[mat + 1L],
    nrow = nrow(mat),
    ncol = ncol(mat),
    dimnames = dimnames(mat)
  )

  cluster_columns <- identical(opts$column_grouping, "cluster")
  # Resistance classes and virulence groups answer different questions, so they
  # are drawn as separate blocks whenever both are present - unless the reader
  # asked for a column dendrogram, which cannot coexist with a split.
  split <- if (!cluster_columns && any(virulence) && !all(virulence)) {
    factor(
      ifelse(virulence, "Virulence", "Resistance"),
      levels = c("Resistance", "Virulence")
    )
  }

  ComplexHeatmap$Heatmap(
    display,
    name = "Call",
    col = setNames(
      c(
        opts$absent_color %||% "#EFEFEF",
        opts$partial_color %||% "#E5C494",
        opts$present_color %||% "#66C2A5"
      ),
      AMR_CLASS_STATES
    ),
    rect_gp = gpar(
      col = opts$grid_color %||% "#FFFFFF",
      lwd = opts$grid_width %||% 1
    ),
    column_title_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_title %||% 14
    ),
    row_title = NULL,
    row_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_row %||% amr_fit_fontsize(nrow(mat))
    ),
    column_names_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_col %||% amr_fit_fontsize(ncol(mat))
    ),
    show_row_names = !isFALSE(opts$show_row_names),
    cluster_rows = .dendrogram(
      mat,
      opts$cluster_rows %||% TRUE,
      opts$cluster_distance %||% "binary",
      opts$cluster_method %||% "average"
    ),
    cluster_columns = if (cluster_columns) {
      .column_dendrogram(
        mat,
        TRUE,
        opts$cluster_distance %||% "binary",
        opts$cluster_method %||% "average"
      )
    } else {
      FALSE
    },
    column_split = split,
    row_dend_width = unit(opts$dend_row %||% 2, "cm"),
    column_dend_height = unit(opts$dend_col %||% 2, "cm"),
    row_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    column_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    left_annotation = .row_annotation(
      mat,
      opts$anno_values,
      opts$anno_label,
      opts$anno_scale,
      text_color,
      legend_gp
    ),
    heatmap_legend_param = list(
      title = "Call",
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    )
  )
}

#' Wrap a ComplexHeatmap as a ggplot.
#'
#' The heatmap is drawn once into a grob and adopted by ggplot2, so the two
#' matrix views share the prevalence chart's render, export and thumbnail paths
#' instead of each needing their own device handling (which is what master did,
#' repeating a png/jpeg/svg/bmp block per format). The plot background is set
#' here rather than on the device, exactly as epi_plot.R does and for the same
#' reason: ggsave derives the device background from the theme, so the whole
#' frame takes the chosen colour.
#' @export
amr_as_ggplot <- function(ht, background = "#FFFFFF") {
  # ComplexHeatmap narrates its automatic colour mapping on stderr; the matrices
  # here are discrete, so there is nothing for it to say that a user needs.
  opt <- ComplexHeatmap$ht_opt
  opt$message <- FALSE
  grob <- grid.grabExpr(
    ComplexHeatmap$draw(ht, merge_legend = TRUE, background = "transparent")
  )
  as.ggplot(grob) +
    theme(plot.background = element_rect(fill = background, colour = NA))
}

# --- prevalence chart --------------------------------------------------------

#' Ranked prevalence bars, as a ggplot.
#'
#' The one view that answers "what is common in this collection" rather than
#' "what does each isolate carry" - and the only one that stays readable when a
#' screen turns up several hundred genes, because it is truncated to the top n.
#' @export
build_amr_prevalence <- function(df, opts = list()) {
  text_color <- opts$text_color %||% "#000000"
  background <- opts$background %||% "#FFFFFF"
  n_iso <- opts$n_isolates %||% NA_integer_

  cats <- sort(unique(df$group))
  cols <- amr_palette(cats, amr_fit_scale(opts$bar_scale, length(cats)))

  # Ranked descending: coord_flip puts the first factor level at the bottom, so
  # the levels go up the ranking for the largest bar to land at the top.
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
    theme_minimal(base_size = opts$fontsize_legend %||% 11) +
    theme(
      text = element_text(colour = text_color),
      axis.text = element_text(
        colour = text_color,
        size = opts$fontsize_row %||% amr_fit_fontsize(nrow(df))
      ),
      axis.title = element_text(colour = text_color),
      legend.text = element_text(colour = text_color),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = background, colour = NA),
      panel.background = element_rect(fill = background, colour = NA)
    )
}

# --- export ------------------------------------------------------------------

#' Render a plot to a PNG at an exact pixel size, for the on-screen view.
#'
#' `width_px`/`height_px` are the CSS pixels the image occupies; `res` is the dpi
#' the layout is measured against (keeping text at its intended point size) and
#' `scale` multiplies the pixels actually rendered - pass the browser's
#' devicePixelRatio so the image stays crisp on HiDPI screens while laying out
#' identically. Mirrors `render_epi_png()`.
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

#' Save the current plot in one of the offered formats. Mirrors
#' `save_epi_plot()` / `save_tree_plot()`.
#' @export
save_amr_plot <- function(
  plot,
  file,
  filetype = "png",
  aspect_ratio = 0.65,
  dpi = 192
) {
  width <- 12
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
