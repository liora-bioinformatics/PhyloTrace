# app/logic/amr_plot.R
#
# Data transformation and plot construction for AMR screening outputs.
# Generates ggplot2-compatible objects for heatmap and prevalence visualizations.

box::use(
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
  grid[gpar, grid.grabExpr, unit],
  stats[dist, hclust, setNames],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / epi_plot[epi_fit_scale, epi_palette, epi_scale_choices],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

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

#' @export
AMR_CLASS_STATES <- c("Absent", "Partial", "Match")

#' @export
AMR_PRESENCE_STATES <- c("Absent", "Present")

#' @export
AMR_UNCLASSIFIED <- "Unclassified"

#' @export
AMR_CLUSTER_METHODS <- c(
  Average = "average",
  Complete = "complete",
  Single = "single",
  `Ward D2` = "ward.D2",
  Centroid = "centroid"
)

#' @export
AMR_CLUSTER_DISTANCES <- c(
  Binary = "binary",
  Euclidean = "euclidean",
  Manhattan = "manhattan"
)

#' @export
amr_scale_choices <- function(n) epi_scale_choices(n)

#' @export
amr_fit_scale <- function(scale, n) epi_fit_scale(scale, n)

#' @export
amr_palette <- function(cats, scale) epi_palette(cats, scale)

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

#' Generates a gene metadata mapping table containing element types and functional groups.
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

#' Formats gene list into grouped select input choices structured by element type and drug class.
#' @export
amr_gene_choices <- function(hits) {
  if (is.null(hits) || !nrow(hits)) {
    return(list())
  }
  genes <- sort(unique(hits$gene_symbol))
  meta <- amr_gene_meta(hits, genes)

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

#' Constructs a binary (0/1) presence/absence matrix (isolates x genes).
#' Attaches gene metadata attributes to support heatmap annotations.
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
  if (nrow(present) && length(genes) && length(isolates)) {
    present <- present[present$gene_symbol %in% genes, , drop = FALSE]
    if (nrow(present)) {
      mat[cbind(
        match(present$isolate, isolates),
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

.section_rank <- function(section) {
  ifelse(section == "partials", 1L, ifelse(is.na(section), 0L, 2L))
}

#' Constructs a ranked confidence matrix (0 = Absent, 1 = Partial, 2 = Match/Virulence).
#' Takes the highest confidence rank when duplicate hits exist per cell.
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
    sections[sections$isolate %in% isolates, , drop = FALSE]
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
    ri <- match(rows$isolate, isolates)
    ci <- match(rows$drug_class, classes)
    rank <- .section_rank(rows$section)
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

# --- Prevalence Calculations --------------------------------------------------

#' Computes gene or drug-class occurrence counts across selected isolates.
#' Returns top `top_n` items ordered by prevalence.
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
      sections[sections$isolate %in% isolates, , drop = FALSE]
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

.legend_gp <- function(text_color, size) {
  list(
    labels = gpar(col = text_color, fontsize = size),
    title = gpar(col = text_color, fontsize = size + 2)
  )
}

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

# --- Heatmap Builders --------------------------------------------------------

#' Builds a ComplexHeatmap instance for gene presence/absence profiles.
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

#' Builds a ComplexHeatmap instance for drug-class call confidence matrices.
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

#' Converts a ComplexHeatmap object into a standard ggplot2 object.
#' @export
amr_as_ggplot <- function(ht, background = "#FFFFFF") {
  opt <- ComplexHeatmap$ht_opt
  opt$message <- FALSE
  grob <- grid.grabExpr(
    ComplexHeatmap$draw(ht, merge_legend = TRUE, background = "transparent")
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

  cats <- sort(unique(df$group))
  cols <- amr_palette(cats, amr_fit_scale(opts$bar_scale, length(cats)))

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

#' Saves a plot object to disk using a specified format and resolution.
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
