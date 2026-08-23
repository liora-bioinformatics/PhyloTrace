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
  grid[gpar, grid.grabExpr, unit],
  rlang[`%||%`],
  stats[dist, hclust, setNames],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / epi_plot[epi_fit_scale, epi_palette, epi_scale_choices],
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

#' @export
AMR_CLASS_STATES <- c("Absent", "Partial", "Match")

#' @export
AMR_PRESENCE_STATES <- c("Absent", "Present")

#' @export
AMR_UNCLASSIFIED <- "Unclassified"

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
#' Measured rather than assumed. Over this repository's two screened databases
#' (250 P. aeruginosa x 32 genes, 29 A. baumannii x 57 genes), the best mean
#' silhouette width reachable in Jaccard space was:
#'
#'   axis        binary   euclidean   manhattan
#'   isolates     0.978      0.980       0.980
#'   genes        0.660      0.519       0.495
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
#'
#' Returns top `top_n` items ordered by prevalence.
#'
#' @param hits Gene hits, for `level = "gene"`.
#' @param sections abritamr rollup rows, for `level = "class"`.
#' @param isolates Character vector of isolates the count covers.
#' @param level Either "gene" or "class".
#' @param top_n Integer. Items to keep; 0 or less keeps every one.
#' @param keep_sections Call sections to count, or NULL for every one. Applies
#'   at class level only — a gene hit carries no section.
#' @return A data frame of `item`, `group`, `n` and `frac`.
#' @export
amr_prevalence <- function(
  hits,
  sections,
  isolates,
  level = "gene",
  top_n = 30L,
  keep_sections = NULL
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
AMR_ASPECT_MIN <- 0.35
AMR_ASPECT_MAX <- 2.6

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

.clamp <- function(x, lo, hi) min(max(x, lo), hi)

.pt_in <- function(pt) pt / 72

#' Fit every size in an AMR heatmap to the shape of its matrix.
#'
#' @param n_rows Integer. Isolates the heatmap draws.
#' @param n_cols Integer. Genes or drug classes it draws.
#' @param width_in Numeric. Canvas width in inches.
#' @param show_row_names Logical. Whether isolate names are drawn.
#' @param row_label_chars Numeric. Longest isolate name, in characters.
#' @param col_label_chars Numeric. Longest column name, in characters.
#' @param block_titles Character vector of column-block titles, or NULL.
#' @param block_cols Integer vector of columns per block, aligned to
#'   `block_titles`.
#' @param dend_cm Numeric. Dendrogram depth the reader asked for, in cm.
#' @param n_strips Integer. Annotation strips drawn beside the rows.
#' @return A list of fitted sizes, plus `title_rot` and `legible`.
#' @export
amr_auto_layout <- function(
  n_rows,
  n_cols,
  width_in = 9,
  show_row_names = FALSE,
  row_label_chars = 12,
  col_label_chars = 12,
  block_titles = NULL,
  block_cols = NULL,
  dend_cm = 1.5,
  n_strips = 0L
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
  body_w <- w * AMR_BODY_FRAC - n_strips * 0.12
  body_w <- max(body_w, w * 0.25)
  cell_w <- body_w / n_cols

  # Column labels are always drawn rotated (a horizontal gene name is wider
  # than any cell it could sit over), so their size is the cell width and the
  # room they need is vertical.
  fontsize_col <- .clamp(72 * cell_w * AMR_LABEL_FILL, AMR_FONT_MIN, AMR_FONT_MAX)
  fontsize_title <- .clamp(fontsize_col + 3, 8, 16)
  # The legend belongs to the page, not to the matrix: it has the same number of
  # keys whether the screen found six genes or six hundred.
  fontsize_legend <- .clamp(w * 1.05, 7, 11)

  rot <- .title_rotation(block_titles, block_cols, cell_w, fontsize_title)

  # Everything stacked above the body: the rotated column labels, the column
  # dendrogram, the block titles and the plot's own margins. Subtracted from the
  # canvas before the rows get their share, so a screen with long gene names
  # does not lose the room to draw them.
  title_in <- if (identical(rot, 90)) {
    .pt_in(fontsize_title) * AMR_CHAR_EM * max(nchar(block_titles %||% ""), 0)
  } else {
    .pt_in(fontsize_title) * 2
  }
  overhead <- .pt_in(fontsize_col) * AMR_CHAR_EM * max(col_label_chars, 1) +
    max(dend_cm, 0) / 2.54 +
    title_in +
    0.5

  # Square cells where the matrix is small enough to allow it, the target pitch
  # where it is not.
  row_in <- if (isTRUE(show_row_names)) AMR_ROW_IN_LABELLED else AMR_ROW_IN_PLAIN
  row_h <- .clamp(cell_w, row_in, AMR_ROW_IN_MAX)

  aspect <- .clamp(
    (n_rows * row_h + overhead) / w,
    AMR_ASPECT_MIN,
    AMR_ASPECT_MAX
  )
  # What the clamp actually left for the body, which is the pitch the row
  # labels have to fit inside — not the pitch that was asked for.
  pitch <- max(aspect * w - overhead, 0.2) / n_rows
  fontsize_row <- .clamp(
    72 * pitch * AMR_LABEL_FILL,
    AMR_FONT_MIN,
    AMR_FONT_MAX
  )
  # A name wider than the room kept for it is the other way this fails.
  fontsize_row <- min(
    fontsize_row,
    .clamp(
      72 * (w - body_w) * 0.45 / (AMR_CHAR_EM * max(row_label_chars, 1)),
      AMR_FONT_MIN,
      AMR_FONT_MAX
    )
  )

  list(
    aspect = round(aspect, 2),
    fontsize_row = round(fontsize_row, 1),
    fontsize_col = round(fontsize_col, 1),
    fontsize_title = round(fontsize_title, 1),
    fontsize_legend = round(fontsize_legend, 1),
    grid_width = if (min(cell_w, pitch) < AMR_GRID_MIN_IN) 0 else 0.5,
    title_rot = rot,
    # Whether the row labels came out at a size worth drawing. The view uses
    # this to say so rather than to override the switch.
    legible = fontsize_row > AMR_FONT_MIN,
    row_pitch_in = round(pitch, 4),
    cell_width_in = round(cell_w, 4)
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

# One mapped variable as a strip beside the rows. Discrete variables get a
# tabulated palette keyed on their categories; a continuous one gets a ramp
# across its own range, because a colour per distinct value is what a collection
# date looked like before the mapping layers arrived.
#
# Isolates the variable is empty for are labelled "NA" rather than dropped: the
# row still exists in the matrix, and a gap in the strip beside a present row is
# read as a rendering fault.
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
  chr[is.na(chr) | !nzchar(chr)] <- "NA"
  cats <- sort(unique(chr))
  list(
    label = label,
    values = chr,
    col = amr_palette(cats, amr_fit_scale(layer$palette, length(cats)))
  )
}

# Every mapped variable as one rowAnnotation. Several strips have to travel in a
# single annotation object rather than as several: ComplexHeatmap takes exactly
# one `left_annotation`, and stacking them any other way puts the second on the
# opposite side of the matrix from the first.
.row_annotation <- function(mat, layers, text_color, legend_gp) {
  layers <- Filter(
    function(l) length(l$values) && nzchar(l$label %||% l$field %||% ""),
    layers %||% list()
  )
  if (!length(layers)) {
    return(NULL)
  }
  specs <- lapply(layers, .strip_spec, mat = mat)
  # Two mappings of the same variable would collide on the name ComplexHeatmap
  # keys the strip by, and the second would silently replace the first.
  labels <- make.unique(vapply(specs, function(x) x$label, character(1)))

  args <- list(
    show_annotation_name = TRUE,
    annotation_name_gp = gpar(col = text_color, fontsize = legend_gp$size),
    annotation_name_side = "top",
    annotation_name_rot = 90,
    col = setNames(lapply(specs, function(x) x$col), labels),
    annotation_legend_param = setNames(
      lapply(labels, function(nm) {
        list(
          title = nm,
          labels_gp = legend_gp$labels,
          title_gp = legend_gp$title
        )
      }),
      labels
    )
  )
  for (i in seq_along(specs)) {
    args[[labels[[i]]]] <- specs[[i]]$values
  }
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
    size = size,
    labels = gpar(col = text_color, fontsize = size),
    title = gpar(col = text_color, fontsize = size + 2)
  )
}

# How one panel's columns are arranged *inside* the panel. Element type is not
# among the options: a screen covering more than one element type is drawn as
# one panel per type (see `.element_blocks`), so that separation is structural
# rather than a grouping the reader can turn off. "element" survives as an
# accepted value only because saved analyses carry it.
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
      cluster = FALSE
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
  types <- .element_order(meta)
  labels <- vapply(
    types,
    function(et) names(AMR_ELEMENT_TYPES)[match(et, AMR_ELEMENT_TYPES)] %||% et,
    character(1),
    USE.NAMES = FALSE
  )
  # Grouped by drug class, the titles a reader actually sees are the class names
  # inside each panel — those are the ones that collide, so those are the ones
  # the fit is asked about.
  if (identical(grouping, "class")) {
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
  list(
    titles = labels,
    cols = vapply(types, function(et) sum(meta$element_type == et), integer(1))
  )
}

# --- Heatmap Builders --------------------------------------------------------

# One panel of the gene heatmap: the columns of a single element type, with
# their own column arrangement and their own dendrogram.
#
# Only the first panel carries the row dendrogram, the annotation strips and the
# fill legend, and only the last carries the isolate names — repeated on every
# panel they are the same information three times, and ComplexHeatmap takes the
# row order from the first panel regardless, so the others must not cluster.
.gene_panel <- function(
  mat,
  meta,
  opts,
  legend_gp,
  title,
  first,
  last,
  row_cluster,
  anno
) {
  text_color <- opts$text_color %||% "#000000"
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
    opts$col_cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
    opts$col_cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
  )
  dend_cm <- max(opts$dend_size %||% 1.5, 0)

  args <- list(
    display,
    name = if (first) "Gene" else paste0("Gene.", title),
    col = setNames(
      c(opts$absent_color %||% "#EFEFEF", opts$present_color %||% "#000000"),
      AMR_PRESENCE_STATES
    ),
    rect_gp = gpar(
      col = opts$grid_color %||% "#FFFFFF",
      lwd = opts$grid_width %||% 0.5
    ),
    column_title_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_title %||% 12
    ),
    column_title_rot = opts$title_rot %||% 0,
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
    cluster_rows = if (first) row_cluster else FALSE,
    show_row_dend = first && dend_cm > 0 && !isFALSE(row_cluster),
    cluster_columns = layout$cluster,
    show_column_dend = dend_cm > 0,
    column_split = layout$split,
    row_dend_width = unit(max(dend_cm, 0.1), "cm"),
    column_dend_height = unit(max(dend_cm, 0.1), "cm"),
    row_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    column_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    top_annotation = if (isTRUE(opts$show_class_anno) && ncol(mat)) {
      .class_annotation(meta$group, opts$class_scale, text_color, legend_gp)
    },
    left_annotation = if (first) anno,
    show_heatmap_legend = first,
    heatmap_legend_param = list(
      title = "Gene",
      labels_gp = legend_gp$labels,
      title_gp = legend_gp$title
    )
  )
  # `column_title` is *omitted*, not set to NULL, when the columns are split by
  # drug class: left out, ComplexHeatmap titles each slice with its own split
  # level, which is the per-class heading the reader needs. Passing NULL is what
  # suppresses them, and passing the panel's element type instead would repeat
  # that one word over every class in the panel.
  if (!identical(opts$column_grouping, "class")) {
    args$column_title <- title
  }
  do.call(ComplexHeatmap$Heatmap, args)
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
  text_color <- opts$text_color %||% "#000000"
  legend_gp <- .legend_gp(text_color, opts$fontsize_legend %||% 9)

  # The row clustering is computed once, over the whole matrix, and handed to
  # the leading panel. Per-panel row dendrograms would each order the isolates
  # differently, and the panels have to share one row order to be read across.
  row_cluster <- .dendrogram(
    mat,
    opts$cluster_rows %||% TRUE,
    opts$cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
    opts$cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
  )
  anno <- .row_annotation(mat, opts$anno_layers, text_color, legend_gp)

  types <- if (is.null(meta) || !nrow(meta)) character(0) else .element_order(meta)
  if (length(types) < 2L) {
    return(.gene_panel(
      mat, meta, opts, legend_gp,
      title = NULL, first = TRUE, last = TRUE,
      row_cluster = row_cluster, anno = anno
    ))
  }

  panels <- lapply(seq_along(types), function(i) {
    keep <- meta$element_type == types[[i]]
    label <- names(AMR_ELEMENT_TYPES)[match(types[[i]], AMR_ELEMENT_TYPES)] %||%
      types[[i]]
    .gene_panel(
      mat[, keep, drop = FALSE],
      meta[keep, , drop = FALSE],
      opts,
      legend_gp,
      title = label,
      first = i == 1L,
      last = i == length(types),
      row_cluster = row_cluster,
      anno = anno
    )
  })
  Reduce(`+`, panels)
}

#' Builds a ComplexHeatmap instance for drug-class call confidence matrices.
#'
#' One panel only: the abritamr rollup has no per-gene metadata, so there is no
#' element type to separate it by. Resistance and virulence classes are still
#' split apart where both are present, which is the one distinction it does
#' carry.
#'
#' @param mat A class matrix from `amr_class_matrix()`.
#' @param opts Named list of display options.
#' @return A ComplexHeatmap `Heatmap`.
#' @export
build_amr_class_heatmap <- function(mat, opts = list()) {
  text_color <- opts$text_color %||% "#000000"
  legend_gp <- .legend_gp(text_color, opts$fontsize_legend %||% 9)
  virulence <- attr(mat, "virulence") %||% rep(FALSE, ncol(mat))
  dend_cm <- max(opts$dend_size %||% 1.5, 0)

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
  row_cluster <- .dendrogram(
    mat,
    opts$cluster_rows %||% TRUE,
    opts$cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
    opts$cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
  )

  ComplexHeatmap$Heatmap(
    display,
    name = "Call",
    col = setNames(
      c(
        opts$absent_color %||% "#EFEFEF",
        opts$partial_color %||% "#E5C494",
        opts$present_color %||% "#000000"
      ),
      AMR_CLASS_STATES
    ),
    rect_gp = gpar(
      col = opts$grid_color %||% "#FFFFFF",
      lwd = opts$grid_width %||% 0.5
    ),
    column_title_gp = gpar(
      col = text_color,
      fontsize = opts$fontsize_title %||% 12
    ),
    column_title_rot = opts$title_rot %||% 0,
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
    cluster_rows = row_cluster,
    show_row_dend = dend_cm > 0 && !isFALSE(row_cluster),
    cluster_columns = if (cluster_columns) {
      .column_dendrogram(
        mat,
        TRUE,
        opts$col_cluster_distance %||% AMR_CLUSTER_DISTANCE_DEFAULT,
        opts$col_cluster_method %||% AMR_CLUSTER_METHOD_DEFAULT
      )
    } else {
      FALSE
    },
    show_column_dend = dend_cm > 0,
    column_split = split,
    row_dend_width = unit(max(dend_cm, 0.1), "cm"),
    column_dend_height = unit(max(dend_cm, 0.1), "cm"),
    row_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    column_dend_gp = gpar(col = opts$dend_color %||% "#000000"),
    left_annotation = .row_annotation(
      mat,
      opts$anno_layers,
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

#' Converts a ComplexHeatmap object or list into a standard ggplot2 object.
#'
#' @param ht A `Heatmap` or `HeatmapList`.
#' @param background Hex colour for the whole frame.
#' @param gap_mm Numeric. Gutter between side-by-side panels, in millimetres.
#' @return A ggplot object.
#' @export
amr_as_ggplot <- function(ht, background = "#FFFFFF", gap_mm = 4) {
  opt <- ComplexHeatmap$ht_opt
  opt$message <- FALSE
  grob <- grid.grabExpr(
    ComplexHeatmap$draw(
      ht,
      merge_legend = TRUE,
      background = "transparent",
      # The gutter between one element type's panel and the next. Wide enough to
      # read as a break, narrow enough that the panels still read as one matrix.
      ht_gap = unit(max(gap_mm, 0), "mm"),
      # Legends sit at the top of the matrix rather than centred against it. A
      # screen of two hundred isolates is several screens tall, and a
      # vertically centred legend lands halfway down it, out of sight of the
      # plot it explains.
      align_heatmap_legend = "heatmap_top",
      align_annotation_legend = "heatmap_top"
    )
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

# File export goes through app/logic/viz_export.R's save_plot_export(), which
# owns the device settings for every plot type; what stays here is the on-screen
# render above, which is sized in pixels rather than in physical units.
