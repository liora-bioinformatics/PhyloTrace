# app/logic/field_profile.R
#
# Field profiling and level-counting utilities. Evaluates metadata column
# contents, data coverage, distinct cardinality, and declared schema types to
# guide variable selection across visualization modules.

box::use(
  stats[setNames],
)

box::use(
  app / logic / field_labels[field_label, GROUP_ORDER, group_of],
  app / logic / field_types[field_types, TYPE_LABELS],
)

# --- Level Counting Bounds ----------------------------------------------------

#' Maximum distinct color levels recommended for ColorBrewer qualitative palettes.
#' @export
MAX_QUAL_LEVELS <- 9L

#' Maximum discrete shape categories supported natively by ggplot2.
#' @export
MAX_SHAPE_LEVELS <- 6L

#' Minimum required data coverage threshold for default field selection.
#' @export
MIN_COVERAGE <- 0.5

#' Count Non-Empty Distinct Field Values
#'
#' @param values Vector of column values.
#' @return Integer count of non-NA, non-blank unique values.
#' @export
field_levels <- function(values) {
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(unique(v))
}

#' Calculate Non-Empty Value Coverage
#'
#' @param values Vector of column values.
#' @return Numeric proportion of non-NA, non-blank entries (0 to 1).
#' @export
field_coverage <- function(values) {
  if (!length(values)) {
    return(0)
  }
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(v) / length(values)
}

# Rank field suitability based on coverage band and distinct cardinality. A
# constant column groups (trivially, into one bucket) but carries no
# discriminating information, so it is ranked last among groupable fields
# rather than recommended alongside ones that actually vary.
.field_rank <- function(levels, coverage) {
  band <- if (levels <= 1L) {
    4L
  } else if (coverage < MIN_COVERAGE) {
    3L
  } else if (levels <= MAX_QUAL_LEVELS) {
    1L
  } else {
    2L
  }
  band + levels / (100 * max(levels, 1) + 1)
}

#' Recommend Best Grouping Fields for Visual Mapping
#'
#' Identifies candidate metadata columns that can partition isolates,
#' excluding only fields with no data or one distinct value per isolate (both
#' of which cannot group at all). A constant column is still included — it
#' groups trivially into a single bucket — but ranks last.
#'
#' @param metadata Data frame of isolate metadata.
#' @param max_levels Upper limit on acceptable distinct values.
#' @return Character vector of field names ordered by suitability.
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
      coverage = field_coverage(metadata[[f]])
    )
  })
  # Exclude fields with no data (0 levels), unique-per-isolate (n levels), or
  # over the caller's limit.
  info <- Filter(
    function(i) i$levels >= 1 && i$levels < n && i$levels <= max_levels,
    info
  )
  if (!length(info)) {
    return(character(0))
  }

  rank <- vapply(
    info,
    function(i) .field_rank(i$levels, i$coverage),
    numeric(1)
  )
  vapply(info[order(rank)], function(i) i$field, character(1))
}

#' Resolve Preferred Color Scale Families
#'
#' @param values Vector of field values.
#' @param numeric_categories Character vector of preferred numeric palette names.
#' @return Character vector of matching color scale class identifiers.
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

# --- Profile Data Frame Assembly ---------------------------------------------

CONTINUOUS_TYPES <- c("integer", "numeric", "date", "datetime")

#' Profile Metadata Column Properties
#'
#' Generates a structured summary data frame describing data types, cardinality,
#' coverage, grouping capability, and recommended UI presentation order.
#'
#' @param metadata Data frame of isolate metadata.
#' @param types Optional named vector of declared field types.
#' @param mlst_cols Explicit vector of MLST column names.
#' @param amr_cols Explicit vector of AMR column names.
#' @param custom_cols Explicit vector of custom column names.
#' @return Data frame containing summary profile metrics per column.
#' @export
field_profiles <- function(
  metadata,
  types = NULL,
  mlst_cols = NULL,
  amr_cols = NULL,
  custom_cols = NULL
) {
  cols <- names(metadata)
  n <- nrow(metadata)
  if (is.null(cols) || !length(cols)) {
    return(.empty_profiles())
  }

  if (is.null(types)) {
    types <- field_types(NULL, cols)
  }
  type <- unname(types[match(cols, names(types))])
  type[is.na(type)] <- "text"

  levels <- vapply(cols, function(f) field_levels(metadata[[f]]), integer(1))
  coverage <- vapply(
    cols,
    function(f) field_coverage(metadata[[f]]),
    numeric(1)
  )
  numeric <- vapply(cols, function(f) is.numeric(metadata[[f]]), logical(1))
  group <- group_of(cols, mlst_cols, amr_cols, custom_cols)

  continuous <- numeric | type %in% CONTINUOUS_TYPES
  # A constant column (1 level) still groups, trivially, into a single bucket —
  # only an empty column (0 levels, nothing to show) or one unique per isolate
  # (n levels, every bucket a singleton) genuinely cannot group.
  groupable <- levels >= 1L & levels < n

  out <- data.frame(
    field = cols,
    label = vapply(cols, field_label, character(1)),
    group = group,
    type = type,
    type_label = unname(TYPE_LABELS[match(type, names(TYPE_LABELS))]),
    levels = as.integer(levels),
    coverage = coverage,
    n = as.integer(n),
    numeric = numeric,
    continuous = continuous,
    groupable = groupable,
    shapeable = groupable & !continuous & levels <= MAX_SHAPE_LEVELS,
    # Non-metadata derived groups (e.g. MLST, AMR) are suitable for heatmaps
    heatmapable = group != GROUP_ORDER[[1]],
    rank = mapply(.field_rank, levels, coverage),
    stringsAsFactors = FALSE
  )
  out$type_label[is.na(out$type_label)] <- "Text"

  # Sort by category group order, groupability status, quality rank, and field name
  ord <- order(
    match(out$group, GROUP_ORDER),
    !out$groupable,
    out$rank,
    out$field
  )
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.empty_profiles <- function() {
  data.frame(
    field = character(0),
    label = character(0),
    group = character(0),
    type = character(0),
    type_label = character(0),
    levels = integer(0),
    coverage = numeric(0),
    n = integer(0),
    numeric = logical(0),
    continuous = logical(0),
    groupable = logical(0),
    shapeable = logical(0),
    heatmapable = logical(0),
    rank = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Generate Subtext Option Description
#'
#' Formats descriptive text labels (e.g. "46 values · Text") for option choices,
#' flagging non-groupable fields explicitly.
#'
#' @param profiles Profiles data frame output from `field_profiles()`.
#' @return Character vector of option subtext strings.
#' @export
profile_description <- function(profiles) {
  n <- profiles$levels
  base <- sprintf(
    "%d %s · %s",
    n,
    ifelse(n == 1L, "value", "values"),
    profiles$type_label
  )
  ifelse(profiles$groupable, base, paste(base, "— cannot group"))
}

#' Extract Profile Entry for Single Field
#'
#' @param profiles Profiles data frame.
#' @param field Target field name.
#' @return Single-row data frame for target field, or NULL if absent.
#' @export
profile_for <- function(profiles, field) {
  if (is.null(profiles) || !nrow(profiles) || is.null(field)) {
    return(NULL)
  }
  hit <- which(profiles$field == field)
  if (!length(hit)) {
    return(NULL)
  }
  profiles[hit[[1]], , drop = FALSE]
}
