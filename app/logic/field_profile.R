# app/logic/field_profile.R
#
# What a metadata column *is*, and how much of it there is.
#
# Two questions get asked of every column the visualization modules offer the
# user, and they have different answers from different places:
#
#   what does it mean?   Its declared type — app/logic/field_types.R, which
#                        reads it from the schema or from the custom-variable
#                        table and never infers it from contents.
#   how much of it?      Its distinct-value count and coverage, which can only
#                        be counted from the frame in hand.
#
# Neither answer is useful alone. A `date` column that arrives as text is
# continuous even though `is.numeric()` says otherwise; a `category` column with
# 46 levels needs a generated palette even though its type says nothing about
# size. `field_profiles()` answers both at once, one row per column, and every
# consumer — the tree's mapping picker, the MST/Epi/AMR field selects, the
# mapping engine — reads that one frame instead of recomputing its own half.
#
# This module also owns the level-counting primitives themselves. They used to
# live in tree_plot.R, which made "how many groups does this column have" a
# property of the tree renderer, when five engines need to know. tree_plot.R
# imports them back.

box::use(
  stats[setNames],
)

box::use(
  app / logic / field_labels[field_label, GROUP_ORDER, group_of],
  app / logic / field_types[field_types, TYPE_LABELS],
)

# --- Level counting ----------------------------------------------------------
#
# A metadata column is only worth mapping to colour, shape or a tile strip if
# its values actually separate the isolates into groups a reader can tell
# apart. Three ways a column fails that, all of them present in a real
# database: one distinct value (every tip identical — `organism` in a
# single-species scheme), one distinct value *per isolate* (`isolate` itself,
# 346 colours, which is what this shipped as the default mapping), and mostly
# empty (`source`, filled for 11 of 346).

# Largest ColorBrewer qualitative palette that is safe: Set1 and Pastel1 carry 9
# colours, Set2/Dark2/Accent only 8. Ask any of them for more and brewer warns
# ("Returning the palette you asked for with that many colors") and hands back
# short — every level past the end draws grey. Above this, a viridis scale,
# which is generated rather than tabulated and so has no ceiling.
#' @export
MAX_QUAL_LEVELS <- 9L

# ggplot2's shape palette stops at six: "more than 6 becomes difficult to
# discriminate". Past it the extra levels get no shape at all, so those tips
# simply vanish from the plot rather than looking wrong.
#' @export
MAX_SHAPE_LEVELS <- 6L

# A column needs values for at least this share of the isolates to be offered as
# a default mapping; below it the plot is mostly "NA".
#' @export
MIN_COVERAGE <- 0.5

# Distinct non-empty values in a metadata column.
#' @export
field_levels <- function(values) {
  v <- values[!is.na(values)]
  if (is.character(v)) {
    v <- v[nzchar(trimws(v))]
  }
  length(unique(v))
}

# Share of isolates the column actually has a value for.
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

# How well a column groups the data, smaller is better. Well-covered columns of
# a handful of groups first, then wider ones, then the sparse. Ranking before
# ordering keeps the sort a plain numeric one.
.field_rank <- function(levels, coverage) {
  band <- if (coverage < MIN_COVERAGE) {
    3L
  } else if (levels <= MAX_QUAL_LEVELS) {
    1L
  } else {
    2L
  }
  band + levels / (100 * max(levels, 1) + 1)
}

# The metadata columns worth offering for a mapping, best first.
#
# Ordering the picker *is* the recommendation: the first entry becomes the
# default, and a user scanning the list meets the usable columns before the
# hopeless ones. Columns that cannot group at all are dropped.
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
  # One value for everyone, or a different value for everyone: neither groups.
  info <- Filter(
    function(i) i$levels > 1 && i$levels < n && i$levels <= max_levels,
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

# Which palette family suits a column: a qualitative one while the levels stay
# inside the smallest brewer palette, a generated (viridis) one past that, and
# the numeric families for numbers. Returns names of `color_scales` groups in
# viz_helpers.R, best first.
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

# --- The profile frame -------------------------------------------------------

# Types that describe an ordered continuum, whatever R class the column arrives
# as. SQLite hands every metadata column back as character, so a declared date
# is a character vector that `is.numeric()` calls discrete — and a discrete
# scale over 300 distinct dates is 300 unordered colours. This is the one place
# the declared type overrides what the contents look like.
CONTINUOUS_TYPES <- c("integer", "numeric", "date", "datetime")

#' One row per column of `metadata`, in the order a picker should list them.
#'
#' `types` is the database-dependent half — `field_types(db_path, names(meta))`
#' — passed in rather than looked up here, so the read of the custom-variable
#' table happens once where the metadata itself is assembled
#' (app/view/visualization.R) instead of once per engine per invalidation.
#' Omitted, every column reads as its prefix's type or as text, which is right
#' for a caller that has no database to ask.
#'
#' The `*_cols` arguments are the attributes `viz_metadata` carries, naming
#' which columns each appender added. Given, they decide the group; omitted,
#' the column's name prefix does.
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
  coverage <- vapply(cols, function(f) field_coverage(metadata[[f]]), numeric(1))
  numeric <- vapply(cols, function(f) is.numeric(metadata[[f]]), logical(1))
  group <- group_of(cols, mlst_cols, amr_cols, custom_cols)

  continuous <- numeric | type %in% CONTINUOUS_TYPES
  # The two ways a column cannot group: one value for everyone, or a different
  # value for everyone.
  groupable <- levels > 1L & levels < n

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
    # Only the appender-built families make a coherent heatmap: their columns
    # are the same kind of measurement repeated, which is what a matrix of one
    # shared fill scale is for. Sample metadata is a bag of unrelated fields.
    heatmapable = group != GROUP_ORDER[[1]],
    rank = mapply(.field_rank, levels, coverage),
    stringsAsFactors = FALSE
  )
  out$type_label[is.na(out$type_label)] <- "Text"

  # Group order first so prepare_choices()' optgroups — which it builds from
  # unique(group) in row order — come out in the same order
  # grouped_field_choices() puts them in.
  #
  # Then, within a group: the columns that can actually group, best first, and
  # the ones that cannot after them. Ordering the picker *is* the
  # recommendation, so a user scanning it meets the usable columns before the
  # hopeless ones — the same argument mapping_fields() makes, except that this
  # list still *shows* the hopeless ones (with "— cannot group" in their
  # sub-text) rather than hiding them, which is what left the old shape picker
  # silently missing entries the user had gone looking for.
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

#' The sub-text one option carries: `"46 values · Text"`.
#'
#' Vectorised over a whole profile frame, which is the form
#' `shinyWidgets::prepare_choices()` wants. A column that cannot group says so
#' instead of leaving the user to work out why picking it did nothing.
#' @export
profile_description <- function(profiles) {
  n <- profiles$levels
  base <- sprintf("%d %s · %s", n, ifelse(n == 1L, "value", "values"),
                  profiles$type_label)
  ifelse(profiles$groupable, base, paste(base, "— cannot group"))
}

#' The single row for `field`, as a one-row frame, or NULL when absent.
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
