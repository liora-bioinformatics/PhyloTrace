# app/logic/field_types.R
#
# Variable type lookup and validation logic for metadata columns.
# Defines core data type mapping rules for standard, derived, and custom fields
# to maintain uniform behavior across interactive inputs and filters.

box::use(
  stats[setNames],
)

box::use(
  app / logic / custom_fields[custom_col, CUSTOM_TYPES, list_custom_fields],
  app / logic / date_bins[parse_dates],
)

#' Map of fixed schema column names to their declared type slugs.
#' Columns not listed default to "text".
#' @export
FIXED_FIELD_TYPES <- c(
  sample_collection_date = "date",
  called_at = "datetime"
)

#' Default type mappings assigned to derived columns based on name prefix.
#' @export
PREFIX_FIELD_TYPES <- c(
  mlst_ = "category",
  amr_ = "category"
)

#' Human-readable labels for variable type slugs.
#' @export
TYPE_LABELS <- c(CUSTOM_TYPES, datetime = "Date & time")

# Determine column type derived from prefix naming conventions
.prefix_type <- function(cols) {
  out <- rep(NA_character_, length(cols))
  for (p in names(PREFIX_FIELD_TYPES)) {
    out[startsWith(cols, p)] <- PREFIX_FIELD_TYPES[[p]]
  }
  out
}

#' Look Up Column Variable Types
#'
#' Retrieves declared column types from fixed schema rules, custom database
#' definitions, and prefix conventions.
#'
#' @param db_path Path to the SQLite database file.
#' @param cols Optional vector of column names to classify. If NULL, returns all
#'   explicitly declared schema and custom field types.
#' @return Named character vector mapping column names to type slugs.
#' @export
field_types <- function(db_path, cols = NULL) {
  defs <- list_custom_fields(db_path)
  custom <- if (is.data.frame(defs) && nrow(defs)) {
    setNames(as.character(defs$type), custom_col(defs$name))
  } else {
    character(0)
  }
  known <- c(FIXED_FIELD_TYPES, custom)

  if (is.null(cols)) {
    return(known)
  }

  # Build type vector: default to "text", apply prefix rules, then apply explicit definitions
  out <- setNames(rep("text", length(cols)), cols)
  pref <- .prefix_type(cols)
  out[!is.na(pref)] <- pref[!is.na(pref)]
  hit <- intersect(cols, names(known))
  out[hit] <- unname(known[hit])
  out
}

#' Extract Date-Typed Column Names
#'
#' Filters input column names to those matching target temporal types.
#'
#' @param db_path Path to the database file.
#' @param cols Vector of column names to inspect.
#' @param types Character vector of acceptable date-like type slugs.
#' @return Character vector of matching date column names in input order.
#' @export
date_fields <- function(db_path, cols, types = c("date", "datetime")) {
  if (is.null(cols) || !length(cols)) {
    return(character(0))
  }
  ft <- field_types(db_path, cols)
  names(ft)[ft %in% types]
}

#' Safely Parse Input to Date Vector
#'
#' Alias of `date_bins$parse_dates()`, kept here because the date-aware callers
#' already import this module. One implementation, two names.
#'
#' @export
as_date_safe <- parse_dates
