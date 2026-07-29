# app/logic/field_types.R
#
# What *type* of variable each metadata column holds.
#
# Custom variables already carry an authoritative type: the user picks one when
# defining the variable and `phylotrace_custom_fields.type` stores its slug (see
# app/logic/custom_fields.R, CUSTOM_TYPES). The fixed GenEpiO schema has no such
# table — its types are a property of the schema itself — so they are declared
# here, in the same vocabulary. One lookup then answers "is this column a date?"
# for a column of either origin, which is what lets a consumer (the browse
# table's cell editors, the visualization modules' time filter) treat a
# user-defined date variable exactly like the built-in collection date without
# knowing where either came from.
#
# Deliberately cheap: one small query against the loaded database, no metadata
# rows read, nothing inferred from a column's contents. A column's type is
# declared, never guessed — guessing is what made the built-in and the custom
# date columns behave differently in the first place.

box::use(
  stats[setNames],
)

box::use(
  app / logic / custom_fields[custom_col, CUSTOM_TYPES, list_custom_fields],
)

#' Types of the fixed metadata schema's columns, as `column = "type slug"`.
#'
#' The slugs are `custom_fields$CUSTOM_TYPES`', plus `"datetime"` for the
#' app-stamped timestamps (a date *and* a time of day — the distinction matters
#' to an editor, not to a time filter, which is why consumers ask for the set of
#' types they can handle rather than for one type).
#'
#' Only columns whose type is something other than free text are listed; every
#' unlisted column — including a metadata field an imported peer database
#' brought in that this app has never seen — reads as `"text"`.
#' @export
FIXED_FIELD_TYPES <- c(
  sample_collection_date = "date",
  called_at = "datetime"
)

#' Types the derived, appender-built columns carry, keyed by name prefix.
#'
#' `mlst_<locus>` holds an allele identifier and `amr_<class>` holds the genes
#' called for a drug class: both are nominal codes, never free text and never
#' ordered — allele "412" is a name that happens to look like a number, and
#' averaging or ranking it is meaningless. Declaring them here rather than
#' letting them fall through to `"text"` is what lets one lookup answer for
#' every column of the visualization metadata table, whichever appender put it
#' there (see app/view/visualization.R).
#'
#' `amr_profile` is the exception and is deliberately absent: it is a
#' comma-joined summary of every class with a hit, so it reads as text.
#' @export
PREFIX_FIELD_TYPES <- c(
  mlst_ = "category",
  amr_ = "category"
)

# Columns a prefix would otherwise claim, but whose contents are not what the
# prefix implies.
PREFIX_EXCEPTIONS <- c("amr_profile")

#' Human-readable name for each type slug, for anything that shows a type to
#' the user (the mapping picker's sub-text, a variable's chip in the browser).
#' One vocabulary so two controls never name the same type differently.
#' @export
TYPE_LABELS <- c(CUSTOM_TYPES, datetime = "Date & time")

# The type a column's name prefix declares, or NA when no prefix claims it.
.prefix_type <- function(cols) {
  out <- rep(NA_character_, length(cols))
  for (p in names(PREFIX_FIELD_TYPES)) {
    hit <- startsWith(cols, p) & !(cols %in% PREFIX_EXCEPTIONS)
    out[hit] <- PREFIX_FIELD_TYPES[[p]]
  }
  out
}

#' Every column's type, as a named character vector `column -> type slug`.
#'
#' With `cols` given, the result covers exactly those columns in that order,
#' with `"text"` for anything not declared. With `cols` NULL it returns only the
#' declared ones (the fixed schema's plus this database's custom variables),
#' which is the form to use when the caller wants to know what exists rather
#' than to classify a frame it already holds.
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

  # Three sources, most specific last: the "text" default, then what a column's
  # name prefix declares, then what the schema or the custom-field table
  # declares for that exact column. A database that somehow carried a custom
  # variable named like an MLST locus gets its own declared type, not the
  # prefix's guess at one.
  out <- setNames(rep("text", length(cols)), cols)
  pref <- .prefix_type(cols)
  out[!is.na(pref)] <- pref[!is.na(pref)]
  hit <- intersect(cols, names(known))
  out[hit] <- unname(known[hit])
  out
}

#' The date-typed columns among `cols`, in `cols` order.
#'
#' `types` is the set the caller can actually handle: a cell editor wants plain
#' `"date"` columns only (a date picker on a timestamp would silently drop its
#' time of day), while a time filter is happy to take `"datetime"` too since it
#' only ever compares whole days.
#' @export
date_fields <- function(db_path, cols, types = c("date", "datetime")) {
  if (is.null(cols) || !length(cols)) {
    return(character(0))
  }
  ft <- field_types(db_path, cols)
  names(ft)[ft %in% types]
}

#' Parse a date-typed metadata column to `Date`, tolerating the two ways it
#' fails.
#'
#' Values are stored as TEXT in a canonical ISO form, but the fixed schema's
#' `sample_collection_date` is free text the user may have typed anything into,
#' and `as.Date.character()` returns NA for unreadable values *only as long as
#' at least one value in the vector parses* — hand it a column where none do and
#' it throws instead. A column that is entirely unparseable has to come back
#' all-NA. Timestamps (`"2024-05-06 11:22:33"`) truncate to their day.
#' @export
as_date_safe <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  tryCatch(
    suppressWarnings(as.Date(as.character(x))),
    error = function(e) rep(as.Date(NA), length(x))
  )
}
