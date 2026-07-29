# app/logic/field_labels.R
#
# Metadata column display label formatting and category grouping logic.
# Maps raw database column names to user-friendly titles and provides
# choice structure generators for Shiny selection controls.

# Curated friendly labels for known metadata columns. Unrecognized columns
# automatically fall back to title-cased, underscore-stripped display labels.
#' @export
field_labels <- c(
  isolate = "Isolate",
  place = "Location",
  sample_collection_date = "Collection Date",
  geo_loc_name_country = "Country",
  geo_loc_name_state_province = "State / Province",
  geo_loc_name_city = "City",
  geo_loc_name_postal_code = "Postcode",
  geo_loc_coordinates = "Coordinates",
  called_at = "Added At",
  source = "Source",
  primary_laboratory_sample_id = "Lab Sample ID",
  specimen_source_id = "Specimen Source",
  sample_collected_by = "Collected By",
  sequence_submitted_by = "Submitted By",
  organism = "Organism",
  purpose_of_sampling = "Purpose of Sampling",
  purpose_of_sequencing = "Purpose of Sequencing"
)

#' Prefix for derived classical-MLST display columns.
#' @export
MLST_COL_PREFIX <- "mlst_"

#' Prefix for derived AMR-screening display columns.
#' @export
AMR_COL_PREFIX <- "amr_"

#' Prefix for user-defined custom variable display columns.
#' @export
CUSTOM_COL_PREFIX <- "custom_"

#' Convert Underscore Field Names to Title Case
#'
#' @param x Character string representing a raw column name.
#' @return Title-cased character string with underscores replaced by spaces.
#' @export
prettify_field <- function(x) {
  words <- strsplit(gsub("_", " ", x), " ")[[1]]
  has_chars <- nchar(words) > 0
  words[has_chars] <- paste0(
    toupper(substring(words[has_chars], 1, 1)),
    substring(words[has_chars], 2)
  )
  paste(words, collapse = " ")
}

#' Resolve Display Label for Column Key
#'
#' Looks up predefined metadata labels or strips prefixes for MLST, AMR, and
#' custom fields before falling back to `prettify_field()`.
#'
#' @param f String column name identifier.
#' @return Human-readable display label.
#' @export
field_label <- function(f) {
  if (f %in% names(field_labels)) {
    return(field_labels[[f]])
  }
  # Render sequence type as "Sequence Type (ST)" and locus names as bare locus keys
  if (startsWith(f, MLST_COL_PREFIX)) {
    locus <- substring(f, nchar(MLST_COL_PREFIX) + 1L)
    return(if (identical(locus, "st")) "Sequence Type (ST)" else locus)
  }
  # Render profile summary as "AMR Profile" and drug classes as bare class keys
  if (startsWith(f, AMR_COL_PREFIX)) {
    key <- substring(f, nchar(AMR_COL_PREFIX) + 1L)
    return(if (identical(key, "profile")) "AMR Profile" else key)
  }
  # Format custom user variables via standard prettify logic
  if (startsWith(f, CUSTOM_COL_PREFIX)) {
    return(prettify_field(substring(f, nchar(CUSTOM_COL_PREFIX) + 1L)))
  }
  prettify_field(f)
}

#' Vectorized Field Label Lookup
#'
#' @param x Character vector of column names.
#' @return Unnamed character vector of formatted display labels.
#' @export
field_labels_for <- function(x) {
  vapply(x, field_label, character(1), USE.NAMES = FALSE)
}

#' Render Field Labels as HTML Chips
#'
#' @param x Character vector of column names.
#' @return List of Shiny HTML div elements representing UI chips.
#' @export
field_chips <- function(x) {
  lapply(field_labels_for(x), function(label) {
    shiny::div(
      class = "species-details_chip",
      shiny::div(class = "species-details_chip-value", label)
    )
  })
}

#' Standard Category Grouping Display Order
#' @export
GROUP_ORDER <- c(
  "Sample metadata",
  "Classical MLST",
  "AMR screening",
  "Custom variables"
)

#' Map Column Names to Category Groups
#'
#' Classifies each field in `fields` into one of the `GROUP_ORDER` categories.
#' Explicit column sets override automatic prefix detection to avoid double-claiming.
#'
#' @param fields Character vector of field names.
#' @param mlst_cols Optional vector of explicit MLST column names.
#' @param amr_cols Optional vector of explicit AMR column names.
#' @param custom_cols Optional vector of explicit custom column names.
#' @return Character vector of group names matching the input length of `fields`.
#' @export
group_of <- function(
  fields,
  mlst_cols = NULL,
  amr_cols = NULL,
  custom_cols = NULL
) {
  claimed <- unlist(lapply(
    list(mlst_cols, amr_cols, custom_cols),
    function(cols) if (is.null(cols)) NULL else intersect(fields, cols)
  ))

  member <- function(cols, prefix) {
    if (is.null(cols)) {
      startsWith(fields, prefix) & !(fields %in% claimed)
    } else {
      fields %in% cols
    }
  }

  out <- rep(GROUP_ORDER[[1]], length(fields))
  out[member(mlst_cols, MLST_COL_PREFIX)] <- GROUP_ORDER[[2]]
  out[member(amr_cols, AMR_COL_PREFIX)] <- GROUP_ORDER[[3]]
  out[member(custom_cols, CUSTOM_COL_PREFIX)] <- GROUP_ORDER[[4]]
  out
}

#' Build Categorized Choices Structure for Selection Controls
#'
#' Groups metadata column names into optgroups suitable for standard Shiny dropdowns
#' (`selectInput`, `pickerInput`, `virtualSelectInput`). If no grouped special
#' columns exist, returns a flat named vector.
#'
#' @param fields Character vector of column names.
#' @param mlst_cols Optional character vector of explicit MLST columns.
#' @param amr_cols Optional character vector of explicit AMR columns.
#' @param custom_cols Optional character vector of explicit custom columns.
#' @return Named list of optgroups or a flat named vector of choices.
#' @export
grouped_field_choices <- function(
  fields,
  mlst_cols = NULL,
  amr_cols = NULL,
  custom_cols = NULL
) {
  group <- group_of(fields, mlst_cols, amr_cols, custom_cols)
  in_group <- function(name) fields[group == name]

  mlst_cols <- in_group(GROUP_ORDER[[2]])
  amr_cols <- in_group(GROUP_ORDER[[3]])
  custom_cols <- in_group(GROUP_ORDER[[4]])
  base_cols <- in_group(GROUP_ORDER[[1]])
  grouped <- c(mlst_cols, amr_cols, custom_cols)
  base_choices <- stats::setNames(base_cols, field_labels_for(base_cols))

  # Return flat named vector when no special grouped columns are present
  if (!length(grouped)) {
    return(base_choices)
  }

  out <- list()
  out[[GROUP_ORDER[[1]]]] <- base_choices
  if (length(mlst_cols)) {
    out[[GROUP_ORDER[[2]]]] <- stats::setNames(
      mlst_cols,
      field_labels_for(mlst_cols)
    )
  }
  if (length(amr_cols)) {
    out[[GROUP_ORDER[[3]]]] <- stats::setNames(
      amr_cols,
      field_labels_for(amr_cols)
    )
  }
  if (length(custom_cols)) {
    out[[GROUP_ORDER[[4]]]] <- stats::setNames(
      custom_cols,
      field_labels_for(custom_cols)
    )
  }
  out
}
