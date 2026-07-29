# app/logic/field_labels.R
#
# Display labels for metadata columns. The `metadata` table's column set is not
# fixed — importing a peer database can add columns this app has never seen — so
# every consumer must be able to label an arbitrary column name.

# Curated friendly labels for known metadata columns (+ the two synthetic
# fields derived at geocoding time). Anything else falls back to a
# title-cased, underscore-stripped version of the raw column name so newly
# added metadata fields still show up with a readable label automatically.
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

# Reserved name prefix for the display-only classical-MLST columns (the
# sequence type plus one per locus) derived from the `classical_mlst` table.
# Kept here, in the label module, because both the column producer
# (load_classical_mlst) and every label consumer share this one convention.
#' @export
MLST_COL_PREFIX <- "mlst_"

# Reserved name prefix for the display-only AMR-screening columns (a resistance
# profile summary plus one per drug class / virulence-stress group) derived from
# the `amr_summary` table. Same convention as MLST_COL_PREFIX.
#' @export
AMR_COL_PREFIX <- "amr_"

# Reserved name prefix for the user-defined custom variables surfaced from
# `phylotrace_custom_values` (see app/logic/custom_fields.R). Same convention as
# MLST_COL_PREFIX; the variable's own name follows the prefix.
#' @export
CUSTOM_COL_PREFIX <- "custom_"

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

#' @export
field_label <- function(f) {
  if (f %in% names(field_labels)) {
    return(field_labels[[f]])
  }
  # Derived classical-MLST columns read as the bare locus name; the sequence
  # type as "Sequence Type (ST)".
  if (startsWith(f, MLST_COL_PREFIX)) {
    locus <- substring(f, nchar(MLST_COL_PREFIX) + 1L)
    return(if (identical(locus, "st")) "Sequence Type (ST)" else locus)
  }
  # Derived AMR columns read as the bare drug class / group; the summary column
  # as "AMR Profile".
  if (startsWith(f, AMR_COL_PREFIX)) {
    key <- substring(f, nchar(AMR_COL_PREFIX) + 1L)
    return(if (identical(key, "profile")) "AMR Profile" else key)
  }
  # Custom variables read as their own name, prettified the same way an unknown
  # metadata column is ("sampling_site" -> "Sampling Site").
  if (startsWith(f, CUSTOM_COL_PREFIX)) {
    return(prettify_field(substring(f, nchar(CUSTOM_COL_PREFIX) + 1L)))
  }
  prettify_field(f)
}

#' Vectorised `field_label()`; returns an unnamed character vector the same
#' length as `x`.
#' @export
field_labels_for <- function(x) {
  vapply(x, field_label, character(1), USE.NAMES = FALSE)
}

#' The same labels as chips, for the panels that list a field set as prose.
#' Plain labels belong in `field_labels_for()` — pickers, table headers and
#' `paste()`d sentences need a character vector, not markup.
#' @export
field_chips <- function(x) {
  lapply(field_labels_for(x), function(label) {
    shiny::div(
      class = "species-details_chip",
      shiny::div(class = "species-details_chip-value", label)
    )
  })
}

#' The four field categories, in the order every picker lists them.
#'
#' Exported because two producers have to agree on it: `grouped_field_choices()`
#' builds optgroups in this order, and `field_profile$field_profiles()` sorts
#' its rows by it so `shinyWidgets::prepare_choices()` — which derives optgroups
#' from the order the rows arrive in — lands on the same sequence. One list, so
#' the two cannot drift.
#' @export
GROUP_ORDER <- c(
  "Sample metadata",
  "Classical MLST",
  "AMR screening",
  "Custom variables"
)

#' Which of `GROUP_ORDER` each of `fields` belongs to, in `fields` order.
#'
#' An explicitly named `*_cols` set wins over prefix detection, for the reason
#' spelled out in `grouped_field_choices()`: a column a caller has declared for
#' one group must not also be claimed by another group's prefix.
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

#' Build a select / picker `choices` structure that groups metadata fields into
#' a "Sample metadata" optgroup and, when present, a "Classical MLST" optgroup
#' for the derived MLST columns (see `MLST_COL_PREFIX`), an "AMR screening" one,
#' and a "Custom variables" one for the user-defined fields. Option labels come
#' from `field_labels_for()`; option values stay the raw column names, so a
#' caller's selection logic still keys on the column name. With no grouped
#' columns at all it returns a flat named vector (no optgroups), identical to a
#' plain labelled metadata select. The nested-list form is understood by
#' `shiny::selectInput()`, `shinyWidgets::pickerInput()` and
#' `shinyWidgets::virtualSelectInput()` alike.
#'
#' Each `*_cols` argument defaults to detecting its columns by name prefix; pass
#' the exact set where the caller already tracks it (e.g. the browse table's
#' `"mlst_cols"` attribute). Order within each group follows `fields`.
#'
#' Callers that keep a current selection across a `choices` swap must validate it
#' against the flat `fields`, not against the returned structure: `x %in% choices`
#' does not see into optgroups.
#' @export
grouped_field_choices <- function(
  fields,
  mlst_cols = NULL,
  amr_cols = NULL,
  custom_cols = NULL
) {
  # One classifier, shared with field_profile$field_profiles(), so a column can
  # never land in one group here and another there.
  group <- group_of(fields, mlst_cols, amr_cols, custom_cols)
  in_group <- function(name) fields[group == name]

  mlst_cols <- in_group(GROUP_ORDER[[2]])
  amr_cols <- in_group(GROUP_ORDER[[3]])
  custom_cols <- in_group(GROUP_ORDER[[4]])
  base_cols <- in_group(GROUP_ORDER[[1]])
  grouped <- c(mlst_cols, amr_cols, custom_cols)
  base_choices <- stats::setNames(base_cols, field_labels_for(base_cols))

  # No grouped columns at all: stay flat, exactly as a plain metadata select.
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
