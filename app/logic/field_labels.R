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
  # Drug-class columns render as the bare class key. Gene columns cannot be
  # labelled from their name — it is a positional id — so `amr_field_map()`
  # carries their gene name instead.
  if (startsWith(f, AMR_COL_PREFIX)) {
    return(substring(f, nchar(AMR_COL_PREFIX) + 1L))
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

#' Prefix marking a gene-level AMR column (`amr_g1`, `amr_g2`, ...).
#'
#' The name is a position, not a gene: `load_amr_matrix()` numbers the genes in
#' the order abritamr first reported them, and the gene each one stands for
#' travels beside the data in the `"amr_gene_labels"` attribute.
#' @export
AMR_GENE_PREFIX <- "g"

#' The two kinds of finding abritamr reports, as they are shown to the user.
#'
#' Resistance (its `matches`/`partials` files) and virulence/stress are separate
#' reports and a drug class only ever comes from one of them, so this labels a
#' column rather than splitting it.
#' @export
AMR_SECTIONS <- c(resistance = "Resistance", virulence = "Virulence / stress")

#' Header a drug class is filed under in a variable picker.
#'
#' One header per drug class rather than one for all of AMR: a screen of any
#' size is dozens of genes, and the class a gene belongs to is how a reader
#' looks for it. The section leads so that resistance and virulence/stress do
#' not interleave — "AMR" rather than "Resistance" because the group is what
#' the tab is called.
#'
#' @param section One of `AMR_SECTIONS`.
#' @param drug_class Drug class or group name.
#' @return Character header.
#' @export
amr_subgroup <- function(section, drug_class) {
  lead <- ifelse(
    is.na(section) | section != AMR_SECTIONS[["virulence"]],
    "AMR",
    AMR_SECTIONS[["virulence"]]
  )
  paste0(lead, " \u00b7 ", drug_class)
}

#' What each AMR column of a metadata frame is.
#'
#' The AMR appenders leave three parallel attributes behind — which columns they
#' added, and for the gene columns the gene and the drug class each one stands
#' for. This reads them back as one table, so everything downstream asks the
#' same question the same way instead of re-deriving it from attribute names.
#'
#' A drug-class column carries its class in its own name; a gene column cannot
#' (see `AMR_GENE_PREFIX`), which is what the attributes are for. A column the
#' attributes do not cover is reported as a class column, because that is the
#' shape it has.
#'
#' @param metadata Data frame carrying the AMR appenders' attributes.
#' @return Data frame with `field`, `role` ("class" or "gene"), `class`, `gene`
#'   and `subgroup`; zero rows when there are no AMR columns.
#' @export
amr_field_map <- function(metadata) {
  attr_or <- function(name) {
    v <- attr(metadata, name, exact = TRUE)
    if (is.null(v)) character(0) else v
  }
  cols <- as.character(attr_or("amr_cols"))
  if (!length(cols)) {
    return(.empty_amr_map())
  }
  genes <- attr_or("amr_gene_labels")
  classes <- attr_or("amr_gene_groups")
  sections <- c(attr_or("amr_class_sections"), attr_or("amr_gene_sections"))

  role <- ifelse(cols %in% names(genes), "gene", "class")
  # abritamr can report an empty drug class or gene name, and a blank header
  # reaches virtual-select as an empty, unselectable row.
  or_default <- function(x, default) {
    x <- unname(x)
    ifelse(is.na(x) | !nzchar(trimws(x)), default, x)
  }
  drug_class <- or_default(
    ifelse(role == "gene", classes[cols], substring(cols, nchar(AMR_COL_PREFIX) + 1L)),
    "Other"
  )
  section <- or_default(sections[cols], AMR_SECTIONS[["resistance"]])

  data.frame(
    field = cols,
    role = role,
    class = drug_class,
    gene = ifelse(role == "gene", or_default(genes[cols], cols), NA_character_),
    section = section,
    subgroup = amr_subgroup(section, drug_class),
    stringsAsFactors = FALSE
  )
}

.empty_amr_map <- function() {
  data.frame(
    field = character(0),
    role = character(0),
    class = character(0),
    gene = character(0),
    section = character(0),
    subgroup = character(0),
    stringsAsFactors = FALSE
  )
}

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
