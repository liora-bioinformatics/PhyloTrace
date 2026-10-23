# app/logic/help_content.R
#
# Content of the in-app help pages, one per module (topic). Written for users,
# not developers: what the module is for and how to use it. Facts that the app
# enforces elsewhere (types, accepted files, missing-value markers, standard
# fields) are read from the code that enforces them, so the text cannot drift.

box::use(
  rlang[`%||%`],
  shiny[tagList, tags],
  stats[setNames],
)

box::use(
  app / logic / custom_fields[CUSTOM_TYPES],
  app / logic / custom_import[IMPORT_EXTENSIONS, MISSING_TOKENS, READONLY_METADATA_COLS],
  app / logic / field_labels[field_labels_for],
)

# Standard metadata fields an import may fill, with what belongs in them.
# Order follows the metadata table.
.STANDARD_FIELDS <- c(
  primary_laboratory_sample_id = "Your laboratory's sample ID",
  specimen_source_id = "Material the sample came from, e.g. blood",
  sample_collection_date = "Date as YYYY-MM-DD, e.g. 2024-03-15",
  geo_loc_name_country = "Country, e.g. Germany",
  geo_loc_name_state_province = "State or province",
  sample_collected_by = "Person or institution",
  sequence_submitted_by = "Person or institution",
  purpose_of_sampling = "Free text",
  purpose_of_sequencing = "Free text",
  geo_loc_name_city = "City",
  geo_loc_name_postal_code = "Postal code, kept exactly as written",
  geo_loc_coordinates = "Latitude, longitude in decimal degrees, e.g. 52.52, 13.40"
)

# What each variable type accepts, in the order of CUSTOM_TYPES.
.TYPE_EXAMPLES <- c(
  text = "Anything",
  integer = "42, -3",
  numeric = "12.5 — with a point, no thousands separators",
  date = "YYYY-MM-DD, e.g. 2024-03-15",
  boolean = "yes or no (y/n, true/false and 1/0 are read as well)",
  category = "One of a list of allowed values you define"
)

# A two-column definition table: term on the left, explanation on the right.
.terms <- function(...) {
  items <- list(...)
  tags$dl(
    class = "help-terms",
    lapply(seq_along(items), function(i) {
      tagList(tags$dt(names(items)[[i]]), tags$dd(items[[i]]))
    })
  )
}

# Help page of the Custom Variables module.
.custom_variables <- function() {
  missing <- paste(
    c("an empty cell", paste0("“", setdiff(MISSING_TOKENS, ""), "”")),
    collapse = ", "
  )
  fields <- names(.STANDARD_FIELDS)

  tagList(
    tags$h1("Custom Variables"),
    tags$p(
      class = "help-lead",
      "Your own per-isolate fields, on top of the standard metadata. They are",
      "stored in the database file, travel with it through Export and Import,",
      "and can be shown in Browse Entries and used to colour, label and group",
      "plots."
    ),

    tags$h2("Add a variable"),
    tags$p(
      "Click", tags$strong("New Variable"), "and give it a name, a type and,",
      "optionally, a description. Names use letters, digits, “_” and",
      "“-” and start with a letter; they are shown tidied up, so",
      "“sampling_site” reads as “Sampling Site”. A name that",
      "would look like an existing field (e.g. “Country”) is refused."
    ),
    tags$p("The type decides what a cell accepts:"),
    do.call(.terms, as.list(setNames(.TYPE_EXAMPLES, CUSTOM_TYPES[names(.TYPE_EXAMPLES)]))),
    tags$p(
      class = "help-note",
      "The type cannot be changed later — create a new variable instead."
    ),

    tags$h2("Fill in and edit values"),
    tags$ul(
      tags$li(
        "The", tags$strong("Values"), "tab works like a spreadsheet: select",
        "with click and drag, type or double-click to edit, paste from Excel,",
        "drag the corner of a selection to fill, and undo with Ctrl+Z."
      ),
      tags$li("A value that does not fit the variable's type is refused."),
      tags$li(
        "Edits are kept until you click", tags$strong("Save Changes"),
        "; ", tags$strong("Discard"), "brings back the stored values."
      ),
      tags$li(
        "Click a variable in the", tags$strong("Variables"), "tab to rename",
        "it, or change its description or allowed values."
      ),
      tags$li(
        tags$strong("Remove"), "deletes the chosen variables and all their",
        "values for good."
      )
    ),

    tags$h2("Import from a table"),
    tags$p(
      "Bring in many variables at once from a spreadsheet with",
      tags$strong("Import from Table"), "."
    ),
    tags$h3("Prepare the table"),
    tags$ul(
      tags$li(
        "File types: ",
        paste0(".", IMPORT_EXTENSIONS, collapse = ", "),
        ". The first row holds the column headers; in a workbook you can",
        "pick the sheet."
      ),
      tags$li(
        "One row per isolate, with the isolate names in a column headed",
        tags$code("isolate"), ". Write them exactly as in the database —",
        "letter case matters. Without such a column you pick one in step 1."
      ),
      tags$li("One column per variable."),
      tags$li(
        "Leave unknown values empty. ", missing, " all count as missing:",
        "they are skipped and never erase a value already stored."
      ),
      tags$li(
        "Numbers with a decimal point and no thousands separators. Codes with",
        "leading zeros (e.g. postal codes) stay as written."
      )
    ),
    tags$h3("Step 1 — Check"),
    .terms(
      "Matched" = "Isolates of the table found in the database. Only these are imported.",
      "Not in database" = paste(
        "Isolates of the table this database does not have. Hover the box to",
        "see them. An import never adds isolates."
      ),
      "Excluded" = paste(
        "Isolates listed more than once with different values. They are left",
        "out completely; correct the table and import again."
      ),
      "Not in table" = "Isolates of the database that this import leaves unchanged."
    ),
    tags$h3("Step 2 — Variables"),
    tags$p("Decide for every column what happens to it:"),
    .terms(
      "New variable" = paste(
        "Creates a custom variable. Choose its name and type; only types",
        "that every value fits can be picked."
      ),
      "Fill empty cells" = paste(
        "The column matches an existing field: only its empty cells are",
        "filled, stored values stay."
      ),
      "Overwrite values" = "As above, but stored values are replaced as well.",
      "Skip" = "The column is not imported."
    ),
    tags$p(
      tags$strong("Import"), "writes everything in one go. If anything fails,",
      "nothing is changed."
    ),

    tags$h2("Fill the standard metadata fields"),
    tags$p(
      "A table can also fill the standard fields. Head the column with the",
      "field's exact column name from the list below — not the name shown",
      "in the app. A column headed “Country” does not fill",
      tags$code("geo_loc_name_country"), "; it is skipped. Use each field",
      "only once per table. The same applies to existing custom variables:",
      "their exact name fills them."
    ),
    tags$table(
      class = "table table-sm help-table",
      tags$thead(tags$tr(
        tags$th("Column header"),
        tags$th("Shown as"),
        tags$th("What to enter")
      )),
      tags$tbody(lapply(fields, function(f) {
        tags$tr(
          tags$td(tags$code(f)),
          tags$td(field_labels_for(f)),
          tags$td(.STANDARD_FIELDS[[f]])
        )
      }))
    ),
    tags$p(
      class = "help-note",
      paste0(
        paste(field_labels_for(READONLY_METADATA_COLS), collapse = ", "),
        " are set by PhyloTrace and cannot be imported. Country, state, city,",
        " postal code and coordinates are what the Map uses to place isolates."
      )
    )
  )
}

# Registered help pages, by topic.
.TOPICS <- list(custom_variables = .custom_variables)

#' Help Page Content
#'
#' @param topic Topic key, e.g. `"custom_variables"`.
#' @return A tag list with the page, or a short notice when the topic has no
#'   page yet.
#' @export
help_page <- function(topic) {
  page <- .TOPICS[[topic %||% ""]]
  if (is.null(page)) {
    return(tags$p("There is no help for this part of PhyloTrace yet."))
  }
  page()
}
