# app/logic/scheme_browser.R

box::use(
  rvest[read_html, html_table],
  curl[curl_fetch_memory],
  tibble[add_row],
  shiny[HTML],
  jsonlite[fromJSON],
  shinyFiles[parseDirPath],
  fs[path_home],
  DBI[dbConnect, dbDisconnect, dbWriteTable],
  RSQLite[SQLite],
  utils[read.delim]
)

box::use(
  app / logic / schemes[cgmlst_org_schemes],
  app / logic / logging[log_event],
)

# Lazily-loaded, package-scoped cache for species metadata (taxonomy and descriptions)
.metadata_cache <- new.env(parent = emptyenv())

species_metadata <- function() {
  if (is.null(.metadata_cache$data)) {
    .metadata_cache$data <- fromJSON(
      "app/logic/data/species_metadata.json",
      simplifyVector = FALSE
    )
  }
  .metadata_cache$data
}

# Normalizes species strings to ensure equivalent matching across space/underscore variations
.norm_species <- function(x) gsub("[ _]+", "_", trimws(x))

#' Assemble Target Database File Path
#'
#' @description Combines a `shinyFiles` directory selection with a user-supplied
#'   database name to construct a clean, sanitized `.db` file path.
#'
#' @param download_location List or character string. Raw selection object from `shinyDirChoose`.
#' @param db_name Character string. User-entered database name.
#'
#' @return Complete file path string ending in `.db`, or `NULL` if inputs are invalid or empty.
#' @export
assemble_db_location <- function(download_location, db_name) {
  download_path <- parseDirPath(
    roots = c(Home = path_home(), Root = "/"),
    download_location
  )

  if (!length(download_path) || !is.character(download_path)) {
    return(NULL)
  }

  if (is.null(db_name) || !length(db_name) || db_name == "") {
    return(NULL)
  }

  # Strip out non-alphanumeric characters except hyphens and underscores to ensure safe file paths
  db_name_safe <- gsub("[^a-zA-Z0-9_-]", "", db_name)

  if (db_name_safe == "") {
    return(NULL)
  }

  file.path(download_path, paste0(db_name_safe, ".db"))
}

#' Resolve Species Asset Image Path
#'
#' @description Maps a selected species name to its corresponding local PNG illustration path.
#'
#' @param species_select Character string. Selected species identifier.
#'
#' @return Character string path to the static image asset.
#' @export
get_species_img <- function(species_select) {
  name <- cgmlst_org_schemes$abb[which(
    cgmlst_org_schemes$species == gsub(" ", "_", species_select)
  )]

  file.path("app/static/species", paste0(name, ".png"))
}

#' Fetch Enriched Species Metadata
#'
#' @description Looks up NCBI taxonomy details and descriptive metadata for a given species name.
#'
#' @param species_select Character string. Target species identifier.
#'
#' @return A named `list` containing metadata attributes, or `NULL` if no matching record exists.
#' @export
get_species_details <- function(species_select) {
  key <- .norm_species(species_select)
  for (record in species_metadata()) {
    if (identical(.norm_species(record$species), key)) {
      return(record)
    }
  }
  NULL
}

#' Store Scheme Overview Table in Database
#'
#' @description Persists key-value overview details for a schema into the `scheme_overview` table.
#'
#' @param scheme_overview `data.frame`. Two-column table containing scheme key-value pairs.
#' @param db_path Character string. Target SQLite database file path.
#'
#' @return Invisible `TRUE` on successful database write.
#' @export
download_scheme_overview <- function(scheme_overview, db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  names(scheme_overview) <- c("key", "value")

  dbWriteTable(
    con,
    "scheme_overview",
    scheme_overview,
    overwrite = TRUE
  )
  log_event(
    "DB",
    "scheme_overview",
    sprintf("written, %d row(s)", nrow(scheme_overview))
  )
  invisible(TRUE)
}

#' Fetch and Store Scheme Target Loci
#'
#' @description Retrieves locus definition tables directly from cgmlst.org and writes them
#'   to the `targets` table in the destination SQLite database.
#'
#' @param select_cgmlst Character string. Species name as selected in UI.
#' @param db_path Character string. Target SQLite database file path.
#'
#' @return Invisible `TRUE` on successful download and table update; `FALSE` if retrieval or parsing fails.
#' @export
download_scheme_targets <- function(select_cgmlst, db_path) {
  select_cgmlst <- gsub(" ", "_", select_cgmlst)
  selection <- cgmlst_org_schemes$species == select_cgmlst

  if (!any(selection)) {
    return(FALSE)
  }

  url <- paste0(
    "https://www.cgmlst.org/ncs/schema/",
    cgmlst_org_schemes$abb[which(selection)],
    "/locus/?content-type=csv"
  )

  targets <- tryCatch(
    {
      response <- curl_fetch_memory(url)
      if (response$status_code != 200) {
        stop("HTTP ", response$status_code)
      }
      txt <- rawToChar(response$content)

      # cgmlst.org CSV endpoints return tab-separated values with a leading empty field in data rows,
      # which causes R's default parser to misalign column headers with row names.
      # Headerless parsing with manual column re-alignment corrects this shift.
      header <- strsplit(strsplit(txt, "\n")[[1]][1], "\t")[[1]]
      header <- header[nzchar(header)]

      body <- read.delim(
        text = txt,
        sep = "\t",
        header = FALSE,
        skip = 1,
        quote = "",
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = "character"
      )

      leading_empty <- all(!nzchar(trimws(body[[1]])))
      cols <- if (leading_empty && ncol(body) > length(header)) {
        seq(2, length.out = length(header))
      } else {
        seq_len(length(header))
      }
      body <- body[, cols, drop = FALSE]
      names(body) <- header
      body
    },
    error = function(e) NULL
  )

  if (is.null(targets) || !nrow(targets)) {
    return(FALSE)
  }

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))

  dbWriteTable(con, "targets", targets, overwrite = TRUE)
  log_event("DB", "targets", sprintf("written, %d locus/loci", nrow(targets)))
  invisible(TRUE)
}

#' Fetch Scheme Overview HTML Summary
#'
#' @description Scrapes and parses the online scheme summary table from cgmlst.org
#'   and appends local navigation links.
#'
#' @param select_cgmlst Character string. Species name as selected in UI.
#'
#' @return A `data.frame` containing summary metadata, or error message string on fetch failure.
#' @export
get_scheme_overview <- function(
  select_cgmlst
) {
  select_cgmlst <- gsub(" ", "_", select_cgmlst)
  selection <- cgmlst_org_schemes$species == select_cgmlst

  if (!any(selection)) {
    return(NULL)
  } else {
    url <- paste0(
      "https://www.cgmlst.org/ncs/schema/",
      cgmlst_org_schemes$abb[which(selection)]
    )
  }

  scheme_overview <- tryCatch(
    {
      response <- curl_fetch_memory(url)
      if (response$status_code != 200) {
        stop("HTTP ", response$status_code)
      }
      read_html(rawToChar(response$content))
    },
    error = function(e) {
      return(paste("Connection to", url, "can't be established"))
    }
  )

  if (!is.null(scheme_overview)) {
    scheme_overview <- scheme_overview |>
      html_table(header = FALSE) |>
      as.data.frame(stringsAsFactors = FALSE)

    names(scheme_overview) <- c("X1", "X2")

    # Filter non-relevant rows and insert source server links at top of summary
    scheme_overview <- scheme_overview[
      scheme_overview$X1 != "Accessory Scheme",
    ]

    scheme_overview <- add_row(
      scheme_overview,
      data.frame(
        X1 = c("URL", "Database"),
        X2 = c(
          paste0('<a href="', url, '/" target="_blank">', url, '</a>'),
          "cgMLST.org Nomenclature Server (h25)"
        )
      ),
      .after = 1
    )

    names(scheme_overview) <- NULL
  }

  return(scheme_overview)
}
