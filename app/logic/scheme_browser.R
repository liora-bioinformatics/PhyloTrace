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
)

# Lazily-loaded, cached species metadata (taxonomy + descriptions).
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

# Normalise a species name so spaces and underscores compare equal
# (e.g. "Providencia stuartii" vs "Providencia_stuartii").
.norm_species <- function(x) gsub("[ _]+", "_", trimws(x))

#' Assemble a database file path from a shinyFiles directory selection and a
#' user-defined database name.
#'
#' `download_location` is the raw `shinyDirChoose` input value and `db_name`
#' the raw text input. The name is sanitised to a safe filename and a `.db`
#' suffix is appended. Returns `NULL` when either input is missing or empty,
#' so callers can guard on a complete selection.
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

  db_name_safe <- gsub("[^a-zA-Z0-9_-]", "", db_name)

  if (db_name_safe == "") {
    return(NULL)
  }

  file.path(download_path, paste0(db_name_safe, ".db"))
}

#' @export
get_species_img <- function(species_select) {
  name <- cgmlst_org_schemes$abb[which(
    cgmlst_org_schemes$species == gsub(" ", "_", species_select)
  )]

  file.path("app/static/species", paste0(name, ".png"))
}

#' Look up enriched metadata (NCBI taxonomy + description) for a species.
#' Returns the record as a list, or NULL when no match is found.
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
  invisible(TRUE)
}

#' Fetch the scheme's target/locus table from cgmlst.org and store it as a
#' `targets` table in the downloaded database. The locus list is the same one
#' shown at `cgmlst.org/ncs/schema/<abb>/locus/` and downloadable there as CSV
#' (actually tab-separated). `select_cgmlst` is the species name as picked in
#' the UI (with spaces); it is normalised and mapped to the URL slug via the
#' `abb` column, exactly like `get_scheme_overview()`.
#'
#' Returns `FALSE` on any fetch/parse failure (transient cgmlst.org problem,
#' unknown species, empty table) so a successful database download is never
#' aborted by a missing targets table.
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

      # The export's header carries the real column names (Locus, Gene, Start,
      # Length, Product, Alleles) plus a trailing tab. Each *data* row is, in
      # addition, prefixed with an empty field, so a plain header=TRUE read
      # misaligns (R consumes that leading column as row names). Read the body
      # headerless, then take the columns aligned to the parsed header names,
      # accounting for the spurious leading column.
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
  invisible(TRUE)
}

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

  # Fetch scheme url
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

    # Drop fields that aren't relevant to the scheme overview
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
