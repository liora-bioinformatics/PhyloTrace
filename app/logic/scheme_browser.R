# app/logic/scheme_browser.R

box::use(
  rvest[read_html, html_table],
  curl[curl_fetch_memory],
  tibble[add_row],
  shiny[HTML],
  jsonlite[fromJSON],
  shinyFiles[parseDirPath],
  fs[path_home],
  DBI[dbDisconnect, dbWriteTable],
  utils[read.delim]
)

box::use(
  app / logic / db_connect[connect],
  app / logic / logging[log_event],
  app / logic / mlst_repo[mangle_species],
  app / logic / schemes[cgmlst_org_schemes],
)

# Lazily-loaded, package-scoped cache for species metadata (taxonomy and descriptions)
.metadata_cache <- new.env(parent = emptyenv())

species_metadata <- function() {
  if (is.null(.metadata_cache$data)) {
    # box::file() resolves against this module's directory, so the table also
    # loads when the working directory is not the app root (testthat).
    .metadata_cache$data <- fromJSON(
      box::file("data", "species_metadata.json"),
      simplifyVector = FALSE
    )
  }
  .metadata_cache$data
}

# Normalizes species strings to ensure equivalent matching across space/underscore variations
.norm_species <- function(x) gsub("[ _]+", "_", trimws(x))

# Case-insensitive form used to compare two spellings of the same scheme.
.match_key <- function(x) tolower(.norm_species(x))

# Drops the curating institute a scheme name can carry ("... (RKI)"), which is
# part of the scheme name on cgmlst.org but of no taxon.
.drop_curator <- function(x) trimws(sub("\\s*\\([^)]*\\)\\s*$", "", x))

# The scheme abbreviation out of a `scheme_overview` table: `get_scheme_overview()`
# stores the cgmlst.org URL it scraped, and its last path segment names the scheme.
.overview_abb <- function(scheme_overview) {
  if (!is.data.frame(scheme_overview) || !nrow(scheme_overview)) {
    return(NULL)
  }

  cells <- as.character(unlist(scheme_overview, use.names = FALSE))
  found <- regmatches(
    cells,
    regexpr("/ncs/schema/[A-Za-z0-9_.-]+", cells)
  )

  if (!length(found)) {
    return(NULL)
  }

  sub("^/ncs/schema/", "", found[[1]])
}

#' Resolve the Scheme a Database Was Built From
#'
#' @description Maps whatever identifies a scheme - a UI selection, the
#'   `scheme_overview` URL stored at download time, or the species string pyMLST
#'   left in `mlst_type` - onto the canonical scheme name shared by
#'   `cgmlst_schemes.csv` and `species_metadata.json`.
#'
#' @param species Character string. Scheme or species name, as selected in the
#'   UI or as stored in `mlst_type.species`.
#' @param scheme_overview `data.frame` or `NULL`. The database's `scheme_overview`
#'   table, whose cgmlst.org URL identifies the scheme exactly.
#'
#' @return Canonical scheme name (underscored, as in the scheme table), or `NULL`
#'   when nothing matches.
#' @export
resolve_scheme_key <- function(species = NULL, scheme_overview = NULL) {
  keys <- cgmlst_org_schemes$species

  # The URL is written by this app rather than by pyMLST, so it survives both
  # defects below and is the only identifier that tells two schemes of the same
  # species ((RKI) vs (FLI)) apart.
  abb <- .overview_abb(scheme_overview)
  if (!is.null(abb)) {
    hit <- which(tolower(cgmlst_org_schemes$abb) == tolower(abb))
    if (length(hit) == 1) {
      return(keys[hit])
    }
  }

  if (
    is.null(species) ||
      length(species) != 1 ||
      is.na(species) ||
      !nzchar(trimws(species))
  ) {
    return(NULL)
  }

  # `mlst_type.species` is neither the scheme name nor reliably the species: the
  # curator suffix is not part of the scraped page, and pyMLST's lstrip eats
  # leading letters (see mangle_species()). Every spelling a scheme can reach the
  # database under is therefore tried, most faithful first. Only the (RKI)/(FLI)
  # pair collapses onto one string; they share a taxonomy, so the first is taken.
  plain <- gsub("_", " ", keys)
  spellings <- list(
    plain,
    .drop_curator(plain),
    vapply(plain, mangle_species, character(1), USE.NAMES = FALSE),
    vapply(.drop_curator(plain), mangle_species, character(1), USE.NAMES = FALSE)
  )

  target <- .match_key(species)
  for (spelling in spellings) {
    hit <- which(.match_key(spelling) == target)
    if (length(hit)) {
      return(keys[hit[1]])
    }
  }

  NULL
}

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
#' @param species_select Character string. Selected species or scheme identifier.
#'
#' @return Character string path to the static image asset, or `NULL` for an
#'   unknown scheme.
#' @export
get_species_img <- function(species_select) {
  key <- resolve_scheme_key(species_select)

  if (is.null(key)) {
    return(NULL)
  }

  name <- cgmlst_org_schemes$abb[match(key, cgmlst_org_schemes$species)]

  file.path("app/static/species", paste0(name, ".png"))
}

#' Fetch Enriched Species Metadata
#'
#' @description Looks up NCBI taxonomy details and descriptive metadata for a given species name.
#'
#' @param species_select Character string. Target species or scheme identifier.
#'
#' @return A named `list` containing metadata attributes, or `NULL` if no matching record exists.
#' @export
get_species_details <- function(species_select) {
  key <- resolve_scheme_key(species_select)

  if (is.null(key)) {
    return(NULL)
  }

  for (record in species_metadata()) {
    if (identical(.match_key(record$species), .match_key(key))) {
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
  con <- connect(db_path)
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

  con <- connect(db_path)
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
