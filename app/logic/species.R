# app/logic/species.R
#
# The species a database is defined for: the built-in catalog (taxonomy,
# description and image of common typing targets) and parsing and validating a
# custom species or species group.

box::use(
  jsonlite[fromJSON],
  rlang[`%||%`],
  utils[head],
)

#' Settings Key of the Species' NCBI Taxonomy Id
#'
#' @description Key of `phylotrace_meta` holding the taxonomy id of the species
#'   a database was set up for.
#' @export
TAXID_KEY <- "species_taxid"

# Lazily loaded species metadata (taxonomy and descriptions).
.cache <- new.env(parent = emptyenv())

# Words of a species name: letters, digits and hyphens only.
NAME_PART_RE <- "^[A-Za-z][A-Za-z0-9-]*$"

# Most epithets one database may cover ("Mycobacterium tuberculosis/bovis/...").
MAX_EPITHETS <- 8L

# Reads species_metadata.json once per process.
species_metadata <- function() {
  if (is.null(.cache$records)) {
    # box::file() resolves against this module's directory, so the table also
    # loads when the working directory is not the app root (testthat).
    .cache$records <- fromJSON(
      box::file("data", "species_metadata.json"),
      simplifyVector = FALSE
    )
  }
  .cache$records
}

# Drops a curating institute suffix ("Burkholderia mallei (RKI)").
drop_curator <- function(x) trimws(sub("\\s*\\([^)]*\\)\\s*$", "", x))

# Case-, space- and underscore-blind comparison key of a species name.
match_key <- function(x) tolower(gsub("[ _]+", "_", trimws(x)))

#' Split a Species Name
#'
#' @description Splits `"Genus epithet1/epithet2"` into its genus and the
#'   epithets it covers. `spp`/`sp` mark a genus-level name.
#'
#' @param species Character scalar.
#'
#' @return List with `genus` (character scalar, `NA` when empty) and `epithets`.
#' @export
species_parts <- function(species) {
  words <- strsplit(trimws(gsub("_", " ", species %||% "")), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) {
    return(list(genus = NA_character_, epithets = character(0)))
  }
  epithets <- trimws(unlist(strsplit(paste(words[-1], collapse = " "), "/", fixed = TRUE)))
  epithets <- epithets[nzchar(epithets) & !tolower(epithets) %in% c("sp", "sp.", "spp", "spp.")]
  list(genus = words[1], epithets = epithets)
}

#' Compose a Species Name
#'
#' @param genus Character scalar.
#' @param epithets Character vector; several make a species group.
#'
#' @return `"Genus epithet1/epithet2"`, or the genus alone.
#' @export
compose_species <- function(genus, epithets = character(0)) {
  genus <- trimws(genus)
  genus <- paste0(toupper(substr(genus, 1L, 1L)), tolower(substring(genus, 2L)))
  epithets <- tolower(trimws(epithets))
  epithets <- unique(epithets[nzchar(epithets)])
  if (!length(epithets)) genus else paste(genus, paste(epithets, collapse = "/"))
}

#' Check a Custom Species
#'
#' @param genus Character scalar.
#' @param epithets Character vector.
#'
#' @return `NA` when valid, otherwise the reason it is refused.
#' @export
species_problem <- function(genus, epithets = character(0)) {
  genus <- trimws(genus %||% "")
  epithets <- trimws(epithets)
  epithets <- epithets[nzchar(epithets)]
  if (!nzchar(genus)) {
    return("Enter a genus.")
  }
  if (!grepl(NAME_PART_RE, genus) || nchar(genus) > 60L) {
    return("The genus may only contain letters, digits and hyphens.")
  }
  if (length(epithets) > MAX_EPITHETS) {
    return(sprintf("At most %d species can share one database.", MAX_EPITHETS))
  }
  if (any(!grepl(NAME_PART_RE, epithets) | nchar(epithets) > 60L)) {
    return("Species epithets may only contain letters, digits and hyphens.")
  }
  NA_character_
}

#' Species Catalog
#'
#' @description The species PhyloTrace ships descriptions and images for, one
#'   row per distinct species or species group.
#'
#' @return Data frame with `label` (the species name as stored), `genus`,
#'   `rank`, `taxid` and `image` (web path of its picture, `NA` without one).
#' @export
species_catalog <- function() {
  if (is.null(.cache$catalog)) {
    records <- species_metadata()
    label <- vapply(
      records,
      function(r) drop_curator(gsub("_", " ", r$species)),
      character(1)
    )
    abb <- vapply(records, function(r) r$abb %||% NA_character_, character(1))
    image_file <- file.path("app", "static", "species", paste0(abb, ".png"))
    out <- data.frame(
      label = label,
      genus = vapply(label, function(l) species_parts(l)$genus, character(1)),
      rank = vapply(records, function(r) r$rank %||% NA_character_, character(1)),
      taxid = vapply(records, function(r) as.integer(r$ncbi_taxid %||% NA), integer(1)),
      image = ifelse(
        file.exists(image_file),
        file.path("static", "species", paste0(abb, ".png")),
        NA_character_
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    out <- out[!duplicated(out$label), , drop = FALSE]
    .cache$catalog <- out[order(out$label), , drop = FALSE]
  }
  .cache$catalog
}

#' Look Up a Species in the Catalog
#'
#' @param species Character species name, spaces or underscores.
#'
#' @return The species' metadata record (list), or `NULL` when unknown.
#' @export
species_record <- function(species) {
  key <- match_key(drop_curator(gsub("_", " ", species %||% "")))
  for (record in species_metadata()) {
    if (identical(match_key(drop_curator(gsub("_", " ", record$species))), key)) {
      return(record)
    }
  }
  NULL
}

#' Picture of a Catalog Species
#'
#' @param species Character species name.
#'
#' @return Web path of the image, or `NA`.
#' @export
species_image <- function(species) {
  catalog <- species_catalog()
  hit <- match(match_key(drop_curator(gsub("_", " ", species %||% ""))), match_key(catalog$label))
  if (is.na(hit)) NA_character_ else catalog$image[hit]
}

#' Default Database Name for a Species
#'
#' @param species Character species name.
#'
#' @return A file-name-safe name such as `"Klebsiella_pneumoniae"`.
#' @export
default_db_name <- function(species) {
  parts <- species_parts(species)
  words <- c(parts$genus, head(parts$epithets, 1L))
  name <- paste(words[!is.na(words)], collapse = "_")
  name <- gsub("[^A-Za-z0-9_-]", "", name)
  if (nzchar(name)) name else "PhyloTrace_database"
}
