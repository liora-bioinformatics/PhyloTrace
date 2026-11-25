# app/logic/urls.R
#
# Every external web address the app writes into its own code, and the remote services it
# draws basemaps from. Other modules import from here instead of spelling out a URL, so the
# link check (tests/testthat/test-external_links.R) covers each of them. Scheme providers keep
# their addresses together with their terms and citation in scheme_sources.R, and the scheme
# index and species metadata carry theirs as data.

box::use(
  curl[curl_escape],
)

#' Website of PhyloTrace
#' @export
WEBSITE <- "https://liora-bioinformatics.com/phylotrace"

#' Source Code Repository
#' @export
GITHUB <- "https://github.com/liora-bioinformatics/PhyloTrace"

#' DOI of the PhyloTrace Release Archive
#' @export
DOI <- "https://doi.org/10.5281/zenodo.10996423"

#' Licence Text of PhyloTrace (GPL v3)
#' @export
GPL_LICENSE <- "https://www.gnu.org/licenses/gpl-3.0"

#' Basemaps Offered on the Map
#'
#' @description Display name to `leaflet.providers` name. Only providers that serve tiles
#'   without an API key: CARTO's basemaps stamp "API KEY REQUIRED" across every tile requested
#'   without one, so they are not offered.
#' @export
MAP_BASEMAPS <- c(
  "OpenStreetMap" = "OpenStreetMap",
  "OSM Humanitarian" = "OpenStreetMap.HOT",
  "OSM (German style)" = "OpenStreetMap.DE",
  "OpenTopoMap" = "OpenTopoMap",
  "Esri Satellite" = "Esri.WorldImagery",
  "Esri Topographic" = "Esri.WorldTopoMap",
  "Esri Streets" = "Esri.WorldStreetMap",
  "Esri NatGeo" = "Esri.NatGeoWorldMap",
  "Esri Gray Canvas" = "Esri.WorldGrayCanvas"
)

#' Label Tiles over the Choropleth
#'
#' @description Esri's labels-only reference layer for its gray canvas. It is not in
#'   `leaflet.providers`, hence a URL rather than a provider name. `{z}`, `{y}` and `{x}` are
#'   filled in by Leaflet.
#' @export
MAP_LABEL_TILES <- paste0(
  "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
  "World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}"
)

#' Nominatim Search Request for a Place
#'
#' @description OpenStreetMap's Nominatim geocoder, asked for the single best match of a
#'   free-text place as JSON.
#'
#' @param place Character string, e.g. `"Berlin, Germany"`.
#' @return URL of the search request.
#' @export
nominatim_search_url <- function(place) {
  paste0(
    "https://nominatim.openstreetmap.org/search?format=jsonv2&limit=1&q=",
    curl_escape(place)
  )
}

#' NCBI Taxonomy Page of a Taxon
#'
#' @param taxid Integer NCBI taxonomy id.
#' @return URL of the taxon's NCBI Taxonomy Browser page.
#' @export
taxonomy_url <- function(taxid) {
  paste0("https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=", taxid)
}

#' Creative Commons Licence Page
#'
#' @param kind Licence kind: `"by"`, `"by-sa"` or `"zero"` (CC0).
#' @param version Licence version such as `"4.0"`; ignored for `"zero"`.
#' @return URL of the licence deed.
#' @export
cc_license_url <- function(kind, version = NULL) {
  if (identical(kind, "zero")) {
    return("https://creativecommons.org/publicdomain/zero/1.0/")
  }
  sprintf("https://creativecommons.org/licenses/%s/%s/", kind, version)
}

#' PubMLST REST API
#'
#' @description Root of the API of PubMLST's databases, named in the scheme download guide's
#'   commands (`<root>/<database>/schemes/<id>/loci`).
#' @export
PUBMLST_API <- "https://rest.pubmlst.org/db"

#' Institut Pasteur BIGSdb Web Interface
#'
#' @description Base of the scheme pages the download guide links to
#'   (`?db=<database>&page=downloadAlleles&scheme_id=<id>`).
#' @export
PASTEUR_ACCOUNT <- "https://bigsdb.pasteur.fr/cgi-bin/bigsdb/bigsdb.pl"

#' How to Request a BIGSdb-Pasteur API Key
#' @export
PASTEUR_API_KEY_HELP <- "https://bigsdb.pasteur.fr/requesting-api-key/"

#' Chewie-NS Schemas Page
#'
#' @description Overview of the hosted schemas; each has a "Schema details" page offering the
#'   compressed schema.
#' @export
CHEWIE_NS_SCHEMAS <- "https://chewbbaca.online/stats"

#' EnteroBase Scheme Directory
#'
#' @description Public directory with one folder per scheme: a gzipped FASTA file per locus
#'   and, for MLST, the profile table.
#' @export
ENTEROBASE_SCHEMES <- "https://enterobase.warwick.ac.uk/schemes/"

#' Chewie-NS API Request
#'
#' @description The API root is no page of its own, so the guide builds its requests from it.
#'
#' @param path Character path below the API root, starting with `/`.
#' @return URL of the request.
#' @export
chewie_ns_api <- function(path) {
  paste0("https://chewbbaca.online/api/NS/api", path)
}
