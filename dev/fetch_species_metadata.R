# dev/fetch_species_metadata.R
#
# Reads app/logic/data/cgmlst_schemes.csv and enriches each bacterial species
# with metadata fetched from public sources:
#   * NCBI Taxonomy (E-utilities) -> TaxID, rank, full lineage
#
# Output: app/logic/data/species_metadata.json
#
# Run from the project root:  Rscript dev/fetch_species_metadata.R

library(curl)
library(jsonlite)
library(xml2)

csv_path <- "app/logic/data/cgmlst_schemes.csv"
out_path <- "app/logic/data/species_metadata.json"

ua <- "PhyloTrace/1.0 (scheme browser metadata fetch; contact: phylotrace)"

# --- helpers ---------------------------------------------------------------

# Polite GET returning raw text, or NULL on failure.
# Retries on transient failures / rate limiting (429, 5xx) with backoff.
http_get <- function(url, accept = NULL, tries = 4L) {
  for (attempt in seq_len(tries)) {
    h <- new_handle(useragent = ua, timeout = 30L)
    if (!is.null(accept)) handle_setheaders(h, Accept = accept)
    res <- tryCatch(curl_fetch_memory(url, handle = h), error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200) return(rawToChar(res$content))
    transient <- is.null(res) || res$status_code == 429 ||
      res$status_code >= 500
    if (!transient || attempt == tries) return(NULL)
    Sys.sleep(attempt) # linear backoff: 1s, 2s, 3s
  }
  NULL
}

# Derive a clean genus/binomial query name from the scheme species string.
clean_name <- function(sp) {
  s <- gsub("_", " ", sp)
  toks <- strsplit(trimws(s), "\\s+")[[1]]
  noise <- c("FLI", "RKI", "complex", "spp", "spp.", "sensu", "lato")
  toks <- toks[!toks %in% noise]
  if (length(toks) > 2) toks <- toks[1:2]
  paste(toks, collapse = " ")
}

ncbi_taxonomy <- function(name) {
  q <- URLencode(name, reserved = TRUE)
  search_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    "?db=taxonomy&retmode=json&term=", q
  )
  sj <- http_get(search_url)
  if (is.null(sj)) return(NULL)
  ids <- fromJSON(sj)$esearchresult$idlist
  if (length(ids) == 0) return(NULL)
  taxid <- ids[[1]]

  fetch_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
    "?db=taxonomy&retmode=xml&id=", taxid
  )
  xml_txt <- http_get(fetch_url)
  if (is.null(xml_txt)) return(NULL)
  doc <- tryCatch(read_xml(xml_txt), error = function(e) NULL)
  if (is.null(doc)) return(NULL)

  taxon <- xml_find_first(doc, "//TaxaSet/Taxon")
  if (inherits(taxon, "xml_missing")) return(NULL)

  ranks_of_interest <- c(
    "superkingdom", "phylum", "class", "order", "family", "genus"
  )
  lineage <- list()
  for (t in xml_find_all(taxon, "./LineageEx/Taxon")) {
    rank <- xml_text(xml_find_first(t, "./Rank"))
    if (rank %in% ranks_of_interest) {
      lineage[[rank]] <- xml_text(xml_find_first(t, "./ScientificName"))
    }
  }

  list(
    ncbi_taxid = as.integer(taxid),
    scientific_name = xml_text(xml_find_first(taxon, "./ScientificName")),
    rank = xml_text(xml_find_first(taxon, "./Rank")),
    lineage = lineage
  )
}

# --- main ------------------------------------------------------------------
# Only runs when executed directly (Rscript), not when source()d for helpers.
main <- function() {

schemes <- read.csv(csv_path, stringsAsFactors = FALSE)

records <- vector("list", nrow(schemes))
for (i in seq_len(nrow(schemes))) {
  sp <- schemes$species[i]
  query <- clean_name(sp)
  message(sprintf("[%2d/%d] %s  ->  '%s'", i, nrow(schemes), sp, query))

  tax <- ncbi_taxonomy(query)
  Sys.sleep(0.4) # respect NCBI rate limits (<3 req/s without key)

  records[[i]] <- c(
    list(
      species = sp,
      abb = schemes$abb[i],
      query_name = query
    ),
    if (!is.null(tax)) tax else list(ncbi_taxid = NA, lineage = NULL)
  )
}

writeLines(
  toJSON(records, pretty = TRUE, auto_unbox = TRUE, na = "null"),
  out_path
)
message("\nWrote ", length(records), " records to ", out_path)

}

# Run main() only when this file is executed via Rscript, not when source()d.
if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  main()
}
