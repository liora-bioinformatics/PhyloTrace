# dev/fetch_species_metadata.R
#
# Extends the species catalogue (app/logic/data/species_metadata.json) with
# every species the scheme index calls for (see the catalogue rule below) and
# enriches new entries with metadata fetched from public sources:
#   * NCBI Taxonomy (E-utilities) -> TaxID, rank, full lineage
#
# Writes the result back to the same file.
#
# Run from the project root:  Rscript dev/fetch_species_metadata.R

box::use(
  curl[curl_fetch_memory, handle_setheaders, new_handle],
  jsonlite[fromJSON, toJSON],
  utils[URLencode, read.csv],
  xml2[read_xml, xml_find_all, xml_find_first, xml_text],
)

index_path <- "app/logic/data/scheme_index.csv"
out_path <- "app/logic/data/species_metadata.json"

ua <- paste(
  "PhyloTrace/1.0",
  "(species catalogue metadata; https://github.com/liora-bioinformatics/PhyloTrace)"
)

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

# --- catalogue rule ---------------------------------------------------------
# The catalogue holds every species (or species group) with at least one
# cgMLST or wgMLST scheme in the app's scheme index; classical MLST is
# optional. A species already covered by a catalogue group gets no entry of
# its own, and a genus-wide scheme only adds "<Genus> spp" where the genus has
# no species entry - the app suggests genus-wide schemes for every species of
# the genus anyway.

split_label <- function(label) {
  words <- strsplit(trimws(gsub("_", " ", label)), " ", fixed = TRUE)[[1]]
  epithets <- if (length(words) > 1) strsplit(words[2], "/", fixed = TRUE)[[1]] else character(0)
  list(genus = words[1], epithets = setdiff(epithets, c("spp", "spp.")))
}

missing_entries <- function(records, index) {
  core <- sort(unique(index$species[index$type %in% c("cgMLST", "wgMLST")]))
  labels <- vapply(records, function(r) sub(" \\(.*\\)$", "", gsub("_", " ", r$species)), "")
  parsed <- lapply(labels, split_label)
  covered <- unlist(lapply(parsed, function(p) tolower(paste(p$genus, p$epithets))))
  genera <- tolower(vapply(parsed, function(p) p$genus, ""))
  out <- character(0)
  genus_only <- character(0)
  for (label in core) {
    p <- split_label(label)
    if (!length(p$epithets)) {
      genus_only <- c(genus_only, p$genus)
    } else if (!all(tolower(paste(p$genus, p$epithets)) %in% covered)) {
      out <- c(out, label)
      genera <- c(genera, tolower(p$genus))
    }
  }
  genus_only <- unique(genus_only[!tolower(genus_only) %in% genera])
  c(out, sprintf("%s spp", genus_only))
}

# --- main ------------------------------------------------------------------
# Keeps every existing record (taxonomy, description, image) and adds the
# species the scheme index now calls for, with NCBI taxonomy. Descriptions are
# added by dev/synthesize_descriptions.R. Only runs when executed via Rscript.
main <- function() {
  records <- fromJSON(out_path, simplifyVector = FALSE)
  index <- read.csv(index_path, stringsAsFactors = FALSE)
  add <- missing_entries(records, index)
  message(length(add), " catalogue entries to add")

  for (label in add) {
    query <- clean_name(label)
    message(sprintf("  %s  ->  '%s'", label, query))
    tax <- ncbi_taxonomy(query)
    Sys.sleep(0.4) # respect NCBI rate limits (<3 req/s without key)
    p <- split_label(label)
    records[[length(records) + 1L]] <- c(
      list(
        species = gsub(" ", "_", label),
        abb = if (length(p$epithets)) {
          paste0(substr(p$genus, 1L, 1L), p$epithets[1])
        } else {
          p$genus
        },
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
