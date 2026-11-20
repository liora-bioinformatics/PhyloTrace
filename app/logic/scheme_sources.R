# app/logic/scheme_sources.R
#
# Providers a typing scheme can come from, with the terms of use the user has to
# respect and the acknowledgement each provider asks for. PhyloTrace never
# contacts these providers: the user downloads the files and imports them, and
# the database records where they came from.

box::use(
  rlang[`%||%`],
  stats[setNames],
)

# One provider per row. `offers` lists the scheme types it publishes, so a
# picker only names providers that can supply the scheme asked for;
# `terms_url` is the provider's own policy, which governs its data (reviewed
# November 2026); `commercial_use` sums up its rule for commercial use, shown
# next to the terms confirmation (the full wording, with the review date, is
# in dev/scheme_index/providers.json). Acknowledgement and citation are
# recorded in the database.
SOURCES <- list(
  cgmlst_org = list(
    offers = c("cgMLST"),
    label = "cgMLST.org Nomenclature Server (Ridom GmbH)",
    url = "https://www.cgmlst.org/ncs",
    terms_url = "https://www.cgmlst.org/serverpolicy.html",
    commercial_use = paste(
      "cgMLST.org: downloads for non-commercial purposes only; commercial use",
      "needs permission from Ridom GmbH."
    ),
    acknowledgement = paste(
      "This publication made use of the cgMLST.org website",
      "(http://www.cgMLST.org/) that is developed and maintained by Ridom GmbH."
    ),
    citation = paste(
      "Jünemann S, Sedlazeck FJ, Prior K, et al. Updating benchtop sequencing",
      "performance comparison. Nat Biotechnol. 2013;31:294-296."
    )
  ),
  pubmlst = list(
    offers = c("cgMLST", "wgMLST", "MLST"),
    label = "PubMLST (University of Oxford)",
    url = "https://pubmlst.org",
    terms_url = "https://pubmlst.org/terms-conditions",
    commercial_use = paste(
      "PubMLST: data submitted from 2025 on is limited to non-commercial",
      "research and public-health surveillance unless licensed; rMLST is",
      "academic only."
    ),
    acknowledgement = paste(
      "This publication made use of the PubMLST website (https://pubmlst.org/)",
      "sited at the University of Oxford. The development of that website was",
      "funded by the Wellcome Trust."
    ),
    citation = paste(
      "Jolley KA, Bray JE, Maiden MCJ. Open-access bacterial population",
      "genomics: BIGSdb software, the PubMLST.org website and their",
      "applications. Wellcome Open Res. 2018;3:124."
    )
  ),
  pasteur = list(
    offers = c("cgMLST", "MLST"),
    label = "Institut Pasteur BIGSdb",
    url = "https://bigsdb.pasteur.fr",
    terms_url = "https://bigsdb.pasteur.fr/policy/",
    commercial_use = paste(
      "Institut Pasteur: academic, non-commercial use only; other uses need a",
      "licence from Institut Pasteur."
    ),
    acknowledgement = NA_character_,
    citation = paste(
      "Jolley KA, Maiden MCJ. BIGSdb: Scalable analysis of bacterial genome",
      "variation at the population level. BMC Bioinformatics. 2010;11:595."
    )
  ),
  chewie_ns = list(
    offers = c("cgMLST", "wgMLST"),
    label = "Chewie-NS (chewBBACA)",
    url = "https://chewbbaca.online",
    terms_url = "https://chewbbaca.online",
    commercial_use = paste(
      "Chewie-NS: no data licence published; schemas adapted from other",
      "providers can carry those providers' terms."
    ),
    acknowledgement = NA_character_,
    citation = paste(
      "Mamede R, Vila-Cerqueira P, Silva M, Carriço JA, Ramirez M. Chewie",
      "Nomenclature Server (chewie-NS): a deployable nomenclature server for",
      "easy sharing of core and whole genome MLST schemas. Nucleic Acids Res.",
      "2021;49(D1):D660-D666."
    )
  ),
  enterobase = list(
    offers = c("cgMLST", "wgMLST", "MLST"),
    label = "EnteroBase (University of Warwick)",
    url = "https://enterobase.warwick.ac.uk",
    terms_url = "https://enterobase.warwick.ac.uk/terms",
    commercial_use = paste(
      "EnteroBase: academic use only; commercial or business use needs a",
      "licence from the University of Warwick."
    ),
    acknowledgement = NA_character_,
    citation = paste(
      "Zhou Z, Alikhan NF, Mohamed K, Fan Y, Achtman M. The EnteroBase user's",
      "guide, with case studies on Salmonella transmissions, Yersinia pestis",
      "phylogeny, and Escherichia core genomic diversity. Genome Res.",
      "2020;30:138-152."
    )
  ),
  own = list(
    offers = c("cgMLST", "wgMLST", "MLST"),
    label = "Own / in-house scheme",
    url = NA_character_,
    terms_url = NA_character_,
    commercial_use = NA_character_,
    acknowledgement = NA_character_,
    citation = NA_character_
  ),
  other = list(
    offers = c("cgMLST", "wgMLST", "MLST"),
    label = "Other provider",
    url = NA_character_,
    terms_url = NA_character_,
    commercial_use = paste(
      "Many scheme providers allow non-commercial use only; check this",
      "provider's terms before commercial use."
    ),
    acknowledgement = NA_character_,
    citation = NA_character_
  )
)

#' Scheme Provider Choices
#'
#' @param types Character scheme types (`"cgMLST"`, `"wgMLST"`, `"MLST"`); only
#'   providers publishing at least one of them are offered. `NULL` for all.
#'
#' @return Named character vector for a select input: labels named by id.
#' @export
source_choices <- function(types = NULL) {
  keep <- vapply(SOURCES, function(s) is.null(types) || any(s$offers %in% types), logical(1))
  sources <- SOURCES[keep]
  setNames(names(sources), vapply(sources, function(s) s$label, character(1)))
}

#' Scheme Provider Details
#'
#' @param id Character provider id, as in `source_choices()`.
#'
#' @return List with `id`, `offers`, `label`, `url`, `terms_url`,
#'   `commercial_use`, `acknowledgement` and `citation`; `NULL` for an unknown
#'   id.
#' @export
scheme_source <- function(id) {
  if (is.null(id) || length(id) != 1L || is.na(id) || !id %in% names(SOURCES)) {
    return(NULL)
  }
  c(list(id = id), SOURCES[[id]])
}

#' Is a Web Address Safe to Store and Link?
#'
#' @param url Character scalar.
#'
#' @return `TRUE` for an `http(s)` URL without whitespace or markup characters.
#' @export
safe_url <- function(url) {
  is.character(url) &&
    length(url) == 1L &&
    !is.na(url) &&
    nchar(url) <= 500L &&
    grepl("^https?://[^[:space:]<>\"'`]+$", url)
}

#' Check a Scheme Version
#'
#' @description Versions are handed to pyMLST on its command line, so only
#'   plain characters are accepted.
#'
#' @param version Character scalar, possibly empty.
#'
#' @return `NULL` when acceptable, otherwise the reason it is refused.
#' @export
version_problem <- function(version) {
  version <- trimws(version %||% "")
  if (nzchar(version) && !grepl("^[[:alnum:] ._:/+-]{1,100}$", version)) {
    "The version may only contain letters, digits, spaces and . _ - + : /"
  }
}
