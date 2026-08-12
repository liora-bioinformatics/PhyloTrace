# app/logic/mlst_repo.R
#
# Deterministic lookup of a classical 7-gene MLST scheme for a database's
# species, against the PubMLST and Institut Pasteur REST APIs.
#
# pyMLST's own `claMLST import` searches both the species and the scheme by
# fuzzy substring match and refuses to guess whenever more than one hit comes
# back, which silently loses the ST for species whose repository holds several
# schemes (Enterococcus faecium) and can just as silently settle on the wrong
# genus (a "pseudotuberculosis" query matching Yersinia for a Corynebacterium
# scheme). This module resolves the database and the scheme itself - genus
# always enforced, never more than one candidate accepted - and hands the exact
# download URLs to loop-pymlst.sh, which builds the reference with
# `claMLST create` instead.

box::use(
  jsonlite[fromJSON],
)
box::use(
  app / logic / logging[log_event],
  app / logic / schemes[cgmlst_org_schemes],
)

#' Classical MLST Repository Endpoints
#'
#' @description REST roots of the two repositories pyMLST supports, in the order
#'   the typing run falls back through them.
#'
#' @return Named character vector of API base URLs.
#' @export
REPO_URLS <- c(
  pubmlst = "https://rest.pubmlst.org/db",
  pasteur = "https://bigsdb.pasteur.fr/api/db"
)

# Words a repository puts in a database description that carry no taxonomy.
# Everything left after these is treated as a genus or species claim.
DESC_NOISE <- c(
  "rest", "api", "access", "to", "the", "of", "and", "for",
  "seqdef", "database", "databases", "sequence", "sequences",
  "profile", "profiles", "definitions", "typing", "mlst",
  "spp", "complex", "candidatus", "group"
)

# Scheme descriptions that are never a classical 7-gene scheme.
SCHEME_BLACKLIST <- c("cgmlst", "wgmlst", "extended mlst")

# Lowercases, drops punctuation and splits into words.
tokenize <- function(x) {
  words <- strsplit(tolower(gsub("[^[:alnum:][:space:]/]+", " ", x)), "[[:space:]/]+")[[1]]
  words[nzchar(words) & is.na(suppressWarnings(as.numeric(words)))]
}

# Genus and the epithets a scheme covers: "Klebsiella pneumoniae/variicola" ->
# genus "klebsiella", epithets "pneumoniae" and "variicola".
species_parts <- function(species) {
  words <- tokenize(species)
  if (!length(words)) {
    return(list(genus = character(0), epithets = character(0)))
  }
  list(genus = words[1], epithets = setdiff(words[-1], c("sp", "spp")))
}

# Meaningful tokens of a repository database description.
desc_tokens <- function(description) {
  setdiff(tokenize(description), DESC_NOISE)
}

# pyMLST reads the cgmlst.org scheme page with `line.lstrip("Genus")` /
# `line.lstrip("Species")` (common/web.py). `lstrip` strips a *character set*,
# so any name starting with one of those letters is eaten into: "pneumoniae"
# reaches the database as "neumoniae". This reproduces the damage so a stored
# name can be matched back to the scheme it came from.
lstrip_set <- function(x, chars) {
  set <- strsplit(chars, "")[[1]]
  letters_x <- strsplit(x, "")[[1]]
  keep <- which(!(letters_x %in% set))
  if (!length(keep)) "" else paste(letters_x[keep[1]:length(letters_x)], collapse = "")
}

#' Reproduce pyMLST's Damage to a Scheme Name
#'
#' @description Returns the form pyMLST would have stored in `mlst_type.species`
#'   for a cgmlst.org scheme name, so a stored name can be matched back to the
#'   scheme it came from.
#'
#' @param species Character string. Scheme species, spelled with spaces.
#'
#' @return The mangled species string, or `""` for an empty name.
#' @export
mangle_species <- function(species) {
  parts <- strsplit(trimws(species), " ", fixed = TRUE)[[1]]
  if (!length(parts)) {
    return("")
  }
  genus <- lstrip_set(parts[1], "Genus")
  epithet <- if (length(parts) > 1) {
    lstrip_set(paste(parts[-1], collapse = " "), "Species")
  } else {
    ""
  }
  trimws(paste(genus, epithet))
}

#' Repair a Scheme Species Name
#'
#' @description Maps the species string stored in a database's `mlst_type` table
#'   back to the cgmlst.org scheme name it was downloaded from, undoing the
#'   letter loss pyMLST inflicts on names whose genus starts with G/e/n/u/s or
#'   whose epithet starts with S/p/e/c/i/s (`Klebsiella neumoniae/...`).
#'
#' @param species Character string. Species as stored in the database.
#'
#' @return The canonical scheme species (spaces, not underscores), the trimmed
#'   input when no scheme matches it unambiguously, or `NA_character_`.
#' @export
canonical_species <- function(species) {
  if (
    is.null(species) ||
      length(species) != 1 ||
      is.na(species) ||
      !nzchar(trimws(species))
  ) {
    return(NA_character_)
  }
  given <- gsub("[[:space:]]+", " ", trimws(gsub("_", " ", species)))
  known <- gsub("_", " ", cgmlst_org_schemes$species)

  exact <- which(tolower(known) == tolower(given))
  if (length(exact)) {
    return(known[exact[1]])
  }
  # Only an unambiguous repair is applied: two schemes damaged into the same
  # string would make the choice a guess.
  damaged <- which(tolower(vapply(known, mangle_species, character(1))) == tolower(given))
  if (length(damaged) == 1) {
    return(known[damaged])
  }
  given
}

#' Pick the Repository Database for a Species
#'
#' @description Chooses the one seqdef database a species belongs to. A
#'   description claiming any other species is rejected outright, so a scheme is
#'   never resolved across genera, and an unresolvable set is reported rather
#'   than guessed.
#'
#' @param descriptions Character vector of seqdef database descriptions.
#' @param species Character string. Canonical scheme species.
#'
#' @return Integer index into `descriptions`, or `NA_integer_`. The attribute
#'   `"reason"` carries why nothing was picked.
#' @export
match_species_db <- function(descriptions, species) {
  fail <- function(reason) structure(NA_integer_, reason = reason)
  parts <- species_parts(species)
  if (!length(parts$genus) || !length(descriptions)) {
    return(fail("no species to look up"))
  }
  # Repositories abbreviate a binomial as genus initial + epithet ("ecoli").
  abbrev <- paste0(substr(parts$genus, 1, 1), parts$epithets)

  level <- vapply(
    descriptions,
    function(desc) {
      toks <- desc_tokens(desc)
      if (!length(toks)) {
        return("")
      }
      ours <- c(parts$genus, parts$epithets, abbrev)
      if (length(setdiff(toks, ours))) {
        return("")
      }
      # A bare genus database ("Salmonella spp.", "Yersinia") serves the whole
      # genus; anything naming one of our epithets is the better match.
      if (any(toks %in% c(parts$epithets, abbrev))) "species" else "genus"
    },
    character(1)
  )

  for (want in c("species", "genus")) {
    hits <- unname(which(level == want))
    if (length(hits) == 1) {
      return(hits)
    }
    if (length(hits) > 1) {
      return(fail(sprintf(
        "%d %s-level databases match: %s",
        length(hits),
        want,
        paste(descriptions[hits], collapse = "; ")
      )))
    }
  }
  fail("no database for this species")
}

#' Pick the Classical MLST Scheme of a Database
#'
#' @description Chooses the classical scheme among a database's MLST-like
#'   schemes. A repository can hold several - Enterococcus faecium carries the
#'   7-locus scheme plus an 8-locus revision, Escherichia the 7-locus Achtman
#'   scheme next to the 8-locus Pasteur one - so the repository's own plain
#'   "MLST" wins, then the seven-locus scheme classical MLST is defined as, then
#'   one named after the species. Some species carry two *equally* classical
#'   7-locus schemes under different naming conventions (Acinetobacter
#'   baumannii: "MLST (Oxford)" next to "MLST (Pasteur)"), which the structural
#'   rules above cannot break - `prefer` settles those by convention name, and
#'   anything still undecidable is reported, never guessed.
#'
#' @param descriptions Character vector of scheme descriptions.
#' @param loci Integer vector of locus counts, parallel to `descriptions`.
#' @param species Character string. Canonical scheme species.
#' @param prefer Character string (optional). `"pasteur"` keeps only tied
#'   candidates naming the Pasteur convention; anything else keeps the rest -
#'   applied only when it narrows a tie to exactly one.
#'
#' @return Integer index into `descriptions`, or `NA_integer_` with a `"reason"`.
#' @export
match_mlst_scheme <- function(descriptions, loci, species, prefer = NA_character_) {
  if (!length(descriptions)) {
    return(structure(NA_integer_, reason = "no classical MLST scheme in this database"))
  }
  normalized <- trimws(tolower(descriptions))
  has_preference <- !is.null(prefer) && length(prefer) == 1 && !is.na(prefer) && nzchar(prefer)

  narrow_by_convention <- function(idx) {
    if (length(idx) <= 1 || !has_preference) {
      return(idx)
    }
    is_pasteur <- grepl("pasteur", normalized[idx], fixed = TRUE)
    kept <- if (identical(tolower(prefer), "pasteur")) idx[is_pasteur] else idx[!is_pasteur]
    if (length(kept) == 1) kept else idx
  }

  plain <- unname(which(normalized == "mlst"))
  if (length(plain)) {
    return(plain[1])
  }

  seven <- narrow_by_convention(unname(which(loci == 7L)))
  if (length(seven) == 1) {
    return(seven)
  }

  epithets <- species_parts(species)$epithets
  named <- integer(0)
  if (length(epithets)) {
    named <- narrow_by_convention(unname(which(vapply(
      normalized,
      function(d) any(vapply(epithets, grepl, logical(1), x = d, fixed = TRUE)),
      logical(1)
    ))))
    if (length(named) == 1) {
      return(named)
    }
  }

  if (length(descriptions) == 1) {
    return(1L)
  }

  # A last pass across everything: settles a tie the structural rules never
  # even narrowed (e.g. two schemes that share both locus count and a species
  # mention).
  all_idx <- narrow_by_convention(seq_along(descriptions))
  if (length(all_idx) == 1) {
    return(all_idx)
  }

  structure(
    NA_integer_,
    reason = sprintf(
      "%d schemes match: %s",
      length(descriptions),
      paste(sprintf("%s (%d loci)", descriptions, loci), collapse = "; ")
    )
  )
}

# Reads a JSON endpoint. Kept small and injectable so scheme resolution can be
# exercised without network access.
fetch_json <- function(endpoint, timeout = 30) {
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  con <- url(endpoint, encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  fromJSON(paste(readLines(con, warn = FALSE), collapse = ""), simplifyVector = FALSE)
}

# All seqdef databases of one repository, flattened out of its record groups.
repo_databases <- function(repo, fetch) {
  records <- fetch(REPO_URLS[[repo]])
  out <- list()
  for (record in records) {
    if (identical(record$name, "test")) {
      next
    }
    for (db in record$databases) {
      if (grepl("seqdef$", db$name)) {
        out[[length(out) + 1L]] <- list(
          description = trimws(sub("sequence/profile definitions", "", db$description)),
          href = db$href
        )
      }
    }
  }
  out
}

# The MLST-like schemes of one seqdef database: everything that is not a
# cg/wgMLST scheme, publishes a profile table and stays under 10 loci - the same
# test pyMLST applies, so a scheme this accepts is one claMLST can build.
mlst_schemes <- function(db_href, fetch) {
  listed <- fetch(paste0(db_href, "/schemes"))$schemes
  out <- list()
  for (scheme in listed) {
    description <- trimws(scheme$description)
    if (any(vapply(SCHEME_BLACKLIST, grepl, logical(1), x = tolower(description), fixed = TRUE))) {
      next
    }
    detail <- fetch(scheme$scheme)
    if (is.null(detail$profiles_csv) || length(detail$loci) >= 10) {
      next
    }
    out[[length(out) + 1L]] <- list(
      description = description,
      url = scheme$scheme,
      profiles = detail$profiles_csv,
      loci = unlist(detail$loci),
      version = if (is.null(detail$last_added)) NA_character_ else detail$last_added
    )
  }
  out
}

#' Resolve a Species to a Downloadable Classical MLST Scheme
#'
#' @description Walks the repositories in order and returns the first one that
#'   resolves the species to exactly one database and one classical scheme.
#'   Network or API failures are treated as "not found" for that repository, so
#'   typing continues without classical MLST rather than failing.
#'
#' @details
#' Where a repository's own naming decides nothing (Acinetobacter baumannii
#' carries an equally classical "MLST (Oxford)" and "MLST (Pasteur)" scheme,
#' tied on locus count and neither named for the species), `repos[1]` - the
#' repository the caller put first, i.e. what the "Scheme source" selection
#' already means - breaks the tie: picking "pasteur" keeps the Pasteur-named
#' convention wherever it is hosted, picking "pubmlst" keeps the other one. It
#' is applied against every repository tried, not just the one currently being
#' queried, since it expresses a naming preference rather than a repository
#' choice once a database has been found.
#'
#' @param species Character string. Scheme species (repaired via `canonical_species()`).
#' @param repos Character vector of repository names, in preference order.
#' @param fetch Function taking a URL and returning parsed JSON. Injectable for tests.
#'
#' @return A list, always. On success: `resolved = TRUE` plus `repository`,
#'   `species`, `database`, `scheme`, `scheme_url`, `profiles_url`, `loci` and
#'   `version`. On failure: `resolved = FALSE`, `species` (canonical when
#'   resolvable, otherwise the raw input) and `attempts` - a character vector of
#'   per-repository failure reasons, named by repository.
#' @export
resolve_mlst_scheme <- function(
  species,
  repos = names(REPO_URLS),
  fetch = fetch_json
) {
  canonical <- canonical_species(species)
  if (is.na(canonical)) {
    return(list(resolved = FALSE, species = species, attempts = character(0)))
  }
  repos <- intersect(repos, names(REPO_URLS))
  prefer <- if (length(repos)) repos[1] else NA_character_
  attempts <- character(0)

  for (repo in repos) {
    hit <- tryCatch(
      {
        dbs <- repo_databases(repo, fetch)
        idx <- match_species_db(vapply(dbs, function(d) d$description, character(1)), canonical)
        if (is.na(idx)) {
          reason <- attr(idx, "reason")
          attempts[repo] <- reason
          log_event("MLST", "scheme lookup", sprintf("%s / %s: %s", repo, canonical, reason))
          NULL
        } else {
          schemes <- mlst_schemes(dbs[[idx]]$href, fetch)
          pick <- match_mlst_scheme(
            vapply(schemes, function(s) s$description, character(1)),
            vapply(schemes, function(s) length(s$loci), integer(1)),
            canonical,
            prefer = prefer
          )
          if (is.na(pick)) {
            reason <- attr(pick, "reason")
            attempts[repo] <- reason
            log_event("MLST", "scheme lookup", sprintf("%s / %s: %s", repo, canonical, reason))
            NULL
          } else {
            list(
              resolved = TRUE,
              repository = repo,
              species = canonical,
              database = dbs[[idx]]$description,
              scheme = schemes[[pick]]$description,
              scheme_url = schemes[[pick]]$url,
              profiles_url = schemes[[pick]]$profiles,
              loci = schemes[[pick]]$loci,
              version = schemes[[pick]]$version
            )
          }
        }
      },
      error = function(e) {
        attempts[repo] <<- conditionMessage(e)
        log_event("MLST", "scheme lookup failed", sprintf("%s: %s", repo, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(hit)) {
      log_event("MLST", "scheme resolved", sprintf(
        "%s -> %s / %s (%s), %d loci",
        canonical, hit$repository, hit$database, hit$scheme, length(hit$loci)
      ))
      return(hit)
    }
  }
  list(resolved = FALSE, species = canonical, attempts = attempts)
}

#' Write a Scheme Specification File
#'
#' @description Serializes a resolved scheme into the `key=value` file
#'   loop-pymlst.sh reads to download the reference and build it with
#'   `claMLST create`.
#'
#' @param spec List returned by `resolve_mlst_scheme()`.
#' @param path Character string. Destination file.
#'
#' @return `path`, invisibly.
#' @export
write_scheme_spec <- function(spec, path) {
  lines <- c(
    paste0("repository=", spec$repository),
    paste0("species=", spec$species),
    paste0("database=", spec$database),
    paste0("scheme=", spec$scheme),
    paste0("version=", if (is.na(spec$version)) "" else spec$version),
    paste0("profiles=", spec$profiles_url),
    paste0("locus=", spec$loci)
  )
  writeLines(lines, path)
  invisible(path)
}

#' Describe a Classical MLST Scheme Resolution
#'
#' @description Formats the outcome of `resolve_mlst_scheme()` as a line (or a
#'   few) for a typing run's own transcript - the place a user reviewing a
#'   finished run actually looks, unlike the app's internal event log, which
#'   `resolve_mlst_scheme()` already reports the same events to as they happen.
#'
#' @param cla_scheme List from `resolve_mlst_scheme()`. `NULL`, or a `species`
#'   of `NA`, both read as "no species was available to resolve at all".
#'
#' @return Character string, one or more lines separated by `"\n"`.
#' @export
describe_resolution <- function(cla_scheme) {
  no_species <- is.null(cla_scheme) ||
    is.null(cla_scheme$species) ||
    is.na(cla_scheme$species)
  if (no_species) {
    return("Classical MLST: no resolvable species for this scheme (skipping ST calls for this run).")
  }
  if (isTRUE(cla_scheme$resolved)) {
    return(sprintf(
      "Classical MLST scheme resolved: %s / %s (%s), %d loci",
      cla_scheme$repository,
      cla_scheme$database,
      cla_scheme$scheme,
      length(cla_scheme$loci)
    ))
  }
  paste(
    c(
      sprintf(
        "Classical MLST: no single scheme resolved for '%s' (skipping ST calls for this run).",
        cla_scheme$species
      ),
      sprintf("  %s: %s", names(cla_scheme$attempts), cla_scheme$attempts)
    ),
    collapse = "\n"
  )
}
