# app/logic/mlst_overview.R
#
# Classical MLST across a whole database: the per-isolate, per-locus and per-ST
# summaries and provenance checks the "MLST Overview" view is built from, all
# derived on read from `classical_mlst` and `typing_provenance`.

box::use(
  DBI[dbDisconnect, dbGetQuery, dbListFields, dbListTables],
  stats[setNames],
)
box::use(
  app / logic / db_connect[connect],
  app /
    logic /
    overview_common[
      blank_na,
      check_list,
      database_isolates,
      modal,
      read_cols,
      tally,
      threshold_pairs,
    ],
)

#' Classical MLST Call Status Labels
#'
#' @description Display label of every per-isolate classical MLST state, in the
#'   order they are reported. The first five come from allele calling itself;
#'   `failed` / `skipped` only from provenance; `unknown` is a provenance row
#'   with no status, `untyped` an isolate with no classical record at all.
#'
#' @export
STATUS_LABELS <- c(
  known = "Known ST",
  inferred = "Inferred ST",
  novel = "Novel profile",
  partial = "Partial profile",
  ambiguous = "Ambiguous",
  failed = "Failed",
  skipped = "Skipped",
  unknown = "Not recorded",
  untyped = "Not typed"
)

#' Classical MLST Call Status Tones
#'
#' @description Bootstrap contextual tone of each status, parallel to
#'   `STATUS_LABELS`.
#'
#' @export
STATUS_TONES <- c(
  known = "success",
  inferred = "success",
  novel = "info",
  partial = "warning",
  ambiguous = "warning",
  failed = "danger",
  skipped = "secondary",
  unknown = "secondary",
  untyped = "secondary"
)

#' Classical MLST Call Statuses
#'
#' @description Statuses that come out of allele calling itself, i.e. those an
#'   isolate should have stored allele rows for.
#'
#' @export
CALLED_STATUSES <- c("known", "inferred", "novel", "partial", "ambiguous")

#' Unresolved Classical MLST Call Statuses
#'
#' @description Calls an updated profile table could still turn into a
#'   registered ST.
#'
#' @export
UNRESOLVED_STATUSES <- c("novel", "partial", "ambiguous")

CALL_COLS <- c(
  "isolate", "gene", "allele", "st", "status", "scheme", "scheme_version",
  "repository", "identity", "coverage", "pymlst_version", "called_at"
)

PROV_COLS <- c(
  "isolate", "cla_status", "cla_scheme", "cla_scheme_version", "cla_repository"
)

# Locus names travel through pyMLST's clean_geneid() (underscores become
# hyphens), so names from different sources are compared stripped down.
norm_locus <- function(x) gsub("[^[:alnum:]]", "", tolower(x))

# A release key that treats an unrecorded version as the empty string.
release_key <- function(repository, scheme, version) {
  version <- as.character(version)
  version[is.na(version)] <- ""
  paste(repository, scheme, version, sep = "\r")
}

#' Load the Classical MLST Overview of a Database
#'
#' @description Reads every classical MLST record of a database in one pass and
#'   summarises it with `build_overview()`.
#'
#' @param db_path Character path to the SQLite database file.
#'
#' @return List from `build_overview()`.
#' @export
load_mlst_overview <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)

  isolates <- database_isolates(con, tables)
  calls <- if ("classical_mlst" %in% tables) {
    read_cols(con, "classical_mlst", CALL_COLS)
  } else {
    NULL
  }
  prov <- if ("typing_provenance" %in% tables) {
    read_cols(con, "typing_provenance", PROV_COLS)
  } else {
    NULL
  }

  build_overview(isolates, calls, prov)
}

# One row per isolate: its merged status and the scheme release it was typed
# against. Stored calls are authoritative; provenance fills in isolates that
# have none (a failed or skipped step, or a call stored without alleles).
isolate_table <- function(isolates, calls, prov) {
  first <- calls[!duplicated(calls$isolate), , drop = FALSE]
  universe <- unique(c(isolates, first$isolate, prov$isolate))
  at_call <- match(universe, first$isolate)
  at_prov <- match(universe, prov$isolate)
  has_calls <- !is.na(at_call)
  in_prov <- !is.na(at_prov)

  pick <- function(call_col, prov_col) {
    out <- blank_na(first[[call_col]][at_call])
    from_prov <- !has_calls
    out[from_prov] <- blank_na(prov[[prov_col]][at_prov[from_prov]])
    out
  }

  st <- blank_na(first$st[at_call])
  status <- pick("status", "cla_status")
  status[has_calls & is.na(status)] <- ifelse(
    is.na(st[has_calls & is.na(status)]),
    "unknown",
    "known"
  )
  status[!has_calls & in_prov & is.na(status)] <- "unknown"
  status[!has_calls & !in_prov] <- "untyped"

  n_loci <- as.integer(table(factor(calls$isolate, levels = universe)))

  out <- data.frame(
    isolate = universe,
    status = status,
    st = st,
    repository = pick("repository", "cla_repository"),
    scheme = pick("scheme", "cla_scheme"),
    version = pick("scheme_version", "cla_scheme_version"),
    n_loci = n_loci,
    identity = suppressWarnings(as.numeric(first$identity[at_call])),
    coverage = suppressWarnings(as.numeric(first$coverage[at_call])),
    pymlst_version = blank_na(first$pymlst_version[at_call]),
    called_at = blank_na(first$called_at[at_call]),
    has_calls = has_calls,
    in_prov = in_prov,
    stringsAsFactors = FALSE
  )

  # Where both sources speak for the same isolate they should agree on which
  # release it was typed against; a disagreement means one of them was
  # rewritten without the other.
  prov_key <- release_key(
    blank_na(prov$cla_repository[at_prov]),
    blank_na(prov$cla_scheme[at_prov]),
    blank_na(prov$cla_scheme_version[at_prov])
  )
  prov_recorded <- in_prov & !is.na(blank_na(prov$cla_scheme[at_prov]))
  out$prov_differs <- has_calls &
    prov_recorded &
    prov_key != release_key(out$repository, out$scheme, out$version)
  out
}

# Loci of the scheme each isolate was typed against, counted as the distinct
# loci any stored call of that repository/scheme carries; NA without a scheme.
scheme_locus_counts <- function(iso, calls) {
  key <- function(repository, scheme) paste(repository, scheme, sep = "\r")
  per_scheme <- tapply(
    norm_locus(calls$gene),
    key(calls$repository, calls$scheme),
    function(genes) length(unique(genes))
  )
  out <- as.integer(per_scheme[key(iso$repository, iso$scheme)])
  out[is.na(iso$scheme)] <- NA_integer_
  out
}

#' Summarise a Database's Classical MLST Records
#'
#' @description Merges stored calls and provenance into the per-isolate,
#'   per-locus and per-ST tables and the provenance checks of the MLST overview.
#'
#' @details The *primary* scheme is the repository/scheme pair most isolates
#'   were called against; loci, STs and alleles are reported for it alone,
#'   since Sequence Types of different schemes are not comparable. Its releases
#'   (profile-table dates) are pooled - a newer release only adds profiles.
#'
#' @param isolates Character vector of every isolate in the database.
#' @param calls Data frame of `classical_mlst` rows, or `NULL`.
#' @param prov Data frame of `typing_provenance` rows, or `NULL`.
#'
#' @return A list with `state` (`"ok"`, `"skipped"` or `"none"`), `isolates`,
#'   `primary`, `releases`, `calls` (allele rows of the primary scheme), `loci`,
#'   `profiles`, `sts`, `status` and `checks`.
#' @export
build_overview <- function(
  isolates = character(0),
  calls = NULL,
  prov = NULL
) {
  empty_frame <- function(cols) {
    as.data.frame(
      setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(calls)) {
    calls <- empty_frame(CALL_COLS)
  }
  if (is.null(prov)) {
    prov <- empty_frame(PROV_COLS)
  }
  calls$allele <- trimws(as.character(calls$allele))
  calls$scheme_version <- blank_na(calls$scheme_version)

  iso <- isolate_table(as.character(isolates), calls, prov)

  # The primary scheme is decided by stored calls; provenance only stands in
  # when no isolate has any (every call failed, say).
  base <- if (any(iso$has_calls)) iso[iso$has_calls, ] else iso
  base <- base[!is.na(base$scheme), , drop = FALSE]
  pair <- modal(paste(base$repository, base$scheme, sep = "\r"))
  primary <- NULL
  if (!is.na(pair)) {
    parts <- strsplit(pair, "\r", fixed = TRUE)[[1]]
    repo <- if (identical(parts[1], "NA")) NA_character_ else parts[1]
    on_primary <- base$scheme == parts[2] & base$repository %in% repo
    versions <- sort(unique(base$version[on_primary]))
    primary <- list(
      repository = repo,
      scheme = parts[2],
      versions = versions,
      latest = if (length(versions)) versions[length(versions)] else NA_character_
    )
  }
  iso$on_primary <- if (is.null(primary)) {
    rep(FALSE, nrow(iso))
  } else {
    iso$scheme %in% primary$scheme & iso$repository %in% primary$repository
  }
  iso$older_release <- if (is.null(primary) || is.na(primary$latest)) {
    rep(FALSE, nrow(iso))
  } else {
    iso$on_primary & !is.na(iso$version) & iso$version < primary$latest
  }
  iso$scheme_loci <- scheme_locus_counts(iso, calls)
  # Calls searched at other thresholds than most of the database's.
  pair <- threshold_pairs(iso$identity, iso$coverage)
  pair[!iso$has_calls] <- NA_character_
  iso$other_thresholds <- !is.na(pair) & pair != modal(pair)

  pcalls <- if (is.null(primary)) {
    calls[0, , drop = FALSE]
  } else {
    calls[
      calls$scheme %in% primary$scheme &
        calls$repository %in% primary$repository,
      ,
      drop = FALSE
    ]
  }

  loci_order <- unique(pcalls$gene)
  loci_order <- loci_order[!duplicated(norm_locus(loci_order))]
  profiles <- profile_matrix(pcalls, loci_order)

  state <- if (nrow(calls)) {
    "ok"
  } else if (any(iso$status == "skipped")) {
    "skipped"
  } else {
    "none"
  }

  list(
    state = state,
    isolates = iso,
    primary = primary,
    releases = release_table(iso),
    calls = pcalls,
    loci = locus_table(pcalls, loci_order, sum(iso$on_primary & iso$has_calls)),
    profiles = profiles,
    sts = st_table(iso, profiles),
    status = status_table(iso$status),
    checks = provenance_checks(iso, primary)
  )
}

# Isolate x locus allele matrix of the primary scheme, loci in scheme order.
profile_matrix <- function(pcalls, loci) {
  typed <- unique(pcalls$isolate)
  m <- matrix(
    NA_character_,
    nrow = length(typed),
    ncol = length(loci),
    dimnames = list(typed, loci)
  )
  if (length(typed) && length(loci)) {
    col <- match(norm_locus(pcalls$gene), norm_locus(loci))
    m[cbind(match(pcalls$isolate, typed), col)] <- pcalls$allele
  }
  m
}

# An allele that is a single registered number: not "new", not a multi-hit
# ("12;40"), not empty.
resolved_allele <- function(allele) {
  !is.na(allele) & nzchar(allele) & allele != "new" & !grepl(";", allele, fixed = TRUE)
}

# One row per locus of the primary scheme: how the isolates in this database
# were called at it. `.gene` is the locus as the stored calls spell it, which is
# what allele lookups are keyed on.
locus_table <- function(pcalls, loci, n_typed) {
  if (!length(loci)) {
    return(data.frame(
      Locus = character(0),
      `Observed Alleles` = integer(0),
      `Isolates Called` = integer(0),
      Missing = integer(0),
      Unresolved = integer(0),
      `Top Allele` = character(0),
      .gene = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  by_locus <- split(pcalls, factor(norm_locus(pcalls$gene), levels = norm_locus(loci)))

  rows <- lapply(seq_along(loci), function(i) {
    rows <- by_locus[[i]]
    ok <- resolved_allele(rows$allele)
    counts <- sort(table(rows$allele[ok]), decreasing = TRUE)
    called <- length(unique(rows$isolate))
    top <- if (length(counts)) {
      sprintf("%s (%.0f%%)", names(counts)[1], 100 * counts[[1]] / max(called, 1))
    } else {
      NA_character_
    }
    data.frame(
      `Observed Alleles` = length(counts),
      `Isolates Called` = called,
      Missing = max(n_typed - called, 0L),
      Unresolved = sum(!ok),
      `Top Allele` = top,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  out <- cbind(
    data.frame(Locus = loci, check.names = FALSE, stringsAsFactors = FALSE),
    out
  )
  gene <- pcalls$gene[match(norm_locus(loci), norm_locus(pcalls$gene))]
  out$.gene <- ifelse(is.na(gene), loci, gene)
  out
}

# One row per ST of the primary scheme, with the allele profile it stands for.
st_table <- function(iso, profiles) {
  typed <- iso[iso$on_primary & !is.na(iso$st), c("isolate", "st", "status"), drop = FALSE]
  if (!nrow(typed)) {
    return(data.frame(
      ST = character(0),
      Isolates = integer(0),
      Share = numeric(0),
      Call = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  counts <- sort(table(typed$st), decreasing = TRUE)
  sts <- names(counts)
  rep_isolate <- typed$isolate[match(sts, typed$st)]
  # "inferred" when every isolate of the ST got it from the profile table.
  inferred <- tapply(typed$status == "inferred", typed$st, all)[sts]

  out <- data.frame(
    ST = if (all(grepl("^[0-9]+$", sts))) as.integer(sts) else sts,
    Isolates = as.integer(counts),
    Share = round(100 * as.numeric(counts) / nrow(typed), 1),
    Call = ifelse(inferred, STATUS_LABELS[["inferred"]], STATUS_LABELS[["known"]]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (ncol(profiles)) {
    alleles <- profiles[match(rep_isolate, rownames(profiles)), , drop = FALSE]
    out <- cbind(out, as.data.frame(alleles, stringsAsFactors = FALSE))
    rownames(out) <- NULL
  }
  out
}

# Count of isolates per status, in reporting order; anything unrecognised is
# appended under its own name.
status_table <- function(status) {
  levels <- c(names(STATUS_LABELS), setdiff(unique(status), names(STATUS_LABELS)))
  counts <- table(factor(status, levels = levels))
  out <- data.frame(
    status = levels,
    label = ifelse(levels %in% names(STATUS_LABELS), STATUS_LABELS[levels], levels),
    tone = ifelse(levels %in% names(STATUS_TONES), STATUS_TONES[levels], "secondary"),
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
  out[out$n > 0, , drop = FALSE]
}

#' Simpson's Index of Diversity
#'
#' @description Hunter-Gaston discriminatory index of a set of type
#'   assignments: the probability that two isolates drawn without replacement
#'   carry different types.
#'
#' @details `D = 1 - sum(n_j * (n_j - 1)) / (N * (N - 1))`: Simpson's diversity
#'   index, applied to typing methods by Hunter & Gaston, who propose D > 0.90
#'   as the mark of a usefully discriminatory method. Here it describes the
#'   collection instead, so clustered sampling (outbreaks) lowers it.
#'
#'   - Hunter PR, Gaston MA. Numerical index of the discriminatory ability of
#'     typing systems: an application of Simpson's index of diversity. J Clin
#'     Microbiol. 1988;26(11):2465-2466. doi:10.1128/jcm.26.11.2465-2466.1988
#'   - Simpson EH. Measurement of diversity. Nature. 1949;163:688.
#'     doi:10.1038/163688a0
#'
#' @param counts Integer vector of isolates per type.
#'
#' @return Numeric between 0 and 1, or `NA` for fewer than two isolates.
#' @export
simpson_diversity <- function(counts) {
  counts <- as.numeric(counts[counts > 0])
  n <- sum(counts)
  if (n < 2) {
    return(NA_real_)
  }
  1 - sum(counts * (counts - 1)) / (n * (n - 1))
}

# One row per scheme release isolates were typed against.
release_table <- function(iso) {
  rel <- iso[!is.na(iso$scheme), , drop = FALSE]
  if (!nrow(rel)) {
    return(data.frame(
      Repository = character(0),
      Scheme = character(0),
      Release = character(0),
      Isolates = integer(0),
      `With ST` = integer(0),
      Primary = logical(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  key <- release_key(rel$repository, rel$scheme, rel$version)
  first <- rel[!duplicated(key), , drop = FALSE]
  ukey <- key[!duplicated(key)]
  out <- data.frame(
    Repository = first$repository,
    Scheme = first$scheme,
    Release = first$version,
    Isolates = as.integer(table(factor(key, levels = ukey))),
    `With ST` = as.integer(tapply(!is.na(rel$st), factor(key, levels = ukey), sum)),
    Primary = first$on_primary,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[order(!out$Primary, out$Scheme, out$Release), , drop = FALSE]
}

# Provenance checks over the whole database, most severe first.
provenance_checks <- function(iso, primary) {
  checks <- check_list()
  add <- checks$add
  typed <- iso[!is.na(iso$scheme), , drop = FALSE]

  if (!is.null(primary)) {
    schemes <- paste0(typed$scheme, " @ ", typed$repository)
    if (length(unique(schemes)) > 1) {
      add(
        "Mixed schemes",
        "danger",
        sprintf(
          paste(
            "Isolates were typed against %d different schemes: %s.",
            "Sequence Types of different schemes are not comparable."
          ),
          length(unique(schemes)),
          tally(schemes)
        ),
        sum(!typed$on_primary)
      )
    } else {
      add(
        "Scheme consistency",
        "success",
        sprintf("Every typed isolate was called against %s.", schemes[1])
      )
    }

    if (length(primary$versions) > 1) {
      stale <- iso$older_release & iso$status %in% UNRESOLVED_STATUSES
      if (any(stale)) {
        add(
          "Outdated unresolved calls",
          "warning",
          sprintf(
            paste(
              "%d unresolved call(s) (%s) were made against a release older than %s.",
              "Profiles registered since may resolve them on retyping."
            ),
            sum(stale),
            tally(STATUS_LABELS[iso$status[stale]]),
            primary$latest
          ),
          sum(stale)
        )
      }
      add(
        "Profile releases",
        "info",
        sprintf(
          paste(
            "%d releases of the profile table are in use (%s to %s).",
            "Newer releases only add profiles, so known STs stay valid."
          ),
          length(primary$versions),
          primary$versions[1],
          primary$latest
        ),
        sum(iso$older_release)
      )
    }
  }

  no_alleles <- !iso$has_calls & iso$status %in% CALLED_STATUSES
  if (any(no_alleles)) {
    add(
      "Calls without alleles",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) have a call recorded in their provenance but no allele",
          "profile stored: %s."
        ),
        sum(no_alleles),
        tally(STATUS_LABELS[iso$status[no_alleles]])
      ),
      sum(no_alleles)
    )
  }

  if (any(iso$prov_differs)) {
    add(
      "Provenance mismatch",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) carry stored calls from a different scheme release",
          "than their provenance records."
        ),
        sum(iso$prov_differs)
      ),
      sum(iso$prov_differs)
    )
  }

  failed <- iso$status == "failed"
  if (any(failed)) {
    add(
      "Failed calls",
      "warning",
      sprintf("Classical MLST failed for %d isolate(s); retyping them may succeed.", sum(failed)),
      sum(failed)
    )
  }

  pairs <- threshold_pairs(iso$identity, iso$coverage)[iso$has_calls]
  pairs <- pairs[!is.na(pairs)]
  if (length(unique(pairs)) > 1) {
    add(
      "Mixed thresholds",
      "warning",
      sprintf("Calls were made with different search thresholds: %s.", tally(pairs)),
      sum(iso$other_thresholds)
    )
  }

  tools <- iso$pymlst_version
  tools <- tools[!is.na(tools)]
  if (length(unique(tools)) > 1) {
    add(
      "Mixed pyMLST versions",
      "info",
      sprintf("Calls were made with several pyMLST versions: %s.", tally(paste0("v", tools))),
      length(tools) - max(table(tools))
    )
  }

  skipped <- iso$status == "skipped"
  if (any(skipped)) {
    add(
      "Skipped",
      "info",
      sprintf(
        "No classical MLST scheme could be resolved for %d isolate(s) when they were typed.",
        sum(skipped)
      ),
      sum(skipped)
    )
  }

  untyped <- iso$status %in% c("untyped", "unknown")
  if (any(untyped)) {
    add(
      "No classical record",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have no classical MLST result - typed before classical",
          "MLST was available, or imported without classical results."
        ),
        sum(untyped)
      ),
      sum(untyped)
    )
  }

  checks$table()
}

#' Allele Frequencies of a Classical MLST Locus
#'
#' @param calls Data frame of the primary scheme's allele rows.
#' @param gene Character string. Locus name.
#'
#' @return Data frame with `allele`, `count` and `share` (percent of isolates
#'   called at the locus), most frequent first.
#' @export
locus_alleles <- function(calls, gene) {
  rows <- calls[calls$gene == gene, , drop = FALSE]
  if (!nrow(rows)) {
    return(data.frame(allele = character(0), count = integer(0), share = numeric(0)))
  }
  counts <- sort(table(rows$allele), decreasing = TRUE)
  data.frame(
    allele = names(counts),
    count = as.integer(counts),
    share = 100 * as.numeric(counts) / length(unique(rows$isolate)),
    stringsAsFactors = FALSE
  )
}

# Sequence filter of one locus of one scheme. Allele numbers are only unique
# within a scheme - two schemes of a species can share locus names - so every
# lookup is pinned to repository and scheme.
locus_filter <- "gene = ? AND scheme = ? AND repository IS ? AND sequence IS NOT NULL"

#' Load a Classical MLST Allele Sequence
#'
#' @param db_path Character path to the SQLite database file.
#' @param gene Character string. Locus name.
#' @param allele Character string. Allele number.
#' @param scheme Character string. Scheme label.
#' @param repository Character string. Repository.
#'
#' @return The reference sequence, or `NULL` when none was stored.
#' @export
load_cla_allele_sequence <- function(db_path, gene, allele, scheme, repository) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  if (!("sequence" %in% dbListFields(con, "classical_mlst"))) {
    return(NULL)
  }
  res <- dbGetQuery(
    con,
    sprintf("SELECT sequence FROM classical_mlst WHERE %s AND allele = ? LIMIT 1", locus_filter),
    params = list(gene, scheme, repository, allele)
  )$sequence
  if (!length(res)) NULL else res[[1]]
}

#' Format a Classical MLST Locus as FASTA
#'
#' @description Every allele of a locus observed in the database that has a
#'   stored reference sequence, ordered by allele number.
#'
#' @param db_path Character path to the SQLite database file.
#' @param gene Character string. Locus name.
#' @param scheme Character string. Scheme label.
#' @param repository Character string. Repository.
#'
#' @return Character vector of FASTA records.
#' @export
cla_locus_fasta <- function(db_path, gene, scheme, repository) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  if (!("sequence" %in% dbListFields(con, "classical_mlst"))) {
    return(character(0))
  }
  res <- dbGetQuery(
    con,
    sprintf(
      "SELECT allele, MIN(sequence) AS sequence FROM classical_mlst
        WHERE %s GROUP BY allele ORDER BY CAST(allele AS INTEGER), allele",
      locus_filter
    ),
    params = list(gene, scheme, repository)
  )
  if (!nrow(res)) {
    return(character(0))
  }
  paste0(">", gene, "_", res$allele, "\n", res$sequence)
}
