# app/logic/amr_overview.R
#
# AMR screening across a whole database: per-isolate screening outcome and
# element counts, per-element prevalence and call quality, the element profiles
# and groups of each element type (Resistance, Virulence, Stress) and the
# provenance checks the "AMR Overview" view is built from. Everything is
# derived on read from `amr_results` (one row per AMRFinderPlus element),
# `amr_summary` (abritamr's curated drug classes of the same elements) and
# `typing_provenance`.

box::use(
  DBI[dbDisconnect, dbGetQuery, dbListTables],
  stats[median, setNames],
)
box::use(
  app /
    logic /
    amr_plot[
      AMR_CLASS_VOCABULARIES,
      AMR_CLASS_VOCABULARY_DEFAULT,
      AMR_CONFIDENCE_STATES,
      amr_method_confidence,
    ],
  app / logic / db_connect[connect],
  app / logic / field_labels[amr_class_label],
  app /
    logic /
    overview_common[
      blank_na,
      check_list,
      database_isolates,
      modal,
      read_cols,
      tally,
    ],
)

#' AMR Element Types
#'
#' @description Display name of each AMRFinderPlus element type.
#'
#' @export
ELEMENT_TYPES <- c(AMR = "Resistance", VIRULENCE = "Virulence", STRESS = "Stress")

# Display name of each AMRFinderPlus element subtype.
SUBTYPE_LABELS <- c(
  AMR = "Acquired gene",
  POINT = "Point mutation",
  VIRULENCE = "Virulence factor",
  METAL = "Metal",
  BIOCIDE = "Biocide",
  HEAT = "Heat",
  ACID = "Acid"
)

#' AMR Screening Outcome Labels
#'
#' @description Display label of every per-isolate screening outcome, in the
#'   order they are reported.
#'
#' @export
SCREEN_LABELS <- c(
  found = "Elements found",
  clean = "No elements",
  incomplete = "Incomplete",
  failed = "Failed",
  skipped = "Skipped",
  unknown = "Not recorded",
  unscreened = "Not screened"
)

#' AMR Screening Outcome Tones
#'
#' @description Bootstrap contextual tone of each outcome, parallel to
#'   `SCREEN_LABELS`.
#'
#' @export
SCREEN_TONES <- c(
  found = "success",
  clean = "info",
  incomplete = "warning",
  failed = "danger",
  skipped = "secondary",
  unknown = "secondary",
  unscreened = "secondary"
)

# Methods that report an incomplete or interrupted element: a partial match,
# one cut at a contig end, a premature stop codon or a bare HMM motif.
PARTIAL_METHOD <- "PARTIAL|INTERNAL_STOP|HMM"

# Bins of elements per isolate.
COUNT_BREAKS <- c(-Inf, 0, 1, 2, 5, 10, 20, Inf)
COUNT_LABELS <- c("None", "1", "2", "3-5", "6-10", "11-20", "Over 20")

# Columns the overview summarises. Where a call sits (contig, position,
# strand, reference accession) is only ever shown for one call at a time, so
# it is read on demand by load_element_calls() instead of for every call.
RESULT_COLS <- c(
  "isolate", "gene_symbol", "element_type", "element_subtype", "class",
  "subclass", "method", "pct_coverage", "pct_identity", "ref_name", "organism",
  "point_mutations", "abritamr_version", "amrfinder_version",
  "amrfinder_db_version", "called_at"
)

SUMMARY_COLS <- c("isolate", "drug_class", "genes")

PROV_COLS <- c(
  "isolate", "amr_status", "amr_organism", "amr_point_mutations", "amr_elements",
  "amr_abritamr_version", "amr_amrfinder_version", "amr_amrfinder_db_version"
)

#' Load the AMR Overview of a Database
#'
#' @description Reads every AMR screening record of a database in one pass and
#'   summarises it with `build_amr_overview()`.
#'
#' @param db_path Character path to the SQLite database file.
#'
#' @return List from `build_amr_overview()`.
#' @export
load_amr_overview <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)

  build_amr_overview(
    isolates = database_isolates(con, tables),
    results = if ("amr_results" %in% tables) read_cols(con, "amr_results", RESULT_COLS),
    summary = if ("amr_summary" %in% tables) read_cols(con, "amr_summary", SUMMARY_COLS),
    prov = if ("typing_provenance" %in% tables) read_cols(con, "typing_provenance", PROV_COLS)
  )
}

#' Summarise a Database's AMR Screening
#'
#' @description Merges element calls and provenance into the per-isolate and
#'   per-element tables, per-type profiles and distributions and the provenance
#'   checks of the AMR overview.
#'
#' @param isolates Character vector of every isolate in the database.
#' @param results Data frame of `amr_results` rows, or `NULL`.
#' @param prov Data frame of `typing_provenance` rows, or `NULL`.
#' @param summary Data frame of `amr_summary` rows, or `NULL`.
#'
#' @return A list with `state` (`"ok"`, `"skipped"` or `"none"`), `isolates`,
#'   `hits` (annotated `amr_results` rows), `elements`, `groups` (a list by
#'   classification, `AMR_CLASS_VOCABULARIES`, then by element type), `counts`
#'   (a list by element type), `status`, `settings` and `checks`. Profiles are
#'   built per type on demand, with `profile_table()`.
#' @export
build_amr_overview <- function(
  isolates = character(0),
  results = NULL,
  prov = NULL,
  summary = NULL
) {
  hits <- annotate_hits(results, summary)
  iso <- isolate_table(as.character(isolates), hits, prov)
  screened <- iso[iso$screened, , drop = FALSE]
  types <- unname(ELEMENT_TYPES)

  state <- if (any(iso$screened)) {
    "ok"
  } else if (any(iso$status == "skipped")) {
    "skipped"
  } else {
    "none"
  }

  list(
    state = state,
    isolates = iso,
    hits = hits,
    elements = element_table(hits, nrow(screened)),
    groups = lapply(
      setNames(nm = unname(AMR_CLASS_VOCABULARIES)),
      function(v) setNames(lapply(types, group_table, hits = hits, vocabulary = v), types)
    ),
    counts = setNames(lapply(types, count_bins, hits = hits, screened = screened), types),
    status = status_table(iso$status),
    settings = settings_table(screened),
    checks = amr_checks(iso, hits)
  )
}

# Class abritamr filed each call under in its isolate's summary, NA where it
# did not summarise the call (a partial hit to a gene family can reach
# `amr_results` without it). abritamr carries its quality flags on the gene
# name ("blaOXA-23*") where the hit table does not, so the key is the stripped
# symbol.
rollup_class <- function(hits, summary) {
  if (is.null(summary) || !nrow(summary) || !nrow(hits)) {
    return(rep(NA_character_, nrow(hits)))
  }
  class <- blank_na(summary$drug_class)
  key <- paste(summary$isolate, sub("[*^]+$", "", summary$genes), sep = "\r")[!is.na(class)]
  class <- trimws(class[!is.na(class)])
  class[match(paste(hits$isolate, hits$gene_symbol, sep = "\r"), key)]
}

# Columns of the annotated hits holding each call's class and group under one
# classification (`AMR_CLASS_VOCABULARIES`).
class_col <- function(vocabulary) if (identical(vocabulary, "rollup")) "rollup_class" else "class"
group_col <- function(vocabulary) if (identical(vocabulary, "rollup")) "rollup_group" else "group"

# The element calls with their type, groups and call quality spelled out.
annotate_hits <- function(results, summary = NULL) {
  if (is.null(results) || !nrow(results)) {
    results <- as.data.frame(
      setNames(replicate(length(RESULT_COLS), character(0), simplify = FALSE), RESULT_COLS),
      stringsAsFactors = FALSE
    )
  }
  hits <- results
  type <- toupper(blank_na(hits$element_type))
  subtype <- toupper(blank_na(hits$element_subtype))
  hits$type <- ifelse(type %in% names(ELEMENT_TYPES), ELEMENT_TYPES[type], type)
  hits$subtype <- ifelse(subtype %in% names(SUBTYPE_LABELS), SUBTYPE_LABELS[subtype], subtype)
  # Some AMRFinderPlus classes arrive with underscores ("FUSIDIC_ACID") next to
  # the same class spelled with a space; both must read, and group, as one.
  hits$class <- amr_class_label(gsub("_", " ", blank_na(hits$class), fixed = TRUE))
  hits$subclass <- amr_class_label(gsub("_", " ", blank_na(hits$subclass), fixed = TRUE))
  hits$abritamr_class <- rollup_class(hits, summary)
  # Under abritamr's classification a call it never summarised keeps its
  # AMRFinderPlus class, as in the visualization's gene heatmap.
  hits$rollup_class <- ifelse(is.na(hits$abritamr_class), hits$class, hits$abritamr_class)
  # Resistance groups by drug class; stress by what it protects against, the
  # subtype when no class is given (heat, acid); virulence has no classes, so
  # its factors stand for themselves.
  group_of <- function(class) {
    ifelse(
      hits$type == ELEMENT_TYPES[["VIRULENCE"]],
      hits$gene_symbol,
      ifelse(is.na(class), hits$subtype, class)
    )
  }
  hits$group <- group_of(hits$class)
  hits$rollup_group <- group_of(hits$rollup_class)
  hits$tier <- AMR_CONFIDENCE_STATES[amr_method_confidence(hits$method) + 1L]
  hits$partial <- grepl(PARTIAL_METHOD, toupper(as.character(hits$method)))
  hits$pct_identity <- suppressWarnings(as.numeric(hits$pct_identity))
  hits$pct_coverage <- suppressWarnings(as.numeric(hits$pct_coverage))
  hits
}

# Number of distinct values of `x` per isolate, over `universe`.
per_isolate <- function(isolate, x, universe) {
  keep <- !is.na(x)
  pairs <- unique(data.frame(isolate = isolate[keep], x = x[keep]))
  as.integer(table(factor(pairs$isolate, levels = universe)))
}

# One row per isolate: how its screen came out, what it found by element type
# and the settings it was screened with.
isolate_table <- function(isolates, hits, prov) {
  universe <- unique(c(isolates, hits$isolate, prov$isolate))
  universe <- universe[!is.na(universe)]
  n <- length(universe)
  at_prov <- if (is.null(prov)) rep(NA_integer_, n) else match(universe, prov$isolate)
  from_prov <- function(col) {
    if (is.null(prov) || !(col %in% names(prov))) rep(NA, n) else prov[[col]][at_prov]
  }
  first <- hits[!duplicated(hits$isolate), , drop = FALSE]
  at_hit <- match(universe, first$isolate)
  # Settings: the stored calls first (they carry the screen's own stamp), the
  # provenance row for isolates without any.
  setting <- function(hit_col, prov_col) {
    out <- blank_na(first[[hit_col]][at_hit])
    out[is.na(out)] <- blank_na(from_prov(prov_col))[is.na(out)]
    out
  }
  count <- function(type, subtype = NULL) {
    keep <- hits$type == type & (is.null(subtype) | hits$subtype %in% subtype)
    as.integer(table(factor(hits$isolate[keep], levels = universe)))
  }
  resistance <- hits$type == ELEMENT_TYPES[["AMR"]]

  rows <- as.integer(table(factor(hits$isolate, levels = universe)))
  status <- blank_na(from_prov("amr_status"))
  status[status %in% "done"] <- NA_character_
  status[is.na(status) & rows > 0] <- "found"
  done <- blank_na(from_prov("amr_status")) %in% "done"
  status[is.na(status) & done] <- "clean"
  status[is.na(status) & !is.na(at_prov)] <- "unknown"
  status[is.na(status)] <- "unscreened"

  pm <- setting("point_mutations", "amr_point_mutations")
  recorded <- suppressWarnings(as.integer(from_prov("amr_elements")))

  iso <- data.frame(
    isolate = universe,
    status = status,
    screened = status %in% c("found", "clean"),
    elements = rows,
    acquired = count(ELEMENT_TYPES[["AMR"]], SUBTYPE_LABELS[["AMR"]]),
    point = count(ELEMENT_TYPES[["AMR"]], SUBTYPE_LABELS[["POINT"]]),
    virulence = count(ELEMENT_TYPES[["VIRULENCE"]]),
    stress = count(ELEMENT_TYPES[["STRESS"]]),
    classes = per_isolate(hits$isolate[resistance], hits$class[resistance], universe),
    partial = as.integer(table(factor(hits$isolate[hits$partial], levels = universe))),
    organism = setting("organism", "amr_organism"),
    point_mutations = ifelse(is.na(pm), NA, pm %in% c("1", "TRUE")),
    abritamr = setting("abritamr_version", "amr_abritamr_version"),
    amrfinder = setting("amrfinder_version", "amr_amrfinder_version"),
    db_version = setting("amrfinder_db_version", "amr_amrfinder_db_version"),
    screened_at = blank_na(first$called_at[at_hit]),
    recorded_elements = recorded,
    stringsAsFactors = FALSE
  )
  iso$resistance <- iso$acquired + iso$point
  # Provenance counted the elements the screen reported; fewer stored means
  # some were lost on the way into the database.
  iso$lost <- done & !is.na(recorded) & recorded > rows
  db <- iso$db_version[iso$screened]
  iso$other_db <- iso$screened & !is.na(iso$db_version) & iso$db_version != modal(db)
  iso
}

# Most frequent non-NA `value` within each group of `key`, in the order of
# `levels` (the first seen on a tie), NA for a group without any value.
group_mode <- function(key, value, levels) {
  out <- rep(NA_character_, length(levels))
  ok <- !is.na(value)
  if (!any(ok)) {
    return(out)
  }
  k <- key[ok]
  v <- as.character(value[ok])
  freq <- stats::ave(seq_along(k), k, v, FUN = length)
  o <- order(match(k, levels), -freq)
  top <- !duplicated(k[o])
  out[match(k[o][top], levels)] <- v[o][top]
  out
}

# One row per element: what it is, how many screened isolates carry it and how
# confidently it was called.
element_table <- function(hits, n_screened) {
  empty <- data.frame(
    Element = character(0),
    Type = character(0),
    Subtype = character(0),
    Class = character(0),
    Subclass = character(0),
    `abritamr Class` = character(0),
    Isolates = integer(0),
    `Prevalence (%)` = numeric(0),
    Perfect = integer(0),
    Strong = integer(0),
    Partial = integer(0),
    Putative = integer(0),
    `Identity (%)` = numeric(0),
    `Coverage (%)` = numeric(0),
    Reference = character(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!nrow(hits)) {
    return(empty)
  }
  key <- paste(hits$gene_symbol, hits$type, hits$subtype, sep = "\r")
  keys <- unique(key)
  first <- match(keys, key)
  # Each isolate counts once per element, at its best call of it.
  rank <- match(hits$tier, AMR_CONFIDENCE_STATES)
  o <- order(key, hits$isolate, -rank)
  best <- !duplicated(paste(key[o], hits$isolate[o], sep = "\r"))
  best_key <- factor(key[o][best], levels = keys)
  tiers <- table(best_key, factor(hits$tier[o][best], levels = AMR_CONFIDENCE_STATES))
  carriers <- tabulate(best_key, nbins = length(keys))
  mean_of <- function(x) {
    out <- tapply(x, factor(key, levels = keys), mean, na.rm = TRUE)
    out <- round(as.numeric(out), 2)
    out[is.nan(out)] <- NA
    out
  }

  out <- data.frame(
    Element = hits$gene_symbol[first],
    Type = hits$type[first],
    Subtype = hits$subtype[first],
    Class = group_mode(key, hits$class, keys),
    Subclass = group_mode(key, hits$subclass, keys),
    `abritamr Class` = group_mode(key, hits$abritamr_class, keys),
    Isolates = carriers,
    `Prevalence (%)` = round(100 * carriers / max(n_screened, 1), 1),
    Perfect = as.integer(tiers[, "Perfect"]),
    Strong = as.integer(tiers[, "Strong"]),
    Partial = as.integer(tiers[, "Partial"]),
    Putative = as.integer(tiers[, "Putative"]),
    `Identity (%)` = mean_of(hits$pct_identity),
    `Coverage (%)` = mean_of(hits$pct_coverage),
    Reference = group_mode(key, hits$ref_name, keys),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  type_order <- match(out$Type, unname(ELEMENT_TYPES))
  out[order(type_order, -out$Isolates, out$Element), , drop = FALSE]
}

#' Element Profiles of One Type
#'
#' @description Distinct element combinations of one type among screened
#'   isolates, with how many isolates carry each; isolates without any form one
#'   "(none)" profile.
#'
#' @param type Character element type (`ELEMENT_TYPES`).
#' @param hits Data frame of annotated element calls (`overview$hits`).
#' @param screened Data frame of the screened isolates' rows.
#' @param vocabulary Classification the profiles' classes are named in, one of
#'   `AMR_CLASS_VOCABULARIES`.
#'
#' @return Data frame of profiles, most frequent first.
#' @export
profile_table <- function(type, hits, screened, vocabulary = AMR_CLASS_VOCABULARY_DEFAULT) {
  h <- hits[hits$type == type, , drop = FALSE]
  # Sorted distinct pairs, joined per isolate in one split.
  joined <- function(values) {
    pairs <- unique(data.frame(isolate = h$isolate, value = values)[!is.na(values), ])
    pairs <- pairs[order(pairs$isolate, pairs$value), , drop = FALSE]
    parts <- split(pairs$value, pairs$isolate)
    list(text = vapply(parts, paste, character(1), collapse = ", "), n = lengths(parts))
  }
  gene_sets <- joined(h$gene_symbol)
  genes <- gene_sets$text
  n_genes <- gene_sets$n
  classes <- joined(h[[class_col(vocabulary)]])$text
  profile <- unname(genes[screened$isolate])
  profile[is.na(profile)] <- "(none)"
  if (!length(profile)) {
    return(data.frame(
      Profile = character(0),
      Elements = integer(0),
      Classes = character(0),
      Isolates = integer(0),
      `Share (%)` = numeric(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  counts <- sort(table(profile), decreasing = TRUE)
  first <- screened$isolate[match(names(counts), profile)]
  out <- data.frame(
    Profile = names(counts),
    Elements = ifelse(names(counts) == "(none)", 0L, as.integer(n_genes[first])),
    Classes = unname(classes[first]),
    Isolates = as.integer(counts),
    `Share (%)` = round(100 * as.numeric(counts) / length(profile), 1),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# Isolates carrying at least one element of each group of one type under one
# classification, most frequent first.
group_table <- function(type, hits, vocabulary) {
  group <- hits[[group_col(vocabulary)]]
  keep <- hits$type == type & !is.na(group)
  h <- unique(data.frame(isolate = hits$isolate[keep], group = group[keep]))
  if (!nrow(h)) {
    return(data.frame(group = character(0), isolates = integer(0), stringsAsFactors = FALSE))
  }
  counts <- sort(table(h$group), decreasing = TRUE)
  data.frame(group = names(counts), isolates = as.integer(counts), stringsAsFactors = FALSE)
}

# Screened isolates by number of distinct elements of one type.
count_bins <- function(type, hits, screened) {
  h <- unique(hits[hits$type == type, c("isolate", "gene_symbol"), drop = FALSE])
  n <- as.integer(table(factor(h$isolate, levels = screened$isolate)))
  data.frame(
    label = COUNT_LABELS,
    n = as.integer(table(cut(n, COUNT_BREAKS, COUNT_LABELS, right = TRUE))),
    stringsAsFactors = FALSE
  )
}

# Isolates per screening outcome, in reporting order; empty outcomes dropped.
status_table <- function(status) {
  counts <- table(factor(status, levels = names(SCREEN_LABELS)))
  out <- data.frame(
    status = names(SCREEN_LABELS),
    label = unname(SCREEN_LABELS),
    tone = unname(SCREEN_TONES),
    n = as.integer(counts),
    stringsAsFactors = FALSE
  )
  out[out$n > 0, , drop = FALSE]
}

# Screened isolates grouped by the settings they were screened with: organism
# (point mutations), abritamr, AMRFinderPlus and its database.
settings_table <- function(screened) {
  if (!nrow(screened)) {
    return(data.frame(
      organism = character(0),
      point_mutations = logical(0),
      tools = character(0),
      db_version = character(0),
      isolates = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  tools <- ifelse(
    is.na(screened$amrfinder),
    NA_character_,
    sprintf(
      "abritamr %s, AMRFinderPlus %s",
      ifelse(is.na(screened$abritamr), "-", screened$abritamr),
      screened$amrfinder
    )
  )
  key <- paste(screened$organism, screened$point_mutations, tools, screened$db_version, sep = "\r")
  first <- !duplicated(key)
  out <- data.frame(
    organism = screened$organism[first],
    point_mutations = screened$point_mutations[first],
    tools = tools[first],
    db_version = screened$db_version[first],
    isolates = as.integer(table(factor(key, levels = key[first]))),
    stringsAsFactors = FALSE
  )
  out[order(-out$isolates), , drop = FALSE]
}

# Screening checks over the whole database, most severe first.
amr_checks <- function(iso, hits) {
  checks <- check_list()
  add <- checks$add
  screened <- iso[iso$screened, , drop = FALSE]

  if (any(iso$lost)) {
    add(
      "Results missing",
      "danger",
      sprintf(
        paste(
          "%d isolate(s) have fewer elements stored than their screen reported;",
          "the missing results were lost on the way into the database."
        ),
        sum(iso$lost)
      ),
      sum(iso$lost)
    )
  }

  failed <- iso$status %in% c("failed", "incomplete")
  if (any(failed)) {
    add(
      "Failed screens",
      "warning",
      sprintf(
        "AMR screening failed or did not finish for %d isolate(s): %s. Retyping them may succeed.",
        sum(failed),
        tally(SCREEN_LABELS[iso$status[failed]])
      ),
      sum(failed)
    )
  }

  dbs <- screened$db_version[!is.na(screened$db_version)]
  if (length(unique(dbs)) > 1) {
    add(
      "Mixed AMRFinderPlus databases",
      "warning",
      sprintf(
        paste(
          "Isolates were screened against different AMRFinderPlus databases: %s.",
          "Elements added to a newer database are missing from isolates screened",
          "against an older one."
        ),
        tally(dbs)
      ),
      sum(screened$other_db)
    )
  }

  pm <- screened$point_mutations[!is.na(screened$point_mutations)]
  if (any(pm) && !all(pm)) {
    add(
      "Point mutations partly screened",
      "warning",
      sprintf(
        paste(
          "%d screened isolate(s) were searched without point mutations; their",
          "chromosomal resistance (e.g. quinolone, colistin) is not covered."
        ),
        sum(!pm)
      ),
      sum(!pm)
    )
  } else if (length(pm) && !any(pm)) {
    add(
      "No point mutations",
      "info",
      paste(
        "AMRFinderPlus has no organism option for this species, so only acquired",
        "genes were screened; chromosomal resistance mutations are not covered."
      )
    )
  }

  if (any(hits$partial)) {
    partial_iso <- unique(hits$isolate[hits$partial])
    add(
      "Partial elements",
      "info",
      sprintf(
        paste(
          "%d element call(s) in %d isolate(s) are partial, cut at a contig end",
          "or interrupted by a stop codon (%s) - assembly breaks or truncated genes."
        ),
        sum(hits$partial),
        length(partial_iso),
        tally(hits$method[hits$partial], limit = 3L)
      ),
      length(partial_iso)
    )
  }

  tools <- c(abritamr = "abritamr", AMRFinderPlus = "amrfinder")
  for (name in names(tools)) {
    versions <- screened[[tools[[name]]]]
    versions <- versions[!is.na(versions)]
    if (length(unique(versions)) > 1) {
      add(
        sprintf("Mixed %s versions", name),
        "info",
        sprintf("Isolates were screened with several %s versions: %s.", name, tally(versions)),
        length(versions) - max(table(versions))
      )
    }
  }

  skipped <- iso$status == "skipped"
  if (any(skipped)) {
    add(
      "Skipped",
      "info",
      sprintf(
        "AMR screening was switched off for %d isolate(s) when they were typed.",
        sum(skipped)
      ),
      sum(skipped)
    )
  }

  unscreened <- iso$status %in% c("unscreened", "unknown")
  if (any(unscreened)) {
    add(
      "No screening record",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have no AMR screening result - typed before screening was",
          "available, or imported without AMR results."
        ),
        sum(unscreened)
      ),
      sum(unscreened)
    )
  }

  out <- checks$table()
  if (nrow(screened) && !any(out$tone %in% c("danger", "warning"))) {
    add(
      "Screening consistency",
      "success",
      sprintf(
        "All %d screened isolates share database, settings and complete results.",
        nrow(screened)
      )
    )
    out <- checks$table()
  }
  out
}

#' Isolates Carrying Any of Several Groups
#'
#' @description Groups overlap - one isolate carries several drug classes - so
#'   the isolates behind a set of groups are counted once each, not summed.
#'
#' @param hits Data frame of annotated element calls (`overview$hits`).
#' @param type Character element type (`ELEMENT_TYPES`).
#' @param groups Character group names.
#' @param vocabulary Classification the groups are named in, one of
#'   `AMR_CLASS_VOCABULARIES`.
#'
#' @return Integer number of distinct isolates.
#' @export
group_carriers <- function(hits, type, groups, vocabulary = AMR_CLASS_VOCABULARY_DEFAULT) {
  in_groups <- hits[[group_col(vocabulary)]] %in% groups
  length(unique(hits$isolate[hits$type %in% type & in_groups]))
}

#' Carriers of One Element
#'
#' @param hits Data frame of annotated element calls (`overview$hits`).
#' @param element Character gene symbol.
#' @param type Character element type (`ELEMENT_TYPES`).
#'
#' @return One row per isolate carrying the element, at its best call, best
#'   calls first: `Isolate`, `Call`, `Identity (%)`, `Coverage (%)` and `Hits`
#'   (how often the element was found in the isolate).
#' @export
element_carriers <- function(hits, element, type) {
  h <- hits[hits$gene_symbol %in% element & hits$type %in% type, , drop = FALSE]
  rank <- match(h$tier, AMR_CONFIDENCE_STATES)
  h <- h[order(-rank, h$isolate, -h$pct_identity), , drop = FALSE]
  n_hits <- table(h$isolate)
  h <- h[!duplicated(h$isolate), , drop = FALSE]
  data.frame(
    Isolate = h$isolate,
    Call = h$tier,
    `Identity (%)` = h$pct_identity,
    `Coverage (%)` = h$pct_coverage,
    Hits = as.integer(n_hits[h$isolate]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Load One Isolate's Calls of One Element
#'
#' @description Reads every stored call of an element in one isolate, with
#'   where it sits - the detail the overview does not keep in memory.
#'
#' @param db_path Character path to the SQLite database file.
#' @param isolate Character isolate name.
#' @param element Character gene symbol.
#' @param type Character element type (`ELEMENT_TYPES`).
#'
#' @return Annotated `amr_results` rows with abritamr's class, best call first.
#' @export
load_element_calls <- function(db_path, isolate, element, type) {
  code <- names(ELEMENT_TYPES)[match(type, ELEMENT_TYPES)]
  if (is.na(code)) {
    code <- toupper(type)
  }
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  rows <- dbGetQuery(
    con,
    "SELECT * FROM amr_results
      WHERE isolate = ? AND gene_symbol = ? AND UPPER(element_type) = ?",
    params = list(isolate, element, code)
  )
  summary <- if ("amr_summary" %in% dbListTables(con)) {
    dbGetQuery(
      con,
      "SELECT isolate, drug_class, genes FROM amr_summary WHERE isolate = ?",
      params = list(isolate)
    )
  }
  calls <- annotate_hits(rows, summary)
  calls[order(-match(calls$tier, AMR_CONFIDENCE_STATES)), , drop = FALSE]
}

#' Median of a Count
#'
#' @param x Numeric vector.
#'
#' @return The median, or `NA` for an empty vector.
#' @export
median_count <- function(x) if (length(x)) median(x) else NA_real_
