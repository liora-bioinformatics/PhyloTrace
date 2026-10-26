# app/logic/isolate_overview.R
#
# The isolates of a whole database: where each came from, the assembly it was
# typed from, when and in which typing run, and the record-keeping checks the
# "Isolate Overview" view is built from. Everything is derived on read from
# `metadata`, `genome_hashes` and the run-level columns of `typing_provenance`
# that the cgMLST, MLST and AMR overviews leave aside.

box::use(
  DBI[dbDisconnect, dbListTables],
  stats[median, setNames],
  utils[head],
)
box::use(
  app / logic / db_connect[connect],
  app / logic / db_sources[SOURCE_COL, SOURCE_LOCAL],
  app / logic / overview_common,
)

#' Assembly Contiguity Classes
#'
#' @description Classes of assemblies by contig count: the upper bound of each
#'   class, its key, display label and tone. Long-read and hybrid assemblies
#'   close most replicons (a handful of contigs); short-read drafts break at
#'   every repeat; past a few hundred contigs loci start to go missing.
#'
#' @export
CONTIGUITY <- data.frame(
  upper = c(10, 200, 500, Inf),
  key = c("complete", "draft", "fragmented", "highly_fragmented"),
  label = c("Complete", "Draft", "Fragmented", "Highly fragmented"),
  tone = c("success", "primary", "warning", "danger"),
  stringsAsFactors = FALSE
)

#' Label of an Isolate Typed in This Database
#'
#' @export
LOCAL_LABEL <- "Typed here"

# Bins of contigs per assembly, toned by the contiguity class they fall in.
CONTIG_BREAKS <- c(-Inf, 1, 10, 50, 100, 200, 500, Inf)
CONTIG_LABELS <- c("1", "2-10", "11-50", "51-100", "101-200", "201-500", "Over 500")
CONTIG_TONES <- c("success", "success", "primary", "primary", "primary", "warning", "danger")

# Bins of assembly length as a percentage off the median; the outer two are
# what the size check flags (SIZE_TOLERANCE).
SIZE_BREAKS <- c(-Inf, -20, -10, -5, 5, 10, 20, Inf)
SIZE_LABELS <- c(
  "Over 20% smaller", "10-20% smaller", "5-10% smaller", "Within 5%",
  "5-10% larger", "10-20% larger", "Over 20% larger"
)
SIZE_TONES <- c("warning", "primary", "primary", "success", "primary", "primary", "warning")

# A timeline spanning more months than the first is shown by quarter, more
# than the second by year.
MONTHLY_SPAN <- 12L
QUARTERLY_SPAN <- 36L

HASH_COLS <- c(
  "isolate", "genome_digest", "file_sha256", "algorithm", "n_contigs",
  "total_length", "file_bytes", "hashed_at"
)

PROV_COLS <- c(
  "isolate", "run_id", "typed_at", "elapsed_seconds", "phylotrace_version",
  "genome_digest", "algorithm", "n_contigs", "total_length"
)

META_COLS <- c("isolate", SOURCE_COL, "called_at")

#' Load the Isolate Overview of a Database
#'
#' @description Reads the isolate list, their origin, assembly records and
#'   typing provenance in one pass and summarises them with
#'   `build_isolate_overview()`.
#'
#' @param db_path Character path to the SQLite database file.
#'
#' @return List from `build_isolate_overview()`.
#' @export
load_isolate_overview <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)
  read_if <- function(table, cols) {
    if (table %in% tables) overview_common$read_cols(con, table, cols)
  }

  build_isolate_overview(
    isolates = overview_common$database_isolates(con, tables),
    meta = read_if("metadata", META_COLS),
    hashes = read_if("genome_hashes", HASH_COLS),
    prov = read_if("typing_provenance", PROV_COLS)
  )
}

#' Summarise a Database's Isolates
#'
#' @description Merges origin, assembly records and typing provenance into the
#'   per-isolate and per-run tables, distributions and record checks of the
#'   isolate overview.
#'
#' @param isolates Character vector of every isolate in the database.
#' @param meta Data frame of `metadata` rows (`isolate`, `source`,
#'   `called_at`), or `NULL`.
#' @param hashes Data frame of `genome_hashes` rows, or `NULL`.
#' @param prov Data frame of `typing_provenance` rows, or `NULL`.
#'
#' @return A list with `state` (`"ok"` or `"none"`), `isolates`, `orphans`
#'   (isolate names of records left behind, by table), `runs`, `environments`,
#'   `contiguity`, `origin`, `contig_bins`, `size_bins`, `median_length`,
#'   `timeline` and `checks`.
#' @export
build_isolate_overview <- function(
  isolates = character(0),
  meta = NULL,
  hashes = NULL,
  prov = NULL
) {
  isolates <- unique(as.character(isolates))
  isolates <- isolates[!is.na(isolates)]
  iso <- isolate_table(isolates, meta, hashes, prov)
  orphans <- list(
    hashes = orphaned(hashes, isolates),
    provenance = orphaned(prov, isolates)
  )
  sized <- iso$length[!is.na(iso$length)]
  median_length <- if (length(sized)) median(sized) else NA_real_

  list(
    state = if (nrow(iso)) "ok" else "none",
    isolates = iso,
    orphans = orphans,
    runs = run_table(iso),
    environments = environment_table(iso),
    contiguity = contiguity_counts(iso$contiguity),
    origin = origin_counts(iso$source),
    contig_bins = bin_counts(iso$contigs, CONTIG_BREAKS, CONTIG_LABELS, CONTIG_TONES),
    size_bins = bin_counts(
      100 * (iso$length - median_length) / median_length,
      SIZE_BREAKS,
      SIZE_LABELS,
      SIZE_TONES
    ),
    median_length = median_length,
    timeline = timeline_counts(iso$typed_date),
    checks = isolate_checks(iso, orphans)
  )
}

# Isolates a table holds records of that are no longer in the database.
orphaned <- function(df, isolates) {
  if (is.null(df) || !nrow(df)) {
    return(character(0))
  }
  left <- unique(overview_common$blank_na(df$isolate))
  sort(left[!is.na(left) & !(left %in% isolates)])
}

#' Contiguity Class of Assemblies
#'
#' @param contigs Numeric vector of contig counts.
#'
#' @return Character vector of `CONTIGUITY` keys, `NA` where the count is
#'   unknown or not positive.
#' @export
contiguity_class <- function(contigs) {
  at <- vapply(
    contigs,
    function(x) if (is.na(x) || x < 1) NA_integer_ else which(x <= CONTIGUITY$upper)[1],
    integer(1)
  )
  CONTIGUITY$key[at]
}

# Calendar date of a timestamp, NA where it does not parse.
as_day <- function(x) {
  as.Date(substr(overview_common$blank_na(x), 1, 10), format = "%Y-%m-%d")
}

# Positive number from text, NA where it does not parse or is not positive.
as_positive <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.na(x) & x <= 0] <- NA_real_
  x
}

# One row per isolate: where it came from, the assembly on record and the one
# it was typed from, its typing run, with the flags the checks are built on.
isolate_table <- function(isolates, meta, hashes, prov) {
  n <- length(isolates)
  at <- function(df) if (is.null(df)) rep(NA_integer_, n) else match(isolates, df$isolate)
  at_meta <- at(meta)
  at_hash <- at(hashes)
  at_prov <- at(prov)
  # A column the table lacks reads as NA, like a value it never recorded.
  pick <- function(df, rows, col) {
    if (is.null(df) || !(col %in% names(df))) rep(NA, n) else df[[col]][rows]
  }
  from_hash <- function(col) pick(hashes, at_hash, col)
  from_prov <- function(col) pick(prov, at_prov, col)
  # The assembly record first (it is what an import or a re-hash refreshes),
  # the provenance snapshot where there is none.
  either <- function(col) {
    out <- from_hash(col)
    out[is.na(out)] <- from_prov(col)[is.na(out)]
    out
  }

  source <- overview_common$blank_na(pick(meta, at_meta, SOURCE_COL))
  source[is.na(source)] <- SOURCE_LOCAL
  hash_digest <- overview_common$blank_na(from_hash("genome_digest"))
  prov_digest <- overview_common$blank_na(from_prov("genome_digest"))
  hash_alg <- overview_common$blank_na(from_hash("algorithm"))
  prov_alg <- overview_common$blank_na(from_prov("algorithm"))
  contigs <- as_positive(either("n_contigs"))
  sizes <- as_positive(either("total_length"))
  # When it was typed: the provenance stamp, else when its assembly was hashed,
  # else when its metadata row was created.
  typed_at <- overview_common$blank_na(from_prov("typed_at"))
  hashed_at <- overview_common$blank_na(from_hash("hashed_at"))
  added_at <- overview_common$blank_na(pick(meta, at_meta, "called_at"))
  typed_at[is.na(typed_at)] <- hashed_at[is.na(typed_at)]
  typed_at[is.na(typed_at)] <- added_at[is.na(typed_at)]
  elapsed <- suppressWarnings(as.numeric(from_prov("elapsed_seconds")))
  elapsed[!is.na(elapsed) & elapsed < 0] <- NA_real_

  iso <- data.frame(
    isolate = isolates,
    source = source,
    imported = source != SOURCE_LOCAL,
    has_hash = !is.na(at_hash),
    in_prov = !is.na(at_prov),
    digest = ifelse(is.na(hash_digest), prov_digest, hash_digest),
    algorithm = ifelse(is.na(hash_digest), prov_alg, hash_alg),
    prov_digest = prov_digest,
    file_sha256 = overview_common$blank_na(from_hash("file_sha256")),
    file_bytes = as_positive(from_hash("file_bytes")),
    hashed_at = overview_common$blank_na(from_hash("hashed_at")),
    contigs = contigs,
    length = sizes,
    mean_contig = round(sizes / contigs),
    contiguity = contiguity_class(contigs),
    run_id = overview_common$blank_na(from_prov("run_id")),
    typed_at = typed_at,
    typed_date = as_day(typed_at),
    elapsed = elapsed,
    phylotrace = overview_common$blank_na(from_prov("phylotrace_version")),
    stringsAsFactors = FALSE
  )

  iso$no_assembly <- is.na(iso$digest)
  # The provenance keeps the digest of the assembly the profile was called
  # from; a different one on record means the record was replaced since.
  # Digests of different algorithms cannot be compared.
  comparable <- is.na(prov_alg) | is.na(hash_alg) | prov_alg == hash_alg
  iso$changed <- !is.na(hash_digest) & !is.na(prov_digest) & comparable &
    hash_digest != prov_digest

  key <- ifelse(iso$no_assembly, NA_character_, paste(iso$algorithm, iso$digest, sep = "\r"))
  iso$duplicate <- !is.na(key) & key %in% key[duplicated(key)]
  iso$twins <- rep(NA_character_, n)
  if (any(iso$duplicate)) {
    groups <- split(iso$isolate[iso$duplicate], key[iso$duplicate])
    iso$twins[iso$duplicate] <- vapply(
      which(iso$duplicate),
      function(i) paste(setdiff(groups[[key[i]]], iso$isolate[i]), collapse = ", "),
      character(1)
    )
  }

  outliers <- overview_common$assembly_outliers(iso$length, iso$contigs)
  iso$odd_size <- outliers$odd_size
  iso$fragmented <- outliers$fragmented
  algorithms <- iso$algorithm[!iso$no_assembly]
  iso$other_algorithm <- !iso$no_assembly & !is.na(iso$algorithm) &
    length(unique(algorithms[!is.na(algorithms)])) > 1 &
    iso$algorithm != overview_common$modal(algorithms)
  iso
}

# Isolates per contiguity class in class order, then those without a count;
# empty classes dropped.
contiguity_counts <- function(contiguity) {
  out <- data.frame(
    key = c(CONTIGUITY$key, "unknown"),
    label = c(CONTIGUITY$label, "Not recorded"),
    tone = c(CONTIGUITY$tone, "secondary"),
    n = c(
      as.integer(table(factor(contiguity, levels = CONTIGUITY$key))),
      sum(is.na(contiguity))
    ),
    stringsAsFactors = FALSE
  )
  out[out$n > 0, , drop = FALSE]
}

# Isolates per origin, those typed here first, then each import source by size.
origin_counts <- function(source) {
  if (!length(source)) {
    return(data.frame(label = character(0), n = integer(0), stringsAsFactors = FALSE))
  }
  counts <- sort(table(source), decreasing = TRUE)
  out <- data.frame(label = names(counts), n = as.integer(counts), stringsAsFactors = FALSE)
  local <- out$label == SOURCE_LOCAL
  out$label[local] <- LOCAL_LABEL
  out[order(!local), , drop = FALSE]
}

# Counts of `x` per bin with each bin's tone, empty bins kept so the charts keep
# their shape.
bin_counts <- function(x, breaks, labels, tones) {
  x <- x[!is.na(x)]
  data.frame(
    label = labels,
    tone = tones,
    n = as.integer(table(cut(x, breaks = breaks, labels = labels, right = TRUE))),
    stringsAsFactors = FALSE
  )
}

# Isolates typed per month, quarter or year depending on the span, oldest
# first, with every period in between kept so gaps show.
timeline_counts <- function(dates) {
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    return(data.frame(label = character(0), n = integer(0), stringsAsFactors = FALSE))
  }
  months <- seq(
    as.Date(format(min(dates), "%Y-%m-01")),
    as.Date(format(max(dates), "%Y-%m-01")),
    by = "month"
  )
  period <- if (length(months) <= MONTHLY_SPAN) {
    function(d) format(d, "%Y-%m")
  } else if (length(months) <= QUARTERLY_SPAN) {
    function(d) sprintf("%s Q%d", format(d, "%Y"), (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L)
  } else {
    function(d) format(d, "%Y")
  }
  levels <- unique(period(months))
  data.frame(
    label = levels,
    n = as.integer(table(factor(period(dates), levels = levels))),
    stringsAsFactors = FALSE
  )
}

# "a" or "a to b" for a sorted range of dates; NA when there are none.
date_span <- function(dates) {
  dates <- sort(dates[!is.na(dates)])
  if (!length(dates)) {
    return(NA_character_)
  }
  first <- format(dates[1])
  last <- format(dates[length(dates)])
  if (first == last) first else paste(first, "to", last)
}

# One row per typing run: when and where it ran, with what, the assemblies it
# typed and how many of its isolates are flagged.
run_table <- function(iso) {
  empty <- data.frame(
    Run = character(0),
    Source = character(0),
    Typed = character(0),
    Isolates = integer(0),
    PhyloTrace = character(0),
    `Median Contigs` = numeric(0),
    `Median Length (bp)` = numeric(0),
    `Median Time (s)` = numeric(0),
    Flagged = integer(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!nrow(iso)) {
    return(empty)
  }
  run <- ifelse(is.na(iso$run_id), "Not recorded", iso$run_id)
  flagged <- flag_any(iso)
  med <- function(x) if (all(is.na(x))) NA_real_ else round(median(x, na.rm = TRUE), 1)
  rows <- lapply(split(seq_len(nrow(iso)), run), function(i) {
    sources <- ifelse(iso$imported[i], iso$source[i], LOCAL_LABEL)
    versions <- unique(iso$phylotrace[i][!is.na(iso$phylotrace[i])])
    data.frame(
      Run = run[i[1]],
      Source = if (length(unique(sources)) == 1) {
        sources[1]
      } else {
        overview_common$tally(sources, limit = 2L)
      },
      Typed = date_span(iso$typed_date[i]),
      Isolates = length(i),
      PhyloTrace = if (length(versions)) paste(sort(versions), collapse = ", ") else NA,
      `Median Contigs` = med(iso$contigs[i]),
      `Median Length (bp)` = med(iso$length[i]),
      `Median Time (s)` = med(iso$elapsed[i]),
      Flagged = sum(flagged[i]),
      `.first` = suppressWarnings(min(iso$typed_date[i], na.rm = TRUE)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  # Chronological, the isolates without a recorded run last.
  out <- out[order(out$Run == "Not recorded", out$.first, out$Run), , drop = FALSE]
  out$.first <- NULL
  rownames(out) <- NULL
  out
}

# Isolates grouped by the PhyloTrace version and digest algorithm behind their
# records.
environment_table <- function(iso) {
  rec <- iso[iso$in_prov | iso$has_hash, , drop = FALSE]
  if (!nrow(rec)) {
    return(data.frame(
      phylotrace = character(0),
      algorithm = character(0),
      isolates = integer(0),
      runs = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  key <- paste(rec$phylotrace, rec$algorithm, sep = "\r")
  first <- !duplicated(key)
  levels <- key[first]
  runs <- tapply(rec$run_id, factor(key, levels = levels), function(x) {
    length(unique(x[!is.na(x)]))
  })
  out <- data.frame(
    phylotrace = rec$phylotrace[first],
    algorithm = rec$algorithm[first],
    isolates = as.integer(table(factor(key, levels = levels))),
    runs = as.integer(runs),
    stringsAsFactors = FALSE
  )
  out[order(-out$isolates), , drop = FALSE]
}

# Whether each isolate carries any flag of the isolate table.
flag_any <- function(iso) {
  iso$changed | iso$duplicate | iso$odd_size | iso$fragmented | iso$other_algorithm |
    iso$no_assembly
}

#' Flags of Each Isolate
#'
#' @param iso Data frame of the overview's `isolates`.
#'
#' @return Character vector of short, `"; "`-separated flags per isolate, empty
#'   where nothing is flagged.
#' @export
isolate_flags <- function(iso) {
  flags <- character(nrow(iso))
  add_flag <- function(hit, text) {
    flags[hit] <<- ifelse(nzchar(flags[hit]), paste(flags[hit], text, sep = "; "), text)
  }
  add_flag(iso$changed, "assembly changed")
  add_flag(iso$duplicate, "duplicate assembly")
  add_flag(iso$odd_size, "unusual size")
  add_flag(iso$fragmented, "fragmented")
  add_flag(iso$other_algorithm, "other digest algorithm")
  add_flag(iso$no_assembly, "no assembly record")
  flags
}

# Isolate record checks over the whole database, most severe first.
isolate_checks <- function(iso, orphans) {
  checks <- overview_common$check_list()
  add <- checks$add
  if (!nrow(iso)) {
    return(checks$table())
  }

  if (any(iso$changed)) {
    add(
      "Assembly changed since typing",
      "danger",
      sprintf(
        paste(
          "%d isolate(s) have a different assembly on record than the one their",
          "allele profile was called from: %s. Their assembly record no longer",
          "describes their profile; retype them from the intended assembly."
        ),
        sum(iso$changed),
        paste(head(iso$isolate[iso$changed], 5), collapse = ", ")
      ),
      sum(iso$changed)
    )
  }

  if (any(iso$duplicate)) {
    key <- paste(iso$algorithm, iso$digest, sep = "\r")[iso$duplicate]
    groups <- split(iso$isolate[iso$duplicate], key)
    add(
      "Duplicate assemblies",
      "warning",
      sprintf(
        paste(
          "%d isolates in %d group(s) share an identical assembly: %s. They are",
          "one genome under several names, so they count twice in clusters and",
          "statistics."
        ),
        sum(iso$duplicate),
        length(groups),
        paste(head(vapply(groups, paste, character(1), collapse = " = "), 3), collapse = "; ")
      ),
      sum(iso$duplicate)
    )
  }

  complete <- iso$contiguity %in% "complete"
  draft <- !is.na(iso$contiguity) & !complete
  if (any(complete) && any(draft)) {
    add(
      "Mixed assembly types",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) have complete or near-complete assemblies (up to %d",
          "contigs, typical of long-read or hybrid sequencing) next to %d draft",
          "assemblies (typical of short reads). Long-read-only assemblies can carry",
          "small indel errors that show up as extra allele differences, so compare",
          "isolates across sequencing technologies with care."
        ),
        sum(complete),
        CONTIGUITY$upper[1],
        sum(draft)
      ),
      min(sum(complete), sum(draft))
    )
  }

  if (any(iso$odd_size)) {
    add(
      "Unusual assembly size",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) have an assembly more than %d%% off the median length",
          "of %s bp - possibly contaminated or a mixed sample (larger), or",
          "incomplete (smaller)."
        ),
        sum(iso$odd_size),
        round(100 * overview_common$SIZE_TOLERANCE),
        format(round(median(iso$length, na.rm = TRUE)), big.mark = ",", scientific = FALSE)
      ),
      sum(iso$odd_size)
    )
  }

  algorithms <- iso$algorithm[!iso$no_assembly & !is.na(iso$algorithm)]
  if (length(unique(algorithms)) > 1) {
    add(
      "Mixed digest algorithms",
      "warning",
      sprintf(
        paste(
          "Assemblies were hashed with different algorithms: %s. Digests of",
          "different algorithms never match, so duplicates between them go unnoticed."
        ),
        overview_common$tally(algorithms)
      ),
      sum(iso$other_algorithm)
    )
  }

  if (any(iso$fragmented)) {
    add(
      "Fragmented assemblies",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have an assembly with over %d times the median contig",
          "count; loci split across contigs are more often missed."
        ),
        sum(iso$fragmented),
        overview_common$FRAGMENT_FACTOR
      ),
      sum(iso$fragmented)
    )
  }

  versions <- iso$phylotrace[!is.na(iso$phylotrace)]
  if (length(unique(versions)) > 1) {
    add(
      "Mixed PhyloTrace versions",
      "info",
      sprintf(
        "Isolates were typed with several PhyloTrace versions: %s.",
        overview_common$tally(versions)
      ),
      length(versions) - max(table(versions))
    )
  }

  if (any(iso$no_assembly)) {
    add(
      "No assembly record",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have no assembly on record - typed before assemblies",
          "were hashed, or imported without their records. Duplicates and",
          "replaced assemblies cannot be detected for them."
        ),
        sum(iso$no_assembly)
      ),
      sum(iso$no_assembly)
    )
  }

  left <- unique(c(orphans$hashes, orphans$provenance))
  if (length(left)) {
    add(
      "Orphaned records",
      "info",
      sprintf(
        paste(
          "%d assembly and %d provenance record(s) belong to isolates no longer in",
          "the database (%s), left behind by an earlier removal. They are not shown",
          "anywhere else and can be ignored."
        ),
        length(orphans$hashes),
        length(orphans$provenance),
        overview_common$tally(left, limit = 3L)
      ),
      length(left)
    )
  }

  out <- checks$table()
  recorded <- sum(!iso$no_assembly)
  if (recorded && !any(out$tone %in% c("danger", "warning"))) {
    add(
      "Record consistency",
      "success",
      sprintf("No assembly or record problems found across the %d recorded assemblies.", recorded)
    )
    out <- checks$table()
  }
  out
}

#' Checks of One Isolate
#'
#' @param iso Data frame of the overview's `isolates`.
#' @param i Integer row of the isolate.
#'
#' @return Data frame in the shape of `check_list()$table()`: one row per flag
#'   the isolate carries, most severe first.
#' @export
one_isolate_checks <- function(iso, i) {
  checks <- overview_common$check_list()
  add <- checks$add
  row <- iso[i, , drop = FALSE]
  short <- function(x) if (is.na(x)) "-" else paste0(substr(x, 1, 12), "...")
  med_length <- median(iso$length, na.rm = TRUE)
  med_contigs <- median(iso$contigs, na.rm = TRUE)

  if (row$changed) {
    add(
      "Assembly changed since typing",
      "danger",
      sprintf(
        "The assembly on record (%s) is not the one the profile was called from (%s).",
        short(row$digest),
        short(row$prov_digest)
      )
    )
  }
  if (row$duplicate) {
    add("Duplicate assembly", "warning", sprintf("Identical to the assembly of %s.", row$twins))
  }
  if (row$odd_size) {
    add(
      "Unusual assembly size",
      "warning",
      sprintf(
        "%+.1f%% off the median length of %s bp.",
        100 * (row$length - med_length) / med_length,
        format(round(med_length), big.mark = ",", scientific = FALSE)
      )
    )
  }
  if (row$fragmented) {
    add(
      "Fragmented assembly",
      "info",
      sprintf("%d contigs against a median of %s.", as.integer(row$contigs), format(med_contigs))
    )
  }
  if (row$other_algorithm) {
    add(
      "Other digest algorithm",
      "info",
      sprintf(
        "Hashed with %s, unlike most assemblies; duplicates of it cannot be found.",
        row$algorithm
      )
    )
  }
  if (row$no_assembly) {
    add(
      "No assembly record",
      "info",
      "Typed before assemblies were hashed, or imported without its records."
    )
  }
  checks$table()
}

#' Format a Duration
#'
#' @param seconds Numeric seconds.
#'
#' @return Character such as `"42 s"`, `"3 min 12 s"` or `"1 h 4 min"`; `NA`
#'   where unknown.
#' @export
format_duration <- function(seconds) {
  s <- round(seconds)
  out <- ifelse(
    s < 60,
    sprintf("%d s", as.integer(s)),
    ifelse(
      s < 3600,
      sprintf("%d min %d s", as.integer(s %/% 60), as.integer(s %% 60)),
      sprintf("%d h %d min", as.integer(s %/% 3600), as.integer((s %% 3600) %/% 60))
    )
  )
  out[is.na(seconds)] <- NA_character_
  out
}

#' Format a Length in Base Pairs
#'
#' @param bp Numeric base pairs.
#'
#' @return Character such as `"3.10 Mb"`, `"52.3 kb"` or `"640 bp"`; `NA`
#'   where unknown.
#' @export
format_bp <- function(bp) {
  out <- ifelse(
    bp >= 1e6,
    sprintf("%.2f Mb", bp / 1e6),
    ifelse(bp >= 1e3, sprintf("%.1f kb", bp / 1e3), sprintf("%d bp", as.integer(round(bp))))
  )
  out[is.na(bp)] <- NA_character_
  setNames(out, NULL)
}
