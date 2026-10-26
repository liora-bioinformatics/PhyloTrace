# app/logic/cgmlst_overview.R
#
# cgMLST across a whole database: per-isolate completeness and assembly
# statistics, per-locus call and allele counts, their distributions and the
# provenance checks the "cgMLST Overview" view is built from. Everything is
# derived on read from `mlst`, `targets`, `scheme_overview`, `genome_hashes`
# and `typing_provenance`.

box::use(
  DBI[dbDisconnect, dbGetQuery, dbListTables, dbReadTable],
  stats[median],
  utils[head],
)
box::use(
  app / logic / db_compat[REF_SOUCHE],
  app / logic / db_connect[connect],
  app / logic / overview_common,
  app / logic / provenance[overview_value],
)

#' Low Completeness Threshold
#'
#' @description Share of the scheme's loci (percent) below which an isolate's
#'   allele profile is flagged as incomplete.
#'
#' @export
LOW_COMPLETENESS <- 95

# Completeness bands of the typing summary: lower bound, label and tone.
COMPLETENESS_BANDS <- data.frame(
  lower = c(99, 98, LOW_COMPLETENESS, -Inf),
  label = c("99-100%", "98-99%", "95-98%", "Below 95%"),
  tone = c("success", "info", "warning", "danger"),
  stringsAsFactors = FALSE
)

# Bins of missing loci per isolate and of alleles per locus.
MISSING_BREAKS <- c(-Inf, 0, 5, 10, 25, 50, 100, Inf)
MISSING_LABELS <- c("None", "1-5", "6-10", "11-25", "26-50", "51-100", "Over 100")
ALLELE_BREAKS <- c(-Inf, 1, 5, 10, 25, 50, 100, 250, Inf)
ALLELE_LABELS <- c("1", "2-5", "6-10", "11-25", "26-50", "51-100", "101-250", "Over 250")

PROV_COLS <- c(
  "isolate", "typed_at", "cg_scheme_version", "cg_identity", "cg_coverage",
  "cg_alleles_added", "cg_partial_genes", "cg_filled_genes", "cg_removed_genes",
  "pymlst_version", "blat_version", "mafft_version", "n_contigs", "total_length",
  "genome_digest"
)

HASH_COLS <- c("isolate", "genome_digest", "n_contigs", "total_length")

# Locus names are compared with underscores and hyphens treated alike, as the
# rest of the loci code does.
norm_locus <- function(x) gsub("[-_]", "-", x)

# Integer column from text, NA where it does not parse.
as_int <- function(x) suppressWarnings(as.integer(x))

#' Load the cgMLST Overview of a Database
#'
#' @description Reads the allele table's per-locus and per-isolate counts, the
#'   scheme tables, assembly hashes and typing provenance in one pass and
#'   summarises them with `build_cg_overview()`.
#'
#' @param db_path Character path to the SQLite database file.
#'
#' @return List from `build_cg_overview()`.
#' @export
load_cgmlst_overview <- function(db_path) {
  con <- connect(db_path)
  on.exit(dbDisconnect(con))
  tables <- dbListTables(con)
  read_table <- function(name) if (name %in% tables) dbReadTable(con, name) else NULL

  has_mlst <- "mlst" %in% tables
  # One pass over the covering index for every per-locus figure, one over the
  # souche index for every per-isolate one.
  locus_counts <- if (has_mlst) {
    dbGetQuery(
      con,
      "SELECT gene, COUNT(DISTINCT seqid) AS alleles, COUNT(*) AS calls
         FROM mlst GROUP BY gene"
    )
  } else {
    NULL
  }
  per_isolate <- if (has_mlst) {
    dbGetQuery(con, "SELECT souche, COUNT(*) AS loci FROM mlst GROUP BY souche")
  } else {
    NULL
  }

  build_cg_overview(
    isolates = overview_common$database_isolates(con, tables),
    scheme = read_table("scheme_overview"),
    targets = read_table("targets"),
    locus_counts = locus_counts,
    per_isolate = per_isolate,
    prov = if ("typing_provenance" %in% tables) {
      overview_common$read_cols(con, "typing_provenance", PROV_COLS)
    } else {
      NULL
    },
    hashes = if ("genome_hashes" %in% tables) {
      overview_common$read_cols(con, "genome_hashes", HASH_COLS)
    } else {
      NULL
    }
  )
}

#' Summarise a Database's cgMLST Typing
#'
#' @description Merges allele counts, scheme tables, assembly hashes and
#'   provenance into the per-isolate and per-locus tables, distributions and
#'   provenance checks of the cgMLST overview.
#'
#' @param isolates Character vector of every isolate in the database.
#' @param scheme Data frame of `scheme_overview` (`key`, `value`), or `NULL`.
#' @param targets Data frame of `targets`, or `NULL`.
#' @param locus_counts Data frame with `gene`, `alleles` (distinct, reference
#'   included) and `calls` (rows, reference included), or `NULL`.
#' @param per_isolate Data frame with `souche` and `loci`, or `NULL`.
#' @param prov Data frame of `typing_provenance` rows, or `NULL`.
#' @param hashes Data frame of `genome_hashes` rows, or `NULL`.
#'
#' @return A list with `scheme` (the overview table as given), `locus_count`,
#'   `scheme_version`, `isolates`, `loci`, `completeness`, `missing_bins`,
#'   `allele_bins`, `settings` and `checks`.
#' @export
build_cg_overview <- function(
  isolates = character(0),
  scheme = NULL,
  targets = NULL,
  locus_counts = NULL,
  per_isolate = NULL,
  prov = NULL,
  hashes = NULL
) {
  if (is.null(locus_counts)) {
    locus_counts <- data.frame(gene = character(0), alleles = integer(0), calls = integer(0))
  }
  if (is.null(per_isolate)) {
    per_isolate <- data.frame(souche = character(0), loci = integer(0))
  }
  ref <- per_isolate$souche == REF_SOUCHE
  ref_loci <- if (any(ref)) per_isolate$loci[ref][1] else NA_integer_
  per_isolate <- per_isolate[!ref, , drop = FALSE]

  # The reference holds every locus of the scheme; the scheme tables stand in
  # for a database that lost it.
  locus_count <- if (!is.na(ref_loci)) {
    ref_loci
  } else if (!is.null(targets) && nrow(targets)) {
    nrow(targets)
  } else {
    overview_value(scheme, "Locus Count", numeric = TRUE)
  }
  scheme_version <- overview_value(scheme, "Version")

  iso <- isolate_table(isolates, per_isolate, prov, hashes, locus_count, scheme_version)
  loci <- locus_table(
    targets,
    locus_counts,
    ref_present = !is.na(ref_loci),
    n_typed = sum(iso$typed)
  )

  list(
    scheme = scheme,
    locus_count = locus_count,
    scheme_version = scheme_version,
    isolates = iso,
    loci = loci,
    completeness = completeness_bands(iso$completeness[iso$typed]),
    missing_bins = bin_counts(iso$missing[iso$typed], MISSING_BREAKS, MISSING_LABELS),
    allele_bins = bin_counts(loci$`Allele Count`, ALLELE_BREAKS, ALLELE_LABELS),
    settings = settings_table(iso),
    checks = cg_checks(iso, scheme_version)
  )
}

# One row per isolate: how complete its profile is, the assembly it came from
# and how it was typed, with the flags the checks are built on.
isolate_table <- function(isolates, per_isolate, prov, hashes, locus_count, scheme_version) {
  universe <- unique(c(as.character(isolates), per_isolate$souche))
  universe <- universe[!is.na(universe) & universe != REF_SOUCHE]
  n <- length(universe)
  at_prov <- if (is.null(prov)) rep(NA_integer_, n) else match(universe, prov$isolate)
  at_hash <- if (is.null(hashes)) rep(NA_integer_, n) else match(universe, hashes$isolate)
  # A column the table lacks reads as NA, like a value it never recorded.
  pick <- function(df, at, col) {
    if (is.null(df) || !(col %in% names(df))) rep(NA, n) else df[[col]][at]
  }
  from_prov <- function(col) pick(prov, at_prov, col)
  from_hash <- function(col) pick(hashes, at_hash, col)
  # Assembly statistics: the hash table first (it also covers imported
  # isolates), provenance where it has none.
  either <- function(col) {
    out <- from_hash(col)
    out[is.na(out)] <- from_prov(col)[is.na(out)]
    out
  }

  loci <- per_isolate$loci[match(universe, per_isolate$souche)]
  loci[is.na(loci)] <- 0L
  typed <- loci > 0
  completeness <- if (is.na(locus_count) || locus_count <= 0) {
    rep(NA_real_, length(universe))
  } else {
    round(100 * loci / locus_count, 2)
  }
  completeness[!typed] <- NA_real_

  iso <- data.frame(
    isolate = universe,
    typed = typed,
    loci = as.integer(loci),
    missing = if (is.na(locus_count)) {
      rep(NA_integer_, n)
    } else {
      as.integer(pmax(locus_count - loci, 0))
    },
    completeness = completeness,
    alleles_added = as_int(from_prov("cg_alleles_added")),
    partial = as_int(from_prov("cg_partial_genes")),
    filled = as_int(from_prov("cg_filled_genes")),
    removed = as_int(from_prov("cg_removed_genes")),
    contigs = as_int(either("n_contigs")),
    length = suppressWarnings(as.numeric(either("total_length"))),
    digest = overview_common$blank_na(either("genome_digest")),
    identity = suppressWarnings(as.numeric(from_prov("cg_identity"))),
    coverage = suppressWarnings(as.numeric(from_prov("cg_coverage"))),
    scheme_version = overview_common$blank_na(from_prov("cg_scheme_version")),
    pymlst_version = overview_common$blank_na(from_prov("pymlst_version")),
    blat_version = overview_common$blank_na(from_prov("blat_version")),
    mafft_version = overview_common$blank_na(from_prov("mafft_version")),
    typed_at = overview_common$blank_na(from_prov("typed_at")),
    in_prov = !is.na(at_prov),
    stringsAsFactors = FALSE
  )
  iso$missing[!typed] <- NA_integer_

  iso$low_completeness <- typed & !is.na(completeness) & completeness < LOW_COMPLETENESS
  dup <- !is.na(iso$digest) & iso$digest %in% iso$digest[duplicated(iso$digest)]
  iso$duplicate <- dup
  outliers <- overview_common$assembly_outliers(iso$length, iso$contigs)
  iso$odd_size <- outliers$odd_size
  iso$fragmented <- outliers$fragmented
  iso$other_version <- !is.na(scheme_version) &
    !is.na(iso$scheme_version) &
    iso$scheme_version != scheme_version
  pair <- overview_common$threshold_pairs(iso$identity, iso$coverage)
  iso$other_thresholds <- !is.na(pair) & pair != overview_common$modal(pair)
  iso
}

# One row per locus: the scheme's own description of it next to how often it
# was called in this database. `.gene` is the locus as `mlst` spells it.
locus_table <- function(targets, locus_counts, ref_present, n_typed) {
  genes <- locus_counts$gene
  if (!is.null(targets) && nrow(targets) && "Locus" %in% names(targets)) {
    keep <- intersect(c("Locus", "Gene", "Start", "Length", "Product"), names(targets))
    out <- targets[, keep, drop = FALSE]
    at <- match(norm_locus(out$Locus), norm_locus(genes))
  } else {
    out <- data.frame(Locus = genes, stringsAsFactors = FALSE)
    at <- seq_along(genes)
  }
  for (col in intersect(c("Start", "Length"), names(out))) {
    out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
  }
  # The reference's own row is a scheme allele but not a call.
  calls <- locus_counts$calls[at] - as.integer(ref_present)
  calls[is.na(calls)] <- 0L
  calls <- pmax(as.integer(calls), 0L)
  alleles <- locus_counts$alleles[at]
  alleles[is.na(alleles)] <- 0L

  out$`Allele Count` <- as.integer(alleles)
  out$`Isolates Called` <- calls
  out$Missing <- pmax(as.integer(n_typed) - calls, 0L)
  out$.gene <- ifelse(is.na(at), out$Locus, genes[at])
  out
}

# Typed isolates per completeness band, every band listed.
completeness_bands <- function(completeness) {
  band <- vapply(
    completeness,
    function(x) which(x >= COMPLETENESS_BANDS$lower)[1],
    integer(1)
  )
  out <- COMPLETENESS_BANDS[, c("label", "tone")]
  out$n <- as.integer(tabulate(band, nbins = nrow(COMPLETENESS_BANDS)))
  out
}

# Counts of `x` per bin, empty bins kept so the charts keep their shape.
bin_counts <- function(x, breaks, labels) {
  x <- x[!is.na(x)]
  data.frame(
    label = labels,
    n = as.integer(table(cut(x, breaks = breaks, labels = labels, right = TRUE))),
    stringsAsFactors = FALSE
  )
}

# Typed isolates grouped by the settings they were typed with: scheme version,
# search thresholds and tool versions.
settings_table <- function(iso) {
  typed <- iso[iso$typed, , drop = FALSE]
  if (!nrow(typed)) {
    return(data.frame(
      scheme_version = character(0),
      thresholds = character(0),
      tools = character(0),
      isolates = integer(0),
      recorded = logical(0),
      stringsAsFactors = FALSE
    ))
  }
  thresholds <- overview_common$threshold_pairs(typed$identity, typed$coverage)
  tools <- ifelse(
    is.na(typed$pymlst_version),
    NA_character_,
    sprintf(
      "pyMLST %s, BLAT %s, MAFFT %s",
      typed$pymlst_version,
      ifelse(is.na(typed$blat_version), "-", typed$blat_version),
      ifelse(is.na(typed$mafft_version), "-", typed$mafft_version)
    )
  )
  key <- paste(typed$scheme_version, thresholds, tools, typed$in_prov, sep = "\r")
  first <- !duplicated(key)
  out <- data.frame(
    scheme_version = typed$scheme_version[first],
    thresholds = thresholds[first],
    tools = tools[first],
    isolates = as.integer(table(factor(key, levels = key[first]))),
    recorded = typed$in_prov[first],
    stringsAsFactors = FALSE
  )
  out[order(-out$isolates), , drop = FALSE]
}

# Provenance and quality checks over the whole database, most severe first.
cg_checks <- function(iso, scheme_version) {
  checks <- overview_common$check_list()
  add <- checks$add
  typed <- iso[iso$typed, , drop = FALSE]
  if (!nrow(typed)) {
    return(checks$table())
  }

  if (any(typed$low_completeness)) {
    add(
      "Low completeness",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) have fewer than %s%% of the scheme's loci called, which",
          "inflates their allele distances to every other isolate."
        ),
        sum(typed$low_completeness),
        format(LOW_COMPLETENESS)
      ),
      sum(typed$low_completeness)
    )
  }

  if (any(typed$duplicate)) {
    groups <- split(typed$isolate[typed$duplicate], typed$digest[typed$duplicate])
    add(
      "Duplicate assemblies",
      "warning",
      sprintf(
        "%d isolate(s) were typed from an assembly identical to another one's: %s.",
        sum(typed$duplicate),
        paste(
          head(vapply(groups, paste, character(1), collapse = " = "), 3),
          collapse = "; "
        )
      ),
      sum(typed$duplicate)
    )
  }

  if (any(typed$other_version)) {
    add(
      "Scheme version changed",
      "warning",
      sprintf(
        "%d isolate(s) were typed against scheme version %s; the database now carries %s.",
        sum(typed$other_version),
        overview_common$tally(typed$scheme_version[typed$other_version]),
        scheme_version
      ),
      sum(typed$other_version)
    )
  }

  if (any(typed$odd_size)) {
    add(
      "Unusual assembly size",
      "warning",
      sprintf(
        paste(
          "%d isolate(s) have an assembly more than %d%% off the median length",
          "of %s bp - possibly contaminated (larger) or incomplete (smaller)."
        ),
        sum(typed$odd_size),
        round(100 * overview_common$SIZE_TOLERANCE),
        format(median(typed$length, na.rm = TRUE), big.mark = ",", scientific = FALSE)
      ),
      sum(typed$odd_size)
    )
  }

  pairs <- overview_common$threshold_pairs(typed$identity, typed$coverage)
  pairs <- pairs[!is.na(pairs)]
  if (length(unique(pairs)) > 1) {
    add(
      "Mixed thresholds",
      "warning",
      sprintf("Alleles were called with different thresholds: %s.", overview_common$tally(pairs)),
      sum(typed$other_thresholds)
    )
  }

  if (any(typed$fragmented)) {
    add(
      "Fragmented assemblies",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have an assembly with over %d times the median contig",
          "count; loci split across contigs are more often missed."
        ),
        sum(typed$fragmented),
        overview_common$FRAGMENT_FACTOR
      ),
      sum(typed$fragmented)
    )
  }

  tools <- c(pyMLST = "pymlst_version", BLAT = "blat_version", MAFFT = "mafft_version")
  for (name in names(tools)) {
    versions <- typed[[tools[[name]]]]
    versions <- versions[!is.na(versions)]
    if (length(unique(versions)) > 1) {
      add(
        sprintf("Mixed %s versions", name),
        "info",
        sprintf(
          "Alleles were called with several %s versions: %s.",
          name,
          overview_common$tally(versions)
        ),
        length(versions) - max(table(versions))
      )
    }
  }

  unrecorded <- !typed$in_prov
  if (any(unrecorded)) {
    add(
      "No provenance",
      "info",
      sprintf(
        paste(
          "%d isolate(s) have no typing provenance - typed before it was",
          "recorded, or imported from a database without it."
        ),
        sum(unrecorded)
      ),
      sum(unrecorded)
    )
  }

  out <- checks$table()
  if (!any(out$tone %in% c("danger", "warning"))) {
    add(
      "Typing consistency",
      "success",
      sprintf(
        "No typing or assembly problems found across the %d typed isolates.",
        nrow(typed)
      )
    )
    out <- checks$table()
  }
  out
}
