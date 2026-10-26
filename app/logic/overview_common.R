# app/logic/overview_common.R
#
# Data helpers shared by the Database overviews (cgMLST, MLST, AMR, Isolates):
# reading the isolate list and optional table columns, tallying values, flagging
# unusual assemblies and assembling provenance checks into one ordered table.

box::use(
  DBI[dbGetQuery, dbListFields],
  stats[median],
)
box::use(
  app / logic / db_compat[REF_SOUCHE],
)

# Severity order of a check's tone, most severe first.
TONE_RANK <- c(danger = 1L, warning = 2L, info = 3L, success = 4L)

#' Assembly Size Tolerance
#'
#' @description Share of the median assembly length an assembly may be off by
#'   before it is flagged; a genome within a species rarely varies by more.
#'
#' @export
SIZE_TOLERANCE <- 0.2

#' Fragmented Assembly Thresholds
#'
#' @description An assembly with `FRAGMENT_FACTOR` times the median contig
#'   count, and at least `FRAGMENT_MIN` contigs, is flagged as fragmented.
#'
#' @export
FRAGMENT_FACTOR <- 3

#' @rdname FRAGMENT_FACTOR
#' @export
FRAGMENT_MIN <- 100

# Medians need a handful of assemblies before an outlier means anything.
OUTLIER_MIN_N <- 5

#' Collapse Blank Strings to NA
#'
#' @param x Vector coerced to character.
#'
#' @return Character vector with empty or whitespace-only values as `NA`.
#' @export
blank_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

#' Read Columns That May Not Exist
#'
#' @description Reads `cols` from `table`, filling any column an older database
#'   lacks with `NA`, so every reader downstream sees one shape.
#'
#' @param con Open DBI connection.
#' @param table Character table name.
#' @param cols Character vector of column names.
#'
#' @return Data frame with exactly `cols`.
#' @export
read_cols <- function(con, table, cols) {
  have <- intersect(cols, dbListFields(con, table))
  df <- dbGetQuery(
    con,
    sprintf("SELECT %s FROM %s", paste(have, collapse = ", "), table)
  )
  for (col in setdiff(cols, have)) {
    df[[col]] <- rep(NA_character_, nrow(df))
  }
  df[cols]
}

#' List the Isolates of a Database
#'
#' @description `metadata` holds one row per isolate (sync_metadata_table()
#'   keeps it level with `mlst` after every typing run and import) and is read
#'   in milliseconds, where the distinct scan of `mlst` takes seconds on a large
#'   database; `mlst` is only scanned when there is no metadata table.
#'
#' @param con Open DBI connection.
#' @param tables Character vector of the database's tables.
#'
#' @return Character vector of isolate names, without the scheme reference.
#' @export
database_isolates <- function(con, tables) {
  if ("metadata" %in% tables) {
    dbGetQuery(
      con,
      "SELECT isolate FROM metadata WHERE isolate != ?",
      params = list(REF_SOUCHE)
    )$isolate
  } else if ("mlst" %in% tables) {
    dbGetQuery(
      con,
      "SELECT DISTINCT souche FROM mlst WHERE souche != ?",
      params = list(REF_SOUCHE)
    )$souche
  } else {
    character(0)
  }
}

#' Flag Unusual Assemblies
#'
#' @description Assemblies far off the collection's median length, and ones
#'   split into many more contigs than is usual for it. Both need at least
#'   five recorded assemblies to compare against; fewer flag nothing.
#'
#' @param sizes Numeric vector of assembly lengths (bp), `NA` where unknown.
#' @param contigs Numeric vector of contig counts, `NA` where unknown.
#'
#' @return A list of two logical vectors parallel to the input: `odd_size`
#'   (more than `SIZE_TOLERANCE` off the median length) and `fragmented`.
#' @export
assembly_outliers <- function(sizes, contigs) {
  sized <- sizes[!is.na(sizes)]
  pieces <- contigs[!is.na(contigs)]
  odd_size <- if (length(sized) >= OUTLIER_MIN_N) {
    m <- median(sized)
    !is.na(sizes) & abs(sizes - m) > SIZE_TOLERANCE * m
  } else {
    rep(FALSE, length(sizes))
  }
  fragmented <- if (length(pieces) >= OUTLIER_MIN_N) {
    !is.na(contigs) & contigs > FRAGMENT_FACTOR * median(pieces) & contigs >= FRAGMENT_MIN
  } else {
    rep(FALSE, length(contigs))
  }
  list(odd_size = odd_size, fragmented = fragmented)
}

#' Most Frequent Value
#'
#' @param x Vector.
#'
#' @return The most frequent non-`NA` value (the first seen on a tie), or `NA`.
#' @export
modal <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_character_)
  }
  counts <- table(factor(x, levels = unique(x)))
  names(counts)[which.max(counts)]
}

#' Tally Values for a Sentence
#'
#' @param x Vector of values.
#' @param limit Integer number of values named before the rest are counted.
#'
#' @return Character string such as `"a (3), b (1)"`, most frequent first.
#' @export
tally <- function(x, limit = 4L) {
  counts <- sort(table(x), decreasing = TRUE)
  shown <- sprintf("%s (%d)", names(counts), as.integer(counts))
  if (length(shown) > limit) {
    shown <- c(shown[seq_len(limit)], sprintf("%d more", length(shown) - limit))
  }
  paste(shown, collapse = ", ")
}

#' Describe Search Thresholds
#'
#' @param identity Numeric vector of identity thresholds, as recorded.
#' @param coverage Numeric vector of coverage thresholds, as recorded.
#'
#' @return Character vector `"identity x / coverage y"`; `NA` where neither
#'   threshold was recorded.
#' @export
threshold_pairs <- function(identity, coverage) {
  shown <- function(x) {
    out <- format(as.numeric(x), trim = TRUE, drop0trailing = TRUE)
    out[is.na(x)] <- "-"
    out
  }
  out <- sprintf("identity %s / coverage %s", shown(identity), shown(coverage))
  out[is.na(identity) & is.na(coverage)] <- NA_character_
  out
}

#' Start a Provenance Check List
#'
#' @description Checks are collected one at a time and then ordered by
#'   severity; each is what was checked, its tone (`danger`, `warning`, `info`
#'   or `success`), a sentence of detail and how many isolates it concerns.
#'
#' @return A list with `add(check, tone, detail, n)` and `table()`, the latter
#'   returning the checks as a data frame, most severe first.
#' @export
check_list <- function() {
  checks <- list()
  list(
    add = function(check, tone, detail, n = NA_integer_) {
      checks[[length(checks) + 1L]] <<- data.frame(
        check = check,
        tone = tone,
        detail = detail,
        n = as.integer(n),
        stringsAsFactors = FALSE
      )
    },
    table = function() {
      if (!length(checks)) {
        return(data.frame(
          check = character(0),
          tone = character(0),
          detail = character(0),
          n = integer(0),
          stringsAsFactors = FALSE
        ))
      }
      out <- do.call(rbind, checks)
      out[order(TONE_RANK[out$tone]), , drop = FALSE]
    }
  )
}
