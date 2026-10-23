# app/logic/overview_common.R
#
# Data helpers shared by the Database overviews (cgMLST, MLST): reading the
# isolate list and optional table columns, tallying values, and assembling
# provenance checks into one ordered table.

box::use(
  DBI[dbGetQuery, dbListFields],
)
box::use(
  app / logic / db_compat[REF_SOUCHE],
)

# Severity order of a check's tone, most severe first.
TONE_RANK <- c(danger = 1L, warning = 2L, info = 3L, success = 4L)

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
