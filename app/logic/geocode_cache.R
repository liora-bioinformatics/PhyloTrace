# app/logic/geocode_cache.R
#
# Geocoded places (place string -> coordinates) kept inside the loaded
# database, so they travel with it through Export and a colleague opening the
# same file maps it without looking anything up again.

box::use(
  DBI[dbDisconnect, dbExecute, dbExistsTable, dbGetQuery, dbWithTransaction],
)
box::use(
  app / logic / db_connect[connect],
)

#' Name of the Geocode Cache Table
#' @export
GEOCODE_TABLE <- "phylotrace_geocodes"

.DDL <- "CREATE TABLE IF NOT EXISTS phylotrace_geocodes (
  place TEXT PRIMARY KEY,
  latitude REAL NOT NULL,
  longitude REAL NOT NULL
)"

# The cache's columns with no rows.
.empty <- function() {
  data.frame(
    place = character(0),
    latitude = numeric(0),
    longitude = numeric(0),
    stringsAsFactors = FALSE
  )
}

# Whether `db_path` names a database file that can be opened.
.usable <- function(db_path) {
  is.character(db_path) && length(db_path) == 1L && !is.na(db_path) &&
    file.exists(db_path)
}

#' Read the Geocode Cache
#'
#' @param db_path Character path to the SQLite database file, or `NULL`.
#' @return Data frame with `place`, `latitude` and `longitude`; empty when there
#'   is no database, no table yet, or it cannot be read.
#' @export
read_geocodes <- function(db_path) {
  if (!.usable(db_path)) {
    return(.empty())
  }
  tryCatch(
    {
      con <- connect(db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      if (!dbExistsTable(con, GEOCODE_TABLE)) {
        return(.empty())
      }
      dbGetQuery(con, "SELECT place, latitude, longitude FROM phylotrace_geocodes")
    },
    error = function(e) .empty()
  )
}

#' Add Places to the Geocode Cache
#'
#' Best effort: a database that cannot be written (read-only, locked) only
#' loses the speed-up, so a failure is ignored.
#'
#' @param db_path Character path to the SQLite database file, or `NULL`.
#' @param df Data frame with `place`, `latitude` and `longitude`. Rows without
#'   coordinates are skipped; a place already cached is replaced.
#' @return `TRUE` when the rows were written, invisibly.
#' @export
write_geocodes <- function(db_path, df) {
  keep <- !is.na(df$place) & nzchar(df$place) & !is.na(df$latitude) & !is.na(df$longitude)
  df <- df[keep, , drop = FALSE]
  if (!.usable(db_path) || !nrow(df)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      con <- connect(db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      dbWithTransaction(con, {
        dbExecute(con, .DDL)
        dbExecute(
          con,
          "INSERT OR REPLACE INTO phylotrace_geocodes (place, latitude, longitude)
             VALUES (?, ?, ?)",
          params = list(df$place, df$latitude, df$longitude)
        )
      })
      TRUE
    },
    error = function(e) FALSE
  )
  invisible(ok)
}

#' Empty the Geocode Cache
#'
#' Every place is looked up afresh on the next map generation, e.g. after one
#' was resolved to the wrong spot.
#'
#' @param db_path Character path to the SQLite database file.
#' @return `TRUE` when the cache is empty afterwards.
#' @export
clear_geocodes <- function(db_path) {
  if (!.usable(db_path)) {
    return(FALSE)
  }
  tryCatch(
    {
      con <- connect(db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      if (dbExistsTable(con, GEOCODE_TABLE)) {
        dbExecute(con, "DELETE FROM phylotrace_geocodes")
      }
      TRUE
    },
    error = function(e) FALSE
  )
}
