# app/logic/db_connect.R
#
# Single entry point for opening the loaded SQLite database, so every
# connection arrives with its lock wait armed.

box::use(
  DBI[dbConnect, dbExecute],
  RSQLite[SQLite],
)

#' How long a statement waits for another writer's lock, in milliseconds.
#'
#' Typing runs pyMLST in a separate process that writes this same file, so a
#' query issued from the Shiny session while that process holds the write lock
#' has to wait rather than fail. R's own writes need no such coordination and no
#' queue: Shiny's reactive loop is single-threaded, so two observers can never
#' write at once.
#'
#' @export
BUSY_TIMEOUT_MS <- 5000L

#' Open a Database Connection
#'
#' Wraps [DBI::dbConnect()] and arms the busy timeout. Use this rather than
#' calling `dbConnect()` directly: RSQLite silently accepts and discards a
#' `busy_timeout =` argument to `dbConnect()`, so passing it there leaves the
#' timeout at 0 and every contended statement fails immediately with
#' `database is locked`. It has to be set as a PRAGMA on the open connection.
#'
#' Callers remain responsible for [DBI::dbDisconnect()].
#'
#' @param db_path Character path to the SQLite database file.
#' @param ... Further arguments passed to [DBI::dbConnect()], e.g.
#'   `synchronous = NULL` or `flags`.
#' @return DBI connection object.
#' @export
connect <- function(db_path, ...) {
  con <- dbConnect(SQLite(), db_path, ...)
  # Succeeds even when the file is not a database - the PRAGMA touches no page,
  # so probing callers still get their error from the first real query.
  dbExecute(con, sprintf("PRAGMA busy_timeout = %d", BUSY_TIMEOUT_MS))
  con
}
