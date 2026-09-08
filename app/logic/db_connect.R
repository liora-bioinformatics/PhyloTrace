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

#' How long a statement waits while a typing run is in flight, in milliseconds.
#'
#' The wait above is the right one for interactive work, where a query has
#' nowhere else to go and a moment's stall is better than an error. It is the
#' wrong one for the writes a typing run banks per isolate: there the lock is
#' held by pyMLST for as long as a genome's alleles take to commit, R is
#' single-threaded (so the whole UI stalls for the duration of the wait), and a
#' write that gives up costs nothing - the closing sweep redoes it once pyMLST
#' has exited. Waiting a moment still absorbs a brief page flush; waiting
#' seconds only buys a frozen window before the same failure.
#'
#' @export
LIVE_BUSY_TIMEOUT_MS <- 250L

#' Open a Database Connection
#'
#' Wraps [DBI::dbConnect()] and arms the busy timeout. Use this rather than
#' calling `dbConnect()` directly: RSQLite silently accepts and discards a
#' `busy_timeout =` argument to `dbConnect()`, so passing it there leaves the
#' timeout at 0 and every contended statement fails immediately with
#' `database is locked`. It has to be set as a PRAGMA on the open connection.
#'
#' The wait is [BUSY_TIMEOUT_MS] unless the `phylotrace.busy_timeout` option
#' says otherwise. That option is how a caller lowers the wait for a bounded
#' stretch of work - see `persist_results()` in the typing module, the one place
#' that sets it - rather than every reader having to thread a timeout through.
#'
#' The file must already exist unless `create = TRUE`: opening a database and
#' making one are different intentions, and only the caller knows which it has.
#' See the guard in the body for why the default is the strict one.
#'
#' Callers remain responsible for [DBI::dbDisconnect()].
#'
#' @param db_path Character path to the SQLite database file.
#' @param ... Further arguments passed to [DBI::dbConnect()], e.g.
#'   `synchronous = NULL` or `flags`.
#' @param create Logical. `TRUE` to allow SQLite to create `db_path` when it
#'   does not exist - for a caller whose job is to build a new database file,
#'   such as the export module's atomic `.part` file. Defaults to `FALSE`.
#' @return DBI connection object.
#' @export
connect <- function(db_path, ..., create = FALSE) {
  # Do not conjure the database. RSQLite's default flags include SQLITE_CREATE,
  # so a path that no longer resolves - a database deleted, renamed or unmounted
  # while the session is open - comes back as a brand new *empty* file. That is
  # far worse than an error: it destroys the evidence (file.exists() now says
  # the database is fine, so the missing-file watcher never fires and the
  # landing page keeps offering the corpse as loadable), and every read that
  # follows dies on "no such table: mlst" instead. Refusing up front keeps a
  # deleted database deleted, which is the state the recovery paths can act on.
  if (
    !isTRUE(create) &&
      (length(db_path) != 1 ||
        is.na(db_path) ||
        !nzchar(db_path) ||
        !file.exists(db_path))
  ) {
    stop(
      sprintf(
        "Database file not found: %s",
        if (length(db_path) == 1 && !is.na(db_path)) db_path else "<none>"
      ),
      call. = FALSE
    )
  }

  con <- dbConnect(SQLite(), db_path, ...)
  # Succeeds even when the file is not a database - the PRAGMA touches no page,
  # so probing callers still get their error from the first real query.
  dbExecute(
    con,
    sprintf(
      "PRAGMA busy_timeout = %d",
      getOption("phylotrace.busy_timeout", BUSY_TIMEOUT_MS)
    )
  )
  con
}
