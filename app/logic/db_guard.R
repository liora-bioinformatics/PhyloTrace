# app/logic/db_guard.R
#
# Keeps a failed database call inside a Shiny session from ending that session:
# the error is logged and shown to the user as a notification naming the action
# and the reason, and the caller backs out instead.

box::use(
  shiny[getDefaultReactiveDomain, req, showNotification],
)
box::use(
  app / logic / logging[log_event],
)

# Returned by guard_db() in place of a value when the guarded work failed.
FAILED <- structure(list(), class = "db_guard_failure")

#' Describe a Database Error for the User
#'
#' SQLite's "database is locked" names the symptom but not the cause or what to
#' do about it, so that case is spelled out. Every other message is already the
#' most specific reason available and is passed through.
#'
#' @param e A condition.
#' @return Character scalar.
#' @export
db_failure_reason <- function(e) {
  msg <- conditionMessage(e)
  if (grepl("database is (locked|busy)", msg, ignore.case = TRUE)) {
    paste(
      "the database is locked by another process (a typing run, an import, or",
      "another PhyloTrace session using the same file). Try again once it has",
      "finished."
    )
  } else {
    msg
  }
}

#' Report a Failed Database Action
#'
#' Logs the raw error to the console and session log and, inside a session,
#' shows an error notification naming the action and the reason. A repeat of
#' the same action replaces its notification rather than stacking another.
#'
#' @param action Character scalar naming what failed, e.g. `"Saving metadata"`.
#' @param e The condition that was raised.
#' @return Invisibly, the reason shown to the user.
#' @export
report_db_failure <- function(action, e) {
  reason <- db_failure_reason(e)
  log_event("DB", paste(action, "failed"), conditionMessage(e))
  if (!is.null(getDefaultReactiveDomain())) {
    showNotification(
      paste0(action, " failed: ", reason),
      id = paste0("db-failure-", gsub("[^a-z0-9]+", "-", tolower(action))),
      type = "error",
      duration = 10
    )
  }
  invisible(reason)
}

#' Run Database Work Without Letting Its Error End the Session
#'
#' An error escaping an observer ends the Shiny session, and SQLite raises one
#' whenever another writer holds the lock past the busy timeout. Wrap the
#' database work an observer does in this, and back out when [db_failed()]
#' says it did not complete.
#'
#' Shiny's silent errors (`req()`, `validate()`) are control flow, not
#' failures: they pass through unreported, so guarded work can still bail out
#' the usual way and a guarded reactive read inside it is not reported twice.
#'
#' @param action Character scalar naming the action for the log and the
#'   notification, e.g. `"Removing isolates"`.
#' @param expr The database work, evaluated once.
#' @return The value of `expr`, or a failure marker to test with [db_failed()].
#' @export
guard_db <- function(action, expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) {
      stop(e)
    }
    report_db_failure(action, e)
    FAILED
  })
}

#' Test Whether Guarded Database Work Failed
#'
#' @param x The value returned by [guard_db()].
#' @return Logical scalar.
#' @export
db_failed <- function(x) {
  inherits(x, "db_guard_failure")
}

#' Read the Database in a Reactive Without Ending the Session
#'
#' A reactive's error reaches every observer that reads it and ends the session
#' there, so a reactive cannot hand back a failure marker the way [guard_db()]
#' does. After reporting, this cancels the way `req(cancelOutput = TRUE)` does:
#' observers reading it stop quietly, and outputs keep what they last showed
#' instead of going blank, until the reactive is next invalidated. Returning an
#' empty table instead would be read downstream as "no isolates".
#'
#' @param action Character scalar naming the read, e.g. `"Loading metadata"`.
#' @param expr The database read, evaluated once.
#' @return The value of `expr`.
#' @export
guard_db_read <- function(action, expr) {
  value <- guard_db(action, expr)
  if (db_failed(value)) {
    req(FALSE, cancelOutput = TRUE)
  }
  value
}
