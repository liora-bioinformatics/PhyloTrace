# app/logic/db_events.R
#
# Cross-module change notification for the loaded database: a revision counter
# per data domain that writers bump and readers depend on, plus the helpers for
# reconciling in-memory state that refers to rows a write may have removed.

box::use(
  shiny[isolate, reactiveValues],
)

#' Data domains a database write can touch.
#'
#' One counter per domain rather than one per table: a domain is the smallest
#' unit any reader distinguishes. Splitting further would add edges to the
#' notification graph that no module reads differently.
#'
#' \describe{
#'   \item{isolates}{The isolate pool itself - rows added or removed from
#'     `mlst` / `metadata`. The only domain that can invalidate a stored
#'     isolate name, so it is the one selection state reconciles against.}
#'   \item{metadata}{Values in the fixed `metadata` columns. Separate from
#'     `isolates` because editing a cell changes labels without moving the
#'     pool, and half the readers care about only one of the two.}
#'   \item{custom_fields}{User-defined variable definitions and their values.}
#'   \item{amr}{`amr_results` / `amr_summary`.}
#'   \item{staged}{Staged (imported, not merged) profile sets.}
#'   \item{analyses}{Saved Analyses and plots.}
#'   \item{schema}{Scheme-level tables - `scheme_overview`, `targets`,
#'     `mlst_type`, `sequences`. Only a merge, a restore or typing moves these.}
#' }
#'
#' @export
DOMAINS <- c(
  "isolates",
  "metadata",
  "custom_fields",
  "amr",
  "staged",
  "analyses",
  "schema"
)

# Rejects a domain that is not in DOMAINS. A mistyped domain would otherwise
# bump a counter nothing reads (or depend on one nothing bumps) and reintroduce
# exactly the silent-staleness bug this module exists to prevent, so it is an
# error rather than a warning.
.check <- function(domains) {
  if (!length(domains)) {
    stop("db_events: no domain given", call. = FALSE)
  }
  unknown <- setdiff(domains, DOMAINS)
  if (length(unknown)) {
    stop(
      sprintf(
        "db_events: unknown domain(s) %s; known: %s",
        paste(sQuote(unknown), collapse = ", "),
        paste(DOMAINS, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  domains
}

#' Create a Revision Bus
#'
#' One bus per session, created in `app/main.R` and passed to every module that
#' reads or writes the database.
#'
#' @return A `reactiveValues` object with one integer counter per entry of
#'   [DOMAINS], all starting at zero.
#' @export
new_bus <- function() {
  start <- as.list(rep(0L, length(DOMAINS)))
  names(start) <- DOMAINS
  do.call(reactiveValues, start)
}

#' Announce a Committed Write
#'
#' Call once per logical write, *after* it has committed - never before, and
#' never per statement inside a transaction that may still roll back.
#'
#' Increments run inside [shiny::isolate()] so a bump from within a reactive
#' expression does not make that expression depend on the counter it just moved.
#'
#' @param bus A bus from [new_bus()].
#' @param ... Domain names, as character scalars.
#' @return Invisible `NULL`.
#' @export
bump <- function(bus, ...) {
  domains <- .check(c(...))
  isolate({
    for (d in domains) {
      bus[[d]] <- bus[[d]] + 1L
    }
  })
  invisible(NULL)
}

#' Announce a Write That Rewrote Everything
#'
#' For a backup restore or a database merge, where naming the touched domains
#' would just enumerate all of them.
#'
#' @param bus A bus from [new_bus()].
#' @return Invisible `NULL`.
#' @export
bump_all <- function(bus) {
  do.call(bump, c(list(bus), as.list(DOMAINS)))
}

#' Declare a Read Dependency
#'
#' Call at the top of any reactive that queries the database, naming every
#' domain whose change would alter the result.
#'
#' @param bus A bus from [new_bus()].
#' @param ... Domain names, as character scalars.
#' @return Invisible `NULL`.
#' @export
depend <- function(bus, ...) {
  for (d in .check(c(...))) {
    bus[[d]]
  }
  invisible(NULL)
}

#' Read Current Revisions
#'
#' Reactive, like [depend()], but returns the counters so a module can hold a
#' snapshot and later detect that the database moved underneath it - the "mark
#' stale, do not clobber" path taken by editors with unsaved changes.
#'
#' @param bus A bus from [new_bus()].
#' @param ... Domain names; defaults to all of [DOMAINS].
#' @return Named integer vector of revision counters.
#' @export
revision <- function(bus, ...) {
  domains <- c(...)
  if (!length(domains)) {
    domains <- DOMAINS
  }
  .check(domains)
  vapply(domains, function(d) as.integer(bus[[d]]), integer(1))
}

#' Reconcile Stored Names Against the Current Pool
#'
#' The other half of change propagation: re-reading data is not enough when
#' module state *names* rows. Given a selection and the pool it must live in,
#' reports what survived and what disappeared, so the caller can apply its own
#' policy (drop silently, drop and tell the user, or refuse to proceed).
#'
#' `NULL` selection means "no restriction" throughout the app and is preserved
#' as-is rather than being turned into an empty selection, which would mean the
#' opposite.
#'
#' @param selection Character vector of names, or `NULL`.
#' @param pool Character vector of names currently in the database.
#' @return List with `kept` (selection order preserved), `dropped`, and
#'   `changed` (whether anything was lost). For a `NULL` selection, `kept` is
#'   `NULL` and `changed` is `FALSE`.
#' @export
reconcile_names <- function(selection, pool) {
  if (is.null(selection)) {
    return(list(kept = NULL, dropped = character(0), changed = FALSE))
  }
  selection <- as.character(selection)
  pool <- as.character(pool)
  keep <- selection %in% pool
  list(
    kept = selection[keep],
    dropped = selection[!keep],
    changed = any(!keep)
  )
}
