# app/logic/logging.R
#
# Central logging sink for the app.
#
# One log file is opened per database load (see start_session_log): its first
# lines name the loaded database, so individual entries never repeat it. Every
# log line is appended to that file (the primary sink) and echoed to the console
# in parallel. Log lines share one shape:
#
#   <time> | <tag> | <event>[ | <detail>]
#
# where `tag` groups related lines for grepping ("DB", "TYPING", ...). The DB
# transaction lines are emitted per logical operation by the mutating functions
# themselves (store_*, delete_*, import/export, ...), not per SQL statement.

box::use(
  app / logic / paths[app_local_share_path]
)

#' @export
logdir <- file.path(app_local_share_path, "logs")

# app_local_share_path itself is created in paths.R, but its logs/ subdir is
# not - ensure it exists so log writers never fail on a missing directory.
if (!dir.exists(logdir)) {
  dir.create(logdir, recursive = TRUE)
}

# Mutable holder for the active session log file. A database load points this at
# a fresh file; every line written afterwards is appended there. Kept inside an
# environment because box locks the module's own top-level bindings, but not the
# contents of an environment it holds - so this stays reassignable at runtime.
.state <- new.env(parent = emptyenv())
.state$file <- NULL

# Append one already-formatted line to the active session log file (if a
# database is loaded) and echo it to the console. The file is the primary sink,
# written best-effort so a filesystem hiccup never breaks the app; the console
# echo always runs, in parallel.
write_line <- function(line) {
  f <- .state$file
  if (!is.null(f)) {
    tryCatch(
      cat(line, "\n", sep = "", file = f, append = TRUE),
      error = function(e) NULL
    )
  }
  message(line)
}

#' Log one app event to the active session log file and the console.
#'
#' Single-line, timestamped shape shared across the app:
#' `<time> | <tag> | <event>[ | <detail>]`. `tag` groups related lines for
#' grepping (e.g. "DB", "TYPING"); `detail` (optional) carries the specifics.
#' @export
log_event <- function(tag, event, detail = NULL) {
  write_line(paste0(
    format(Sys.time(), digits = 3L),
    " | ",
    tag,
    " | ",
    event,
    if (!is.null(detail)) paste0(" | ", detail)
  ))
}

#' Start a fresh session log for a newly loaded database.
#'
#' Called from main.R's load_database handler. Opens a new timestamped file in
#' logdir, writes a short header naming the database (so later lines need not
#' repeat it) and the open time, makes it the active sink for every subsequent
#' log line, and records the load itself as the first entry.
#'
#' @param db_path Path of the database being loaded (written into the header).
#' @return The new log file path (invisibly).
#' @export
start_session_log <- function(db_path) {
  db <- if (
    length(db_path) == 1 && !is.na(db_path) && nzchar(db_path)
  ) {
    db_path
  } else {
    "(none)"
  }
  path <- file.path(logdir, format(Sys.time(), "session_%Y%m%d_%H%M%S.log"))
  header <- c(
    "# PhyloTrace session log",
    paste("# database :", db),
    paste("# opened   :", format(Sys.time(), digits = 3L)),
    strrep("-", 72)
  )
  tryCatch(writeLines(header, path), error = function(e) NULL)
  .state$file <- path
  # First real entry, landing in the new file and on the console.
  log_event("DB", "Database loaded", db)
  invisible(path)
}

#' Path for one typing run's process log (separate from the session log).
#'
#' loop-pymlst.sh's combined stdout/stderr (the raw output of every external
#' command a run drives - wgMLST/BLAT, claMLST, abritamr/AMRFinderPlus) is
#' streamed here by the Typing module, which also tails it for live progress.
#' Each run gets its own timestamped file - so the live-tail parser only ever
#' sees the current run, and successive runs never clobber each other - and,
#' unlike the tempfile it replaces, it persists after the run for inspection.
#'
#' @return Absolute path to a fresh, unique log file inside `logdir`.
#' @export
typing_log_file <- function() {
  tempfile(
    pattern = format(Sys.time(), "typing_%Y%m%d_%H%M%S_"),
    tmpdir = logdir,
    fileext = ".log"
  )
}
