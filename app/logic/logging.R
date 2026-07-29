# app/logic/logging.R
#
# Central application logging sink.
# Manages session log file creation, structured event logging to file and console,
# and typing module log file generation.

box::use(
  app / logic / paths[app_local_share_path]
)

#' Directory path for application log files.
#' @export
logdir <- file.path(app_local_share_path, "logs")

if (!dir.exists(logdir)) {
  dir.create(logdir, recursive = TRUE)
}

# Environment container for active session log state
.state <- new.env(parent = emptyenv())
.state$file <- NULL

# Write formatted entry to active log file (if initialized) and console
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

#' Log Application Event
#'
#' Writes a timestamped log entry formatted as `<time> | <tag> | <event>[ | <detail>]`
#' to the active log file and system console.
#'
#' @param tag Category identifier for grouping events (e.g., "DB", "TYPING")[cite: 11].
#' @param event Short description of the event[cite: 11].
#' @param detail Optional detailed string or metadata payload[cite: 11].
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

#' Initialize Session Log File
#'
#' Opens a new timestamped log file in `logdir` for a loaded database,
#' writes header metadata, and sets it as the target sink for future events.
#'
#' @param db_path File path of the loaded SQLite database[cite: 11].
#' @return Invisible character path to the initialized log file[cite: 11].
#' @export
start_session_log <- function(db_path) {
  db <- if (length(db_path) == 1 && !is.na(db_path) && nzchar(db_path)) {
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
  log_event("DB", "Database loaded", db)
  invisible(path)
}

#' Generate Path for Typing Run Process Log
#'
#' Creates a unique timestamped file path in `logdir` to capture stdout/stderr
#' streams from typing pipeline execution.
#'
#' @return Absolute file path string for typing log output[cite: 11].
#' @export
typing_log_file <- function() {
  tempfile(
    pattern = format(Sys.time(), "typing_%Y%m%d_%H%M%S_"),
    tmpdir = logdir,
    fileext = ".log"
  )
}
