# app/logic/issue_report.R
#
# Error and inquiry reports: bundles the user's message, an optional screenshot
# and the recent logs into one zip, and uploads it to the Liora report endpoint
# in the background. Every report waits in an outbox until its upload succeeds,
# so one that cannot be sent now is retried at the next start, never lost.

box::use(
  base64enc[base64decode],
  curl[
    curl_fetch_multi, form_file, handle_setform, handle_setopt, multi_run,
    new_handle, new_pool
  ],
  jsonlite[write_json],
  later[later],
  utils[tail],
  zip[zip],
)
box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / logging[log_event, logdir, session_log_file],
)

#' Report endpoint, overridable for testing via `PHYLOTRACE_REPORT_URL`.
#' @export
endpoint <- function() {
  # ponytail: placeholder URL until the receiver is deployed
  Sys.getenv(
    "PHYLOTRACE_REPORT_URL",
    "https://liora-bioinformatics.com/api/phylotrace-report"
  )
}

#' Folder of reports waiting to be uploaded.
#' @export
outbox <- function(dir = logdir) file.path(dir, "reports")

# Last `n` lines of a log with the home directory masked.
redacted_tail <- function(path, n = 2000L) {
  lines <- tail(readLines(path, warn = FALSE), n)
  gsub(path.expand("~"), "~", lines, fixed = TRUE)
}

# Active session log, console log and newest typing log - whichever exist.
report_logs <- function(dir) {
  typing <- list.files(dir, "^typing_.*\\.log$", full.names = TRUE)
  typing <- typing[which.max(file.mtime(typing))]
  logs <- unique(c(session_log_file(), file.path(dir, "last_session.log"), typing))
  logs[file.exists(logs)]
}

#' Build a Report Zip
#'
#' Never raises: a failure is logged and reported as `NULL`, so it cannot end
#' the Shiny session.
#'
#' @param kind `"error"` or `"inquiry"`.
#' @param message Error text the report was raised from (may be empty).
#' @param comment The user's description.
#' @param email Optional reply address.
#' @param screenshot `data:image/jpeg;base64,...` URL, or NULL.
#' @param include_logs Whether to attach the recent logs.
#' @param dir Log directory to collect from; the zip goes in its [outbox()].
#' @return Path of the zip in the outbox, or `NULL` if it could not be built.
#' @export
build_report <- function(kind, message = "", comment = "", email = "",
                         screenshot = NULL, include_logs = TRUE, dir = logdir) {
  stage <- tempfile("phylotrace_report_")
  on.exit(unlink(stage, recursive = TRUE))
  tryCatch(
    {
      dir.create(file.path(stage, "logs"), recursive = TRUE)

      write_json(
        list(
          kind = kind,
          message = message,
          comment = comment,
          email = email,
          app_version = APP_VERSION,
          r_version = R.version.string,
          os = paste(Sys.info()[c("sysname", "release")], collapse = " "),
          time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
        ),
        file.path(stage, "report.json"),
        auto_unbox = TRUE,
        pretty = TRUE
      )

      if (length(screenshot) == 1 && nzchar(screenshot)) {
        writeBin(
          base64decode(sub("^data:image/[a-z]+;base64,", "", screenshot)),
          file.path(stage, "screenshot.jpg")
        )
      }

      if (include_logs) {
        for (log in report_logs(dir)) {
          writeLines(redacted_tail(log), file.path(stage, "logs", basename(log)))
        }
      }

      dir.create(outbox(dir), showWarnings = FALSE, recursive = TRUE)
      out <- file.path(
        outbox(dir),
        format(Sys.time(), paste0("phylotrace_", kind, "_%Y%m%d_%H%M%S.zip"))
      )
      zip(out, list.files(stage), root = stage, recurse = TRUE)
      out
    },
    error = function(e) {
      log_event("APP", "Issue report could not be created", conditionMessage(e))
      NULL
    }
  )
}

#' Upload a Report in the Background
#'
#' Returns at once; the upload runs on the `later` event loop. Never raises and
#' never notifies the user: on success the zip leaves the outbox, on any failure
#' it stays there for [flush_outbox()] at the next start.
#'
#' @param path Zip from [build_report()].
#' @param done Called once with `TRUE` (sent) or `FALSE` (kept for retry).
#' @export
upload_report <- function(path, done = function(sent) NULL) {
  finished <- FALSE
  finish <- function(sent) {
    if (finished) return()
    finished <<- TRUE
    if (sent) unlink(path)
    log_event(
      "APP",
      if (sent) "Issue report sent" else "Issue report kept for retry",
      basename(path)
    )
    tryCatch(done(sent), error = function(e) NULL)
  }
  tryCatch(
    {
      pool <- new_pool()
      h <- new_handle()
      handle_setopt(h, connecttimeout = 10L, timeout = 60L)
      handle_setform(
        h,
        kind = sub("^phylotrace_([a-z]+)_.*", "\\1", basename(path)),
        file = form_file(path, "application/zip")
      )
      curl_fetch_multi(
        endpoint(),
        handle = h,
        pool = pool,
        done = function(res) finish(res$status_code >= 200 && res$status_code < 300),
        fail = function(msg) finish(FALSE)
      )
      # Errors inside a later() callback would reach Shiny's event loop.
      pump <- function() {
        tryCatch(
          if (multi_run(timeout = 0, pool = pool)$pending > 0) later(pump, 0.2),
          error = function(e) finish(FALSE)
        )
      }
      pump()
    },
    error = function(e) finish(FALSE)
  )
  invisible(NULL)
}

#' Retry Reports Left in the Outbox
#'
#' Uploads queued reports one after another in the background, stopping at the
#' first failure so an unreachable server is not hammered.
#'
#' @param dir Log directory whose [outbox()] to flush.
#' @export
flush_outbox <- function(dir = logdir) {
  files <- list.files(outbox(dir), "^phylotrace_.*\\.zip$", full.names = TRUE)
  step <- function(i) {
    if (i <= length(files)) {
      upload_report(files[i], function(sent) if (sent) step(i + 1))
    }
  }
  step(1)
  invisible(length(files))
}
