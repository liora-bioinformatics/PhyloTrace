# app/logic/logging.R

box::use(
  app / logic / paths[app_local_share_path]
)

#' @export
logdir <- file.path(app_local_share_path, "logs")

#' @export
logfile <- file.path(logdir, "phylotrace.log")
