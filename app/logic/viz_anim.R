# app/logic/viz_anim.R
#
# Server side of the time animations the Epi curve and the Map play: a run's
# frames are prepared once, when Play is pressed, and app/js/viz-anim.js plays
# them on the browser's own clock, so no frame waits on a server round trip.

box::use(
  parallel[detectCores, mclapply],
  shiny[httpResponse, parseQueryString],
)

#' Most worker processes a frame render is spread over.
#'
#' Each frame is a whole ggplot draw, so a few workers already cut a long run's
#' preparation to a fraction; more mostly contend for the same memory.
ANIM_WORKERS <- 4L

#' Hand a run's images to the browser.
#'
#' The PNGs are read into memory, their files removed, and served from one
#' session data object: they live no longer than the session, and the module's
#' next run, registered under the same name, replaces them.
#'
#' @param session Shiny session.
#' @param name Character. Data object name, unique per module instance.
#' @param files Character vector of PNG paths, in frame order.
#' @return Character vector, one URL per image.
#' @export
serve_images <- function(session, name, files) {
  images <- lapply(files, function(file) {
    on.exit(unlink(file))
    readBin(file, "raw", file.info(file)$size)
  })
  url <- session$registerDataObj(name, images, function(data, req) {
    i <- suppressWarnings(as.integer(parseQueryString(req$QUERY_STRING)$frame))
    if (length(i) != 1L || is.na(i) || i < 1L || i > length(data)) {
      return(httpResponse(404L, "text/plain", "Not found"))
    }
    httpResponse(200L, "image/png", data[[i]])
  })
  paste0(url, "&frame=", seq_along(images))
}

#' Run `fun` over `items`, spread over forked workers where the platform can.
#'
#' Forking copies the session's process, so a worker sees every object `fun`
#' needs without anything being shipped to it, and it only draws and writes a
#' file — no database, no Shiny. Windows cannot fork and runs the items in turn.
#' The items go in chunks so `progress` can be told between them.
#'
#' @param items List or vector to map over.
#' @param fun Function of one item.
#' @param progress Function of the fraction done (0 to 1), or NULL.
#' @return List of `fun`'s results, in `items`' order.
#' @export
render_frames <- function(items, fun, progress = NULL) {
  cores <- if (identical(.Platform$OS.type, "unix")) detectCores() else NA
  workers <- if (is.na(cores)) 1L else max(1L, min(ANIM_WORKERS, cores - 1L))
  chunks <- split(seq_along(items), ceiling(seq_along(items) / (workers * 2L)))
  out <- vector("list", length(items))
  for (chunk in chunks) {
    res <- if (workers > 1L) {
      # A worker's warnings and messages have nowhere to go: handlers set up in
      # the parent (Shiny's, testthat's) would act on state the fork only has a
      # copy of. They are muffled where they arise instead.
      mclapply(
        items[chunk],
        function(item) {
          withCallingHandlers(
            fun(item),
            warning = function(w) invokeRestart("muffleWarning"),
            message = function(m) invokeRestart("muffleMessage")
          )
        },
        mc.cores = workers,
        mc.preschedule = FALSE
      )
    } else {
      lapply(items[chunk], fun)
    }
    # A worker that errored returns a try-error; one that died returns NULL.
    failed <- vapply(res, function(r) is.null(r) || inherits(r, "try-error"), logical(1))
    if (any(failed)) {
      bad <- res[[which(failed)[1]]]
      stop(
        if (is.null(bad)) "A frame worker failed." else conditionMessage(attr(bad, "condition")),
        call. = FALSE
      )
    }
    out[chunk] <- res
    if (is.function(progress)) {
      progress(max(chunk) / length(items))
    }
  }
  out
}
