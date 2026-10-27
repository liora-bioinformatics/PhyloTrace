# app/logic/session_restart.R
#
# Returning to the start screen replaces the Shiny session with a new one rather
# than resetting the old one, so the next database opens in a session that has
# never seen another. This module owns that hand-over and the R-process state
# that has to be cleared between the two sessions.
#
# Why a new session rather than a reset signal: the module servers live for the
# whole session, and each holds state of its own - reactiveVals, cached
# reactive values, registered DT data, observers. A reset that is only as
# complete as every module's handler is never quite complete. Worse, the
# browser keeps the last value of every output and draws it again the moment
# an element with the same id is bound (shiny.js `bindOutput()`), so a rebuilt
# panel showed the previous database's tables until the server caught up. A
# page reload discards both sides at once and leaves nothing to forget.
#
# What it cannot discard is the R process itself, which serves every session.
# Anything a module keeps at box-module level (a top-level environment, an
# option, a file in tempdir()) survives the restart; database-derived state of
# that kind is cleared in purge_process_state().

box::use(
  later[later],
  shiny[stopApp],
  shinyjs[runjs],
)
box::use(
  app / logic / mst_plot[clear_layout_cache],
)

# How long the server waits for the reloaded page to connect before concluding
# the window was closed instead and stopping the app.
RESTART_TIMEOUT_S <- 30

.state <- new.env(parent = emptyenv())
.state$active <- 0L
.state$pending <- FALSE
.state$notice <- NULL

#' Restart the Shiny Session
#'
#' Reloads the page. The new session starts on the landing page with every
#' module in its initial state; `notice`, if given, is shown there as a modal
#' (the old session's own modals and notifications go with the page).
#'
#' The colour theme survives through sessionStorage, which the restore script
#' in [theme_restore_script()] reads back before the new page paints.
#'
#' @param notice A `modalDialog()` to show in the new session, or NULL.
#' @return Called for its side effect.
#' @export
restart_session <- function(notice = NULL) {
  .state$pending <- TRUE
  .state$notice <- notice
  runjs(
    "try {
       sessionStorage.setItem(
         'phylotrace-theme',
         document.documentElement.dataset.bsTheme || 'light'
       );
     } catch (e) {}
     location.reload();"
  )
  invisible(NULL)
}

#' Register a Session
#'
#' Called once at the start of every session.
#'
#' @return The notice a restart left for this session, or NULL.
#' @export
begin_session <- function() {
  .state$active <- .state$active + 1L
  .state$pending <- FALSE
  notice <- .state$notice
  .state$notice <- NULL
  notice
}

#' Deregister a Session
#'
#' Called from `session$onSessionEnded()`. Closing the window stops the app; a
#' restart does not, but stops it after all if the reloaded page never
#' connects. Counting live sessions keeps this correct whichever order the old
#' session's end and the new session's start arrive in.
#'
#' @param stop Function stopping the app.
#' @param schedule Function with the signature of `later::later()`.
#' @return Called for its side effect.
#' @export
end_session <- function(stop = stopApp, schedule = later) {
  .state$active <- max(.state$active - 1L, 0L)
  if (.state$active > 0L) {
    return(invisible(NULL))
  }
  if (!.state$pending) {
    stop()
    return(invisible(NULL))
  }
  # Both callbacks are built here, at module level, rather than inside the
  # session: a closure from the session would keep the old session reachable
  # through its enclosing environment, and the collection would free nothing.
  schedule(purge_process_state, delay = 0)
  schedule(
    function() {
      if (.state$pending && .state$active == 0L) {
        stop()
      }
    },
    delay = RESTART_TIMEOUT_S
  )
  invisible(NULL)
}

#' Clear R-Process State Between Two Sessions
#'
#' Empties the process-level caches that hold database-derived data, then runs
#' a full garbage collection so the old session's memory goes back to the OS
#' instead of sitting in R's heap.
#'
#' Caches of static, database-independent data (species metadata, the world
#' map, the fork probe) are left alone; reloading those would only cost time.
#'
#' @return Called for its side effect.
#' @export
purge_process_state <- function() {
  clear_layout_cache()
  gc(full = TRUE)
  invisible(NULL)
}

#' Restore the Colour Theme Across a Restart
#'
#' Runs as the page loads, before the first paint, so a session restarted from
#' dark mode does not flash light. The stored value is consumed, so it carries
#' over this one reload only.
#'
#' @return The script source.
#' @export
theme_restore_script <- function() {
  "(function () {
     try {
       var theme = sessionStorage.getItem('phylotrace-theme');
       if (theme) {
         document.documentElement.dataset.bsTheme = theme;
         sessionStorage.removeItem('phylotrace-theme');
       }
     } catch (e) {}
   })();"
}
