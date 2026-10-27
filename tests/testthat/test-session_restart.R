box::use(
  testthat[expect_false, expect_identical, expect_null, expect_true, test_that],
)
box::use(
  app / logic / mst_plot,
  app / logic / session_restart,
)

impl <- attr(session_restart, "namespace")
state <- impl$.state

# Puts the process-level state back to "no session yet", so each test starts
# from the state a freshly launched app is in.
fresh_state <- function() {
  state$active <- 0L
  state$pending <- FALSE
  state$notice <- NULL
}

# A stand-in for later::later() that records instead of scheduling.
recorder <- function() {
  calls <- list()
  list(
    schedule = function(fn, delay) {
      calls[[length(calls) + 1L]] <<- list(fn = fn, delay = delay)
    },
    calls = function() calls
  )
}

test_that("closing the window stops the app", {
  fresh_state()
  stopped <- FALSE
  session_restart$begin_session()
  session_restart$end_session(stop = function() stopped <<- TRUE)
  expect_true(stopped)
})

test_that("a restart does not stop the app when the old session ends first", {
  fresh_state()
  stopped <- FALSE
  rec <- recorder()
  session_restart$begin_session()
  state$pending <- TRUE

  session_restart$end_session(
    stop = function() stopped <<- TRUE,
    schedule = rec$schedule
  )
  expect_false(stopped)

  session_restart$begin_session()
  # The fallback finds the new session and leaves the app running.
  rec$calls()[[2]]$fn()
  expect_false(stopped)
})

test_that("a restart does not stop the app when the new session starts first", {
  fresh_state()
  stopped <- FALSE
  session_restart$begin_session()
  state$pending <- TRUE

  session_restart$begin_session()
  session_restart$end_session(stop = function() stopped <<- TRUE)
  expect_false(stopped)
})

test_that("a restart whose page never reconnects stops the app", {
  fresh_state()
  stopped <- FALSE
  rec <- recorder()
  session_restart$begin_session()
  state$pending <- TRUE

  session_restart$end_session(
    stop = function() stopped <<- TRUE,
    schedule = rec$schedule
  )
  fallback <- rec$calls()[[2]]
  expect_identical(fallback$delay, impl$RESTART_TIMEOUT_S)
  fallback$fn()
  expect_true(stopped)
})

test_that("a restart purges process state once the old session is gone", {
  fresh_state()
  rec <- recorder()
  session_restart$begin_session()
  state$pending <- TRUE
  session_restart$end_session(stop = function() NULL, schedule = rec$schedule)
  expect_identical(rec$calls()[[1]]$fn, session_restart$purge_process_state)
})

test_that("the notice left by a restart reaches the next session only", {
  fresh_state()
  state$notice <- "notice"
  expect_identical(session_restart$begin_session(), "notice")
  expect_null(session_restart$begin_session())
})

test_that("purging empties the MST layout cache", {
  cache <- attr(mst_plot, "namespace")$.layout_cache
  assign("key", 1, envir = cache)
  cache$.order <- "key"
  session_restart$purge_process_state()
  expect_identical(length(ls(cache, all.names = TRUE)), 0L)
})
