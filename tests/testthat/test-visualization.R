box::use(
  shiny[isolate, reactive, testServer],
  testthat[expect_identical, expect_null, expect_true, test_that],
)
box::use(
  app / logic / db_events,
  app / view / visualization,
)

meta_for <- function(isolates) {
  data.frame(
    isolate = isolates,
    host = paste0("host-", isolates),
    stringsAsFactors = FALSE
  )
}

# The coordinator with one plot tab open. `db_path` stays NULL so nothing
# touches a database file; only the tab registry's lifetime is under test.
with_open_tab <- function(expr) {
  bus <- db_events$new_bus()
  testServer(
    visualization$server,
    args = list(
      db_path = reactive(NULL),
      db_rev = bus
    ),
    {
      session$flushReact()
      create_tab("AMR", "Plot 1")
      session$flushReact()
      expect_identical(length(isolate(tabs())), 1L)
      eval(expr)
    }
  )
}

test_that("a database revision bump leaves open plot tabs alone", {
  # "Reload Database" bumps every revision and leaves the panels standing. If
  # that also tore the tabs down they would become destroyed servers behind nav
  # markup still on screen: a plot tab that is blank, inert and cannot even be
  # closed. Reloading needs no teardown here — the plots belong to the same
  # database, and the revisions are what refresh their reads.
  with_open_tab(quote({
    db_events$bump_all(db_rev)
    session$flushReact()
    expect_identical(length(isolate(tabs())), 1L)
  }))
})

test_that("a surviving tab is still alive after a revision bump", {
  # Not just present in the registry: `alive` is what gates every reactive the
  # tab feeds its engine, so a tab left registered but flipped dead would look
  # exactly like the bug it is meant to rule out.
  with_open_tab(quote({
    db_events$bump_all(db_rev)
    session$flushReact()
    entry <- isolate(tabs())[[1]]
    expect_true(isolate(entry$alive()))
  }))
})

# Every call in `x` that satisfies `pred`, depth-first. Handles the expression
# object parse() returns as well as the calls nested inside it.
.calls_matching <- function(x, pred) {
  if (is.expression(x) || is.list(x)) {
    return(unlist(lapply(as.list(x), .calls_matching, pred = pred), FALSE))
  }
  if (!is.call(x)) {
    return(list())
  }
  found <- if (pred(x)) list(x) else list()
  c(found, unlist(lapply(as.list(x), .calls_matching, pred = pred), FALSE))
}

test_that("main.R hands Visualization no reset signal", {
  # "Reload Database" raises data_reset and leaves every panel standing; a
  # teardown on it left a blank, inert plot tab that could not even be closed.
  # Returning to the start screen restarts the session, so no signal is needed
  # for that either.
  #
  # Asserted against the source because the defect was in the wiring, not in
  # the module.
  hits <- .calls_matching(
    parse("../../app/main.R"),
    function(cl) identical(paste(deparse(cl[[1]]), collapse = ""), "visualization$server")
  )
  expect_identical(length(hits), 1L)
  expect_null(as.list(hits[[1]])$session_reset)
})

test_that("an open tab with no generated plot is not counted as unsaved", {
  # main.R lists unsaved plots in its close-database dialog; a tab nobody
  # pressed Generate in has nothing to lose.
  with_open_tab(quote({
    expect_identical(isolate(unsaved_plots()), 0L)
  }))
})
