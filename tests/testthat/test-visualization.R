box::use(
  shiny[isolate, reactive, reactiveVal, testServer],
  testthat[expect_identical, expect_true, test_that],
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
  # A reactiveVal rather than the signature's default reactive(), so the test
  # can raise the signal the way app/main.R does.
  reset <- reactiveVal(0L)
  testServer(
    visualization$server,
    args = list(
      db_path = reactive(NULL),
      db_rev = bus,
      session_reset = reset
    ),
    {
      raise_reset <- function() reset(isolate(reset()) + 1L)
      session$flushReact()
      create_tab("AMR", "Plot 1")
      session$flushReact()
      expect_identical(length(isolate(tabs())), 1L)
      eval(expr)
    }
  )
}

test_that("a session reset takes every plot tab down", {
  # main.R removes the whole Visualization panel before raising this, so the
  # tab servers have to be destroyed explicitly or they outlive their markup.
  with_open_tab(quote({
    raise_reset()
    session$flushReact()
    expect_identical(length(isolate(tabs())), 0L)
  }))
})

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

test_that("main.R wires Visualization to session_reset, not data_reset", {
  # The module's teardown destroys tab servers without removing their nav
  # markup, which is only safe because input$reset removes the whole panel
  # first. data_reset is the signal that looks interchangeable and is not:
  # "Reload Database" raises it and leaves every panel standing, which left a
  # blank, inert plot tab that could not even be closed.
  #
  # Asserted against the source because the defect was in the wiring, not in
  # either module — both behave correctly for the signal they are handed.
  hits <- .calls_matching(
    parse("../../app/main.R"),
    function(cl) identical(paste(deparse(cl[[1]]), collapse = ""), "visualization$server")
  )
  expect_identical(length(hits), 1L)
  expect_identical(as.list(hits[[1]])$session_reset, as.symbol("session_reset"))
})

test_that("tab ids are never reused after a reset", {
  # tab_seq keeps counting, so a stale client-side reference from the previous
  # database can never address a new tab.
  with_open_tab(quote({
    first <- names(isolate(tabs()))[[1]]
    raise_reset()
    session$flushReact()
    create_tab("AMR", "Plot 2")
    session$flushReact()
    expect_true(!identical(names(isolate(tabs()))[[1]], first))
  }))
})
