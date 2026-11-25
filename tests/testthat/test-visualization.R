box::use(
  shiny[isolate, reactive, testServer],
  testthat[expect_false, expect_identical, expect_null, expect_true, test_that],
  withr[local_tempdir],
)
box::use(
  app / logic / analysis_store,
  app / logic / db_events,
  app / view / visualization,
)

impl <- attr(visualization, "namespace")

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

test_that("no more than .MAX_TABS plot tabs can be open at once", {
  # The tab strip scrolls rather than wraps, and every tab keeps a live engine
  # server; create_tab() itself enforces the cap so the creator form and the
  # dashboard's reopen path cannot get past it.
  testServer(
    visualization$server,
    args = list(db_path = reactive(NULL), db_rev = db_events$new_bus()),
    {
      session$flushReact()
      for (i in seq_len(impl$.MAX_TABS)) {
        create_tab("AMR", sprintf("Plot %d", i))
      }
      session$flushReact()
      expect_true(isolate(at_capacity()))
      seq_before <- isolate(tab_seq())

      expect_null(create_tab("AMR", "One too many"))
      expect_identical(length(isolate(tabs())), impl$.MAX_TABS)
      # A refused tab mints no id, so nothing is left registered under it.
      expect_identical(isolate(tab_seq()), seq_before)

      close_tab(names(isolate(tabs()))[1])
      session$flushReact()
      expect_false(isolate(at_capacity()))
      expect_true(is.character(create_tab("AMR", "Fits again")))
    }
  )
})

# --- automatic plot names -----------------------------------------------------

test_that("a suggested plot name is the type and its lowest free number", {
  expect_identical(impl$.next_plot_name("Epi"), "Epi 1")
  expect_identical(impl$.next_plot_name("Epi", c("Epi 1", "Epi 2")), "Epi 3")
  # A number freed by closing an unsaved tab is offered again.
  expect_identical(impl$.next_plot_name("Epi", c("Epi 1", "Epi 3")), "Epi 2")
  # Other types, other numbering; a longer number is not a prefix match.
  expect_identical(impl$.next_plot_name("Epi", c("MST 1", "Epi 10")), "Epi 1")
  # Typed or saved names are matched ignoring case and surrounding space.
  expect_identical(impl$.next_plot_name("Epi", c(" epi 1 ", "EPI 2")), "Epi 3")
})

# The coordinator on a real database holding one Analysis with the given saved
# plot names, so both the open tabs and the saved plots count as taken.
with_saved_plots <- function(saved, expr) {
  dir <- local_tempdir()
  path <- file.path(dir, "db.sqlite")
  build_db(path, default_local(), metadata = meta_df(c("A", "B")))
  analysis_store$ensure_schema(path)
  aid <- analysis_store$add_analysis(path, "Analysis 1")
  for (nm in saved) {
    analysis_store$upsert_plot(path, aid, NULL, nm, "Epi", "{}", "")
  }
  testServer(
    visualization$server,
    args = list(db_path = reactive(path), db_rev = db_events$new_bus()),
    {
      session$setInputs(plot_type = "MST", plot_name = "MST 1")
      # Picks a type in the creator form, then echoes back the name the form
      # offered for it, as the browser does after updateTextInput().
      pick_type <- function(type) {
        session$setInputs(plot_type = type)
        session$setInputs(plot_name = isolate(suggested_name()))
      }
      eval(expr)
    }
  )
}

# Names of the open tabs, in tab order.
tab_names <- function(tabs) {
  unname(vapply(tabs, function(e) isolate(e$handle$name()), ""))
}

test_that("the suggestion follows the plot type and skips saved names", {
  with_saved_plots("Epi 1", quote({
    expect_identical(isolate(suggested_name()), "MST 1")
    session$setInputs(plot_type = "Epi")
    expect_identical(isolate(suggested_name()), "Epi 2")
  }))
})

test_that("Initiate names the tab after its type, not the open-tab count", {
  with_saved_plots(character(0), quote({
    pick_type("Epi")
    session$setInputs(initiate = 1L)
    pick_type("AMR")
    session$setInputs(initiate = 2L)
    pick_type("Epi")
    session$setInputs(initiate = 3L)
    expect_identical(tab_names(isolate(tabs())), c("Epi 1", "AMR 1", "Epi 2"))
  }))
})

test_that("a plot reopened from the dashboard is not given a twin name", {
  # The field still shows "Epi 1", offered before a saved plot of that name
  # was reopened in a tab. Initiate must not open a second "Epi 1" beside it.
  with_saved_plots(character(0), quote({
    pick_type("Epi")
    create_tab("Epi", "Epi 1")
    session$setInputs(initiate = 1L)
    expect_identical(tab_names(isolate(tabs())), c("Epi 1", "Epi 2"))
  }))
})

test_that("a saved plot keeps its number even with its tab closed", {
  # An unsaved tab's number is free again once closed; a saved plot's is not,
  # or a new plot could be saved into the Analysis next to a same-named one.
  with_saved_plots("Epi 1", quote({
    pick_type("Epi")
    expect_identical(isolate(input$plot_name), "Epi 2")
    session$setInputs(initiate = 1L)
    expect_identical(isolate(suggested_name()), "Epi 3")
    close_tab(names(isolate(tabs()))[1])
    session$flushReact()
    expect_identical(isolate(suggested_name()), "Epi 2")
  }))
})

test_that("a typed name is kept and the field then gets a fresh suggestion", {
  with_saved_plots(character(0), quote({
    pick_type("Epi")
    session$setInputs(plot_name = "Outbreak curve")
    # A typed name is the user's: a type switch or a new tab leaves it alone.
    expect_false(holds_suggestion("Outbreak curve"))
    session$setInputs(initiate = 1L)
    # Cleared field: the suggestion stands in.
    session$setInputs(plot_name = "  ")
    session$setInputs(initiate = 2L)
    expect_identical(tab_names(isolate(tabs())), c("Outbreak curve", "Epi 1"))
  }))
})
