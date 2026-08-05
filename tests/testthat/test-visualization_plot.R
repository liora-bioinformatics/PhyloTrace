box::use(
  shiny[isolate, reactive, reactiveVal, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_null,
    expect_true,
    test_that
  ],
)
box::use(
  app / view / visualization_plot,
)

impl <- attr(visualization_plot, "namespace")

meta_for <- function(isolates) {
  data.frame(
    isolate = isolates,
    host = paste0("host-", isolates),
    stringsAsFactors = FALSE
  )
}

# A tab wired to a metadata table the test controls, standing in for the
# coordinator's shared read. `plot_type = "AMR"` because it needs no distance
# computation and no database file to instantiate.
with_tab <- function(pool, expr) {
  meta <- reactiveVal(meta_for(pool))
  testServer(
    visualization_plot$server,
    args = list(
      plot_type = "AMR",
      viz_metadata = reactive(meta()),
      db_path = reactive(NULL)
    ),
    {
      pool_is <- function(isolates) meta(meta_for(isolates))
      # Settle the module before any test touches it. The Generate observers
      # carry ignoreInit, so without this their first run coincides with the
      # flush that sets `generate` and the click is swallowed as the initial
      # value - the observers fire in the app only because the tab has already
      # flushed by the time anyone can press the button.
      session$flushReact()
      eval(expr)
    }
  )
}

# --- the drawn plot is a snapshot -------------------------------------------

test_that("before the first Generate the live metadata is used", {
  # The engines fill their variable pickers as soon as a database is loaded,
  # which has to keep working before anything has been drawn.
  with_tab(c("A", "B"), quote({
    session$flushReact()
    expect_null(isolate(applied_metadata()))
    expect_identical(isolate(viz_metadata_selected()$isolate), c("A", "B"))
  }))
})

test_that("Generate pins the metadata the plot was drawn from", {
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    expect_identical(isolate(applied_metadata()$isolate), c("A", "B", "C"))
  }))
})

test_that("removing isolates does not change what the plot draws from", {
  # The regression this guards: the distance engines keep the computed topology
  # in a reactiveVal that only Generate replaces, but rebuild the drawing
  # whenever their metadata changes. A live table therefore redrew the old tree
  # against a shorter table - same branches and tips, labels silently gone.
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()

    pool_is(c("A", "B"))
    session$flushReact()

    expect_identical(isolate(viz_metadata_selected()$isolate), c("A", "B", "C"))
  }))
})

test_that("a further Generate picks the change up", {
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    pool_is(c("A", "B"))
    session$flushReact()

    session$setInputs(generate = 2)
    session$flushReact()
    expect_identical(isolate(viz_metadata_selected()$isolate), c("A", "B"))
  }))
})

# --- staleness --------------------------------------------------------------

test_that("a freshly generated plot is not stale", {
  with_tab(c("A", "B"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    expect_false(isolate(plot_stale()))
    expect_identical(isolate(plot_missing()), character(0))
  }))
})

test_that("isolates removed from the database are reported as missing", {
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    pool_is(c("A"))
    session$flushReact()

    expect_identical(sort(isolate(plot_missing())), c("B", "C"))
    expect_true(isolate(plot_stale()))
  }))
})

test_that("new isolates count as stale for a plot drawn from the whole database", {
  # No explicit selection means "all isolates", so a newly typed isolate would
  # join the plot on the next Generate.
  with_tab(c("A", "B"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    pool_is(c("A", "B", "typed_since"))
    session$flushReact()

    expect_identical(isolate(plot_new()), "typed_since")
    expect_true(isolate(plot_stale()))
  }))
})

test_that("new isolates are not stale for a plot with an explicit selection", {
  # With a selection the user made, an isolate they never picked is simply not
  # part of the plot - no reason to call it out of date.
  with_tab(c("A", "B"), quote({
    selected_isolates(c("A", "B"))
    session$setInputs(generate = 1)
    session$flushReact()
    pool_is(c("A", "B", "typed_since"))
    session$flushReact()

    expect_identical(isolate(plot_new()), character(0))
    expect_false(isolate(plot_stale()))
  }))
})

test_that("a database change alone does not arm Generate", {
  # Reported but not acted on: the plot is still a valid answer for the
  # isolates it was built from, so a redraw stays the user's call.
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    expect_false(isolate(pending_changes()))

    pool_is(c("A", "B"))
    session$flushReact()

    expect_true(isolate(plot_stale()))
    expect_false(isolate(pending_changes()))
  }))
})

test_that("confirming the isolate modal arms Generate", {
  # Even when the confirmation collapses back to the same NULL the plot was
  # generated from. Without this a plot drawn from the whole database could
  # never be rebuilt after new isolates arrived: ticking them and confirming
  # would compare equal to the applied selection and leave Generate disabled.
  with_tab(c("A", "B"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    expect_false(isolate(pending_changes()))

    pool_is(c("A", "B", "typed_since"))
    session$flushReact()
    expect_false(isolate(pending_changes()))

    selection_touched(TRUE)
    session$flushReact()
    expect_null(isolate(selected_isolates()))
    expect_null(isolate(applied_selection()))
    expect_true(isolate(pending_changes()))
  }))
})

test_that("Generate clears the confirmation flag", {
  with_tab(c("A", "B"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    selection_touched(TRUE)
    session$flushReact()
    expect_true(isolate(pending_changes()))

    session$setInputs(generate = 2)
    session$flushReact()
    expect_false(isolate(selection_touched()))
    expect_false(isolate(pending_changes()))
  }))
})

test_that("the applied selection is left alone so the difference stays visible", {
  # Only the sidebar form is reconciled. Reconciling the applied selection too
  # would erase the very difference that marks the plot out of date.
  with_tab(c("A", "B", "C"), quote({
    selected_isolates(c("A", "B", "C"))
    session$setInputs(generate = 1)
    session$flushReact()

    pool_is(c("A"))
    session$flushReact()

    expect_identical(isolate(selected_isolates()), "A")
    expect_identical(isolate(applied_selection()), c("A", "B", "C"))
    expect_true(isolate(pending_changes()))
  }))
})

test_that("a selection wiped out entirely falls back to all isolates", {
  with_tab(c("A", "B"), quote({
    selected_isolates(c("A", "B"))
    session$flushReact()
    pool_is(c("C"))
    session$flushReact()
    expect_null(isolate(selected_isolates()))
  }))
})

# --- the subsetting helper --------------------------------------------------

test_that("a NULL selection subsets to everything", {
  meta <- meta_for(c("A", "B"))
  expect_identical(impl$.subset_meta(meta, NULL)$isolate, c("A", "B"))
})

test_that("a selection keeps only its own rows", {
  meta <- meta_for(c("A", "B", "C"))
  expect_identical(impl$.subset_meta(meta, c("A", "C"))$isolate, c("A", "C"))
})

# --- export -----------------------------------------------------------------

test_that("the sidebar panel holds only the trigger and the download target", {
  # Every setting lives in the modal now. The one thing that must NOT move there
  # is the hidden downloadButton: removeModal() destroys the modal's DOM, and
  # the server clicks this target afterwards — for a widget engine, seconds
  # afterwards, once the browser has finished re-rendering.
  html <- as.character(visualization_plot$ui("t", "AMR"))
  expect_true(grepl("t-export_open", html, fixed = TRUE))
  expect_true(grepl("t-export_file", html, fixed = TRUE))
  expect_false(grepl("t-export_filetype", html, fixed = TRUE))
  expect_false(grepl("t-export_dpi", html, fixed = TRUE))
})

test_that("an unusable width falls back instead of reaching the device", {
  # ggsave would fail on a cleared numeric input, and a 500 cm canvas would
  # allocate until it did.
  with_tab(c("A", "B"), quote({
    session$setInputs(export_width = 500)
    expect_identical(isolate(export_opts()$width_cm), 60)

    session$setInputs(export_width = NA)
    expect_identical(isolate(export_opts()$width_cm), 25)

    session$setInputs(export_width = 18)
    expect_identical(isolate(export_opts()$width_cm), 18)
  }))
})

test_that("the export defaults to PNG so the button works untouched", {
  with_tab(c("A", "B"), quote({
    expect_identical(isolate(export_format()), "png")
  }))
})
