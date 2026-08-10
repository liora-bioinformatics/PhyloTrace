box::use(
  shiny[isolate, reactive, reactiveVal, testServer],
  testthat[
    expect_false,
    expect_identical,
    expect_null,
    expect_setequal,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / viz_export,
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
#
# `typing` is the reactiveVal behind typing_active, so a test can start or end
# a simulated typing run with `typing_is(TRUE/FALSE)`.
with_tab <- function(pool, expr) {
  meta <- reactiveVal(meta_for(pool))
  typing <- reactiveVal(FALSE)
  testServer(
    visualization_plot$server,
    args = list(
      plot_type = "AMR",
      viz_metadata = reactive(meta()),
      db_path = reactive(NULL),
      typing_active = typing
    ),
    {
      pool_is <- function(isolates) meta(meta_for(isolates))
      typing_is <- function(x) typing(x)
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

test_that("export_opts() reads straight from the selected preset", {
  # No raw width/DPI/target-width inputs exist any more — the preset radio is
  # the only thing that decides them, read directly rather than through an
  # update*Input() round-trip (which testServer can't simulate anyway; see
  # [[shiny-testing-gotchas]] and the real-browser check this is paired with).
  with_tab(c("A", "B"), quote({
    print_preset <- viz_export$export_preset("print")
    session$setInputs(export_preset = "print")
    expect_identical(isolate(export_opts()$width_cm), print_preset$ggplot$width_cm)
    expect_identical(
      isolate(export_opts()$dpi),
      as.numeric(print_preset$ggplot$dpi)
    )

    web_preset <- viz_export$export_preset("web")
    session$setInputs(export_preset = "web")
    expect_identical(isolate(export_opts()$width_cm), web_preset$ggplot$width_cm)
  }))
})

test_that("export_opts() falls back to the first preset with nothing selected", {
  with_tab(c("A", "B"), quote({
    default <- viz_export$export_presets[[1]]
    expect_identical(isolate(export_opts()$width_cm), default$ggplot$width_cm)
  }))
})

test_that("the export defaults to PNG so the button works untouched", {
  with_tab(c("A", "B"), quote({
    expect_identical(isolate(export_format()), "png")
  }))
})

# ---------------------------------------------------------------------------
# The isolate list handed to the distance engines
# ---------------------------------------------------------------------------

test_that("engine_isolates is concrete even when the selection is 'all'", {
  # The invariant behind the leak: NULL must never reach the engines, because
  # load_allele_profile() would resolve it with its own live `mlst` query
  # rather than against what the UI is showing.
  with_tab(c("A", "B", "C"), quote({
    expect_null(isolate(selected_isolates()))
    expect_setequal(isolate(engine_isolates()), c("A", "B", "C"))
  }))
})

test_that("engine_isolates matches the frame the tip labels come from", {
  # Topology and labels agree by construction only if they are derived from
  # one object. This is that claim, checked directly.
  with_tab(c("A", "B", "C"), quote({
    session$setInputs(generate = 1)
    session$flushReact()
    expect_identical(
      isolate(engine_isolates()),
      isolate(viz_metadata_selected_all()$isolate)
    )
  }))
})

test_that("engine_isolates honours an explicit selection", {
  with_tab(c("A", "B", "C"), quote({
    selected_isolates(c("A", "C"))
    session$setInputs(generate = 1)
    session$flushReact()
    expect_setequal(isolate(engine_isolates()), c("A", "C"))
  }))
})

test_that("engine_isolates stays on the generated snapshot when the pool moves", {
  # The mid-typing-run shape: isolates appear in the database that this tab has
  # not been told about. The engines must keep computing from what the plot
  # actually shows, not from whatever `mlst` happens to hold.
  with_tab(c("A", "B"), quote({
    session$setInputs(generate = 1)
    session$flushReact()

    pool_is(c("A", "B", "MID_RUN"))
    session$flushReact()

    expect_false("MID_RUN" %in% isolate(engine_isolates()))
    expect_setequal(isolate(engine_isolates()), c("A", "B"))
  }))
})

# ---------------------------------------------------------------------------
# Generate is withheld while typing writes the database
# ---------------------------------------------------------------------------

test_that("the engines are actually handed engine_isolates, not the raw selection", {
  # Asserted against the source, because the two behavioural tests above pass
  # whether or not the resolved list is the one that reaches the engines -
  # they exercise the reactive, and reverting the hand-off leaves the reactive
  # perfectly correct and simply unused. The wiring is the defect.
  find_named_arg <- function(x, name) {
    if (is.expression(x) || is.list(x)) {
      return(unlist(lapply(as.list(x), find_named_arg, name = name), FALSE))
    }
    if (!is.call(x)) {
      return(list())
    }
    args <- as.list(x)
    hit <- if (name %in% names(args)) list(args[[name]]) else list()
    c(hit, unlist(lapply(args, find_named_arg, name = name), FALSE))
  }

  src <- parse("../../app/view/visualization_plot.R")
  passed <- find_named_arg(src, "selected_isolates")
  # One in the server signature's default, one in engine_args.
  handed_to_engine <- Filter(
    function(e) is.call(e) && identical(as.character(e[[1]]), "gate"),
    passed
  )
  expect_identical(length(handed_to_engine), 1L)
  expect_identical(
    handed_to_engine[[1]],
    quote(gate(engine_isolates))
  )
})

test_that("a typing run withholds Generate even with changes pending", {
  with_tab(c("A", "B"), quote({
    selected_isolates(c("A"))
    session$flushReact()
    expect_true(isolate(pending_changes()))

    typing_is(TRUE)
    session$flushReact()
    # The pending state itself is unchanged - it is the button that is
    # withheld, so the user's selection survives the run.
    expect_true(isolate(pending_changes()))
    expect_true(isolate(typing_active()))
  }))
})

test_that("Generate returns once the run finishes", {
  with_tab(c("A", "B"), quote({
    selected_isolates(c("A"))
    typing_is(TRUE)
    session$flushReact()

    typing_is(FALSE)
    session$flushReact()
    expect_false(isolate(typing_active()))
    expect_true(isolate(pending_changes()))
  }))
})
