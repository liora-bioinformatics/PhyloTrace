box::use(
  shiny[reactive, reactiveVal, testServer],
  testthat[
    expect_false,
    expect_gt,
    expect_identical,
    expect_s3_class,
    expect_true,
    test_that
  ],
)
box::use(
  app / logic / epi_plot,
  app / view / visualization_epi,
)

meta_fixture <- function() {
  data.frame(
    isolate = paste0("ISO-", 1:6),
    sample_collection_date = c(
      "2026-01-14",
      "2026-01-18",
      "2026-02-03",
      "junk",
      NA,
      "2026-02-04"
    ),
    organism = c(
      "E. coli",
      "E. coli",
      "S. enterica",
      "E. coli",
      "E. coli",
      "S. enterica"
    ),
    geo_loc_name_country = c(
      "Germany",
      "France",
      "Germany",
      "Germany",
      "France",
      "France"
    ),
    stringsAsFactors = FALSE
  )
}

# The controls the curve reads. Mirrors what the UI declares, so a test starts
# from the same place a freshly loaded sidebar does. "" is stratify_ui's
# sentinel for "No stratification" — see stratify_selected() in the module.
set_default_inputs <- function(session) {
  session$setInputs(
    epi_interval = "week",
    epi_plot_mode = "stacked",
    epi_col_scale = "Set2",
    epi_stratify = ""
  )
}

test_that("Generate bins the metadata only while Epi is the active engine", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      # Two ISO weeks carry the four isolates with a readable date.
      expect_true(generated())
      expect_identical(nrow(epi_data()), 2L)
    }
  )
})

test_that("a sibling engine's Generate leaves the Epi curve untouched", {
  # The Generate button is shared by every engine, so the guard in the observer
  # is the only thing stopping an MST Generate from clearing this one.
  generate <- reactiveVal(0L)
  plot_type <- reactiveVal("Epi")

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = plot_type
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      before <- epi_data()

      plot_type("MST")
      generate(2L)
      session$flushReact()

      expect_true(generated())
      expect_identical(epi_data(), before)
    }
  )
})

test_that("the interval re-bins the curve without another Generate", {
  # The interval used to be read only at Generate while the axis tick format
  # was live, so changing it moved the labels but not the bars — which read as
  # a broken control. Binning is cheap, so it tracks the picker directly.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      weekly <- nrow(epi_data())

      session$setInputs(epi_interval = "day")
      daily <- nrow(epi_data())

      # Four datable isolates fall in two ISO weeks but four separate days.
      expect_identical(weekly, 2L)
      expect_identical(daily, 4L)
    }
  )
})

test_that("stratification re-splits the curve without another Generate", {
  # epi_stratify is a plain single-select select (see stratify_ui): "" reads as
  # no stratification, any field name splits by that one field. build_epi_data
  # still accepts a vector for a composite category, but the picker no longer
  # offers a way to select more than one at once — see epi_plot's own tests for
  # composite-category coverage.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      expect_identical(unique(epi_data()$stratum), epi_plot$EPI_ALL_LABEL)

      session$setInputs(epi_stratify = "organism")
      expect_identical(sort(unique(epi_data()$stratum)), c("E. coli", "S. enterica"))

      session$setInputs(epi_stratify = "geo_loc_name_country")
      expect_identical(sort(unique(epi_data()$stratum)), c("France", "Germany"))

      session$setInputs(epi_stratify = "")
      expect_identical(unique(epi_data()$stratum), epi_plot$EPI_ALL_LABEL)
    }
  )
})

test_that("the interval default is fitted to the dataset's span", {
  # A fixed default can't serve both a short outbreak and a decade of
  # surveillance. The fixture spans three weeks, so days read fine.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      session$flushReact()

      expect_identical(fitted_interval(), "day")
    }
  )
})

test_that("a decade of data fits the interval up to months", {
  meta <- meta_fixture()
  meta$sample_collection_date <- format(
    seq(as.Date("2015-01-01"), as.Date("2024-12-31"), length.out = 6)
  )

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      session$flushReact()

      # Days would need 3653 bars across the span; months fit the budget.
      expect_identical(fitted_interval(), "month")
    }
  )
})

test_that("a database with no readable dates produces no curve", {
  generate <- reactiveVal(0L)
  meta <- meta_fixture()
  meta$sample_collection_date <- NA_character_

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_false(generated())
    }
  )
})

test_that("annotations are added, rejected without a label, and deleted", {
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(
        epi_anno_type = "milestone",
        epi_anno_start = as.Date("2026-01-20"),
        epi_anno_label = "Intervention"
      )
      session$setInputs(epi_add_anno = 1)
      expect_identical(nrow(annotations()), 1L)

      # A blank label is refused rather than adding an unlabelled marker.
      session$setInputs(epi_anno_label = "")
      session$setInputs(epi_add_anno = 2)
      expect_identical(nrow(annotations()), 1L)

      # Deletes arrive through one delegated input carrying the row's id (see
      # the single observeEvent(input$anno_delete)), not a per-row observer.
      session$setInputs(anno_delete = annotations()$id[1])
      expect_identical(nrow(annotations()), 0L)
    }
  )
})

test_that("a period annotation ending before it starts is refused", {
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(
        epi_anno_type = "period",
        epi_anno_start = as.Date("2026-03-01"),
        epi_anno_end = as.Date("2026-02-01"),
        epi_anno_label = "Backwards"
      )
      session$setInputs(epi_add_anno = 1)

      expect_identical(nrow(annotations()), 0L)
    }
  )
})

test_that("an exactly duplicated period is refused, an overlapping one is not", {
  # Two periods over the same span can't be told apart however they're drawn,
  # so the duplicate is refused. Merely overlapping spans are fine — the layout
  # gives each its own lane.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(
        epi_anno_type = "period",
        epi_anno_start = as.Date("2026-01-01"),
        epi_anno_end = as.Date("2026-03-01"),
        epi_anno_label = "First"
      )
      session$setInputs(epi_add_anno = 1)
      expect_identical(nrow(annotations()), 1L)

      # Same span, different label → still indistinguishable on the plot.
      session$setInputs(epi_anno_label = "Same span")
      session$setInputs(epi_add_anno = 2)
      expect_identical(nrow(annotations()), 1L)

      # Overlapping but not identical → allowed.
      session$setInputs(
        epi_anno_end = as.Date("2026-04-01"),
        epi_anno_label = "Overlapping"
      )
      session$setInputs(epi_add_anno = 3)
      expect_identical(nrow(annotations()), 2L)
    }
  )
})

test_that("Reset settings clears the annotation list", {
  # The annotations reactiveVal is not an input, so shinyjs::reset() cannot see
  # it — without the explicit clear in reset_epi_settings() the milestones would
  # survive a reset.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(
        epi_anno_type = "milestone",
        epi_anno_start = as.Date("2026-01-20"),
        epi_anno_label = "Intervention"
      )
      session$setInputs(epi_add_anno = 1)
      expect_identical(nrow(annotations()), 1L)

      session$setInputs(reset_settings = 1)
      expect_identical(nrow(annotations()), 0L)
    }
  )
})

test_that("Reset settings rebuilds the server-rendered stratify picker", {
  # The picker is built by renderUI, so shinyjs::reset() has no page-load value
  # to restore it from — the reset has to force a rebuild instead.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      before <- stratify_rebuild()

      session$setInputs(reset_settings = 1)

      expect_gt(stratify_rebuild(), before)
    }
  )
})

test_that("session reset tears down the generated curve", {
  generate <- reactiveVal(0L)
  session_reset <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi"),
      session_reset = session_reset
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      expect_true(generated())

      session_reset(1L)
      session$flushReact()

      expect_false(generated())
    }
  )
})

test_that("the stratify picker offers the metadata fields but not the date", {
  # Built server-side: the choices are this database's own columns, unknown
  # until the metadata exists.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      html <- as.character(output$stratify_ui$html)

      expect_true(grepl("organism", html, fixed = TRUE))
      expect_true(grepl("geo_loc_name_country", html, fixed = TRUE))
      # The plotted date and the isolate id are not stratifiers.
      expect_false(grepl("sample_collection_date", html, fixed = TRUE))
      # The "No stratification" sentinel is always offered, and is what an
      # empty selection actually means to stratify_selected().
      expect_true(grepl('value=""', html, fixed = TRUE))
    }
  )
})

test_that("the plot carries no background grid", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      grid <- ggplot2::ggplot_build(epi_ggplot())$plot$theme$panel.grid

      expect_s3_class(grid, "element_blank")
    }
  )
})

test_that("the plot builds from the generated data in every style", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(epi_stratify = "organism")
      generate(1L)
      session$flushReact()

      # "square" is a PLOT_MODES choice, not a separate switch — see mode_for()
      # /square_for(). It must fold back to the stacked curve with square cells
      # rather than reach build_epi_ggplot as a third, unknown mode.
      for (mode in c("stacked", "square", "cumulative")) {
        session$setInputs(epi_plot_mode = mode)
        expect_s3_class(epi_ggplot(), "ggplot")
      }
    }
  )
})

test_that("the 'square' mode choice folds to the stacked curve with squares on", {
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)

      session$setInputs(epi_plot_mode = "square")
      expect_identical(mode_for(), "stacked")
      expect_true(square_for())

      session$setInputs(epi_plot_mode = "stacked")
      expect_identical(mode_for(), "stacked")
      expect_false(square_for())

      session$setInputs(epi_plot_mode = "cumulative")
      expect_identical(mode_for(), "cumulative")
      expect_false(square_for())
    }
  )
})

test_that("show_cumulative_for is guarded off once Cumulative is picked", {
  # The switch itself is hidden by a conditionalPanel once Cumulative is
  # picked, but its last value survives underneath — guard it server-side too
  # rather than trust the client to have cleared it.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      expect_false(show_cumulative_for())

      session$setInputs(epi_show_cumulative = TRUE)
      expect_true(show_cumulative_for())

      session$setInputs(epi_plot_mode = "square")
      expect_true(show_cumulative_for())

      session$setInputs(epi_plot_mode = "cumulative")
      expect_false(show_cumulative_for())
    }
  )
})

test_that("the cumulative overlay switch reaches the plot only outside Cumulative", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$setInputs(epi_stratify = "organism", epi_show_cumulative = TRUE)
      generate(1L)
      session$flushReact()

      n_steps <- function() {
        sum(vapply(epi_ggplot()$layers, function(l) inherits(l$geom, "GeomStep"), logical(1)))
      }

      # Stacked: bars plus the overlay's step line.
      expect_identical(n_steps(), 1L)

      # Cumulative's own curve is already a step line; the overlay is guarded
      # off rather than doubled up on top of it.
      session$setInputs(epi_plot_mode = "cumulative")
      expect_identical(n_steps(), 1L)
    }
  )
})

test_that("the zoom-axis switch is off by default and narrows the axis when on", {
  # Off (the declared default) must mean fixed_axis = TRUE reaches the plot —
  # build_epi_ggplot treats a missing fixed_axis as fixed too, but the view
  # should not rely on that: it always sends an explicit value.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      session$setInputs(epi_step_end_prev = 1)
      x_range <- function() {
        ggplot2::ggplot_build(epi_ggplot())$layout$panel_params[[1]]$x.range
      }

      fixed <- diff(x_range())
      session$setInputs(epi_zoom_axis = TRUE)
      zoomed <- diff(x_range())

      expect_true(zoomed < fixed)
    }
  )
})

test_that("a fresh curve starts with the window untouched", {
  # 0 means "not overridden" for both handles: track the natural start/end of
  # whatever epi_bins() currently is, rather than a Date captured up front.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      expect_identical(win_start_idx(), 0L)
      expect_identical(win_end_idx(), 0L)
      bins <- epi_bins()
      expect_identical(win_start_date(), bins[1])
      expect_identical(win_end_date(), bins[length(bins)])
    }
  )
})

test_that("Play freezes the start, sweeps the end forward, and stops at the target", {
  # The window is server-side state, not the slider read back: an
  # updateSliderInput only reaches the input a client round trip later, so a
  # loop that consulted the slider would race its own update — the same
  # reasoning documented on the Map's own anim_idx, which this mirrors.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      session$setInputs(epi_play = 1)
      # Collapsed to the start and already stepping (the tick loop runs on the
      # same flush, so it has taken at least its first step by now).
      expect_true(win_end_idx() >= 1L)
      expect_true(anim_playing())

      # Pressed again: pauses without finishing the sweep.
      session$setInputs(epi_play = 2)
      expect_false(anim_playing())
    }
  )
})

test_that("the step buttons move one interval and clamp against each other", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()

      # Two bins in the fixture. Stepping the end back lands on the first.
      session$setInputs(epi_step_end_prev = 1)
      expect_identical(win_end_idx(), 1L)
      # Clamped: the end can't pass below the start.
      session$setInputs(epi_step_end_prev = 2)
      expect_identical(win_end_idx(), 1L)

      session$setInputs(epi_step_end_next = 1)
      expect_identical(win_end_idx(), 2L)

      # The start clamps symmetrically against the end.
      session$setInputs(epi_step_start_next = 1)
      expect_identical(win_start_idx(), 2L)
      session$setInputs(epi_step_start_next = 2)
      expect_identical(win_start_idx(), 2L)

      session$setInputs(epi_step_start_prev = 1)
      expect_identical(win_start_idx(), 1L)
    }
  )
})

test_that("the window reveals only the bins inside it", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      bars <- function() nrow(ggplot2::ggplot_build(epi_ggplot())$data[[1]])

      expect_identical(bars(), 2L)
      session$setInputs(epi_step_end_prev = 1)
      expect_identical(bars(), 1L)
    }
  )
})

test_that("dragging the range slider sets the window, snapped to the nearest bin", {
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      bins <- epi_bins()

      session$setInputs(epi_daterange = c(bins[1], bins[1]))
      expect_identical(win_start_date(), bins[1])
      expect_identical(win_end_date(), bins[1])
    }
  )
})

test_that("a re-bin resets the window and stops any playback", {
  # New bins mean the old window points at nothing meaningful.
  generate <- reactiveVal(0L)

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = generate,
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      generate(1L)
      session$flushReact()
      session$setInputs(epi_step_end_prev = 1)
      expect_identical(win_end_idx(), 1L)

      session$setInputs(epi_interval = "day")
      session$flushReact()

      expect_identical(win_start_idx(), 0L)
      expect_identical(win_end_idx(), 0L)
      expect_false(anim_playing())
    }
  )
})

test_that("Reset settings rebuilds the interval and date-range controls", {
  # Both are rendered by renderUI (bucket 5 in the viz_helpers.R checklist):
  # shinyjs::reset() has no page-load value for either, so the reset has to
  # force a rebuild instead of trying to restore one.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      before <- interval_rebuild()

      session$setInputs(reset_settings = 1)

      expect_gt(interval_rebuild(), before)
    }
  )
})

test_that("plot_area renders the stage with the shared overlay classes", {
  # The stage is built server-side by renderUI, not in ui(): the plot output,
  # the prompt/loading overlays and the hidden download target all mount here.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      html <- as.character(output$plot_area$html)

      # Ids carry the module's namespace, whatever testServer mocks it as.
      expect_true(grepl(session$ns("epi_plot"), html, fixed = TRUE))
      expect_true(grepl(session$ns("plot_stage"), html, fixed = TRUE))
      # The export action button clicks this hidden download target.
      expect_true(grepl(session$ns("download_epi"), html, fixed = TRUE))
      # Keeps .viz-plot-stage so the shared .viz-plot-prompt overlay and the
      # `.viz-plot-stage.is-loading .viz-loading` toggle still reach it, and
      # adds .epi-stage to undo that class's flex centering.
      expect_true(grepl("viz-plot-stage epi-stage", html, fixed = TRUE))
    }
  )
})

test_that("ui() mounts the controls and the loading overlay under the id", {
  html <- as.character(visualization_epi$ui("epi", generate_id = "viz-generate"))

  # The blanket shinyjs::reset() target.
  expect_true(grepl("epi-controls_wrap", html, fixed = TRUE))
  expect_true(grepl("epi-controls_sidebar", html, fixed = TRUE))
  # The overlay script has to hook the *parent's* Generate button id.
  expect_true(grepl("viz-generate", html, fixed = TRUE))
})
