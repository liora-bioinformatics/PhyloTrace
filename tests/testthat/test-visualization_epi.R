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
  app / logic / field_profile,
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
# from the same place a freshly loaded sidebar does.
set_default_inputs <- function(session) {
  session$setInputs(
    epi_interval = "week",
    epi_plot_mode = "stacked"
  )
}

# Map a variable onto the bar fill, the way picking one in the Mapping tab
# does. The palette and any date grouping are the engine's own choices from
# here, exactly as they are for a real pick.
map_variable <- function(session, field) {
  session$setInputs(epi_layer_add = field)
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

test_that("a mapping re-splits the curve without another Generate", {
  # The bar fill is the medium's only channel and it does not stack, so one
  # variable at a time. build_epi_data still accepts a vector for a composite
  # category — see epi_plot's own tests for that coverage — but the curve no
  # longer offers a way to ask for one.
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

      map_variable(session, "organism")
      expect_identical(sort(unique(epi_data()$stratum)), c("E. coli", "S. enterica"))

      # A second variable is refused rather than composited: one channel.
      map_variable(session, "geo_loc_name_country")
      expect_identical(length(epi_layers()), 1L)
      expect_identical(epi_layers()[[1]]$field, "organism")

      # Removing the mapping puts the single series back.
      session$setInputs(epi_layer_delete = epi_layers()[[1]]$id)
      expect_identical(unique(epi_data()$stratum), epi_plot$EPI_ALL_LABEL)

      map_variable(session, "geo_loc_name_country")
      expect_identical(sort(unique(epi_data()$stratum)), c("France", "Germany"))
    }
  )
})

test_that("a mapped variable lands on the bar fill with a palette of its own", {
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      map_variable(session, "organism")

      l <- epi_layers()[[1]]
      expect_identical(l$field, "organism")
      expect_identical(l$aesthetic, "bar_fill")
      expect_identical(l$n_levels, 2L)
      # The engine picks the palette off the variable's own profile; a curve
      # with no mapping falls back to the module's default.
      expect_true(nzchar(l$palette %||% ""))
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

test_that("Reset settings clears the mapping", {
  # The layer list is reactiveVal state, not an input, so shinyjs::reset()
  # cannot reach it — the reset has to clear it itself.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      map_variable(session, "organism")
      expect_identical(length(epi_layers()), 1L)

      session$setInputs(reset_settings = 1)

      expect_identical(length(epi_layers()), 0L)
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

test_that("the mapping picker offers the metadata fields but not the plotted date", {
  # The choices are this database's own columns, unknown until the metadata
  # exists, so they are pushed at the picker rather than declared with it.
  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta_fixture()),
      generate = reactiveVal(0L),
      plot_type = reactiveVal("Epi")
    ),
    {
      set_default_inputs(session)
      session$flushReact()

      expect_identical(
        sort(mappable_fields()),
        c("geo_loc_name_country", "organism")
      )
      # Splitting the curve by its own x axis draws one series per bar, and an
      # isolate id names every isolate uniquely.
      expect_false("sample_collection_date" %in% mappable_fields())
      expect_false("isolate" %in% mappable_fields())
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

test_that("export aspect matches square_ratio() in square mode", {
  # The exported file has to be a genuine square-cell layout whenever the
  # on-screen preview is one — using the plain aspect slider instead (the bug
  # this replaces) drew an exported file whose cells were not squares at all.
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
      session$setInputs(epi_aspect_ratio = 0.9)
      generate(1L)
      session$flushReact()

      session$setInputs(epi_plot_mode = "square")
      session$flushReact()
      expect_true(square_for())
      ratio <- square_ratio()
      expect_true(!is.null(ratio))
      expect_identical(export$aspect(), ratio)
      # Not merely non-NULL — genuinely not the slider's value, or this would
      # pass by coincidence whenever the two happened to be close.
      expect_false(isTRUE(all.equal(export$aspect(), 0.9)))

      # Every other mode still uses the plain slider, exactly as before.
      session$setInputs(epi_plot_mode = "stacked")
      session$flushReact()
      expect_identical(export$aspect(), 0.9)
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
      map_variable(session, "organism")
      session$setInputs(epi_show_cumulative = TRUE)
      generate(1L)
      session$flushReact()

      n_steps <- function() {
        sum(vapply(epi_ggplot()$layers, function(l) inherits(l$geom, "GeomStep"), logical(1)))
      }

      # Stacked: the overlay's halo pass plus its visible line (see the comment
      # in build_epi_ggplot on why the line needs the halo to stay legible over
      # the bars). Two strata means a legend is shown, but its invisible seed
      # layer draws as GeomCol in stacked mode, so it adds nothing here.
      expect_identical(n_steps(), 2L)

      # Cumulative's own curve is already a step line, so the overlay is
      # guarded off rather than doubled up on top of it — if it leaked through,
      # this would be 4 (curve + seed + the overlay's own halo + line) instead
      # of 2. The legend seed layer draws as GeomStep here (matching the curve
      # style) rather than GeomCol, so it still counts for one.
      session$setInputs(epi_plot_mode = "cumulative")
      expect_identical(n_steps(), 2L)
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

test_that("dragging the range slider sets the window, snapped to the bin it lands in", {
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

      # The right handle rests on its bin's LAST day, not the bin's name.
      # Nearest-bin-start snapping rounded that into the following bin; the
      # bin a handle lands *inside* is the one it means.
      session$setInputs(
        epi_daterange = c(bins[1], epi_plot$bin_end_date(bins[1], "week"))
      )
      expect_identical(win_end_date(), bins[1])
    }
  )
})

test_that("the range slider is labelled in collected dates, not bin names", {
  # A bin is named by its first day, so the last bin's name trails the last
  # collection date — the slider read "up to 2020-12-01" for a database
  # collected through 2020-12-12. The handles carry the span the bins cover,
  # clipped to what was actually collected.
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

      # Full span: the first and last dates in the metadata, not 2026-01-12
      # (the opening bin's name) through 2026-02-02 (the closing bin's).
      expect_identical(
        slider_bounds(1L, length(bins)),
        as.Date(c("2026-01-14", "2026-02-04"))
      )
      # A window closing on an interior bin runs to that bin's own last day.
      expect_identical(
        slider_bounds(1L, 1L),
        as.Date(c("2026-01-14", "2026-01-18"))
      )
      # The window itself still travels as bin starts — that is what the plot
      # compares its date_bin column against.
      expect_identical(win_start_date(), bins[1])
      expect_identical(win_end_date(), bins[length(bins)])
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

test_that("the annotation types are named for what they mark, not how", {
  html <- as.character(visualization_epi$ui("epi", generate_id = "viz-generate"))

  expect_true(grepl(">Timestamp<", html, fixed = TRUE))
  expect_true(grepl(">Time period<", html, fixed = TRUE))
  expect_false(grepl("Milestone", html, fixed = TRUE))
  expect_false(grepl("(shaded)", html, fixed = TRUE))
  # The stored type keys are untouched — a saved plot still restores.
  expect_true(grepl("\"milestone\"", html, fixed = TRUE))
})

# --- restore(): the saved mapping --------------------------------------------
# A mapping is reactiveVal state, so restoring one is a plain write rather than
# the renderUI dance the old stratify picker needed (an update*Input() could not
# survive: restoring a plot writes the tab's selection reactiveVals, which
# re-rendered the control in the same flush and replaced whatever had been
# pushed at it). These assert on the layer list, which is what the curve is
# actually drawn from.
#
# They also cover the other half: a plot saved before this rewrite carries no
# `.layers` at all, only the flat epi_stratify / epi_stratify_granularity /
# epi_col_scale keys, and has to come back as a layer anyway.

stratify_meta_fixture <- function() {
  meta <- meta_fixture()
  meta$enrollment_date <- c(
    "2026-01-05",
    "2026-01-06",
    "2026-01-20",
    "2026-01-21",
    "2026-02-01",
    "2026-02-02"
  )
  meta
}

stratify_profiles_fixture <- function(meta) {
  field_profile$field_profiles(
    meta,
    types = c(
      isolate = "text",
      sample_collection_date = "date",
      organism = "category",
      geo_loc_name_country = "category",
      enrollment_date = "date"
    )
  )
}

test_that("restore() brings a saved mapping back", {
  meta <- stratify_meta_fixture()

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(stratify_profiles_fixture(meta))
    ),
    {
      set_default_inputs(session)
      restore(list(.layers = list(list(
        id = "L1",
        field = "geo_loc_name_country",
        title = "Country",
        aesthetic = "bar_fill",
        palette = "Dark2",
        n_levels = 2L,
        auto = FALSE
      ))))
      session$flushReact()

      expect_identical(length(epi_layers()), 1L)
      expect_identical(epi_layers()[[1]]$field, "geo_loc_name_country")
      expect_identical(epi_layers()[[1]]$palette, "Dark2")
    }
  )
})

test_that("restore() drops a saved layer whose column the database no longer has", {
  meta <- stratify_meta_fixture()

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(stratify_profiles_fixture(meta))
    ),
    {
      set_default_inputs(session)
      # A layer needs a field to be drawn from; one that is gone cannot be
      # rebuilt, and a card claiming otherwise is worse than none.
      restore(list(.layers = list(list(
        id = "L1",
        field = NA_character_,
        aesthetic = "bar_fill"
      ))))
      session$flushReact()

      expect_identical(length(epi_layers()), 0L)
    }
  )
})

test_that("a restore carrying no mapping leaves the curve unsplit", {
  meta <- stratify_meta_fixture()

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(stratify_profiles_fixture(meta))
    ),
    {
      set_default_inputs(session)
      restore(list(epi_plot_mode = "stacked"))
      session$flushReact()

      expect_identical(length(epi_layers()), 0L)
    }
  )
})

test_that("a plot saved before the rewrite comes back as a mapping layer", {
  # The flat keys every saved curve carried. Without the migration each one
  # silently reopened unsplit, having lost the stratification it was saved with.
  meta <- stratify_meta_fixture()

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(stratify_profiles_fixture(meta))
    ),
    {
      set_default_inputs(session)
      restore(list(
        epi_stratify = "enrollment_date",
        epi_stratify_granularity = "month",
        epi_col_scale = "Set1"
      ))
      session$flushReact()

      expect_identical(length(epi_layers()), 1L)
      l <- epi_layers()[[1]]
      expect_identical(l$field, "enrollment_date")
      expect_identical(l$aesthetic, "bar_fill")
      # The saved grouping and palette are the user's own choices, so they are
      # pinned against rebalance_layers() rebuilding the layer from scratch.
      expect_identical(l$granularity, "month")
      expect_identical(l$palette, "Set1")
      expect_false(l$auto)
      # And the grouping reaches the builder: two months, not six dates.
      expect_identical(
        sort(unique(stratified_meta(meta)$enrollment_date)),
        c("2026-01", "2026-02")
      )
    }
  )
})

test_that("a restore with no saved stratifier at all adds nothing", {
  meta <- stratify_meta_fixture()

  testServer(
    visualization_epi$server,
    args = list(
      viz_metadata = reactive(meta),
      field_profiles = reactive(stratify_profiles_fixture(meta))
    ),
    {
      set_default_inputs(session)
      restore(list(epi_stratify = "", epi_col_scale = "Set1"))
      session$flushReact()

      expect_identical(length(epi_layers()), 0L)
    }
  )
})
