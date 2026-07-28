# app/view/visualization_epi.R
#
# Epidemiological curve (ggplot2) visualization submodule. Owns its own control
# panel, the plot render, and all Epi-specific reactive state. Mounted by
# app/view/visualization.R inside a navset_hidden panel; the shared Generate
# button, plot type and per-isolate metadata are forwarded in as reactives.
#
# Unlike the MST/Tree engines this one needs no pairwise distances — it bins the
# metadata's collection dates straight into counts — so, like the Map, it takes
# local isolates only and ignores na_handling. Binning is cheap (a group_by over
# the metadata), so unlike those engines nothing here is deferred to Generate
# beyond the first press: once a curve exists, every control re-bins live. See
# epi_data() for why that matters.
#
# The plot is a ggplot rendered by renderImage (through ggsave, so the whole
# frame takes the theme background — see the epi_plot output), like the Tree but
# for that one detail. Playback
# is this module's own tick loop (see anim_playing), which re-renders the curve
# one interval further along on each tick — the pattern the Map's "Play" already
# uses. gganimate would have wanted a GIF renderer (gifski/magick/av), none of
# which install here without system packages, and a server-rendered GIF would be
# far slower than simply redrawing a ggplot.

box::use(
  bslib[
    accordion,
    accordion_panel,
    as_fill_carrier,
    card,
    card_body,
    input_switch,
    layout_sidebar,
    nav_panel,
    navset_tab,
    sidebar,
  ],
  shiny,
  shinyWidgets[radioGroupButtons, pickerInput, pickerOptions],
)
box::use(
  app / logic / epi_plot,
  app / logic / field_labels[field_label, grouped_field_choices],
  app / logic / functions[render_info],
  app /
    logic /
    viz_helpers[
      reset_viz_colors,
      scale_select,
      viz_color,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Only a fallback for before a dataset is known: the real default is fitted to
# the loaded data's date span (see fitted_interval).
INTERVAL_DEFAULT <- "week"

# "square" is not a distinct curve — it's the stacked curve with square-cell
# bars instead of solid ones (see build_epi_ggplot's mode/square split in
# epi_plot.R). It's offered here as its own choice anyway rather than a
# separate switch, because a switch that only means something for one of three
# modes needs a conditionalPanel to hide it for the other two, and that reads
# as more state than the choice actually has: at any moment there is exactly
# one plot style in effect, and one select says so directly. mode_for()/
# square_for() below translate the UI's three-way choice back to the logic
# layer's two-way one.
PLOT_MODES <- c(
  Stacked = "stacked",
  `Square blocks` = "square",
  Cumulative = "cumulative"
)
PLOT_MODE_DEFAULT <- "stacked"

# How long one playback step is held, in milliseconds.
STEP_MS <- 450

ASPECT_DEFAULT <- 0.55

# Roughly the pixels the axes, labels and legend need — the panel gets the rest.
# Only used to size square mode's plot, where the panel's own shape is fixed by
# the data and everything else has to be budgeted around it.
PANEL_CHROME_PX <- 200

COL_SCALE_DEFAULT <- "Set2"
# The single-series colour used when the curve is not stratified. Matches Set2's
# first swatch so switching stratification on and off keeps the same base hue.
SINGLE_COLOR_DEFAULT <- "#66C2A5"
BACKGROUND_DEFAULT <- "#ffffff"
TEXT_COLOR_DEFAULT <- "#000000"
# The running-total overlay's line colour. Defaults to black, which is what it
# was drawn in (text_color) before it became its own control.
CUMULATIVE_COLOR_DEFAULT <- "#000000"
# The moving-average trend line's colour. A strong red so the smoothed trend
# reads clearly against the bars it is drawn over, and stays distinct from the
# (black by default) cumulative line when both overlays are shown at once.
MOVING_AVG_COLOR_DEFAULT <- "#c0392b"
# How each moving-average point is placed over its window: "center" (no phase
# lag — the smoothed peak sits over the bars' peak) or "trailing" (the
# surveillance convention, "the last N intervals").
MOVING_AVG_ALIGNMENTS <- c(Centered = "center", Trailing = "trailing")
MOVING_AVG_ALIGN_DEFAULT <- "center"
# Widest smoothing window the slider offers, in intervals. Its default lives in
# the logic layer (epi_plot$EPI_MOVING_AVG_WINDOW_DEFAULT) so the plot and the
# control agree on it.
MOVING_AVG_WINDOW_MIN <- 2L
MOVING_AVG_WINDOW_MAX <- 30L
# The colour a freshly added annotation takes; the orange the period wash used
# before annotations became individually colourable. Kept in step with
# epi_plot$EPI_ANNO_COLOR_DEFAULT, the fallback the plot builder applies.
ANNO_COLOR_DEFAULT <- "#f39c12"

ANNO_PERIOD <- "period"

# --- Epi control tabs --------------------------------------------------------

epi_controls <- function(ns) {
  shiny$tagList(
    navset_tab(
      # Layout -----------------------------------------------------------------
      nav_panel(
        "Layout",
        icon = shiny$icon("sliders"),
        accordion(
          open = "Plot Mode",
          # Plot Mode ----------------------------------------------------------
          accordion_panel(
            "Plot Mode",
            icon = shiny$icon("chart-column"),
            shiny$pickerInput(
              ns("epi_plot_mode"),
              "Plot mode",
              choices = PLOT_MODES,
              selected = PLOT_MODE_DEFAULT
            ),
            # Square blocks/Stacked only: overlays the running total on a
            # secondary axis. Cumulative already *is* that total, drawn as the
            # main curve, so the overlay would just repeat it.
            shiny$conditionalPanel(
              condition = "input.epi_plot_mode != 'cumulative'",
              ns = ns,
              input_switch(
                ns("epi_show_cumulative"),
                "Show cumulative curve",
                FALSE
              )
            ),
            # Square blocks/Stacked only: a smoothed trend line over the bars.
            # Cumulative is a running total — already monotone, with no
            # per-interval counts to smooth — so the option is hidden there,
            # exactly like the cumulative overlay above. The window (in
            # intervals) and its alignment only appear once the line itself is
            # switched on.
            shiny$conditionalPanel(
              condition = "input.epi_plot_mode != 'cumulative'",
              ns = ns,
              input_switch(
                ns("epi_show_moving_avg"),
                "Show moving average",
                FALSE
              ),
              shiny$conditionalPanel(
                condition = "input.epi_show_moving_avg",
                ns = ns,
                shiny$sliderInput(
                  ns("epi_moving_avg_window"),
                  # Refined server-side to name the span in the current
                  # interval's unit ("… 7 weeks") — see the updateSliderInput
                  # observer.
                  "Moving average window",
                  min = MOVING_AVG_WINDOW_MIN,
                  max = MOVING_AVG_WINDOW_MAX,
                  value = epi_plot$EPI_MOVING_AVG_WINDOW_DEFAULT,
                  step = 1,
                  ticks = FALSE
                ),
                shiny$pickerInput(
                  ns("epi_moving_avg_align"),
                  "Window alignment",
                  choices = MOVING_AVG_ALIGNMENTS,
                  selected = MOVING_AVG_ALIGN_DEFAULT
                )
              )
            ),
            # Cumulative only: names each line where it currently ends instead
            # of relying on the legend to say which colour is which.
            shiny$conditionalPanel(
              condition = "input.epi_plot_mode == 'cumulative'",
              ns = ns,
              input_switch(ns("epi_label_ends"), "Label lines", TRUE)
            )
          ),
          # Stratification -----------------------------------------------------
          accordion_panel(
            "Stratification",
            icon = shiny$icon("layer-group"),
            # Rendered server-side: the choices are this database's metadata
            # columns, so the picker is built once they are known rather than
            # being declared empty here and back-filled (an empty <select>
            # initialises bootstrap-select in its disabled state). Same pattern
            # as the staged peers picker in visualization.R.
            shiny$uiOutput(ns("stratify_ui"))
          ),
          # Axes & Sizing ------------------------------------------------------
          accordion_panel(
            "Axes & Sizing",
            icon = shiny$icon("ruler-combined"),
            # Off hides the x-axis title ("Date of collection") — the axis is
            # self-evidently dates, so some users would rather reclaim the room.
            input_switch(ns("epi_show_x_label"), "Show x-axis label", TRUE),
            # Square blocks mode fixes the panel's shape to the data grid via
            # coord_fixed (see square_ratio / epi_plot's square split), so the
            # aspect ratio has no effect there. The wrapper carries the id the
            # server toggles: disabled and given an explanatory hover title while
            # square mode is active.
            shiny$div(
              id = ns("epi_aspect_ratio_wrap"),
              shiny$sliderInput(
                ns("epi_aspect_ratio"),
                "Aspect ratio",
                min = 0.3,
                max = 1,
                value = ASPECT_DEFAULT,
                step = 0.05,
                ticks = FALSE
              )
            )
          )
        )
      ),
      # Time — the same date-range slider, Interval buttons and Play/step
      # controls as the Map's own Time tab (visualization_map.R), adapted to
      # what an epi curve actually has: no Hour granularity (collection dates
      # carry no time of day) and no separate map-mode-specific tabs.
      nav_panel(
        "Time",
        icon = shiny$icon("clock"),
        shiny$uiOutput(ns("daterange_ui")),
        # Rendered server-side so it is *created* carrying the interval fitted
        # to this dataset. Declaring it here with a static default and updating
        # it later loses the update: this panel is only inserted into the DOM
        # once a database has loaded, and the fit is known the moment that
        # happens — so an update fires at a control the browser has not built
        # yet, and the message goes nowhere.
        shiny$uiOutput(ns("interval_ui")),
        # Off (default): the axis stays fixed to the dataset's full span, so
        # dragging the handles or pressing Play windows the curve inside a
        # stable frame. On: the axis zooms to match whatever the handles
        # currently select — see build_epi_ggplot's fixed_axis opt.
        input_switch(ns("epi_zoom_axis"), "Zoom axis to selection", FALSE),
        shiny$div(
          class = "time-step-buttons",
          shiny$div(
            class = "btn-group time-step-group",
            shiny$actionButton(
              ns("epi_step_start_prev"),
              NULL,
              icon = shiny$icon("angle-left"),
              class = "btn-step",
              title = "Move start date back one interval"
            ),
            shiny$actionButton(
              ns("epi_step_start_next"),
              NULL,
              icon = shiny$icon("angle-right"),
              class = "btn-step",
              title = "Move start date forward one interval"
            )
          ),
          shiny$actionButton(
            ns("epi_play"),
            "Play",
            icon = shiny$icon("play"),
            class = "btn-play"
          ),
          shiny$div(
            class = "btn-group time-step-group",
            shiny$actionButton(
              ns("epi_step_end_prev"),
              NULL,
              icon = shiny$icon("angle-left"),
              class = "btn-step",
              title = "Move end date back one interval"
            ),
            shiny$actionButton(
              ns("epi_step_end_next"),
              NULL,
              icon = shiny$icon("angle-right"),
              class = "btn-step",
              title = "Move end date forward one interval"
            )
          )
        ),
        shiny$helpText(
          class = "epi-help",
          "Drag either handle to window the curve to that span, or press Play",
          "to sweep forward from the start handle one interval at a time."
        )
      ),
      # Colors -------------------------------------------------------------
      nav_panel(
        "Colors",
        icon = shiny$icon("palette"),
        # A colour scale only means anything once the curve is split into
        # strata to tell apart; with a single series there is just one bar
        # colour to set. So the scale select shows only while stratified, and a
        # plain colour picker takes its place otherwise. The empty string is the
        # "no stratification" sentinel (see stratify_selected); an undefined
        # epi_stratify — before stratify_ui has rendered — counts as unstratified
        # too, which `!= null` (JS ==/!= treat undefined and null alike) covers.
        shiny$conditionalPanel(
          condition = "input.epi_stratify != '' && input.epi_stratify != null",
          ns = ns,
          # Placeholder choices: which scales are actually offered depends on how
          # many strata there are, and is swapped in by apply_scale_choices().
          scale_select(ns, "epi_col_scale", categories = "Qualitative")
        ),
        # The single-series colour lives in the same grid as Text and Background
        # so all three colour rows share one gap; only its visibility is
        # conditional (shown when not stratified). Keeping it in a separate grid
        # would space it differently from the rows below it.
        shiny$div(
          class = "viz-color-grid",
          shiny$conditionalPanel(
            condition = "input.epi_stratify == '' || input.epi_stratify == null",
            ns = ns,
            viz_color(ns, "epi_single_color", "Series", SINGLE_COLOR_DEFAULT)
          ),
          viz_color(ns, "epi_text_color", "Text", TEXT_COLOR_DEFAULT),
          viz_color(
            ns,
            "epi_background_color",
            "Background",
            BACKGROUND_DEFAULT
          ),
          # The running-total overlay only exists in Stacked/Square blocks mode
          # (Cumulative already *is* that total, so there is no separate line to
          # colour) *and* only when the "Show cumulative curve" switch is on — so
          # its picker mirrors both conditions the overlay itself is drawn under,
          # and there is no colour to pick when there is no line.
          shiny$conditionalPanel(
            condition = "input.epi_plot_mode != 'cumulative' && input.epi_show_cumulative",
            ns = ns,
            viz_color(
              ns,
              "epi_cumulative_color",
              "Cumulative line",
              CUMULATIVE_COLOR_DEFAULT
            )
          ),
          # Same idea as the cumulative line: the moving-average line only exists
          # in the bar modes and only when its switch is on, so its picker
          # mirrors both conditions the line is drawn under.
          shiny$conditionalPanel(
            condition = "input.epi_plot_mode != 'cumulative' && input.epi_show_moving_avg",
            ns = ns,
            viz_color(
              ns,
              "epi_moving_avg_color",
              "Moving average line",
              MOVING_AVG_COLOR_DEFAULT
            )
          )
        )
      ),
      # Annotations ------------------------------------------------------------
      nav_panel(
        "Annotations",
        icon = shiny$icon("calendar-plus"),
        shiny$pickerInput(
          ns("epi_anno_type"),
          "Type",
          c(`Milestone (line)` = "milestone", `Time period (shaded)` = "period")
        ),
        # startview = "decade" opens the picker on a year grid, so jumping to a
        # collection year (often years before today's default) is one click
        # rather than paging month-by-month. autoclose closes on day selection.
        shiny$dateInput(
          ns("epi_anno_start"),
          "Start date",
          value = Sys.Date(),
          startview = "decade",
          autoclose = TRUE,
          format = "yyyy/mm/dd"
        ),
        shiny$conditionalPanel(
          condition = "input.epi_anno_type == 'period'",
          ns = ns,
          shiny$dateInput(
            ns("epi_anno_end"),
            "End date",
            value = Sys.Date(),
            startview = "decade",
            autoclose = TRUE,
            format = "yyyy/mm/dd"
          )
        ),
        # allow-free-text: an annotation label is display text, so it takes
        # spaces and punctuation — it opts out of the identifier charset
        # restriction in app/js/index.js.
        shiny$div(
          class = "allow-free-text",
          shiny$textInput(ns("epi_anno_label"), "Label", value = "")
        ),
        # Stacked (label above, full-width swatch) to match the Type/date/Label
        # fields around it, rather than the Colors tab's side-by-side row.
        shiny$div(
          class = "epi-anno-color",
          viz_color(ns, "epi_anno_color", "Color", ANNO_COLOR_DEFAULT)
        ),
        shiny$actionButton(
          ns("epi_add_anno"),
          "Add to timeline",
          icon = shiny$icon("plus"),
          width = "100%"
        ),
        shiny$hr(),
        shiny$uiOutput(ns("active_annotations_ui")),
        shiny$actionButton(
          ns("epi_clear_anno"),
          "Clear all",
          icon = shiny$icon("trash"),
          width = "100%"
        )
      )
    ),
    shiny$div(
      class = "reset-buttons",
      shiny$actionButton(
        ns("reset_settings"),
        "Reset settings",
        icon = shiny$icon("rotate-left"),
        width = "100%"
      )
    )
  )
}

#' @export
ui <- function(id, generate_id) {
  ns <- shiny$NS(id)

  layout_sidebar(
    # See visualization_mst.R: `padding` replaces the old non-unique
    # `id = "plot-sidebar"`, which layout_sidebar has no formal for.
    padding = 0,
    border = FALSE,
    sidebar = sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(
        shiny$div(
          id = ns("controls_wrap"),
          class = "viz-nav-wrap",
          epi_controls(ns)
        )
      )
    ),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    # Loading overlay: shown when Generate (parent namespace) is clicked, scoped
    # to this engine's own stage id, and cleared when the plot re-renders — the
    # Tree's shiny:value variant, since this is an image output too.
    shiny$tags$script(
      shiny$HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var out='",
          ns("epi_plot"),
          "';var stage='",
          ns("plot_stage"),
          "';var timer;",
          "function set(on){var s=document.getElementById(stage);if(!s)return;",
          # Ignore Generate clicks while this engine's panel is hidden (a
          # sibling engine is active) — offsetParent is null when display:none.
          "if(on&&s.offsetParent===null)return;",
          "s.classList.toggle('is-loading',on);",
          "if(on){var p=s.querySelector('.viz-plot-prompt');if(p)p.style.display='none';",
          "clearTimeout(timer);timer=setTimeout(function(){set(false);},45000);}",
          "else{clearTimeout(timer);}}",
          "$(document).on('click','#'+gen.replace(/([:.])/g,'\\\\$1'),",
          "function(){set(true);});",
          "$(document).on('shiny:value shiny:recalculated',",
          "function(e){if(e.target.id===out)set(false);});",
          "$(document).on('shiny:error',",
          "function(e){if(e.target.id===out)set(false);});",
          "})();"
        )
      )
    ),
    card(
      full_screen = TRUE,
      class = "plot-card",
      card_body(shiny$uiOutput(ns("plot_area")))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L),
  viz_metadata = shiny$reactive(NULL),
  # Accepted for a uniform `shared` bundle from the coordinator; the Epi curve
  # plots straight from the already-filtered viz_metadata(), so it needs no
  # separate handling.
  selected_isolates = shiny$reactive(NULL),
  # Accepted for the same reason; an epi curve computes no pairwise distances,
  # so missing-value handling never applies (same as the Map).
  na_handling = shiny$reactive("ignore_na"),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("MST")
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # PLOT_MODES offers "square" as its own choice, but the logic layer
    # (epi_plot$build_epi_ggplot) only knows the stacked/cumulative curves plus
    # a separate `square` flag — squares are the stacked curve drawn with
    # square cells, not a third kind of curve. These two translate the UI's
    # three-way choice back to that two-way shape; every reader of
    # input$epi_plot_mode goes through one of them rather than re-deriving the
    # mapping (square -> stacked, TRUE) at each call site.
    mode_for <- function() {
      m <- input$epi_plot_mode %||% PLOT_MODE_DEFAULT
      if (identical(m, "square")) "stacked" else m
    }
    square_for <- function() identical(input$epi_plot_mode, "square")

    # The aspect ratio is ignored in Square blocks mode (coord_fixed sizes the
    # panel to the data grid — see the epi_plot height handler), so the slider
    # is disabled there and given a hover title saying why. The title lives on
    # the wrapper div because a disabled slider swallows its own hover events.
    ASPECT_SQUARE_HINT <-
      "Aspect ratio is fixed in Square blocks mode: the layout is sized to the data grid."
    shiny$observeEvent(
      input$epi_plot_mode,
      {
        square <- square_for()
        shinyjs::toggleState("epi_aspect_ratio", condition = !square)
        wrap <- ns("epi_aspect_ratio_wrap")
        shinyjs::runjs(sprintf(
          "var el=document.getElementById('%s'); if(el){%s}",
          wrap,
          if (square) {
            sprintf("el.setAttribute('title','%s');", ASPECT_SQUARE_HINT)
          } else {
            "el.removeAttribute('title');"
          }
        ))
      },
      ignoreNULL = FALSE
    )

    # The switch is hidden once Cumulative is picked (see the conditionalPanel
    # in epi_controls), but its last value survives underneath — guard here too
    # rather than trust the client to have cleared it.
    show_cumulative_for <- function() {
      isTRUE(input$epi_show_cumulative) &&
        !identical(input$epi_plot_mode, "cumulative")
    }

    # The moving-average line, hidden in Cumulative mode for the same reason
    # (see the conditionalPanel in epi_controls): a running total has no per-
    # interval counts to smooth. Guarded server-side too rather than trusting
    # the client to have cleared the switch.
    show_moving_avg_for <- function() {
      isTRUE(input$epi_show_moving_avg) &&
        !identical(input$epi_plot_mode, "cumulative")
    }

    # Keep the window slider's label naming the span it actually smooths, in
    # whatever interval the Time tab currently uses — "Moving average window:
    # 7 weeks" — so the abstract "intervals" count reads as a concrete duration.
    # Tracks both the interval buttons and the slider itself; the label-only
    # update leaves the value untouched, so it can't feed back on the slider it
    # depends on. Falls back to the fitted interval before the buttons render.
    shiny$observe({
      iv <- input$epi_interval %||% fitted_interval()
      n <- input$epi_moving_avg_window %||%
        epi_plot$EPI_MOVING_AVG_WINDOW_DEFAULT
      shiny$updateSliderInput(
        session,
        "epi_moving_avg_window",
        label = sprintf(
          "Moving average window: %d %s",
          as.integer(n),
          epi_plot$epi_interval_noun(iv, n)
        )
      )
    })

    # Whether Generate has been pressed for this engine. Retained across
    # plot-type switches (only session reset clears it). Once TRUE the curve
    # tracks the controls live — see epi_data().
    generated <- shiny$reactiveVal(FALSE)

    # Timeline annotations the user has added (see the Annotations tab).
    annotations <- shiny$reactiveVal(epi_plot$empty_epi_annotations())

    # Bumped to rebuild the server-rendered pickers (Reset settings).
    stratify_rebuild <- shiny$reactiveVal(0L)
    interval_rebuild <- shiny$reactiveVal(0L)

    # TRUE for exactly one stratify_ui rebuild: the reset path forces the picker
    # back to "No stratification", whereas a plain data-driven re-render (a new
    # database / isolate set changing stratify_fields()) keeps the current field
    # so a deliberate choice sticks. stratify_ui re-renders on both, and can't
    # tell them apart from its dependencies alone — this flag is the signal,
    # set by reset just before it bumps stratify_rebuild() and consumed (reset to
    # FALSE) on read. Mirrors the force_default argument the Tree/Map pass to
    # their update*Input reset path (visualization_tree.R populate_metadata_selects),
    # adapted to a renderUI-rebuilt (bucket 5) control.
    stratify_force_default <- shiny$reactiveVal(FALSE)

    # Whether "Play" is running. Drives the tick loop, the button label, and
    # locking the date-range slider and its step buttons (a manual drag
    # mid-playback is indistinguishable server-side from a tick).
    anim_playing <- shiny$reactiveVal(FALSE)

    # The curve's currently displayed window, as bin *indices* into epi_bins():
    # 0 means "not overridden", i.e. track whatever epi_bins() currently starts
    # or ends at. That sentinel is what makes a reset or a dataset/interval
    # change self-healing — every reader resolves 0 against the *current*
    # epi_bins() rather than a Date captured at some possibly-stale earlier
    # moment, so nothing has to be explicitly re-derived when the bins
    # underneath change; it already reads as "the full span" no matter what
    # the full span now is.
    #
    # These are the source of truth, NOT input$epi_daterange — updateSliderInput
    # only pushes a value to the client and does not update the input until the
    # browser echoes it back on a later flush. Reading the slider back to decide
    # the next step would race that round trip, exactly as documented on the
    # Map's own anim_idx. Tracking the window as plain server state sidesteps
    # it.
    win_start_idx <- shiny$reactiveVal(0L)
    win_end_idx <- shiny$reactiveVal(0L)
    # The end index Play is walking towards; frozen at Play-press time so a
    # mid-run change to epi_bins() (a data update while playing) can't move the
    # goalposts.
    anim_target_idx <- shiny$reactiveVal(0L)

    # --- controls fitted to the data ----------------------------------------

    # The interval that suits this dataset: the finest one that keeps the curve
    # under EPI_MAX_BARS bars. A fixed default can't serve both a month-long
    # outbreak and a decade of surveillance — the same "Week" that reads well on
    # one draws 500 hairlines on the other.
    fitted_interval <- shiny$reactive({
      meta <- viz_metadata()
      if (
        is.null(meta) ||
          !nrow(meta) ||
          !epi_plot$EPI_DATE_FIELD %in% names(meta)
      ) {
        return(INTERVAL_DEFAULT)
      }
      epi_plot$epi_default_interval(meta[[epi_plot$EPI_DATE_FIELD]])
    })

    # Rebuilt only when the fitted answer itself changes (a new database, a
    # different isolate selection) or on reset, so a deliberate choice of
    # interval sticks through everything else the user does.
    #
    # This is a renderUI rather than an updateRadioGroupButtons for the reason
    # given on interval_ui's placeholder in the Time tab: the Epi panel is
    # inserted into the DOM only once a database has loaded, which is the same
    # moment the fit becomes known. An update sent then arrives before the
    # control exists and is silently dropped, leaving the buttons on their
    # declared default — a week's worth of hairlines over a decade of data.
    # Creating the control already carrying the right value has nothing to
    # lose. Restyled as radioGroupButtons (rather than the plain select this
    # used to be) to match the Map's own "Interval" control.
    output$interval_ui <- shiny$renderUI({
      render_info("visualization_epi interval_ui")
      interval_rebuild()
      radioGroupButtons(
        ns("epi_interval"),
        "Interval",
        choices = epi_plot$EPI_INTERVALS,
        selected = fitted_interval(),
        justified = TRUE,
        size = "sm"
      )
    })

    # Anything except the isolate id and the date being plotted is a candidate
    # stratifier.
    stratify_fields <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(character())
      }
      setdiff(names(meta), c("isolate", epi_plot$EPI_DATE_FIELD))
    })

    # A single field, or none. Composite (multi-field) stratification is
    # something build_epi_data still supports (stratify_by takes a vector), but
    # a picker with 40 countries selectable two at a time confused more than it
    # clarified — the curve reads better naming one thing at a time.
    output$stratify_ui <- shiny$renderUI({
      render_info("visualization_epi stratify_ui")
      fields <- stratify_fields()
      stratify_rebuild()
      if (!length(fields)) {
        return(NULL)
      }
      prev <- shiny$isolate(input$epi_stratify)
      # Consumed here: TRUE only on a reset-driven rebuild, where the picker must
      # return to "No stratification" rather than preserve prev (which a reset
      # otherwise wouldn't clear, since prev is still a valid field). Read under
      # isolate so writing it back doesn't re-invalidate this render.
      force_default <- shiny$isolate(stratify_force_default())
      if (force_default) {
        stratify_force_default(FALSE)
      }
      # Categorised, human-readable labels (a "Classical MLST" group holding the
      # derived ST + locus columns); the selected value stays the raw column
      # name the plot builder keys on. "" is the sentinel for no stratification
      # — see stratify_selected().
      pickerInput(
        ns("epi_stratify"),
        "Stratify by",
        choices = c(
          list(`No stratification` = ""),
          grouped_field_choices(fields, attr(viz_metadata(), "mlst_cols"))
        ),
        selected = if (!force_default && isTRUE(prev %in% fields)) prev else "",
        width = "100%",
        options = pickerOptions(
          size = 10,
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search fields ...",
          container = "body"
        )
      )
    })

    # input$epi_stratify is always exactly one string: "" for the sentinel "no
    # stratification" choice, or a field name otherwise. Every reader goes
    # through this rather than re-checking for "" at each call site.
    stratify_selected <- function() {
      s <- input$epi_stratify
      if (is.null(s) || identical(s, "")) character() else s
    }

    # Restrict the colour-scale picker to the scales that can actually carry the
    # current number of strata, and move the selection if it no longer can.
    # Mirrors filter_scale_choices() in visualization_tree.R, but keyed on how
    # many strata there are rather than the mapped variable's type — see
    # epi_plot$epi_scale_choices().
    apply_scale_choices <- function(n_strata, force_default = FALSE) {
      choices <- epi_plot$epi_scale_choices(n_strata)
      selected <- if (force_default) {
        unlist(choices, use.names = FALSE)[1]
      } else {
        epi_plot$epi_fit_scale(input$epi_col_scale, n_strata)
      }
      updatePickerInput(
        session,
        "epi_col_scale",
        choices = choices,
        selected = selected
      )
    }

    # --- reset --------------------------------------------------------------

    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path below, so both routes return the controls
    # identically. See the reset checklist in app/logic/viz_helpers.R.
    #
    # Three things shinyjs::reset() cannot do on its own are patched up here:
    # colorPickr swatches (it doesn't recognise them), the state that isn't an
    # input at all (the annotation list and the playback/window position), and
    # the colour scale, whose *choices* are swapped in at runtime — the
    # checklist's bucket 4, hence the delay. The mode select and the "Label
    # lines"/"Square blocks" switches are plain bucket-1 controls with choices
    # fixed at declaration, so the blanket reset already covers them. The
    # Interval buttons and the date-range slider are bucket 5 — server-rendered,
    # so shinyjs::reset() has no page-load value to restore them from — hence
    # the rebuild bump.
    reset_epi_settings <- function() {
      shinyjs::reset(id = "controls_wrap")

      reset_viz_colors(
        session,
        epi_single_color = SINGLE_COLOR_DEFAULT,
        epi_cumulative_color = CUMULATIVE_COLOR_DEFAULT,
        epi_moving_avg_color = MOVING_AVG_COLOR_DEFAULT,
        epi_text_color = TEXT_COLOR_DEFAULT,
        epi_background_color = BACKGROUND_DEFAULT,
        epi_anno_color = ANNO_COLOR_DEFAULT
      )
      annotations(epi_plot$empty_epi_annotations())
      anim_playing(FALSE)
      win_start_idx(0L)
      win_end_idx(0L)
      anim_target_idx(0L)
      # All three pickers are rendered by renderUI, so shinyjs::reset() has no
      # page-load value to restore them from — rebuild them instead, which
      # drops any selection and re-applies the fit. daterange_ui shares the
      # interval's counter because its bounds are just this dataset's bins at
      # the (possibly just-reset) interval — one bump keeps both in step.
      # stratify_ui would otherwise *preserve* the current field across the
      # rebuild (that's what keeps a deliberate choice through a data change),
      # so signal it to force "No stratification" for this one rebuild. Set
      # before the bump so the re-render sees it.
      stratify_force_default(TRUE)
      stratify_rebuild(stratify_rebuild() + 1L)
      interval_rebuild(interval_rebuild() + 1L)

      # Nothing is stratified after a reset, so one series. Deferred past
      # shinyjs::reset()'s own asynchronous, stale restoration.
      shinyjs::delay(400, apply_scale_choices(1L, force_default = TRUE))
    }

    shiny$observeEvent(input$reset_settings, reset_epi_settings())

    # --- annotations -------------------------------------------------------

    shiny$observeEvent(input$epi_add_anno, {
      label <- trimws(input$epi_anno_label %||% "")
      if (!nzchar(label)) {
        shiny$showNotification(
          "Give the annotation a label first.",
          type = "warning"
        )
        return()
      }
      is_period <- identical(input$epi_anno_type, ANNO_PERIOD)
      start <- input$epi_anno_start
      end <- if (is_period) input$epi_anno_end else as.Date(NA)

      if (is_period && end < start) {
        shiny$showNotification(
          "The annotation's end date is before its start date.",
          type = "warning"
        )
        return()
      }
      # Two annotations covering exactly the same span can never be told apart
      # on the plot, however they are drawn — so refuse the duplicate rather
      # than stack a second, unreadable copy on top of the first. (Merely
      # *overlapping* spans are fine: epi_annotation_lanes gives each its own
      # lane.)
      if (.is_duplicate(annotations(), input$epi_anno_type, start, end)) {
        shiny$showNotification(
          "That exact period is already annotated.",
          type = "warning"
        )
        return()
      }

      annotations(rbind(
        annotations(),
        data.frame(
          # Unique per row; the delete handler keys on it.
          id = paste0("a", as.integer(Sys.time()), "_", nrow(annotations())),
          type = input$epi_anno_type,
          start = start,
          end = end,
          label = label,
          color = input$epi_anno_color %||% ANNO_COLOR_DEFAULT,
          stringsAsFactors = FALSE
        )
      ))
      shiny$updateTextInput(session, "epi_anno_label", value = "")
    })

    shiny$observeEvent(input$epi_clear_anno, {
      annotations(epi_plot$empty_epi_annotations())
    })

    # One delegated handler for every row's delete button, rather than
    # registering an observer per row: dynamically created observeEvent()s would
    # be re-registered on every annotations() change (each render adds another
    # observer for ids that already have one), so the button ids instead push
    # their own id into a single input.
    shiny$observeEvent(input$anno_delete, {
      annotations(
        annotations()[annotations()$id != input$anno_delete, , drop = FALSE]
      )
    })

    output$active_annotations_ui <- shiny$renderUI({
      df <- annotations()
      if (!nrow(df)) {
        return(shiny$div(
          class = "text-muted fst-italic mb-2 epi-anno-empty",
          "No active annotations."
        ))
      }
      shiny$div(
        lapply(seq_len(nrow(df)), function(i) {
          anno <- df[i, ]
          is_period <- identical(anno$type, ANNO_PERIOD) && !is.na(anno$end)
          color <- if (
            "color" %in% names(anno) && !is.na(anno$color) && nzchar(anno$color)
          ) {
            anno$color
          } else {
            ANNO_COLOR_DEFAULT
          }
          subtitle <- if (is_period) {
            paste(
              format(anno$start, "%d %b %Y"),
              "–",
              format(anno$end, "%d %b %Y")
            )
          } else {
            format(anno$start, "%d %b %Y")
          }
          shiny$div(
            class = paste(
              "card p-2 mb-2 d-flex flex-row align-items-center",
              "justify-content-between epi-anno-card"
            ),
            # The annotation's own colour, drawn as a left stripe by
            # .epi-anno-card so the list mirrors what is on the curve. Passed as
            # a CSS custom property — the only per-row value the stylesheet needs.
            style = sprintf("--epi-anno-stripe: %s;", color),
            shiny$div(
              class = "epi-anno_body",
              shiny$div(
                class = "epi-anno_label",
                title = anno$label,
                anno$label
              ),
              shiny$div(
                class = "epi-anno_meta",
                paste(
                  if (is_period) "Time period" else "Milestone",
                  "|",
                  subtitle
                )
              )
            ),
            shiny$tags$button(
              type = "button",
              class = "btn btn-sm epi-anno_delete",
              title = "Remove annotation",
              `aria-label` = paste("Remove annotation", anno$label),
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                ns("anno_delete"),
                anno$id
              ),
              shiny$icon("xmark")
            )
          )
        })
      )
    })

    # --- data --------------------------------------------------------------

    # The binned curve. Gated on Generate having been pressed once (so the
    # engine still opens on the "press Generate" prompt like its siblings), but
    # reactive to the data controls thereafter: binning 341 isolates is
    # microseconds, and making the interval/stratification wait for another
    # Generate press made them look broken — the tick labels moved but the bars
    # didn't, because only the axis format was live.
    epi_data <- shiny$reactive({
      shiny$req(generated())
      meta <- viz_metadata()
      shiny$req(!is.null(meta), nrow(meta) > 0)
      epi_plot$build_epi_data(
        meta,
        epi_plot$EPI_DATE_FIELD,
        stratify_selected(),
        input$epi_interval %||% fitted_interval()
      )
    })

    # The bins playback walks.
    epi_bins <- shiny$reactive(epi_plot$epi_bins(epi_data()))

    # The panel shape square cells force on this data; NULL when it doesn't
    # apply. Drives the plot's height — see the epi_plot renderImage below.
    square_ratio <- shiny$reactive({
      epi_plot$square_panel_ratio(epi_data(), mode_for())
    })

    shiny$observeEvent(epi_data(), {
      apply_scale_choices(length(unique(epi_data()$stratum)))
    })

    shiny$observeEvent(
      session_reset(),
      {
        generated(FALSE)
        reset_epi_settings()
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(generate(), {
      if (!identical(plot_type(), "Epi")) {
        return()
      }
      meta <- viz_metadata()
      if (is.null(meta) || !nrow(meta)) {
        shiny$showNotification("No isolate metadata to plot.", type = "warning")
        shinyjs::removeClass(id = "plot_stage", class = "is-loading")
        return()
      }

      binned <- tryCatch(
        epi_plot$build_epi_data(
          meta,
          epi_plot$EPI_DATE_FIELD,
          stratify_selected(),
          input$epi_interval %||% fitted_interval()
        ),
        error = function(e) {
          shiny$showNotification(
            paste("Epi curve failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )

      if (is.null(binned) || !nrow(binned)) {
        # An empty collection-date column is the common case for a freshly typed
        # database (the field is only ever filled in by hand), so point at where
        # it gets filled rather than just reporting the curve is empty.
        shiny$showNotification(
          shiny$tagList(
            shiny$tags$b("Nothing to plot."),
            shiny$br(),
            paste0(
              "No isolate has a readable \"",
              field_label(epi_plot$EPI_DATE_FIELD),
              "\". Add dates under Database › Browse."
            )
          ),
          duration = 10,
          type = "warning"
        )
        # No plot will render, so the overlay's clearing event never fires —
        # hide it now rather than leaving the spinner up for 45s.
        shinyjs::removeClass(id = "plot_stage", class = "is-loading")
        generated(FALSE)
        return()
      }

      # Don't silently under-report the curve: say how many isolates were left
      # out for want of a readable date.
      dropped <- attr(binned, "dropped") %||% 0L
      if (dropped > 0L) {
        shiny$showNotification(
          sprintf(
            "%d isolate%s excluded: no readable collection date.",
            dropped,
            if (dropped == 1L) "" else "s"
          ),
          type = "message"
        )
      }
      generated(TRUE)
    })

    # --- playback ------------------------------------------------------------

    # Resolve a possibly-0 (sentinel) index against the CURRENT bins, clamped
    # into range. Centralised because every reader — the slider, the step
    # buttons, Play, the plot itself — needs this same "0 means the natural
    # end of that side" resolution, and none of them should be trusted to
    # re-derive it slightly differently.
    resolve_idx <- function(idx, bins, default) {
      if (idx < 1L || idx > length(bins)) default else idx
    }

    win_start_date <- shiny$reactive({
      bins <- epi_bins()
      shiny$req(length(bins) >= 1)
      bins[resolve_idx(win_start_idx(), bins, 1L)]
    })
    win_end_date <- shiny$reactive({
      bins <- epi_bins()
      shiny$req(length(bins) >= 1)
      bins[resolve_idx(win_end_idx(), bins, length(bins))]
    })

    # The date-range slider. Rendered server-side, like interval_ui, because
    # its bounds are this dataset's bin extent — not known until the data
    # exists, and re-derived (via epi_bins()) whenever the interval changes.
    # Reads win_start_date()/win_end_date() rather than raw bin indices so a
    # rebuild (reset, or a genuinely new bin set) always bakes in "the full
    # span" without needing its own special-cased default.
    #
    # Those two window reactives are read under isolate(), NOT as live
    # dependencies. That is what makes playback flicker-free: the tick loop
    # advances win_end_idx() every frame, and if this renderUI depended on it
    # the whole slider uiOutput would re-render each tick — the browser draws
    # the "recalculating" grey overlay over an output while it rebuilds, which
    # is exactly the defective flashing that was showing. The only things that
    # SHOULD rebuild this control are structural (a new bin set, or a reset —
    # interval_rebuild()/epi_bins()), and on every one of those the window has
    # just been rewound to the full span anyway, so an isolated read still bakes
    # in the correct value. Live window moves (stepping, playback) reach the
    # slider through updateSliderInput instead, never a re-render.
    output$daterange_ui <- shiny$renderUI({
      render_info("visualization_epi daterange_ui")
      interval_rebuild()
      bins <- epi_bins()
      shiny$req(length(bins) >= 2)
      win_value <- shiny$isolate(c(win_start_date(), win_end_date()))
      shiny$div(
        class = "custom-slider date-slider",
        # Covered while Play is running (see the anim_playing observer): even
        # with the re-render gone, the per-tick updateSliderInput still redraws
        # the ion.rangeSlider handles and rotated tick labels underneath. The
        # window it would show is already narrated by the on-plot reveal, so the
        # slider is hidden behind an opaque "Animating…" cover for the duration
        # rather than left twitching.
        id = ns("daterange_slider_wrap"),
        shiny$sliderInput(
          ns("epi_daterange"),
          NULL,
          min = bins[1],
          max = bins[length(bins)],
          value = win_value,
          step = epi_plot$bin_width_days(
            input$epi_interval %||% fitted_interval()
          ),
          timeFormat = "%Y-%m-%d"
        ),
        shiny$div(
          class = "date-slider_cover",
          shiny$icon("circle-notch", class = "fa-spin"),
          shiny$span("Animating…")
        )
      )
    })

    # A fresh bin set (new database, changed interval) rewinds to fully
    # revealed and stops any playback that was running against the old bins.
    shiny$observeEvent(epi_bins(), {
      anim_playing(FALSE)
      win_start_idx(0L)
      win_end_idx(0L)
    })

    # Dragging either handle sets the window — but only when a tick isn't
    # already driving it, or the echo of our own update would fight the loop.
    # A Date slider's drag granularity is in whole days even when the bin
    # interval is coarser (a month isn't a fixed number of days), so the
    # dragged value is snapped to its nearest bin rather than trusted exactly.
    shiny$observeEvent(input$epi_daterange, {
      if (isTRUE(anim_playing())) {
        return(invisible(NULL))
      }
      bins <- shiny$isolate(epi_bins())
      if (!length(bins)) {
        return(invisible(NULL))
      }
      vals <- input$epi_daterange
      snap <- function(d) which.min(abs(as.numeric(bins - d)))
      win_start_idx(snap(vals[1]))
      win_end_idx(snap(vals[2]))
    })

    step_start_by <- function(delta) {
      bins <- shiny$isolate(epi_bins())
      if (length(bins) < 1) {
        return(invisible(NULL))
      }
      s <- resolve_idx(shiny$isolate(win_start_idx()), bins, 1L)
      e <- resolve_idx(shiny$isolate(win_end_idx()), bins, length(bins))
      nxt <- min(max(s + delta, 1L), e)
      win_start_idx(nxt)
      shiny$updateSliderInput(
        session,
        "epi_daterange",
        value = c(bins[nxt], bins[e])
      )
    }
    step_end_by <- function(delta) {
      bins <- shiny$isolate(epi_bins())
      if (length(bins) < 1) {
        return(invisible(NULL))
      }
      s <- resolve_idx(shiny$isolate(win_start_idx()), bins, 1L)
      e <- resolve_idx(shiny$isolate(win_end_idx()), bins, length(bins))
      nxt <- max(min(e + delta, length(bins)), s)
      win_end_idx(nxt)
      shiny$updateSliderInput(
        session,
        "epi_daterange",
        value = c(bins[s], bins[nxt])
      )
    }
    shiny$observeEvent(input$epi_step_start_prev, step_start_by(-1L))
    shiny$observeEvent(input$epi_step_start_next, step_start_by(1L))
    shiny$observeEvent(input$epi_step_end_prev, step_end_by(-1L))
    shiny$observeEvent(input$epi_step_end_next, step_end_by(1L))

    # Play freezes the start handle wherever it currently sits, collapses the
    # window to that single point, and then sweeps the end handle forward to
    # wherever the end handle was — exactly the Map's own Play (which freezes
    # its start bin and advances only the end index each tick). A pressed-again
    # Play just pauses.
    shiny$observeEvent(input$epi_play, {
      if (isTRUE(anim_playing())) {
        anim_playing(FALSE)
        return(invisible(NULL))
      }
      bins <- shiny$isolate(epi_bins())
      shiny$req(length(bins) >= 2)
      s <- resolve_idx(shiny$isolate(win_start_idx()), bins, 1L)
      e <- resolve_idx(shiny$isolate(win_end_idx()), bins, length(bins))
      shiny$req(e > s)
      anim_target_idx(e)
      win_start_idx(s)
      win_end_idx(s)
      shiny$updateSliderInput(
        session,
        "epi_daterange",
        value = c(bins[s], bins[s])
      )
      anim_playing(TRUE)
    })

    shiny$observeEvent(anim_playing(), {
      playing <- isTRUE(anim_playing())
      shiny$updateActionButton(
        session,
        "epi_play",
        label = if (playing) "Pause" else "Play",
        icon = shiny$icon(if (playing) "pause" else "play")
      )
      # Lock every other Time control for the duration of playback: a manual
      # drag, step, interval switch or axis-zoom toggle mid-run is either
      # indistinguishable server-side from a tick or rebinds the very bins the
      # loop is walking, both of which race the animation into buggy behaviour.
      # Play itself stays live so it can pause.
      shinyjs::toggleState("epi_daterange", condition = !playing)
      shinyjs::toggleState("epi_step_start_prev", condition = !playing)
      shinyjs::toggleState("epi_step_start_next", condition = !playing)
      shinyjs::toggleState("epi_step_end_prev", condition = !playing)
      shinyjs::toggleState("epi_step_end_next", condition = !playing)
      shinyjs::toggleState("epi_interval", condition = !playing)
      shinyjs::toggleState("epi_zoom_axis", condition = !playing)
      # Hide the flickering slider behind its cover (see daterange_ui).
      shinyjs::toggleClass(
        id = "daterange_slider_wrap",
        class = "date-slider--playing",
        condition = playing
      )
    })

    # The tick loop. Mirrors the guarded observe() + invalidateLater() pattern
    # the Map's animation and typing.R's progress poll already use. Stops (does
    # not reschedule) once the target is reached, and goes dormant while
    # another engine is showing — navset_hidden only hides a panel with CSS, it
    # does not suspend a running observe().
    shiny$observe({
      if (!isTRUE(anim_playing())) {
        return(NULL)
      }
      if (!identical(plot_type(), "Epi")) {
        return(NULL)
      }
      bins <- shiny$isolate(epi_bins())
      cur <- shiny$isolate(win_end_idx())
      target <- shiny$isolate(anim_target_idx())
      if (!length(bins) || target > length(bins) || cur >= target) {
        anim_playing(FALSE)
        return(NULL)
      }
      nxt <- cur + 1L
      win_end_idx(nxt)
      s <- shiny$isolate(win_start_idx())
      shiny$updateSliderInput(
        session,
        "epi_daterange",
        value = c(bins[s], bins[nxt])
      )
      shiny$invalidateLater(STEP_MS, session)
    })

    # --- plot ----------------------------------------------------------------

    # Rebuilt live as the controls change; the binning above is what re-runs
    # when a data control moves, this only redraws.
    epi_ggplot <- shiny$reactive({
      binned <- epi_data()
      shiny$req(nrow(binned) > 0)
      # The browser-reported panel width, so the legend can be given a column
      # count that keeps it inside the plot rather than clipped at the edges
      # (see epi_legend_ncol). NULL until the first render reports it, which
      # build_epi_ggplot handles with a sensible fallback; re-reading it here
      # re-flows the legend when the panel is resized.
      plot_width <- session$clientData[[
        paste0("output_", ns("epi_plot"), "_width")
      ]]
      epi_plot$build_epi_ggplot(
        binned,
        list(
          mode = mode_for(),
          plot_width = plot_width,
          col_scale = input$epi_col_scale %||% COL_SCALE_DEFAULT,
          single_color = input$epi_single_color %||% SINGLE_COLOR_DEFAULT,
          cumulative_color = input$epi_cumulative_color %||%
            CUMULATIVE_COLOR_DEFAULT,
          background = input$epi_background_color %||% BACKGROUND_DEFAULT,
          text_color = input$epi_text_color %||% TEXT_COLOR_DEFAULT,
          interval = input$epi_interval %||% fitted_interval(),
          square = square_for(),
          fixed_axis = !isTRUE(input$epi_zoom_axis),
          label_ends = isTRUE(input$epi_label_ends),
          show_x_label = isTRUE(input$epi_show_x_label),
          show_cumulative = show_cumulative_for(),
          show_moving_avg = show_moving_avg_for(),
          moving_avg_window = input$epi_moving_avg_window %||%
            epi_plot$EPI_MOVING_AVG_WINDOW_DEFAULT,
          moving_avg_align = input$epi_moving_avg_align %||%
            MOVING_AVG_ALIGN_DEFAULT,
          moving_avg_color = input$epi_moving_avg_color %||%
            MOVING_AVG_COLOR_DEFAULT,
          annos = annotations(),
          reveal_from = win_start_date(),
          reveal_to = win_end_date()
        )
      )
    })

    # The plot output element is kept mounted so each Generate re-renders the
    # *same* output. The "press Generate" prompt is an overlay toggled
    # separately.
    output$plot_area <- shiny$renderUI({
      render_info("visualization_epi plot_area")
      prompt <- shiny$div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny$isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        shiny$icon("chart-column", class = "viz-plot-icon"),
        shiny$p(
          "Configure the Epi options, then press ",
          shiny$tags$strong("Generate Plot"),
          "."
        )
      )

      loading <- shiny$div(
        class = "viz-loading",
        shiny$div(
          class = "spinner-custom viz-spinner-dark",
          waiter::spin_flower(),
          shiny$tags$h5("Generating plot …", class = "viz-loading_text")
        )
      )

      shiny$div(
        class = "viz-plot-stage epi-stage",
        id = ns("plot_stage"),
        prompt,
        loading,
        shiny$plotOutput(ns("epi_plot"), height = "auto"),
        # Hidden target the export action button clicks to start the download.
        shiny$div(
          class = "d-none",
          shiny$downloadButton(ns("download_epi"), "Download plot")
        )
      )
    })

    # Hide the prompt overlay once a plot has been generated.
    shiny$observeEvent(
      generated(),
      {
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    # Rendered through ggsave (via renderImage) rather than renderPlot so the
    # whole frame takes the chosen background. In Square blocks mode the curve
    # uses coord_fixed, whose respect=TRUE centres the entire ggplot in the
    # device and leaves a letterbox around it; renderPlot fills that letterbox
    # with its device background, which is hardwired white and — unlike a normal
    # argument — forced once at module init, so it can never track the colour
    # picker. ggsave instead derives the device background from the theme's
    # plot.background, so the letterbox takes the selected colour too — exactly
    # as the PNG export already does (see save_epi_plot). This is the same image
    # the Download button writes, just sized to the on-screen panel.
    output$epi_plot <- shiny$renderImage(
      {
        render_info("visualization_epi epi_plot")
        p <- epi_ggplot()
        w <- session$clientData[[paste0("output_", ns("epi_plot"), "_width")]]
        w <- as.integer(w %||% 900L)
        # Height follows the panel width, like the Tree's. Square mode is the
        # exception: coord_fixed ties the panel's shape to the data grid (see
        # square_ratio), so it gets the height its squares actually need rather
        # than the aspect slider's — a decade of months against a handful of
        # cases is legitimately a long, low plot.
        ratio <- if (square_for()) square_ratio() else NULL
        h <- if (is.null(ratio)) {
          as.integer(w * (input$epi_aspect_ratio %||% ASPECT_DEFAULT))
        } else {
          # The axes, labels and legend take room the panel doesn't.
          as.integer(min(
            2000,
            max(240, (w - PANEL_CHROME_PX) * ratio + PANEL_CHROME_PX)
          ))
        }
        # Lay the plot out as if at 96dpi (what the renderPlot `res` used to do,
        # so 13pt text keeps its size) but render at the browser's device pixel
        # ratio so the PNG stays crisp on HiDPI screens.
        pr <- session$clientData$pixelratio %||% 1
        tmp <- tempfile(fileext = ".png")
        epi_plot$render_epi_png(
          p,
          tmp,
          width_px = w,
          height_px = h,
          res = 96,
          scale = pr
        )
        list(src = tmp, width = w, height = h, alt = "Epidemiological curve")
      },
      deleteFile = TRUE
    )

    output$download_epi <- shiny$downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_epi_curve.", input$epi_filetype %||% "png")
      },
      content = function(file) {
        epi_plot$save_epi_plot(
          epi_ggplot(),
          file,
          input$epi_filetype %||% "png",
          input$epi_aspect_ratio %||% ASPECT_DEFAULT
        )
      }
    )

    # The export tab uses an action button; route it to the hidden download
    # link.
    shiny$observeEvent(input$epi_download, {
      if (!isTRUE(generated())) {
        shiny$showNotification(
          "Generate a plot before saving it.",
          type = "warning"
        )
        return()
      }
      shinyjs::click("download_epi")
    })

    # `plot_area` is a cheap renderUI gating the "press Generate" prompt, and
    # the plot output has to bind through it, so it stays live while hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    # The curve is a server-side ggplot with no client state to lose — see the
    # matching note in visualization_tree.R for why it may suspend while its
    # plot tab is in the background.
    shiny$outputOptions(output, "epi_plot", suspendWhenHidden = TRUE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    # Snapshot the epi_* controls plus the annotation list (a reactiveVal, not
    # an input).
    snapshot <- shiny$reactive(c(
      collect_input_snapshot(input, "epi_"),
      list(.annotations = annotations())
    ))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "epi_label_ends",
          "epi_show_cumulative",
          "epi_show_moving_avg",
          "epi_show_x_label",
          "epi_zoom_axis"
        ),
        selects = c(
          "epi_plot_mode",
          "epi_moving_avg_align",
          "epi_anno_type",
          "epi_col_scale"
        ),
        sliders = c("epi_aspect_ratio", "epi_moving_avg_window"),
        texts = "epi_anno_label",
        colors = c(
          "epi_single_color",
          "epi_text_color",
          "epi_anno_color",
          "epi_cumulative_color",
          "epi_background_color",
          "epi_moving_avg_color"
        ),
        # Time interval / stratify are server-rendered controls (rebuilt on a
        # counter); best-effort here, corrected by the user's Generate.
        radio_groups = "epi_interval",
        pickers = "epi_stratify"
      )

      if (!is.null(vals$.annotations)) {
        a <- vals$.annotations
        if (is.data.frame(a) && nrow(a)) {
          try(annotations(a), silent = TRUE)
        }
      }
    }

    # Thumbnail: server-render the Epi curve to a small PNG.
    save_thumb <- function(file, w, h) {
      epi_plot$render_epi_png(
        epi_ggplot(),
        file,
        width_px = w,
        height_px = h,
        res = 96,
        scale = 1
      )
    }

    list(
      snapshot = snapshot,
      restore = restore,
      save_thumb = save_thumb,
      request_thumb = NULL,
      thumb_data = NULL
    )
  })
}

# An annotation of the same type covering exactly the same span. NA ends (a
# milestone) match only other NA ends.
.is_duplicate <- function(annos, type, start, end) {
  if (!nrow(annos)) {
    return(FALSE)
  }
  same_end <- if (is.na(end)) {
    is.na(annos$end)
  } else {
    !is.na(annos$end) & annos$end == end
  }
  any(annos$type == type & annos$start == start & same_end)
}
