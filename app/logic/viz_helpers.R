# app/logic/viz_helpers.R
#
# Namespace-agnostic UI helpers and option-set constants shared by the two
# visualization submodules (app/view/visualization_tree.R and
# app/view/visualization_mst.R). Every helper takes the caller's `ns` and
# returns tags, so it is safe to reuse across module namespaces (same pattern
# as `sidebar_menu` in app/logic/functions.R).
#
# --- adding a new plot control input: reset checklist -----------------------
#
# Every control in visualization_map.R / _tree.R / _mst.R's sidebars must
# come back to a known default when its "Reset settings" button is clicked.
# Each of those three modules wires this via one shinyjs::reset(id =
# "controls_wrap") call in its `reset_settings` observer, PLUS explicit
# patch-up code for whatever shinyjs::reset() can't handle correctly. When
# you add or change a control, work out which bucket it's in — it is NOT safe
# to assume shinyjs::reset() alone covers it:
#
#  1. Plain input, static default (sliderInput/textInput/numericInput/
#     checkboxInput/input_switch, or a selectInput/radioButtons whose
#     `choices` are fixed at UI-declaration time and never swapped out by an
#     update*Input() call elsewhere): shinyjs::reset() already handles this
#     correctly and synchronously. Nothing to add.
#
#  2. A viz_color() swatch (colorPickr): shinyjs::reset() doesn't even
#     recognize colorPickr as a resettable input (it only knows the unrelated
#     `colourpicker` package's widget) — it's silently skipped. Add
#     your_id = "#default" to that module's reset_viz_colors(session, ...)
#     call. (See reset_viz_colors() below for the deeper reason this can't
#     just be a plain shinyWidgets::updateColorPickr() call either — pickr's
#     own setColor() doesn't fire the "changestop" event the widget is
#     configured to update on.)
#
#  3. A shinyWidgets::radioGroupButtons(): shinyjs::reset() *does* detect
#     these (they share a CSS class with plain radioButtons) and attempts to
#     restore them, but via shiny::updateRadioButtons(), which messages a
#     "value" key — the widget's own JS binding only reacts to "selected", so
#     the attempt is a silent no-op. Add your_id = "default_choice" to that
#     module's reset_viz_radio_buttons(session, ...) call.
#
#  4. A selectInput/pickerInput whose `choices` (and often `selected`) get
#     swapped out at Generate time for data loaded at runtime (metadata
#     columns, isolate names, a date range fitted to the data, etc.) rather
#     than being fixed at UI-declaration time: add it to that module's
#     populate_metadata_selects() (map.R: also reset_data_fitted_controls())
#     — always computing the SAME default Generate itself would use for data
#     it's never seen a selection for, not the UI's placeholder default.
#     Critically, the call into that function from `reset_settings` MUST stay
#     wrapped in shinyjs::delay(400, ...), not called immediately after
#     shinyjs::reset(): these ARE plain <select>s/sliderInputs, so
#     shinyjs::reset() also recognizes and restores them — but only
#     asynchronously, via a client round-trip. An immediate, same-tick
#     correction gets silently overwritten a moment later when that delayed,
#     stale (pre-Generate) restoration lands. This is why the reset observer
#     in each module is NOT simply "shinyjs::reset() then fix up the rest" —
#     order and timing both matter. See the comment on that observer in
#     whichever module you're editing for the full trace of this failure
#     mode.
#
#  5. A control the server *renders* (shiny::renderUI + uiOutput, e.g. the Epi
#     engine's "Time interval" and "Stratify by" pickers, whose value/choices
#     are fitted to the loaded database): shinyjs::reset() restores an input to
#     the value it had at *page load*, and a server-rendered control had no
#     existence then — so there is nothing for it to restore and it is skipped.
#     Rebuild the control instead: have its renderUI depend on a reactiveVal
#     counter and bump that from `reset_settings` (see visualization_epi.R's
#     interval_rebuild / stratify_rebuild). Note this bucket exists precisely
#     because bucket 4's update*Input() route is *unusable* for these: this
#     engine's panel is only inserted into the DOM once a database loads, which
#     is the same moment the fitted value becomes known, so the update fires at
#     a control the browser has not built yet and is dropped on the floor.
#     Rendering the control already carrying the right value has nothing to
#     lose.
#
#  6. State that is not an input at all (the Epi engine's annotation list and
#     playback position — plain reactiveVals): shinyjs::reset() cannot see it,
#     no update*Input() addresses it. Reset it by hand in `reset_settings`.
#
# When in doubt, prefer testing the actual "Reset settings" button over
# reasoning about it — bucket 1 is easy to get wrong by assuming it applies
# to something that's actually bucket 4 (any control whose value is ever set
# by an update*Input() call outside the UI declaration is a strong hint it's
# NOT bucket 1).

box::use(
  shiny[
    div,
    selectInput,
    actionButton,
    icon,
    hr,
    tags,
    HTML,
    singleton,
    reactiveValuesToList,
    updateSelectInput,
    updateSliderInput,
    updateNumericInput,
    updateTextInput,
  ],
  bslib[nav_panel, update_switch],
  shinyWidgets[
    colorPickr,
    updateRadioGroupButtons,
    updatePrettyRadioButtons,
    updatePickerInput,
  ],
)

# --- shared option sets ------------------------------------------------------

# Metadata columns mappable to plot aesthetics.
#' @export
meta_vars <- c("Isolation Date", "Host", "Country", "City", "Database")

# Sources for the isolate (tip) label.
#' @export
label_vars <- c("Assembly Name", "Assembly ID", meta_vars)

# Sources for branch labels (allelic distance is the numeric default).
#' @export
branch_vars <- c("Allelic Distance", meta_vars)

#' @export
fontfaces <- c(
  Plain = "plain",
  Bold = "bold",
  Italic = "italic",
  `Bold Italic` = "bold.italic"
)

#' @export
point_shapes <- c(
  Circle = "circle",
  Square = "square",
  Diamond = "diamond",
  Triangle = "triangle",
  Cross = "cross",
  Asterisk = "asterisk"
)

# ColorBrewer / viridis palettes grouped for the color-scale selects.
#' @export
color_scales <- list(
  Qualitative = c(
    "Set1",
    "Set2",
    "Set3",
    "Pastel1",
    "Paired",
    "Dark2",
    "Accent"
  ),
  Sequential = c(
    "Blues",
    "Greens",
    "Reds",
    "Purples",
    "Oranges",
    "Greys",
    "YlGnBu"
  ),
  Gradient = c(
    "viridis",
    "magma",
    "plasma",
    "inferno",
    "cividis",
    "turbo",
    "mako"
  ),
  Diverging = c("Spectral", "RdYlGn", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")
)

# Which color_scales categories (Qualitative/Sequential/Gradient/Diverging)
# make sense for a variable resolved to the given type. Categorical data only
# suits Qualitative; numeric data suits Sequential and Gradient (both are
# "ordered, single direction" families) and additionally Diverging only when
# the values genuinely straddle zero — a real domain-meaningful threshold
# (centering on the mean/median instead would make almost any numeric column
# look "diverging" by construction, which would defeat the filter). Shared by
# every submodule that dynamically restricts a color-scale picker's choices to
# whatever suits the variable currently driving it (map's map_col_scale, tree's
# variable-mapping scales), so they all agree on what a variable "is".
#' @export
suitable_scale_categories <- function(resolved_type, vals) {
  if (identical(resolved_type, "Factor")) {
    return("Qualitative")
  }
  num <- suppressWarnings(as.numeric(vals))
  num <- num[!is.na(num)]
  diverging <- length(num) > 0 && any(num < 0) && any(num > 0)
  if (diverging) {
    c("Sequential", "Gradient", "Diverging")
  } else {
    c("Sequential", "Gradient")
  }
}

# shiny$dateInput's text box is freely user-editable — typing something the
# client's date parser can't make sense of (e.g. "20152-07-17") does not
# reject the keystrokes; it round-trips to the server as Date(NA), silently
# emitting an "not in a standard unambiguous format" warning. Any comparison
# on that NA (e.g. `end < start`) then throws "missing value where TRUE/FALSE
# needed" out of the `if`, which crashes the whole app rather than just the
# observer. Every dateInput() value MUST be checked with this before it's
# compared, arithmetic'd, or stored — show a notification and bail instead.
#' @export
any_invalid_date <- function(...) {
  any(vapply(list(...), function(x) length(x) != 1 || is.na(x), logical(1)))
}

# --- small UI helpers --------------------------------------------------------

# colorPickr is configured update = "changestop" below, which is pickr's
# "user finished dragging/typing" event — the *only* event both pickr's own
# swatch repaint and Shiny's own subscribe() (the thing that pushes a new
# value into input$<id>) are wired to. Programmatic updates
# (shinyWidgets::updateColorPickr(), i.e. pickr.setColor()) never fire it —
# setColor() internally emits "save" instead — so a server-side reset changes
# pickr's internal color but leaves the swatch unpainted *and* input$<id>
# stale. singleton() below registers a message handler, once no matter how
# many viz_color() calls render it, that does what a real user interaction
# would: set the color, then force-emit "changestop" so both the repaint and
# the input push happen. See reset_viz_colors().
viz_color_reset_script <- singleton(tags$script(HTML(
  "if (!window.__vizColorResetHandlerRegistered) {
    window.__vizColorResetHandlerRegistered = true;
    Shiny.addCustomMessageHandler('viz-reset-color', function(msg) {
      var el = document.getElementById(msg.id);
      if (!el) return;
      var entry = Shiny.inputBindings.bindingNames['shinyWidgets.colorPickr'];
      if (!entry) return;
      var pickr = entry.binding.getPickr(el);
      if (!pickr) return;
      pickr.setColor(msg.value);
      pickr._emit('changestop', null);
    });
  }"
)))

# A labelled color picker laid out as one row (label left, swatch right).
#' @export
viz_color <- function(ns, id, label, value) {
  div(
    class = "viz-color-row",
    tags$label(label, class = "viz-color-label"),
    div(
      class = "viz-color-pick",
      colorPickr(
        inputId = ns(id),
        label = NULL,
        selected = value,
        update = "changestop",
        interaction = list(clear = FALSE, save = FALSE),
        position = "right-start",
        width = "100%"
      )
    ),
    viz_color_reset_script
  )
}

# Grouped color-scale select used by every variable mapping. `categories`
# statically restricts the offered groups (e.g. a heatmap's intensity scale,
# which is always non-negative numeric and so never needs Qualitative or
# Diverging); leave it at the default to offer every group, or update the
# choices dynamically at runtime for pickers whose suitable groups depend on
# whichever variable is currently mapped (see suitable_scale_categories()).
#' @export
scale_select <- function(ns, id, categories = names(color_scales)) {
  # Wrapped in a static class rather than styled off ns(id) directly: each
  # plot tab is its own module instance, so the rendered id is namespaced
  # per-tab (e.g. "plot_xyz-engine-nj_tiplab_scale") and differs across tabs
  # showing the same engine. A shared class lets one CSS rule in main.scss
  # (.viz-scale-select .option[data-value=...]) swatch every scale picker's
  # dropdown regardless of which tab or instance renders it.
  div(
    class = "viz-scale-select",
    selectInput(
      ns(id),
      "Color scale",
      choices = color_scales[categories],
      width = "100%"
    )
  )
}

# Export tab, shared by both engines (prefix keeps input ids unique per engine).
#' @export
export_panel <- function(ns, prefix, formats) {
  nav_panel(
    "Export",
    icon = icon("download"),
    div(
      class = "viz-export",
      selectInput(ns(paste0(prefix, "_filetype")), "File format", formats),
      actionButton(
        ns(paste0(prefix, "_download")),
        "Save plot",
        icon = icon("download")
      ),
      hr(),
      actionButton(
        ns(paste0(prefix, "_report")),
        "Print report",
        icon = icon("file-lines")
      )
    )
  )
}

# --- reset-settings helpers ---------------------------------------------

# colorPickr (see viz_color() above) ships a custom JS input binding with no
# ".shiny-input-container" subclass shinyjs::reset() knows how to read — it's
# silently skipped by a blanket shinyjs::reset(), so every color needs restoring
# by id here after that call. Goes through the "viz-reset-color" message
# handler (registered by viz_color()) rather than
# shinyWidgets::updateColorPickr() directly — see the comment on
# viz_color_reset_script for why updateColorPickr() alone isn't enough. Call as
# reset_viz_colors(session, some_id = "#rrggbb", other_id = "#rrggbb", ...),
# with each default matching the corresponding viz_color() call's `value`.
#' @export
reset_viz_colors <- function(session, ...) {
  defaults <- list(...)
  for (id in names(defaults)) {
    session$sendCustomMessage(
      "viz-reset-color",
      list(id = session$ns(id), value = defaults[[id]])
    )
  }
}

# shinyWidgets::radioGroupButtons() *is* picked up by shinyjs::reset() (it
# shares shiny's "shiny-input-radiogroup" class), but that then calls
# shiny::updateRadioButtons(), which messages a "value" key — the widget's own
# JS binding only reacts to "selected", so the call is a silent no-op. Must be
# restored via shinyWidgets::updateRadioGroupButtons() instead. Call as
# reset_viz_radio_buttons(session, some_id = "default_choice", ...), with each
# default matching the corresponding radioGroupButtons() call's `selected`.
#' @export
reset_viz_radio_buttons <- function(session, ...) {
  defaults <- list(...)
  for (id in names(defaults)) {
    updateRadioGroupButtons(session, id, selected = defaults[[id]])
  }
}

# --- plot snapshot / restore (dashboard "Save Analysis") --------------------

# A plain named list of an engine's control inputs (keyed by namespace-relative
# id), restricted to ids beginning with `prefix`. This is the reproduction
# payload the dashboard stores per saved plot; call it reactively so it reads
# the live control values at save time. Button/trigger inputs sharing the prefix
# come along harmlessly — restore only touches the ids it is told about.
#' @export
collect_input_snapshot <- function(input, prefix) {
  vals <- reactiveValuesToList(input)
  vals[startsWith(names(vals), prefix)]
}

# Restore an engine's controls from a snapshot produced by
# collect_input_snapshot(). Each widget family needs its own update path (see
# the reset checklist at the top of this file for why a blanket approach fails);
# callers pass the ids grouped by family. `colors` is a named vector of ids used
# only to select which snapshot colors to push — the values come from the
# snapshot, applied through the same changestop-emitting handler reset uses so
# both the swatch and input$<id> update. Missing ids in `vals` are skipped, so a
# snapshot from before a control existed restores everything else cleanly.
#' @export
apply_input_snapshot <- function(
  session,
  vals,
  switches = character(),
  selects = character(),
  sliders = character(),
  numerics = character(),
  texts = character(),
  colors = character(),
  radio_groups = character(),
  pretty_radios = character(),
  pickers = character()
) {
  if (is.null(vals)) {
    return(invisible(NULL))
  }
  get <- function(id) if (id %in% names(vals)) vals[[id]] else NULL

  for (id in switches) {
    v <- get(id)
    if (!is.null(v)) update_switch(id, value = isTRUE(v), session = session)
  }
  for (id in selects) {
    v <- get(id)
    if (!is.null(v)) updateSelectInput(session, id, selected = v)
  }
  for (id in sliders) {
    v <- get(id)
    if (!is.null(v)) updateSliderInput(session, id, value = v)
  }
  for (id in numerics) {
    v <- get(id)
    if (!is.null(v)) updateNumericInput(session, id, value = v)
  }
  for (id in texts) {
    v <- get(id)
    if (!is.null(v)) updateTextInput(session, id, value = v)
  }
  for (id in radio_groups) {
    v <- get(id)
    if (!is.null(v)) updateRadioGroupButtons(session, id, selected = v)
  }
  for (id in pretty_radios) {
    v <- get(id)
    if (!is.null(v)) updatePrettyRadioButtons(session, id, selected = v)
  }
  for (id in pickers) {
    v <- get(id)
    if (!is.null(v)) updatePickerInput(session, id, selected = v)
  }

  color_vals <- list()
  for (id in colors) {
    v <- get(id)
    if (!is.null(v)) color_vals[[id]] <- v
  }
  if (length(color_vals)) {
    do.call(reset_viz_colors, c(list(session), color_vals))
  }

  invisible(NULL)
}
