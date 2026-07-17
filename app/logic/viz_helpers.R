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
# When in doubt, prefer testing the actual "Reset settings" button over
# reasoning about it — bucket 1 is easy to get wrong by assuming it applies
# to something that's actually bucket 4 (any control whose value is ever set
# by an update*Input() call outside the UI declaration is a strong hint it's
# NOT bucket 1).

box::use(
  shiny[div, selectInput, actionButton, icon, hr, tags, HTML, singleton],
  bslib[nav_panel],
  shinyWidgets[colorPickr, updateRadioGroupButtons],
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
  selectInput(
    ns(id),
    "Color scale",
    choices = color_scales[categories],
    width = "100%"
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
