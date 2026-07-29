# app/logic/viz_helpers.R
#
# Shared, namespace-agnostic UI helpers and option-set constants for visualization
# modules (app/view/visualization_tree.R and app/view/visualization_mst.R).

box::use(
  shiny[
    div,
    actionButton,
    icon,
    hr,
    tags,
    HTML,
    singleton,
    reactiveValuesToList,
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
    pickerInput
  ],
)

# --- Control Reset Handling Reference ----------------------------------------
# Sidebar controls require specific reset handlers when triggering "Reset settings":
# 1. Static inputs (slider/text/numeric/switch): Handled natively by shinyjs::reset().
# 2. Color pickers (colorPickr): Handled via reset_viz_colors() to trigger JS 'changestop'.
# 3. Radio group buttons: Handled via reset_viz_radio_buttons() (updateRadioGroupButtons).
# 4. Dynamic metadata selects: Reset via populate_metadata_selects() in shinyjs::delay(400)
#    to prevent race conditions with async shinyjs::reset() client round-trips.
# 5. Server-rendered UI (renderUI): Must bump a reactiveVal counter to re-render.
# 6. Non-input reactive state: Reset manually inside the module's reset observer.

# --- Shared Option Sets ------------------------------------------------------

#' Metadata Column Options for Plot Aesthetic Mapping
#' @export
meta_vars <- c("Isolation Date", "Host", "Country", "City", "Database")

#' Metadata/ID Options for Tip Labeling
#' @export
label_vars <- c("Assembly Name", "Assembly ID", meta_vars)

#' Point Shape Mapping Definitions
#' @export
point_shapes <- c(
  Circle = "circle",
  Square = "square",
  Diamond = "diamond",
  Triangle = "triangle",
  Cross = "cross",
  Asterisk = "asterisk"
)

#' Color Palette Definitions Grouped by Scale Category
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

#' Determine Suitable Color Scale Categories
#'
#' Filters available color scale families based on variable type and value distribution.
#' Categorical data defaults to Qualitative; continuous data defaults to Sequential/Gradient,
#' adding Diverging only when values cross zero.
#'
#' @param resolved_type Character. Data type classification ("Factor", "Numeric", etc.).
#' @param vals Vector. Data values for the mapped variable.
#' @return Character vector of suitable palette category names.
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

#' Validate Input Dates
#'
#' Evaluates date inputs to ensure they are single, non-NA Date objects, avoiding
#' runtime evaluation crashes.
#'
#' @param ... Date inputs or vectors to evaluate.
#' @return Logical scalar; TRUE if any input date is invalid or NA, FALSE otherwise.
#' @export
any_invalid_date <- function(...) {
  any(vapply(list(...), function(x) length(x) != 1 || is.na(x), logical(1)))
}

# --- UI Components -----------------------------------------------------------

# Custom JS handler emitting 'changestop' events for pickr color inputs.
# Programmatic pickr.setColor() calls emit 'save' rather than 'changestop', which
# leaves Shiny bindings and UI swatch state out of sync without this listener.
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

#' Labelled Color Picker UI Row
#'
#' Renders a single-row color picker widget paired with a label.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param id Character. Input ID.
#' @param label Character. Field label display text.
#' @param value Character. Initial hex color string.
#' @return Shiny UI tag list.
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

#' Grouped Palette Selector UI Component
#'
#' Renders a dropdown picker for selecting grouped visual color scales.
#' Uses `.viz-scale-select` CSS class for global styling across instances.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param id Character. Input ID.
#' @param categories Character vector. Palette categories to expose (default: all).
#' @return Shiny UI tag list.
#' @export
scale_select <- function(ns, id, categories = names(color_scales)) {
  div(
    class = "viz-scale-select",
    pickerInput(
      ns(id),
      "Color scale",
      choices = color_scales[categories],
      width = "100%"
    )
  )
}

#' Shared Visualization Export Panel UI
#'
#' Renders a standard plot export tab panel containing format selection and download trigger.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param prefix Character. ID prefix unique to the calling submodule.
#' @param formats Character vector. Allowed export file formats (e.g., png, pdf, svg).
#' @return bslib `nav_panel` UI element.
#' @export
export_panel <- function(ns, prefix, formats) {
  nav_panel(
    "Export",
    icon = icon("download"),
    div(
      class = "viz-export",
      pickerInput(ns(paste0(prefix, "_filetype")), "File format", formats),
      actionButton(
        ns(paste0(prefix, "_download")),
        "Save plot",
        icon = icon("download")
      )
    )
  )
}

# --- State Reset Helpers -----------------------------------------------------

#' Reset Color Pickers to Default Hex Values
#'
#' Emits custom JS messages to reset colorPickr elements and force-emit `changestop`.
#'
#' @param session Shiny session object.
#' @param ... Named hex values where parameter names match input IDs.
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

#' Reset Radio Group Buttons to Default Selection
#'
#' Restores radioGroupButtons widgets using their native Shiny binding update path.
#'
#' @param session Shiny session object.
#' @param ... Named choice strings where parameter names match input IDs.
#' @export
reset_viz_radio_buttons <- function(session, ...) {
  defaults <- list(...)
  for (id in names(defaults)) {
    updateRadioGroupButtons(session, id, selected = defaults[[id]])
  }
}

# --- Plot Snapshot & Restoration Helpers ------------------------------------

#' Collect Module Input Snapshot
#'
#' Filters and captures current values of all inputs sharing a specified prefix
#' for session saving/restoration.
#'
#' @param input Shiny input object.
#' @param prefix Character. Prefix filtering relevant input IDs.
#' @return Named list of matching input values.
#' @export
collect_input_snapshot <- function(input, prefix) {
  vals <- reactiveValuesToList(input)
  vals[startsWith(names(vals), prefix)]
}

#' Apply Saved Input Snapshot to Restore UI State
#'
#' Restores saved input state across multiple Shiny widget types using their
#' respective specific update procedures.
#'
#' @param session Shiny session object.
#' @param vals Named list. Input values captured by `collect_input_snapshot()`.
#' @param switches Character vector of switch input IDs.
#' @param selects Character vector of standard select input IDs.
#' @param sliders Character vector of slider input IDs.
#' @param numerics Character vector of numeric input IDs.
#' @param texts Character vector of text input IDs.
#' @param colors Character vector of color pickr input IDs.
#' @param radio_groups Character vector of radio group button input IDs.
#' @param pretty_radios Character vector of pretty radio button input IDs.
#' @param pickers Character vector of picker input IDs.
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
    if (!is.null(v)) updatePickerInput(session, id, selected = v)
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
