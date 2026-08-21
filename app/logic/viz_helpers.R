# app/logic/viz_helpers.R
#
# Shared, namespace-agnostic UI helpers and option-set constants for visualization
# modules (app/view/visualization_tree.R and app/view/visualization_mst.R).

box::use(
  shiny[
    div,
    icon,
    tags,
    HTML,
    singleton,
    reactiveValuesToList,
    updateSliderInput,
    updateNumericInput,
    updateTextInput,
  ],
  bslib[update_switch],
  RColorBrewer[brewer.pal, brewer.pal.info],
  stats[setNames],
  shinyWidgets[
    colorPickr,
    updateRadioGroupButtons,
    updatePrettyRadioButtons,
    updatePickerInput,
    updateVirtualSelect,
    pickerInput,
    virtualSelectInput
  ],
  viridisLite[viridis],
  rlang[`%||%`],
)

box::use(
  app / logic / date_bins[DATE_GRANULARITIES, DATE_GRANULARITY_NONE],
  app / logic / field_profile[profile_description],
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

# Inline `style` swatch for one palette's <option>, previewing the actual
# colours it renders (RColorBrewer's tabulated stops, or a sampled viridis
# ramp) rather than a value hardcoded separately from the plotting code. Brewer
# families get hard-edged bands (they're discrete palettes); viridis families
# blend smoothly (they're continuous ones). Passed through pickerInput's
# `style` choicesOpt, which bootstrap-select sets as the `style` attribute of
# the rendered <a class="dropdown-item">) itself — the option row's real,
# full-width box — rather than on some nested span that would only ever cover
# its own shrink-wrapped content. The picker's toggle button doesn't inherit
# an option's style this way, so app/js/viz-scale-swatch.js mirrors it across
# on selection.
.scale_swatch_style <- function(name) {
  gradient <- name %in% color_scales$Gradient
  cols <- if (gradient) {
    viridis(20, option = name)
  } else {
    brewer.pal(brewer.pal.info[name, "maxcolors"], name)
  }
  stops <- if (gradient) {
    paste(cols, collapse = ", ")
  } else {
    n <- length(cols)
    step <- 100 / n
    paste(
      vapply(seq_len(n), function(i) {
        sprintf("%s %g%%, %s %g%%", cols[i], (i - 1) * step, cols[i], i * step)
      }, character(1)),
      collapse = ", "
    )
  }
  sprintf(
    "background: linear-gradient(to right, %s); color: %s;",
    stops,
    if (gradient) "white" else "black"
  )
}

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
    # The row carries an id so a module can grey out a swatch whose element is
    # switched off. It has to be the *row*: colorPickr puts `id` on a hidden
    # input and renders the swatch as a sibling, so shinyjs::toggleState() on
    # the input disables nothing the user can see or click.
    id = ns(paste0(id, "_row")),
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

# --- Metadata field pickers ---------------------------------------------------

# Turn a profile frame into virtual-select choices, optionally prefixed with a
# sentinel entry ("No stratification", "No annotation") that is not a field.
#
# Assembled in virtual-select's own nested format rather than through
# prepare_choices(): that helper puts *every* row in a group, so the sentinel
# could only travel as a one-entry group of its own, and virtual-select draws a
# title row for every group it is given. A blank group label therefore rendered
# as a blank, unselectable row above the sentinel. Here the sentinel is a
# plain top-level option and only the profile rows carry groups.
#
# The structure reaches the widget untouched (an unrecognised `type` is passed
# through verbatim by the JS binding), so each option has to be a complete
# object rather than the column-wise form prepare_choices() emits.
.field_choices <- function(profiles, extra = NULL) {
  # `alias` is searched but never drawn (virtual-select lowercases it and asks
  # `includes`). It carries the heading, so typing a drug class finds the class
  # column *and* every gene in it — searching only labels found the one row
  # whose name happened to be the class, which is the opposite of what someone
  # typing "carbapenemase" is after.
  option <- function(label, value, description, alias = "") {
    list(
      label = label,
      value = value,
      description = description,
      alias = alias
    )
  }
  description <- profile_description(profiles)
  # One heading per group, except where a group hands out its own: an AMR screen
  # is dozens of columns under "AMR screening", and a flat list of them is the
  # thing this picker exists to avoid. `subgroup` files each one under its drug
  # class instead, so the class and the genes in it arrive together and a reader
  # can see what belongs to what. Blank for every other column, which is how
  # those keep their single group heading.
  sub <- as.character(profiles$subgroup %||% rep("", nrow(profiles)))
  sub[is.na(sub)] <- ""
  heading <- ifelse(nzchar(sub), sub, as.character(profiles$group))
  # `unique()` and not `sort()`: the frame arrives in the order the pickers
  # present it (see field_profiles()), and re-sorting here would split a drug
  # class from the group it was filed under.
  choices <- lapply(unique(heading), function(name) {
    rows <- which(heading == name)
    list(
      label = name,
      options = lapply(rows, function(i) {
        option(
          profiles$label[[i]],
          profiles$field[[i]],
          description[[i]],
          alias = sub[[i]]
        )
      })
    )
  })
  if (length(extra)) {
    sentinels <- lapply(seq_along(extra), function(i) {
      option(names(extra)[[i]], unname(extra)[[i]], "")
    })
    choices <- c(sentinels, choices)
  }
  structure(
    list(choices = choices, type = "formatted"),
    class = c("list", "vs_choices")
  )
}

#' Metadata Field Picker
#'
#' A grouped, searchable single-select over the database's own columns, each
#' option carrying its distinct-value count and declared type as a second line
#' ("46 values · Text"). Shared by every visualization engine so a variable
#' describes itself identically wherever it is offered.
#'
#' Columns that cannot group the isolates are listed but disabled, with the
#' reason in their sub-text — hiding them is what left users hunting for a
#' variable that was simply absent.
#'
#' @param ns Function. Module namespace function.
#' @param id Character. Input ID.
#' @param label Character. Control label.
#' @param profiles Data frame from `field_profile$field_profiles()`, or NULL.
#' @param selected Character. Initially selected value.
#' @param extra Named character vector of sentinel entries (name = label).
#' @param placeholder Character. Empty-state text.
#' @return A `virtualSelectInput`.
#' @export
field_select <- function(
  ns,
  id,
  label,
  profiles = NULL,
  selected = NULL,
  extra = NULL,
  placeholder = "Pick a variable ..."
) {
  has <- !is.null(profiles) && nrow(profiles) > 0L
  virtualSelectInput(
    ns(id),
    label,
    choices = if (has) .field_choices(profiles, extra) else as.list(extra),
    selected = selected %||% character(0),
    multiple = FALSE,
    search = TRUE,
    searchPlaceholderText = "Search variables ...",
    placeholder = placeholder,
    # Not a formal — reaches the widget config through `...`. Turns on the
    # second line of each option.
    hasOptionDescription = TRUE,
    # Defaults to TRUE for a single select, which would silently pick whatever
    # sorts first the moment the choices land.
    autoSelectFirstOption = FALSE,
    # Eight rows, not five: with the AMR screen filed one drug class per
    # heading, a five-row box shows barely one class at a time.
    optionsCount = 8,
    dropboxWrapper = "body",
    showDropboxAsPopup = TRUE,
    popupDropboxBreakpoint = "10000px",
    width = "100%"
  )
}

#' Refill a `field_select()` from a profile frame.
#'
#' @param session Shiny session object.
#' @param id Character. Input ID (unnamespaced).
#' @param profiles Data frame from `field_profile$field_profiles()`.
#' @param selected Character. Value to select.
#' @param extra Named character vector of sentinel entries.
#' @export
update_field_select <- function(
  session,
  id,
  profiles,
  selected = NULL,
  extra = NULL
) {
  if (is.null(profiles) || !nrow(profiles)) {
    return(invisible(NULL))
  }
  # Note the argument order: updateVirtualSelect() takes `inputId` first and
  # `session` last, the opposite of updatePickerInput(). Naming both is what
  # keeps a copy-paste from the picker version from silently passing the
  # session in as an id.
  updateVirtualSelect(
    inputId = id,
    session = session,
    choices = .field_choices(profiles, extra),
    selected = selected %||% character(0),
    disabledChoices = profiles$field[!profiles$groupable]
  )
}

#' Grouped Palette Selector UI Component
#'
#' Renders a dropdown picker for selecting grouped visual color scales, each
#' option previewing its own colours via an inline gradient swatch.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param id Character. Input ID.
#' @param categories Character vector. Palette categories to expose (default: all).
#' @param selected Character. Palette to preselect (default: none, first choice wins).
#' @return Shiny UI tag list.
#' @export
scale_select <- function(ns, id, categories = names(color_scales), selected = NULL) {
  palettes <- unlist(color_scales[categories], use.names = FALSE)
  div(
    class = "viz-scale-select",
    pickerInput(
      ns(id),
      "Color scale",
      choices = color_scales[categories],
      selected = selected,
      choicesOpt = list(style = vapply(palettes, .scale_swatch_style, character(1))),
      # Rendered into <body> (see main.scss's "Dropdown overflow" rules) so the
      # long option list is capped and scrolled against the viewport instead
      # of expanding whatever small container (often a modal) it opens in.
      options = list(container = "body"),
      width = "100%"
    )
  )
}

#' Calendar-interval selector for a mapped date variable.
#'
#' A collection date is near-unique per isolate, so raw it groups nothing.
#' This is the control that coarsens it into something a legend, a pie or a
#' palette can carry. Shown only for date columns; every engine uses this one
#' control so the option reads the same wherever a date can be mapped.
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param id Character. Input ID.
#' @param selected Character. Granularity to preselect, or NULL for "none".
#' @param label Character. Control label.
#' @param allow_none Logical. Offer the ungrouped option (a continuous scale).
#' @return Shiny UI tag.
#' @export
granularity_select <- function(ns, id, selected = NULL, label = "Group dates by",
                               allow_none = TRUE) {
  choices <- as.list(DATE_GRANULARITIES)
  if (allow_none) {
    choices <- c(setNames(list(DATE_GRANULARITY_NONE), "Exact date"), choices)
  }
  if (is.null(selected)) {
    selected <- DATE_GRANULARITY_NONE
  }
  div(
    class = "viz-granularity-select",
    pickerInput(
      ns(id),
      label,
      choices = choices,
      selected = selected,
      options = list(container = "body"),
      width = "100%"
    )
  )
}

#' Row-action button that reports which record it belongs to.
#'
#' The record id travels in the *value* rather than in the button's own input id,
#' so one observer serves every row however many times the list re-renders — an
#' observeEvent created inside renderUI is re-registered on every render, which
#' is how a two-click delete happens.
#'
#' @param ns Function. Module namespace function.
#' @param input_id Character. Shared input the click writes to.
#' @param record_id Character. Value the click writes.
#' @param icon_name Character. Font Awesome icon name.
#' @param title Character. Tooltip and accessible label.
#' @return A `<button>` tag.
#' @export
layer_action_btn <- function(ns, input_id, record_id, icon_name, title) {
  tags$button(
    type = "button",
    class = "btn btn-sm tree-layer_btn",
    title = title,
    `aria-label` = title,
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ns(input_id),
      record_id
    ),
    icon(icon_name)
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
