# app/logic/functions.R
#
# Shared UI component generators and helper utilities for Shiny modules.

box::use(
  shiny[div, actionButton, icon, tagList],
  bslib[
    as_fill_carrier,
    card,
    card_body,
    card_header,
    value_box,
    value_box_theme
  ],
)

#' Build a Vertical Sidebar Menu of Action Buttons
#'
#' Turns a menu definition into the clickable button list used in module
#' sidebars (e.g. the Database menu). Each button id is `menu_<value>`,
#' namespaced through `ns`, so the calling module observes
#' `input[["menu_<value>"]]`. The first entry is marked `active` so the default
#' panel is highlighted on load.
#'
#' @param ns Namespace function of the calling module (`session$ns` or `NS(id)`).
#' @param items List of menu entries, each a list with at least `value` (button
#'   id suffix / panel id) and `label` (visible text).
#' @return A `div.sidebar-menu` containing one action button per entry.
#' @export
sidebar_menu <- function(ns, items) {
  div(
    class = "sidebar-menu",
    lapply(seq_along(items), function(i) {
      item <- items[[i]]
      actionButton(
        ns(paste0("menu_", item$value)),
        label = item$label,
        icon = icon("caret-right"),
        class = paste("db-menu-item", if (i == 1L) "active")
      )
    })
  )
}

#' Create Titled Container Card
#'
#' Used for stacked UI sections such as the database Import and Export panels.
#'
#' @param title Card header text string.
#' @param ... Card body contents.
#' @param fill Logical indicating if the card should grow to fill vertical space.
#'   Defaults to `FALSE` to fit content dimensions.
#' @return A `bslib::card()` UI element.
#' @export
panel_card <- function(title, ..., fill = FALSE) {
  card(
    fill = fill,
    card_header(title),
    card_body(...)
  )
}

#' Create Fillable Layout Container for Transfer Cards
#'
#' Wraps elements in `bslib::as_fill_carrier()` to maintain vertical fill CSS
#' inheritance down the DOM tree in Import and Export summary views.
#'
#' @param ... Content elements, typically `transfer_row()` calls and panel cards.
#' @return A `div.transfer-cards` fill carrier element.
#' @export
transfer_cards <- function(...) {
  as_fill_carrier(div(class = "transfer-cards", ...))
}

#' Build Headline Metric Value Box
#'
#' Renders a compact summary tile for key metrics. Icons are embedded in the header
#' label rather than the showcase slot to prevent line breaks at small widths.
#'
#' @param label Display title for the metric.
#' @param value Pre-formatted character or numeric value.
#' @param icon_name Optional Font Awesome icon name.
#' @return A `bslib::value_box()` UI element with fixed height (`fill = FALSE`).
#' @export
stat_tile <- function(label, value, icon_name = NULL) {
  value_box(
    title = if (is.null(icon_name)) {
      label
    } else {
      tagList(icon(icon_name), label)
    },
    value = value,
    theme = value_box_theme(
      bg = "var(--bs-tertiary-bg)",
      fg = "var(--bs-body-color)"
    ),
    fill = FALSE,
    class = "stat-box"
  )
}

#' Log UI Rendering Execution
#'
#' Prints a formatted timestamped log message to the console when rendering outputs.
#'
#' @param output String identifier of the UI output being rendered.
#' @export
render_info <- function(output) {
  message(
    format(Sys.time(), digits = 3L),
    " | ",
    "----- Rendering '",
    output,
    "' UI"
  )
}
