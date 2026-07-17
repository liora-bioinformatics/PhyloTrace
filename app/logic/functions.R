# app/logic/functions.R

box::use(
  shiny[div, actionButton, icon, tagList],
  bslib[card, card_body, card_header, value_box, value_box_theme],
)

#' Build a vertical sidebar menu of action buttons.
#'
#' Turns a menu definition into the clickable button list used in module
#' sidebars (e.g. the Database menu). Each button id is `menu_<value>`,
#' namespaced through `ns`, so the calling module observes
#' `input[["menu_<value>"]]`. The first entry is marked `active` so the default
#' panel is highlighted on load. The icon is fixed for every item.
#'
#' @param ns Namespace function of the calling module (`session$ns` or `NS(id)`).
#' @param items List of menu entries, each a list with at least `value` (button
#'   id suffix / panel id) and `label` (visible text).
#' @return A `div.sidebar-menu` containing one action button per entry.
#'
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
        # The first item is the default panel, so mark it active on load.
        class = paste("db-menu-item", if (i == 1L) "active")
      )
    })
  )
}

#' A titled card, as used for the side-by-side sections of the database Import
#' and Export panels.
#'
#' @param title Card header text.
#' @param ... Card body contents.
#' @return A `bslib::card()`. `fill = FALSE` because these cards are laid out on
#'   a grid that sizes them; a filling card would fight that for its height.
#' @export
panel_card <- function(title, ...) {
  card(
    fill = FALSE,
    card_header(class = "bg-dark", title),
    card_body(...)
  )
}

#' A single headline figure, as used in the summary rows of the database Import
#' and Export panels.
#'
#' The icon rides in the box's title rather than in its `showcase`: a showcase is
#' laid out by bslib for a box that owns a whole column, and below 300px it stops
#' sitting beside the value and drops onto its own line. These boxes are ~150px by
#' design — five to a card — so the showcase slot is the wrong tool for them.
#'
#' @param label Caption for the figure (e.g. "Novel alleles").
#' @param value The figure itself, pre-formatted by the caller.
#' @param icon_name Name of a Font Awesome icon to set before the caption.
#' @return A `bslib::value_box()`. `fill = FALSE` keeps the box at its content
#'   height: the summary row lays the boxes out on a grid, and a filling box
#'   would stretch to the tallest cell in its row.
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
