# app/logic/viz_layers.R
#
# The variable-mapping layer list, shared by every engine that lets a reader map
# metadata onto a plot. What a layer *means* lives in app/logic/mapping_engine.R
# (which aesthetic, which palette); what a layer *looks like* in the sidebar,
# and how a saved one is rebuilt, lives here — so the Tree, the MST and the AMR
# heatmap present the same card, the same buttons and the same summary line
# rather than three near-copies that drift apart.

box::use(
  rlang[`%||%`],
  shiny[div],
)
box::use(
  app / logic / date_bins[granularity_label],
  app / logic / mapping_engine[aesthetic_labels, normalize_layer_records],
  app / logic / viz_helpers[layer_action_btn],
)

#' Canonical shape of one mapping layer.
#'
#' Every engine's snapshot/restore path has to rebuild exactly this, so it is
#' written once. An engine whose default channel is not the tree's overrides
#' `aesthetic` on top of this rather than restating the whole record.
#' @export
LAYER_DEFAULTS <- list(
  id = NA_character_,
  field = NA_character_,
  title = NA_character_,
  aesthetic = NA_character_,
  palette = "viridis",
  family = "Gradient",
  n_levels = 1L,
  continuous = FALSE,
  transform = NULL,
  granularity = NULL,
  auto = TRUE
)

#' Layer defaults for one medium, with its own opening channel filled in.
#'
#' @param medium Name of a `mapping_engine$MAPPING_MEDIA` entry.
#' @param ... Fields to override.
#' @return A named list shaped like `LAYER_DEFAULTS`.
#' @export
layer_defaults <- function(medium, ...) {
  out <- LAYER_DEFAULTS
  out$aesthetic <- names(aesthetic_labels(medium))[[1]]
  overrides <- list(...)
  for (f in names(overrides)) {
    out[[f]] <- overrides[[f]]
  }
  out
}

#' Rebuild a saved snapshot's layers into records this medium can draw.
#'
#' Two things go wrong with a snapshot and both are handled here rather than in
#' each engine: jsonlite hands a JSON array back as a data frame rather than the
#' list-of-lists the reactiveVal holds, and a saved Analysis may name a channel
#' or a column that no longer exists. A layer on a withdrawn channel is handed
#' back to the engine as an automatic one — dropping it would silently lose a
#' mapping the user asked for — while a layer whose *field* is gone cannot be
#' drawn at all and is removed.
#'
#' @param x A list, a data frame, or NULL.
#' @param defaults Named list from `layer_defaults()`.
#' @param medium Name of a `mapping_engine$MAPPING_MEDIA` entry.
#' @return List of layer records, or NULL when `x` carried nothing.
#' @export
normalize_layers <- function(x, defaults, medium) {
  out <- normalize_layer_records(x, defaults)
  if (is.null(out)) {
    return(NULL)
  }
  channels <- names(aesthetic_labels(medium))
  out <- lapply(out, function(l) {
    if (!isTRUE(l$aesthetic %in% channels)) {
      l$aesthetic <- defaults$aesthetic
      l$auto <- TRUE
    }
    l
  })
  Filter(function(l) !is.na(l$field %||% NA), out)
}

# The one-line summary under a layer's title: the channel it draws on, how many
# values it carries, the calendar interval a date is grouped by and the palette.
# Assembled here so a mapping reads identically in every sidebar.
.layer_meta <- function(layer, labels, legend_max = NULL) {
  parts <- c(
    labels[[layer$aesthetic]] %||% layer$aesthetic,
    sprintf("%d values", layer$n_levels),
    if (!is.null(granularity_label(layer$granularity))) {
      paste("by", tolower(granularity_label(layer$granularity)))
    },
    layer$palette,
    # A guide lists at most `legend_max` values and counts the rest. Said here
    # too, so a short key list beside a long variable does not read as a fault.
    if (!is.null(legend_max) && isTRUE(layer$n_levels > legend_max)) {
      sprintf("%d listed", legend_max)
    }
  )
  paste(parts, collapse = " \u00b7 ")
}

#' The sidebar's list of mapping layers.
#'
#' One card per layer, each with an edit and a remove button. Both buttons carry
#' the layer id in their *value* rather than in their own input id, so one
#' observer per action serves every row however many times the list re-renders
#' (`viz_helpers$layer_action_btn`).
#'
#' @param ns Function. Module namespace function (`session$ns`).
#' @param layers List of layer records, in draw order.
#' @param medium Name of a `mapping_engine$MAPPING_MEDIA` entry.
#' @param edit_id Character. Input the edit button writes to.
#' @param delete_id Character. Input the remove button writes to.
#' @param empty_text Character. Shown when nothing is mapped yet.
#' @param legend_max Integer or NULL. Keys a guide draws before it starts
#'   counting the remainder.
#' @return A Shiny tag.
#' @export
layer_cards <- function(
  ns,
  layers,
  medium,
  edit_id,
  delete_id,
  empty_text = "No mappings yet.",
  legend_max = NULL
) {
  if (!length(layers)) {
    return(div(class = "text-muted fst-italic mb-2 tree-layer-empty", empty_text))
  }
  labels <- aesthetic_labels(medium)
  div(
    class = "tree-layer-list",
    lapply(layers, function(l) {
      div(
        class = "tree-layer-card",
        div(
          class = "tree-layer_body",
          div(class = "tree-layer_title", title = l$title, l$title),
          div(class = "tree-layer_meta", .layer_meta(l, labels, legend_max))
        ),
        layer_action_btn(ns, edit_id, l$id, "pen", "Edit mapping"),
        layer_action_btn(ns, delete_id, l$id, "xmark", "Remove mapping")
      )
    })
  )
}

#' Whether a layer set already holds this variable.
#'
#' One variable, one layer: mapping the same column twice draws the same marks
#' over themselves and produces two identical legend entries.
#'
#' @param layers List of layer records.
#' @param field Column name.
#' @return TRUE when the field is already mapped.
#' @export
layer_has_field <- function(layers, field) {
  any(vapply(layers, function(l) identical(l$field, field), logical(1)))
}

#' Drop one layer by id.
#'
#' @param layers List of layer records.
#' @param id Layer id.
#' @return The list without it.
#' @export
drop_layer <- function(layers, id) {
  Filter(function(l) !identical(l$id, id), layers)
}

#' Fetch one layer by id.
#'
#' @param layers List of layer records.
#' @param id Layer id.
#' @return The record, or NULL.
#' @export
find_layer <- function(layers, id) {
  hit <- Filter(function(l) identical(l$id, id), layers)
  if (length(hit)) hit[[1]] else NULL
}

#' A layer id generator backed by a counter reactiveVal.
#'
#' Ids must never be reused: a stale card's button would otherwise address the
#' layer that replaced it.
#'
#' @param seq_val A `shiny::reactiveVal` holding the counter.
#' @return A function returning the next id.
#' @export
layer_id_source <- function(seq_val) {
  function() {
    n <- seq_val() + 1L
    seq_val(n)
    paste0("L", n)
  }
}
