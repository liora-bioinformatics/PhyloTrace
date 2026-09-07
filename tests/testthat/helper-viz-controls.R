# Reading a control panel back out of its own markup.
#
# Every engine's "Reset settings" is a catalogue of the controls its sidebar
# renders (see control_families() in app/logic/viz_helpers.R). A catalogue can
# only be trusted if it is checked against the *panel* rather than against
# itself, which means listing what the panel actually renders — so each engine's
# test file renders its `*_controls(ns)` and compares.

box::use(
  testthat[expect_equal],
)

# Ids that belong to something other than a control: the labels shiny emits, the
# wrappers the CSS and shinyjs address, and renderUI mount points.
NON_CONTROL_SUFFIX <- "-label$|_row$|_wrap$|_hint$|_ui$|_note$"

#' Every input id a control panel renders, minus the scaffolding.
#'
#' @param tags Shiny UI returned by an engine's `*_controls()`.
#' @param prefix Namespace the panel was rendered with.
#' @param drop Character vector of further ids to ignore.
#' @return Character vector of input ids, unnamespaced.
rendered_control_ids <- function(tags, prefix = "x", drop = character()) {
  html <- as.character(tags)
  pattern <- sprintf('id="%s-[^"]+"', prefix)
  ids <- unique(regmatches(html, gregexpr(pattern, html))[[1]])
  ids <- sub('"$', "", sub(sprintf('^id="%s-', prefix), "", ids))
  ids <- ids[!grepl(NON_CONTROL_SUFFIX, ids)]
  setdiff(ids, drop)
}

#' Assert every rendered control appears in the engine's reset catalogue.
#'
#' @param ids From `rendered_control_ids()`.
#' @param defaults The engine's `*_CONTROL_DEFAULTS` list.
expect_catalogued <- function(ids, defaults) {
  expect_equal(setdiff(ids, names(defaults)), character(0))
}

#' Record every input message a module server sends.
#'
#' testServer never round-trips through a browser, so an `update*Input()` is
#' invisible in `input$` — what it did is only observable as the message it put
#' on the wire. The recorder goes on the mock session under the module's own
#' `session`, which is a proxy and refuses assignment; ids therefore arrive
#' already namespaced.
#'
#' @param session The module's session, inside `testServer()`.
#' @return A function returning the messages so far, named by namespaced id.
record_input_messages <- function(session) {
  root <- .subset2(session, "parent")
  sent <- list()
  original <- root$sendInputMessage
  root$sendInputMessage <- function(inputId, message) {
    sent[[inputId]] <<- message
    original(inputId, message)
  }
  function() sent
}
