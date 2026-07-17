# app/view/amr_screening.R

box::use(
  shiny[NS, moduleServer, observeEvent],
  bslib[page_sidebar, sidebar],
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    sidebar = sidebar(
      title = "Resistance Screening"
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset module state when the user returns to the landing screen.
    # No local reactive state to clear yet; placeholder for future results UI.
    observeEvent(session_reset(), {}, ignoreInit = TRUE)
  })
}
