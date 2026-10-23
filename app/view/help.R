# app/view/help.R
#
# Floating "?" button that opens the help page of a module in a portrait,
# paper-like dialog. Placed per module for now; `topic` may also be a reactive,
# so a single instance can later follow whichever module is open.

box::use(
  shiny,
)

box::use(
  app / logic / help_content[help_page],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$actionButton(
    ns("open"),
    label = NULL,
    icon = shiny$icon("question"),
    class = "help-fab",
    title = "Help",
    `aria-label` = "Help"
  )
}

#' @export
server <- function(id, topic) {
  shiny$moduleServer(id, function(input, output, session) {
    # Shows the help page of the current topic.
    shiny$observeEvent(input$open, {
      current <- if (shiny$is.reactive(topic)) topic() else topic
      shiny$showModal(shiny$div(
        class = "help-modal",
        shiny$modalDialog(
          title = NULL,
          shiny$div(class = "help-page", help_page(current)),
          footer = shiny$modalButton("Close"),
          easyClose = TRUE
        )
      ))
    })
  })
}
