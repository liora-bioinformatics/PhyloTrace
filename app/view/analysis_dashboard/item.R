# app/view/analysis_dashboard/item.R
# Individual analysis value box module.

box::use(
  shiny[
    NS,
    actionButton,
    div,
    icon,
    modalButton,
    modalDialog,
    moduleServer,
    observeEvent,
    p,
    reactiveVal,
    removeModal,
    renderUI,
    showModal,
    textInput,
    uiOutput,
  ],
  bslib[
    value_box,
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("box_container"))
}

#' @export
server <- function(
  id,
  default_index,
  remove_box_callback,
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define reactives
    title_text <- reactiveVal(paste("Analysis", default_index))
    is_editing <- reactiveVal(FALSE)

    analysis_classes <- c(
      "Tree",
      "MST",
      "AMR",
      "Note",
      "Summary"
    )

    # Reset the value box to its default label and close any open edit field.
    observeEvent(
      session_reset(),
      {
        title_text(paste("Analysis", default_index))
        is_editing(FALSE)
      },
      ignoreInit = TRUE
    )

    observeEvent(input$toggle_edit, {
      if (is_editing()) {
        if (nzchar(input$title_input)) {
          title_text(input$title_input)
        }
        is_editing(FALSE)
      } else {
        is_editing(TRUE)
      }
    })

    observeEvent(input$trigger_delete_box, {
      showModal(modalDialog(
        title = "Delete Analysis",
        paste0(
          "Are you sure you want to permanently delete '",
          title_text(),
          "'?"
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_box"),
            "Delete Element",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_box, {
      removeModal()
      remove_box_callback()
    })

    output$box_container <- renderUI({
      header_content <- if (is_editing()) {
        textInput(
          ns("title_input"),
          label = NULL,
          value = title_text(),
          width = "100%"
        )
      } else {
        title_text()
      }

      btn_icon <- if (is_editing()) icon("check") else icon("pencil")

      value_box(
        title = header_content,
        showcase = icon("circle-nodes"),
        theme = "teal",
        value = NULL,
        full_screen = TRUE,
        p(paste("Created:", Sys.Date())),
        p(paste("Last Modified", Sys.time())),
        div(
          class = "ad-box-actions",
          actionButton(
            ns("toggle_edit"),
            label = NULL,
            icon = btn_icon,
            class = "btn-sm btn-light"
          ),
          actionButton(
            ns("trigger_delete_box"),
            label = NULL,
            icon = icon("trash"),
            class = "btn-sm btn-light"
          )
        )
      )
    })
  })
}
