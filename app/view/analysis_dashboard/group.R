# app/view/analysis_dashboard/group.R
# Analysis group container module. Holds one or more item modules.

box::use(
  shiny[
    NS,
    div,
    actionButton,
    icon,
    modalButton,
    modalDialog,
    moduleServer,
    observeEvent,
    reactiveVal,
    reactiveValues,
    removeModal,
    renderUI,
    showModal,
    span,
    tags,
    textInput,
    uiOutput,
  ],
  bslib[
    card,
    card_body,
    card_header,
    layout_column_wrap,
    value_box,
  ],
  app / view / analysis_dashboard / item,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("group_card_layout"))
}

#' @export
server <- function(id, assigned_name, remove_group_callback, session_reset = shiny::reactive(0L)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_name <- reactiveVal(assigned_name)
    created_timestamp <- Sys.time()
    modified_timestamp <- reactiveVal(Sys.time())

    box_counter <- reactiveVal(1)
    active_boxes <- reactiveVal(1)

    touch_group <- function() {
      modified_timestamp(Sys.time())
    }

    # Reset all value boxes back to a single box with default state and title.
    observeEvent(session_reset(), {
      box_counter(1)
      active_boxes(1)
      group_name(assigned_name)
      modified_timestamp(Sys.time())
    }, ignoreInit = TRUE)

    local({
      initial_box_id <- 1
      item$server(
        id = paste0("box_", initial_box_id),
        default_index = initial_box_id,
        remove_box_callback = function() {
          active_boxes(setdiff(active_boxes(), initial_box_id))
          touch_group()
        },
        session_reset = session_reset
      )
    })

    observeEvent(input$add_box_btn, {
      new_id <- box_counter() + 1
      box_counter(new_id)

      local({
        current_id <- new_id
        item$server(
          id = paste0("box_", current_id),
          default_index = current_id,
          remove_box_callback = function() {
            active_boxes(setdiff(active_boxes(), current_id))
            touch_group()
          },
          session_reset = session_reset
        )
      })

      active_boxes(c(active_boxes(), new_id))
      touch_group()
    })

    observeEvent(input$trigger_delete_group, {
      showModal(modalDialog(
        title = "⚠️ Delete Whole Group",
        paste0(
          "Warning: This will destroy the group '",
          group_name(),
          "' and all nested analyses inside it."
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_group"),
            "Delete Group",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_group, {
      removeModal()
      remove_group_callback()
    })

    output$group_card_layout <- renderUI({
      ids <- active_boxes()

      box_uis <- lapply(ids, function(i) {
        item$ui(ns(paste0("box_", i)))
      })

      add_card_placeholder <- value_box(
        class = "add-box",
        title = NULL,
        value = actionButton(
          ns("add_box_btn"),
          "Add Analysis",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        theme = "secondary"
      )

      card(
        card_header(
          div(
            class = "ad-group-header",
            span(class = "ad-group-name", tags$strong(group_name())),
            actionButton(
              ns("trigger_delete_group"),
              "Delete Group",
              icon = icon("folder-minus"),
              class = "btn-sm btn-danger"
            )
          )
        ),
        card_body(
          div(
            class = "ad-group-meta",
            span(paste(
              "Created:",
              format(created_timestamp, "%Y-%m-%d %H:%M:%S")
            )),
            " | ",
            span(paste(
              "Last Modified:",
              format(modified_timestamp(), "%Y-%m-%d %H:%M:%S")
            ))
          ),
          layout_column_wrap(
            width = "250px",
            !!!c(box_uis, list(add_card_placeholder))
          )
        )
      )
    })
  })
}
