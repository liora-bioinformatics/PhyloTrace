# app/view/analysis_dashboard/group.R
# Tier 2: one Analysis container. Holds the saved Plot boxes for a single
# `analysis_id` and offers "Add Plot" (which hands off to Visualization).
#
# Data-driven like the item tier: the plot list comes from the database and is
# refreshed on the shared `plots_changed` tick. A plot's item server is created
# exactly once (tracked in `instantiated`); deleted plots simply stop rendering.
# Servers persist across database reloads and re-read `db_path()` reactively, so
# reusing a container id for a different database is safe.

box::use(
  shiny[
    NS,
    actionButton,
    div,
    icon,
    isolate,
    modalButton,
    modalDialog,
    moduleServer,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    removeModal,
    renderUI,
    req,
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
  app / logic / analysis_store,
  jsonlite[fromJSON],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("group_card_layout"))
}

#' @export
server <- function(
  id,
  analysis_id,
  db_path = shiny::reactive(NULL),
  plots_changed = shiny::reactiveVal(0L),
  on_add_plot = function(analysis_id) NULL,
  on_open_plot = function(plot_id) NULL,
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_editing_name <- reactiveVal(FALSE)

    # This Analysis's row (for its name) and its plots, refreshed on any change.
    analysis_row <- reactive({
      plots_changed()
      analysis_store$get_analysis(db_path(), analysis_id)
    })

    plots <- reactive({
      plots_changed()
      analysis_store$list_plots(db_path(), analysis_id)
    })

    observeEvent(session_reset(), is_editing_name(FALSE), ignoreInit = TRUE)

    # Instantiate an item server once per plot id it hasn't been seen for.
    instantiated <- reactiveVal(integer(0))
    observe({
      ids <- plots()$id
      isolate({
        new_ids <- setdiff(ids, instantiated())
        for (pid in new_ids) {
          local({
            this_pid <- pid
            item$server(
              id = paste0("box_", this_pid),
              plot_id = this_pid,
              db_path = db_path,
              plots_changed = plots_changed,
              on_open = on_open_plot,
              session_reset = session_reset
            )
          })
        }
        if (length(new_ids)) {
          instantiated(c(instantiated(), new_ids))
        }
      })
    })

    # "Add Plot" hands off to the Visualization module for this Analysis.
    observeEvent(input$add_box_btn, {
      on_add_plot(analysis_id)
    })

    # Inline Analysis rename.
    observeEvent(input$toggle_group_edit, {
      if (is_editing_name()) {
        new_name <- input$group_name_input
        if (!is.null(new_name) && nzchar(new_name)) {
          analysis_store$rename_analysis(db_path(), analysis_id, new_name)
          plots_changed(plots_changed() + 1L)
        }
        is_editing_name(FALSE)
      } else {
        is_editing_name(TRUE)
      }
    })

    observeEvent(input$trigger_delete_group, {
      row <- analysis_row()
      showModal(modalDialog(
        title = "⚠️ Delete Analysis",
        paste0(
          "Warning: this permanently deletes the Analysis '",
          if (is.null(row)) "this analysis" else row$name,
          "' and every plot saved inside it."
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_group"),
            "Delete Analysis",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_group, {
      removeModal()
      analysis_store$delete_analysis(db_path(), analysis_id)
      plots_changed(plots_changed() + 1L)
    })

    output$group_card_layout <- renderUI({
      row <- analysis_row()
      # Analysis was deleted: render nothing (dashboard drops it on refresh).
      if (is.null(row)) {
        return(NULL)
      }

      ids <- plots()$id
      box_uis <- lapply(ids, function(i) {
        item$ui(ns(paste0("box_", i)))
      })

      add_card_placeholder <- value_box(
        class = "add-box",
        title = NULL,
        value = actionButton(
          ns("add_box_btn"),
          "Add Plot",
          icon = icon("plus"),
          class = "btn-primary"
        ),
        theme = "secondary"
      )

      name_content <- if (is_editing_name()) {
        # allow-free-text: an Analysis name is display text, so it takes spaces
        # and punctuation — it opts out of the identifier charset restriction in
        # app/js/index.js.
        div(
          class = "allow-free-text",
          textInput(
            ns("group_name_input"),
            label = NULL,
            value = row$name,
            width = "100%"
          )
        )
      } else {
        tags$strong(row$name)
      }

      edit_icon <- if (is_editing_name()) icon("check") else icon("pencil")

      card(
        card_header(
          div(
            class = "ad-group-header",
            span(class = "ad-group-name", name_content),
            div(
              class = "ad-group-header-actions",
              actionButton(
                ns("toggle_group_edit"),
                label = NULL,
                icon = edit_icon,
                class = "btn-sm btn-light",
                title = "Rename Analysis"
              ),
              actionButton(
                ns("trigger_delete_group"),
                "Delete Analysis",
                icon = icon("folder-minus"),
                class = "btn-sm btn-danger"
              )
            )
          )
        ),
        card_body(
          div(
            class = "ad-group-meta",
            span(paste("Created:", row$created)),
            " | ",
            span(paste("Last Modified:", row$modified)),
            {
              raw <- row$isolate_selection
              if (!is.null(raw) && length(raw) == 1 && !is.na(raw)) {
                n <- length(fromJSON(raw))
                span(
                  class = "ad-static-badge",
                  icon("lock"),
                  sprintf(" %d fixed isolate%s", n, if (n == 1) "" else "s")
                )
              }
            }
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
