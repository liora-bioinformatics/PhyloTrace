# app/view/analysis_dashboard/item.R
# Tier 1: a single saved Plot, rendered as a value box with a live thumbnail.
#
# The box is data-driven: it reads its own row from the database (keyed by
# `plot_id`) and re-reads whenever the shared `plots_changed` tick advances, so
# an overwrite (new thumbnail), rename, or delete elsewhere is reflected here
# without re-instantiating the module. Clicking the thumbnail (or the open
# button) bubbles an "open this plot" request up to the dashboard, which asks
# the Visualization module to restore it.

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
    value_box,
  ],
  app / logic / analysis_store,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("box_container"))
}

# The plot miniature, or a neutral placeholder when no thumbnail was captured.
.thumb_ui <- function(b64, ns) {
  has_thumb <- !is.null(b64) && length(b64) == 1 && !is.na(b64) && nzchar(b64)
  inner <- if (has_thumb) {
    tags$img(
      src = paste0("data:image/png;base64,", b64),
      class = "ad-plot-thumb",
      alt = "plot preview"
    )
  } else {
    div(class = "ad-plot-thumb ad-plot-thumb-empty", icon("image"))
  }

  # Clicking the preview triggers the (visually hidden) open button.
  div(
    class = "ad-plot-thumb-wrap",
    title = "Open in Visualization",
    onclick = sprintf(
      "document.getElementById('%s').click()",
      ns("open_plot")
    ),
    inner
  )
}

#' @export
server <- function(
  id,
  plot_id,
  db_path = shiny::reactive(NULL),
  plots_changed = shiny::reactiveVal(0L),
  on_open = function(plot_id) NULL,
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_editing <- reactiveVal(FALSE)

    # This plot's current persisted row; re-read on every change tick.
    plot_row <- reactive({
      plots_changed()
      analysis_store$get_plot(db_path(), plot_id)
    })

    observeEvent(session_reset(), is_editing(FALSE), ignoreInit = TRUE)

    # Open (thumbnail click or explicit button) -> bubble up to the dashboard.
    observeEvent(input$open_plot, {
      req(plot_id)
      on_open(plot_id)
    })

    # Inline rename: pencil enters edit mode, check commits to the database.
    observeEvent(input$toggle_edit, {
      if (is_editing()) {
        new_name <- input$title_input
        if (!is.null(new_name) && nzchar(new_name)) {
          analysis_store$rename_plot(db_path(), plot_id, new_name)
          plots_changed(plots_changed() + 1L)
        }
        is_editing(FALSE)
      } else {
        is_editing(TRUE)
      }
    })

    observeEvent(input$trigger_delete_box, {
      row <- plot_row()
      showModal(modalDialog(
        title = "Delete Plot",
        paste0(
          "Are you sure you want to permanently delete '",
          if (is.null(row)) "this plot" else row$name,
          "'?"
        ),
        footer = list(
          actionButton(
            ns("confirm_delete_box"),
            "Delete Plot",
            class = "btn-danger"
          ),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_box, {
      removeModal()
      analysis_store$delete_plot(db_path(), plot_id)
      plots_changed(plots_changed() + 1L)
    })

    output$box_container <- renderUI({
      row <- plot_row()
      # Deleted rows briefly render nothing; the group drops the box on refresh.
      if (is.null(row)) {
        return(NULL)
      }

      header_content <- if (is_editing()) {
        # allow-free-text: a plot name is display text, so it takes spaces and
        # punctuation — it opts out of the identifier charset restriction in
        # app/js/index.js.
        div(
          class = "allow-free-text",
          textInput(
            ns("title_input"),
            label = NULL,
            value = row$name,
            width = "100%"
          )
        )
      } else {
        row$name
      }

      btn_icon <- if (is_editing()) icon("check") else icon("pencil")

      div(
        div(
          class = "ad-box-actions",
          actionButton(
            ns("open_plot_visible"),
            label = NULL,
            icon = icon("up-right-from-square"),
            class = "btn-sm btn-light",
            title = "Open in Visualization"
          ),
          actionButton(
            ns("toggle_edit"),
            label = NULL,
            icon = btn_icon,
            class = "btn-sm btn-light",
            title = "Rename"
          ),
          actionButton(
            ns("trigger_delete_box"),
            label = NULL,
            icon = icon("trash"),
            class = "btn-sm btn-light",
            title = "Delete"
          )
        ),
        .thumb_ui(row$thumb_b64, ns),
        div(
          header_content
        ),
        div(
          span(class = "ad-plot-badge", row$plot_type)
        ),
        p(class = "ad-box-meta", paste("Saved:", row$created)),
        actionButton(
          ns("open_plot"),
          label = NULL,
          class = "ad-open-trigger"
        )
      )

      # value_box(
      #   title = header_content,
      #   showcase = .thumb_ui(row$thumb_b64, ns),
      #   theme = "teal",
      #   value = span(class = "ad-plot-badge", row$plot_type),
      #   p(class = "ad-box-meta", paste("Saved:", row$created)),
      #   # Hidden open trigger, clicked by the thumbnail wrapper.
      #   actionButton(
      #     ns("open_plot"),
      #     label = NULL,
      #     class = "ad-open-trigger"
      #   ),
      #   div(
      #     class = "ad-box-actions",
      #     actionButton(
      #       ns("open_plot_visible"),
      #       label = NULL,
      #       icon = icon("up-right-from-square"),
      #       class = "btn-sm btn-light",
      #       title = "Open in Visualization"
      #     ),
      #     actionButton(
      #       ns("toggle_edit"),
      #       label = NULL,
      #       icon = btn_icon,
      #       class = "btn-sm btn-light",
      #       title = "Rename"
      #     ),
      #     actionButton(
      #       ns("trigger_delete_box"),
      #       label = NULL,
      #       icon = icon("trash"),
      #       class = "btn-sm btn-light",
      #       title = "Delete"
      #     )
      #   )
      # )
    })

    # The visible open button shares the open action.
    observeEvent(input$open_plot_visible, {
      req(plot_id)
      on_open(plot_id)
    })
  })
}
