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
    showNotification,
    span,
    tags,
    textInput,
    uiOutput,
  ],
  bslib[
    value_box,
  ],
  app / logic / analysis_store,
  jsonlite[fromJSON],
)

# The concrete isolate set a saved plot was built from, or NULL when it can't
# be determined (plots saved before `selection_resolved` was recorded fall back
# to the stored restriction; if that is absent too there is nothing to compare
# against, so no staleness is claimed).
.plot_isolates <- function(inputs_json) {
  if (
    is.null(inputs_json) || length(inputs_json) != 1 || is.na(inputs_json)
  ) {
    return(NULL)
  }
  snap <- tryCatch(fromJSON(inputs_json), error = function(e) NULL)
  if (is.null(snap)) {
    return(NULL)
  }
  resolved <- snap$selection_resolved
  if (!is.null(resolved)) {
    return(as.character(unlist(resolved, use.names = FALSE)))
  }
  sel <- snap$selection
  if (is.null(sel)) {
    return(NULL)
  }
  as.character(unlist(sel, use.names = FALSE))
}

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
  analysis_isolates = shiny::reactive(NULL),
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

    # Whether this plot was built from a different isolate set than the
    # Analysis currently resolves to — either because the Analysis's selection
    # was edited afterwards, or because isolates were added to / removed from
    # the database while the Analysis is unrestricted. NULL when undeterminable.
    stale_isolates <- reactive({
      row <- plot_row()
      if (is.null(row)) {
        return(NULL)
      }
      built_from <- .plot_isolates(row$inputs_json)
      current <- analysis_isolates()
      if (is.null(built_from) || is.null(current)) {
        return(NULL)
      }
      if (!analysis_store$selection_differs(built_from, current)) {
        return(NULL)
      }
      list(
        added = setdiff(current, built_from),
        removed = setdiff(built_from, current)
      )
    })

    # Clicking the thumbnail opens the plot (via the hidden .ad-open-trigger
    # button below) -> bubble the request up to the dashboard.
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
        class = "ad-plot-box",
        # Fills the box edge-to-edge; everything else overlays on top of it.
        .thumb_ui(row$thumb_b64, ns),
        div(
          class = "ad-box-actions",
          actionButton(
            ns("duplicate_plot"),
            label = NULL,
            icon = icon("copy"),
            class = "btn-sm btn-light",
            title = "Duplicate this plot"
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
        # Flagged when the Analysis's isolate set no longer matches the one this
        # plot was built from, so a card that silently no longer represents its
        # Analysis is visible at a glance.
        {
          stale <- stale_isolates()
          if (!is.null(stale)) {
            parts <- c(
              if (length(stale$added)) sprintf("+%d", length(stale$added)),
              if (length(stale$removed)) sprintf("-%d", length(stale$removed))
            )
            span(
              class = "ad-plot-stale",
              title = paste(
                "Built from a different isolate set than this Analysis now",
                "uses — reopen it to rebuild against the current selection."
              ),
              icon("triangle-exclamation"),
              sprintf(" Isolates changed (%s)", paste(parts, collapse = " "))
            )
          }
        },
        div(
          class = "ad-plot-overlay-info",
          div(class = "ad-plot-name", header_content),
          div(
            class = "ad-plot-overlay-meta",
            span(class = "ad-plot-badge", row$plot_type),
            span(class = "ad-box-meta", paste("Saved:", row$created))
          )
        ),
        # Hidden open trigger, clicked by the thumbnail wrapper.
        actionButton(
          ns("open_plot"),
          label = NULL,
          class = "ad-open-trigger"
        )
      )
    })

    # Duplicate: copies this plot's settings snapshot and thumbnail into a new
    # card in the same Analysis. Purely additive, so no confirmation — but the
    # copy is appended at the end of the row and may be off-screen, hence the
    # notification naming it.
    observeEvent(input$duplicate_plot, {
      req(plot_id)
      new_id <- analysis_store$duplicate_plot(db_path(), plot_id)
      if (is.null(new_id)) {
        showNotification("Could not duplicate this plot.", type = "error")
        return()
      }
      plots_changed(plots_changed() + 1L)
      copy <- analysis_store$get_plot(db_path(), new_id)
      showNotification(
        paste0(
          "Duplicated as '",
          if (is.null(copy)) "copy" else copy$name,
          "'."
        ),
        type = "message"
      )
    })
  })
}
