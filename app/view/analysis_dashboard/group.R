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

`%||%` <- function(a, b) if (is.null(a)) b else a

# A stored isolate JSON array as a character vector, or NULL when unset.
.parse_selection <- function(raw) {
  if (is.null(raw) || length(raw) != 1 || is.na(raw)) {
    return(NULL)
  }
  as.character(fromJSON(raw))
}

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
  on_edit_settings = function(analysis_id) NULL,
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # This Analysis's row (for its name) and its plots, refreshed on any change.
    analysis_row <- reactive({
      plots_changed()
      analysis_store$get_analysis(db_path(), analysis_id)
    })

    plots <- reactive({
      plots_changed()
      analysis_store$list_plots(db_path(), analysis_id)
    })

    # The isolate set this Analysis resolves to *right now*: its restriction if
    # it has one, otherwise every isolate currently in the database. Computed
    # once here and handed to each plot card, so a card can tell whether it was
    # built from a different set (changed restriction, or isolates added to the
    # database since) without each one re-querying.
    current_isolates <- reactive({
      plots_changed()
      row <- analysis_row()
      restriction <- if (is.null(row)) {
        NULL
      } else {
        .parse_selection(row$isolate_selection)
      }
      restriction %||% analysis_store$list_isolates(db_path())
    })

    # Isolates added to (or removed from) the database since this Analysis was
    # set up. Only meaningful once a universe was recorded.
    universe_drift <- reactive({
      plots_changed()
      row <- analysis_row()
      recorded <- if (is.null(row)) {
        NULL
      } else {
        .parse_selection(row$isolate_universe)
      }
      if (is.null(recorded)) {
        return(NULL)
      }
      now <- analysis_store$list_isolates(db_path())
      added <- setdiff(now, recorded)
      removed <- setdiff(recorded, now)
      if (!length(added) && !length(removed)) {
        return(NULL)
      }
      list(added = added, removed = removed)
    })

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
              analysis_isolates = current_isolates,
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

    # The pencil opens the Analysis settings wizard (name / description /
    # isolate selection), which the dashboard tier owns.
    observeEvent(input$toggle_group_edit, {
      on_edit_settings(analysis_id)
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

      add_card_placeholder <- actionButton(
        ns("add_box_btn"),
        "Add Plot",
        icon = icon("plus"),
        class = "add-box"
      )

      name_content <- tags$strong(row$name)
      edit_icon <- icon("pencil")

      card(
        card_header(
          div(
            class = "ad-group-header",
            span(class = "ad-group-name", name_content),
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
            div(
              class = "ad-group-header-actions",
              actionButton(
                ns("toggle_group_edit"),
                label = NULL,
                icon = edit_icon,
                class = "btn-sm btn-light",
                title = "Analysis settings"
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
          if (
            !is.null(row$description) &&
              length(row$description) == 1 &&
              !is.na(row$description) &&
              nzchar(row$description)
          ) {
            div(class = "ad-group-description", row$description)
          },
          # The database gained/lost isolates since this Analysis was set up.
          # Relevant even for an unrestricted Analysis: its plots were built
          # from a smaller set than "all isolates" now resolves to.
          {
            drift <- universe_drift()
            if (!is.null(drift)) {
              parts <- c(
                if (length(drift$added)) {
                  sprintf(
                    "%d new isolate%s in the database",
                    length(drift$added),
                    if (length(drift$added) == 1) "" else "s"
                  )
                },
                if (length(drift$removed)) {
                  sprintf(
                    "%d isolate%s removed",
                    length(drift$removed),
                    if (length(drift$removed) == 1) "" else "s"
                  )
                }
              )
              div(
                class = "ad-drift-note",
                icon("triangle-exclamation"),
                sprintf(
                  " %s since this Analysis was set up. Reopen its settings to
                   take them into account.",
                  paste(parts, collapse = " and ")
                )
              )
            }
          },
          layout_column_wrap(
            width = "250px",
            !!!c(box_uis, list(add_card_placeholder))
          )
        )
      )
    })
  })
}
