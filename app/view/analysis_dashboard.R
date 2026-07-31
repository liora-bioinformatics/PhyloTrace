# app/view/analysis_dashboard.R
# Tier 3: the Analysis Dashboard. Lists the Analyses stored in the loaded
# database and lets the user add/navigate them. Each Analysis is a persistent
# container of saved Plots (see group.R / item.R).

box::use(
  bslib[page_sidebar, sidebar, tooltip],
  DT[datatable, dataTableProxy, DTOutput, JS, renderDT, selectRows],
  jsonlite[fromJSON, toJSON],
  shiny[
    actionButton,
    dateRangeInput,
    div,
    hr,
    icon,
    isolate,
    modalButton,
    modalDialog,
    moduleServer,
    NS,
    observe,
    observeEvent,
    outputOptions,
    p,
    reactive,
    reactiveVal,
    removeModal,
    renderUI,
    req,
    showModal,
    showNotification,
    span,
    tagList,
    textAreaInput,
    textInput,
    uiOutput,
    updateDateRangeInput,
  ],
  shinyWidgets[pickerInput, pickerOptions],
)

box::use(
  app / logic / analysis_store,
  app / logic / database_functions[append_classical_mlst, make_metadata_table],
  app / logic / field_labels[field_labels_for],
  app / logic / field_types[as_date_safe, date_fields],
  app / view / analysis_dashboard / group,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    fillable = TRUE,
    shinyjs::useShinyjs(),
    sidebar = sidebar(
      title = "Analysis Dashboard",
      width = "280px",
      actionButton(
        ns("trigger_group_modal"),
        "Add Analysis",
        icon = icon("folder-plus"),
        class = "btn-success w-100 mb-3"
      ),
      uiOutput(ns("sidebar_navigation"))
    ),
    div(
      class = "ad-group-stack",
      uiOutput(ns("groups_vertical_stack"))
    )
  )
}

# Returns default value if a vector or list is NULL or empty
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# An Analysis's stored isolate_selection JSON as a character vector, or NULL
# when it fixes nothing (i.e. all isolates).
.parse_selection <- function(raw) {
  if (is.null(raw) || length(raw) != 1 || is.na(raw)) {
    return(NULL)
  }
  as.character(fromJSON(raw))
}

# Validates if a file path is a valid non-empty string referencing an existing file
.usable_path <- function(path) {
  !is.null(path) &&
    length(path) == 1 &&
    !is.na(path) &&
    nzchar(path) &&
    file.exists(path)
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  plots_changed = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_view <- reactiveVal("all")

    # Requests bubbled up to main.R. Each carries a monotonic `n` so repeated
    # clicks on the same target still fire the downstream observeEvent.
    request_add_plot <- reactiveVal(NULL)
    request_open_plot <- reactiveVal(NULL)
    add_seq <- 0L
    open_seq <- 0L

    # Handles user request to add a new plot to an analysis
    handle_add_plot <- function(analysis_id) {
      add_seq <<- add_seq + 1L
      request_add_plot(list(analysis_id = analysis_id, n = add_seq))
    }

    # Handles user request to open an existing saved plot
    handle_open_plot <- function(plot_id) {
      open_seq <<- open_seq + 1L
      request_open_plot(list(plot_id = plot_id, n = open_seq))
    }

    # Current Analyses, refreshed whenever the store changes or the DB reloads.
    analyses <- reactive({
      plots_changed()
      analysis_store$list_analyses(db_path())
    })

    # On DB load / reload: ensure the schema exists and guarantee at least one
    # Analysis to land in, then trigger a refresh.
    sync_db <- function() {
      path <- db_path()
      if (!.usable_path(path)) {
        return()
      }
      analysis_store$ensure_schema(path)
      if (nrow(analysis_store$list_analyses(path)) == 0L) {
        analysis_store$add_analysis(path, "Analysis 1")
      }
      plots_changed(isolate(plots_changed()) + 1L)
    }
    observeEvent(db_path(), sync_db(), ignoreNULL = FALSE)
    observeEvent(
      session_reset(),
      {
        current_view("all")
        sync_db()
      },
      ignoreInit = TRUE
    )

    # Instantiate a group server once per Analysis id. Servers persist and read
    # db_path() reactively, so reusing an id for a different DB is safe.
    instantiated <- reactiveVal(integer(0))
    observe({
      ids <- analyses()$id
      isolate({
        new_ids <- setdiff(ids, instantiated())
        for (aid in new_ids) {
          local({
            this_aid <- aid
            group$server(
              id = paste0("group_instance_", this_aid),
              analysis_id = this_aid,
              db_path = db_path,
              plots_changed = plots_changed,
              on_add_plot = handle_add_plot,
              on_open_plot = handle_open_plot,
              on_edit_settings = handle_edit_settings,
              session_reset = session_reset
            )
          })
        }
        if (length(new_ids)) {
          instantiated(c(instantiated(), new_ids))
        }
      })
    })

    # ----------------------------------------------- Analysis setup wizard ---
    # Creating (or editing) an Analysis walks the user through two steps:
    #   1. name + optional description
    #   2. the isolate table — the set every plot in this Analysis will use
    # `editing_analysis` is NULL while creating, or the id being edited. The
    # step values live in reactiveVals so they survive the modal being re-shown
    # as the user moves between steps.
    wizard_step <- reactiveVal(1L)
    editing_analysis <- reactiveVal(NULL)
    wizard_name <- reactiveVal("")
    wizard_desc <- reactiveVal("")
    wizard_selection <- reactiveVal(NULL)

    # Isolate names ticked in the wizard's table right now — kept separately
    # from input$wiz_table_rows_selected (row *indices*, which point at
    # different isolates once the time window changes and the table
    # re-renders) so a tick survives narrowing or widening the window. Seeded
    # from wizard_selection() each time step 2 is (re)shown; see show_wizard().
    wiz_checked <- reactiveVal(character(0))

    # Isolate metadata backing the selection step — the same table the
    # Visualization module's isolate picker shows.
    settings_meta <- reactive({
      req(db_path())
      append_classical_mlst(make_metadata_table(db_path()), db_path())
    })

    # The date-typed columns the time filter can work along: the fixed
    # schema's collection date, the app-stamped add time, and any user-defined
    # `date` custom variable. Mirrors the Visualization module's isolate
    # picker (see app/view/visualization_plot.R, sel_date_choices).
    wiz_date_choices <- reactive({
      meta <- settings_meta()
      if (is.null(meta)) {
        return(character(0))
      }
      cols <- date_fields(db_path(), names(meta))
      stats::setNames(cols, field_labels_for(cols))
    })

    # The chosen column parsed to Date, NA where the cell is empty or (for the
    # free-text collection date) unreadable. NULL when no column is chosen.
    wiz_dates <- reactive({
      meta <- settings_meta()
      col <- input$wiz_date_field
      if (is.null(meta) || is.null(col) || !nzchar(col)) {
        return(NULL)
      }
      if (!col %in% names(meta)) {
        return(NULL)
      }
      as_date_safe(meta[[col]])
    })

    # The rows the table shows. Without a chosen column, or before the range
    # input has reported a complete window, that is every isolate.
    wiz_filtered <- reactive({
      meta <- settings_meta()
      req(meta)
      d <- wiz_dates()
      rng <- input$wiz_date_range
      if (is.null(d) || is.null(rng) || length(rng) != 2 || anyNA(rng)) {
        return(meta)
      }
      keep <- !is.na(d) & d >= as.Date(rng[1]) & d <= as.Date(rng[2])
      meta[keep, , drop = FALSE]
    })

    # Isolate names the wizard's table currently represents as "selected": the
    # ticked set if anything is ticked, otherwise every isolate the active
    # time window leaves (all of them, with no window) — the same "empty
    # means everything visible" convention the Visualization module's isolate
    # picker uses. NULL only when there is no metadata to select from at all.
    selected_from_table <- function() {
      meta <- settings_meta()
      tbl <- wiz_filtered()
      if (is.null(meta) || is.null(tbl)) {
        return(NULL)
      }
      ticked <- intersect(wiz_checked(), tbl$isolate)
      if (length(ticked)) ticked else tbl$isolate
    }

    # Displays the multi-step analysis creation and editing wizard modal
    show_wizard <- function() {
      creating <- is.null(editing_analysis())
      if (identical(wizard_step(), 1L)) {
        showModal(modalDialog(
          title = if (creating) "New Analysis" else "Analysis settings",
          # allow-free-text: name/description are display text, so they take
          # spaces and punctuation (see isExemptFromCharset in app/js/index.js).
          div(
            class = "allow-free-text",
            textInput(
              ns("wiz_name"),
              "Analysis name",
              value = wizard_name(),
              width = "100%"
            ),
            textAreaInput(
              ns("wiz_desc"),
              "Description (optional)",
              value = wizard_desc(),
              width = "100%",
              rows = 3
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("wiz_next"),
              "Next",
              icon = icon("arrow-right"),
              class = "btn-primary"
            )
          ),
          easyClose = TRUE
        ))
      } else {
        # Editing an Analysis that already holds plots: warn up front that the
        # isolate set is what those plots were built from.
        n_saved <- if (creating) {
          0L
        } else {
          nrow(analysis_store$list_plots(db_path(), editing_analysis()))
        }
        retro_warning <- if (n_saved > 0) {
          plural <- if (n_saved == 1) "" else "s"
          tooltip(
            div(
              class = "ad-retro-warning",
              icon("triangle-exclamation"),
              sprintf(" %d saved plot%s built from this set", n_saved, plural)
            ),
            sprintf(
              paste(
                "This Analysis already contains %d saved plot%s built from",
                "the current isolate set. Changing it re-bases them onto",
                "different data — they will be flagged as out of date."
              ),
              n_saved,
              plural
            )
          )
        }

        # Sticky across reopens, like the Visualization module's isolate
        # picker: the ticked set carries over from wizard_selection() (the
        # prior/persisted selection — from an edited Analysis, or from
        # wiz_back on the way out of step 2) and the date controls come back
        # holding whatever they were last set to rather than resetting, so
        # neither Back nor reopening to edit discards a chosen time window.
        wiz_checked(wizard_selection() %||% character(0))
        dates <- wiz_date_choices()
        field <- isolate(input$wiz_date_field) %||% ""
        rng <- isolate(input$wiz_date_range)

        showModal(div(
          class = "selection-modal",
          modalDialog(
            title = NULL,
            div(
              class = "selection-modal-toolbar",
              actionButton(
                ns("wiz_sel_all"),
                title = "Select all",
                label = "All",
                icon = icon("check-double")
              ),
              actionButton(
                ns("wiz_sel_none"),
                title = "Select none",
                label = "None",
                icon = icon("xmark")
              ),
              retro_warning,
              uiOutput(ns("wiz_sel_count"), class = "selection-modal-count")
            ),
            div(
              class = "isolate-selection-table",
              DTOutput(ns("wiz_table"), fill = FALSE)
            ),
            footer = tagList(
              # Shares the footer row with the buttons (pushed to its left by
              # .selection-modal-filter); omitted when the database has no
              # date-typed column to filter along.
              if (length(dates)) {
                div(
                  class = "selection-modal-filter",
                  pickerInput(
                    ns("wiz_date_field"),
                    label = NULL,
                    choices = c("No time filter" = "", dates),
                    selected = if (field %in% dates) field else "",
                    width = "fit"
                  ),
                  div(
                    id = ns("wiz_date_range_wrap"),
                    dateRangeInput(
                      ns("wiz_date_range"),
                      label = NULL,
                      start = rng[1],
                      end = rng[2],
                      separator = "–",
                      width = "17rem"
                    )
                  )
                )
              },
              modalButton("Cancel"),
              actionButton(ns("wiz_back"), "Back", icon = icon("arrow-left")),
              actionButton(
                ns("wiz_confirm"),
                if (creating) "Create Analysis" else "Save settings",
                class = "btn-primary"
              )
            ),
            easyClose = TRUE
          )
        ))
        # The range input is created enabled; disable it right away when the
        # modal opens with no column chosen (the observer on
        # input$wiz_date_field owns it from then on).
        shinyjs::toggleState(
          "wiz_date_range",
          condition = nzchar(field) && field %in% dates
        )
      }
    }

    # "Add Analysis" starts a fresh wizard.
    observeEvent(input$trigger_group_modal, {
      editing_analysis(NULL)
      wizard_name(paste("Analysis", nrow(isolate(analyses())) + 1L))
      wizard_desc("")
      wizard_selection(NULL)
      wizard_step(1L)
      show_wizard()
    })

    # The pencil on an Analysis card reopens the same wizard, prefilled.
    handle_edit_settings <- function(analysis_id) {
      row <- analysis_store$get_analysis(db_path(), analysis_id)
      if (is.null(row)) {
        return()
      }
      editing_analysis(analysis_id)
      wizard_name(row$name)
      wizard_desc(if (is.na(row$description)) "" else row$description)
      wizard_selection(.parse_selection(row$isolate_selection))
      wizard_step(1L)
      show_wizard()
    }

    observeEvent(input$wiz_next, {
      wizard_name(if (is.null(input$wiz_name)) "" else input$wiz_name)
      wizard_desc(if (is.null(input$wiz_desc)) "" else input$wiz_desc)
      wizard_step(2L)
      show_wizard()
    })

    observeEvent(input$wiz_back, {
      # Carry the table's current ticks back so returning to step 2 keeps them.
      wizard_selection(selected_from_table())
      wizard_step(1L)
      show_wizard()
    })

    # Switching the time axis re-bases the window on the new column's own
    # span, since a window tuned for the collection date is meaningless on,
    # say, the date an isolate entered the database.
    observeEvent(input$wiz_date_field, {
      d <- wiz_dates()
      shinyjs::toggleState("wiz_date_range", condition = !is.null(d))
      if (is.null(d) || all(is.na(d))) {
        return()
      }
      updateDateRangeInput(
        session,
        "wiz_date_range",
        start = min(d, na.rm = TRUE),
        end = max(d, na.rm = TRUE),
        min = min(d, na.rm = TRUE),
        max = max(d, na.rm = TRUE)
      )
    })

    # Isolates ticked in the table, held by name rather than by row index so
    # that changing the window — which re-renders the table and renumbers its
    # rows — does not silently discard them.
    observeEvent(
      input$wiz_table_rows_selected,
      {
        rows <- input$wiz_table_rows_selected
        tbl <- wiz_filtered()
        wiz_checked(if (length(rows)) tbl$isolate[rows] else character(0))
      },
      ignoreNULL = FALSE
    )

    # Renders the interactive DataTables view for isolate selection in wizard step 2
    output$wiz_table <- renderDT(
      {
        tbl <- wiz_filtered()
        req(tbl)

        keep <- match(isolate(wiz_checked()), tbl$isolate)

        # Pinned (FixedColumns) columns: `isolate` always, plus whatever date
        # column the time filter is currently set to, moved to sit right
        # after it — mirrors the Visualization module's isolate picker (see
        # app/view/visualization_plot.R).
        date_col <- input$wiz_date_field
        pin_cols <- if (
          !is.null(date_col) &&
            nzchar(date_col) &&
            !identical(date_col, "isolate") &&
            date_col %in% names(tbl)
        ) {
          c("isolate", date_col)
        } else {
          "isolate"
        }
        tbl <- tbl[, c(pin_cols, setdiff(names(tbl), pin_cols)), drop = FALSE]

        datatable(
          tbl,
          rownames = FALSE,
          filter = "top",
          class = "row-border hover order-column",
          selection = list(
            mode = "multiple",
            selected = keep[!is.na(keep)]
          ),
          extensions = "FixedColumns",
          options = list(
            dom = "tip",
            pageLength = 20,
            scrollX = TRUE,
            scrollY = "1px",
            scrollCollapse = TRUE,
            fixedColumns = list(leftColumns = length(pin_cols)),
            # FixedColumns pins each pinned column's header cell but not the
            # filter = "top" row underneath (a plain <td> row DT bolts onto
            # <thead> outside the header API) — copy the `left` offset
            # FixedColumns already worked out for the matching header cell
            # onto the filter cell, so it stays put too. See the identical
            # comment in visualization_plot.R, where this first landed.
            initComplete = JS(sprintf(
              "function(settings) {
                 var rows = $(this.api().table().header()).find('tr');
                 var headerCells = rows.eq(0).find('th');
                 var filterCells = rows.eq(1).find('td');
                 for (var i = 0; i < %d; i++) {
                   filterCells.eq(i).addClass('dtfc-fixed-left').css({
                     position: 'sticky',
                     left: headerCells.eq(i).css('left') || '0px',
                     zIndex: 3
                   });
                 }
               }",
              length(pin_cols)
            ))
          )
        )
      },
      server = FALSE
    )

    # Renders selection summary text showing selected vs total count and filter info
    output$wiz_sel_count <- renderUI({
      meta <- settings_meta()
      req(meta)
      shown <- nrow(wiz_filtered())
      n <- length(input$wiz_table_rows_selected)
      if (shown == nrow(meta)) {
        return(span(sprintf("%d of %d selected", n, shown)))
      }
      # A row leaves the table for one of two different reasons — its date
      # falls outside the chosen window, or it has no usable date at all — see
      # the identical breakdown in visualization_plot.R's sel_count.
      d <- wiz_dates()
      missing <- if (is.null(d)) 0L else sum(is.na(d))
      outside <- nrow(meta) - shown - missing
      detail <- if (missing > 0 && outside > 0) {
        sprintf(" · %d outside window, %d missing date", outside, missing)
      } else if (missing > 0) {
        sprintf(" · %d missing date", missing)
      } else {
        sprintf(" · %d outside window", outside)
      }
      span(
        sprintf("%d of %d selected", n, shown),
        span(class = "selection-modal-count-total", detail)
      )
    })

    # Select all / none act on the currently filtered rows, like the
    # Visualization module's isolate picker.
    wiz_proxy <- dataTableProxy("wiz_table")
    observeEvent(
      input$wiz_sel_all,
      selectRows(wiz_proxy, input$wiz_table_rows_all)
    )
    observeEvent(input$wiz_sel_none, selectRows(wiz_proxy, NULL))

    # Writes the wizard's collected settings. `isolate_universe` records the
    # concrete isolate list the database held at this moment even when nothing
    # is restricted, so isolates added later are detectable (see the drift hint
    # on the Analysis card).
    commit_settings <- function() {
      meta <- settings_meta()
      req(meta)
      sel <- selected_from_table()
      # Empty (or everything ticked) means "no restriction", stored as NULL.
      sel_json <- if (is.null(sel) || setequal(sel, meta$isolate)) {
        NULL
      } else {
        as.character(toJSON(sel, auto_unbox = FALSE))
      }
      universe_json <- as.character(
        toJSON(as.character(meta$isolate), auto_unbox = FALSE)
      )
      nm <- wizard_name()
      if (!nzchar(nm)) {
        nm <- "Analysis"
      }
      desc <- wizard_desc()
      desc <- if (nzchar(desc)) desc else NULL

      if (is.null(editing_analysis())) {
        new_id <- analysis_store$add_analysis(
          db_path(),
          nm,
          desc,
          sel_json,
          universe_json
        )
        current_view(as.character(new_id))
      } else {
        analysis_store$update_analysis_settings(
          db_path(),
          editing_analysis(),
          nm,
          desc,
          sel_json,
          universe_json
        )
      }
      plots_changed(isolate(plots_changed()) + 1L)
      removeModal()
    }

    observeEvent(input$wiz_confirm, {
      if (!length(selected_from_table())) {
        showNotification(
          "No isolates left to select — widen the time filter.",
          type = "warning"
        )
        return()
      }

      aid <- editing_analysis()

      # Editing an existing Analysis that already holds plots: changing the
      # isolate set retroactively re-bases every saved plot onto different
      # data, so confirm before committing. Creating a new Analysis, or
      # editing one with no plots yet, has nothing to invalidate.
      if (!is.null(aid)) {
        n_plots <- nrow(analysis_store$list_plots(db_path(), aid))
        row <- analysis_store$get_analysis(db_path(), aid)
        prev_sel <- if (is.null(row)) {
          NULL
        } else {
          .parse_selection(row$isolate_selection)
        }
        meta <- settings_meta()
        new_sel <- selected_from_table()
        if (
          !is.null(meta) && !is.null(new_sel) && setequal(new_sel, meta$isolate)
        ) {
          new_sel <- NULL
        }

        if (
          n_plots > 0 && analysis_store$selection_differs(prev_sel, new_sel)
        ) {
          showModal(modalDialog(
            title = "⚠️ Change isolate selection?",
            paste0(
              "This Analysis already contains ",
              n_plots,
              " saved plot",
              if (n_plots == 1) "" else "s",
              ". They were built from the current isolate set; changing it",
              " means they no longer match the data they were saved with.",
              " Affected plots will be flagged on the dashboard, and",
              " reopening one rebuilds it against the new selection."
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                ns("confirm_selection_change"),
                "Change selection",
                class = "btn-danger"
              )
            ),
            easyClose = TRUE
          ))
          return()
        }
      }

      commit_settings()
    })

    observeEvent(input$confirm_selection_change, commit_settings())

    # Renders the sidebar dropdown to switch view filter between analyses
    output$sidebar_navigation <- renderUI({
      df <- analyses()
      choices_list <- c("Show All Analyses" = "all")

      if (nrow(df) > 0) {
        for (i in seq_len(nrow(df))) {
          choices_list[df$name[i]] <- as.character(df$id[i])
        }
      }

      # Keep the current selection valid after a delete.
      sel <- isolate(current_view())
      if (!sel %in% choices_list) {
        sel <- "all"
      }

      pickerInput(
        ns("selected_group_view"),
        "Navigate Analyses",
        choices = choices_list,
        selected = sel,
        options = pickerOptions(
          title = "No analysis",
          size = 10,
          container = "body"
        )
      )
    })

    observeEvent(input$selected_group_view, {
      current_view(input$selected_group_view)
    })

    # Renders the list of active analysis group UI modules
    output$groups_vertical_stack <- renderUI({
      df <- analyses()

      if (nrow(df) == 0) {
        return(p(
          class = "ad-empty ad-empty-block",
          paste(
            "No analyses yet. Click 'Add Analysis' to create a container,",
            "then build and save plots into it from the Visualization tab."
          )
        ))
      }

      view <- current_view()
      visible_ids <- if (view == "all") {
        df$id
      } else {
        intersect(df$id, suppressWarnings(as.numeric(view)))
      }

      if (length(visible_ids) == 0) {
        return(p(
          class = "ad-empty",
          "The selected analysis is no longer active."
        ))
      }

      lapply(visible_ids, function(a_id) {
        group$ui(ns(paste0("group_instance_", a_id)))
      })
    })

    # Keep outputs live while the panel is detached from the DOM (nav_remove on
    # session reset) so reset-triggered changes propagate before re-insertion.
    outputOptions(output, "sidebar_navigation", suspendWhenHidden = FALSE)
    outputOptions(output, "groups_vertical_stack", suspendWhenHidden = FALSE)

    list(
      request_add_plot = request_add_plot,
      request_open_plot = request_open_plot
    )
  })
}
