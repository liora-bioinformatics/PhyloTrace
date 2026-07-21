# app/view/database_browser.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    reactiveVal,
    div,
    h5,
    h2,
    reactiveValues,
    showNotification,
    actionButton,
    icon,
    req,
    uiOutput,
    renderUI,
    modalDialog,
    modalButton,
    showModal,
    removeModal,
    tagList
  ],
  bslib[as_fill_carrier],
  shinyjs[disabled, disable, enable, addClass, removeClass],
  shinyWidgets[pickerInput, pickerOptions],
  waiter[Waiter, spin_flower, useWaiter],
  DT[
    DTOutput,
    renderDT,
    datatable,
    editData,
    dataTableProxy,
    replaceData,
    showCols,
    hideCols
  ],
)

box::use(
  app /
    logic /
    database_functions[
      make_metadata_table,
      save_metadata_table,
      remove_isolates,
      append_classical_mlst,
      append_amr
    ],
  app / logic / field_labels[field_labels_for, grouped_field_choices],
)

# Columns the user may never edit: `isolate` is the isolate identity (shared
# with mlst.souche) and `organism` is fixed by mlst_type.species for the whole
# database. Located by name, not position — an imported peer database can carry
# extra metadata columns in any order.
READONLY_COLS <- c("isolate", "organism")

# Column rendered with a native date picker instead of a text input.
DATE_COL <- "sample_collection_date"

# 0-based DT column indices for `cols` within `all_cols`; NAs dropped.
.dt_idx <- function(all_cols, cols) {
  idx <- match(cols, all_cols)
  as.integer(idx[!is.na(idx)] - 1L)
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      id = ns("module-container"),
      useWaiter(),
      div(
        class = "db-page_header help-header",
        div(
          class = "db-browse-controls",
          div(
            class = "control-group",
            div(class = "control-group-label", "Column Selection"),
            div(
              class = "control-group-items",
              uiOutput(ns("col_picker_ui"))
            )
          ),
          div(
            class = "control-group",
            div(class = "control-group-label", "Edit"),
            div(
              class = "control-group-items",
              disabled(
                actionButton(
                  ns("discard"),
                  "Discard",
                  icon = icon("rotate-left")
                )
              ),
              disabled(
                actionButton(
                  ns("save"),
                  "Save Changes",
                  class = "btn-success",
                  icon = icon("floppy-disk")
                )
              )
            )
          ),
          div(
            class = "control-group",
            div(class = "control-group-label", "Remove isolates"),
            div(
              class = "control-group-items",
              uiOutput(ns("remove_picker_ui")),
              disabled(
                actionButton(
                  ns("remove_btn"),
                  "Remove",
                  class = "btn-danger",
                  icon = icon("trash")
                )
              )
            )
          )
        )
      ),
      as_fill_carrier(
        div(
          class = "db-page_body",
          DTOutput(ns("metadata_table"), fill = TRUE)
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    State <- reactiveValues(data = NULL, pending = FALSE)

    # Removing many isolates re-hashes what's left and can block the R
    # session for a while; cover the whole tab so the UI reads as busy
    # rather than frozen, same pattern as the Import panel.
    remove_waiter <- Waiter$new(
      id = ns("module-container"),
      html = div(
        class = "spinner-custom",
        spin_flower(),
        h5("Removing isolates ...")
      )
    )

    # Incremented only when the user explicitly requests a data reload (via
    # the "Reload Database" notification button, which calls reset_session()).
    # This is the sole mechanism that invalidates metadata_base's cache after
    # the initial load — db_updated() is intentionally NOT a dependency so
    # that background typing never wipes pending client-side edits.
    reload_token <- reactiveVal(0L)

    metadata_base <- reactive({
      reload_token()
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(NULL)
      }
      df <- make_metadata_table(path)
      if (is.null(df) || !nrow(df)) {
        return(NULL)
      }

      # Append the classical-MLST and AMR-screening columns for display only
      # (see append_classical_mlst / append_amr): never persisted - the Save
      # handler strips them again before writing.
      df <- append_classical_mlst(df, path)
      df <- append_amr(df, path)
      appended_mlst <- attr(df, "mlst_cols", exact = TRUE)
      appended_amr <- attr(df, "amr_cols", exact = TRUE)

      df[is.na(df)] <- ""
      # Re-set last so they survive the NA replacement above.
      attr(df, "mlst_cols") <- appended_mlst
      attr(df, "amr_cols") <- appended_amr
      df
    })

    # Names of the appended classical-MLST columns (empty when the database has
    # no classical typing). The single source of truth for which columns are
    # read-only, grouped under "Classical MLST", and stripped before save.
    mlst_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "mlst_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    # Names of the appended AMR-screening columns (empty when the database has no
    # AMR screening). Like mlst_cols: read-only, grouped under "AMR screening",
    # and stripped before save.
    amr_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "amr_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    observeEvent(metadata_base(), {
      State$data <- metadata_base()
      State$pending <- FALSE
    })

    # Columns the user can toggle: everything except the always-visible
    # `isolate`. Choice *values* are the raw column names (stable across
    # imports); the labels are the prettified ones.
    optional_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) character(0) else setdiff(names(df), "isolate")
    })

    output$col_picker_ui <- renderUI({
      cols <- optional_cols()
      mlst <- mlst_cols()
      amr <- amr_cols()

      # Group the derived MLST and AMR columns under their own headings so the
      # user can pick them out as categories. Without any derived columns the
      # picker stays flat, exactly as before.
      choices <- grouped_field_choices(cols, mlst, amr)

      pickerInput(
        ns("col_picker"),
        label = NULL,
        choices = choices,
        # Derived MLST / AMR columns start hidden - the user opts into the
        # category to show them. Keep this consistent with the DT's initial
        # column visibility.
        selected = setdiff(cols, c(mlst, amr)),
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Show fields …",
          selectedTextFormat = "count > 3",
          countSelectedText = paste0("{0} / ", length(cols), " fields"),
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search fields ...",
          # This tab pane is a bslib fill container (overflow: hidden), which
          # clips the dropdown menu once it grows taller than the remaining
          # space. Render it into <body> instead so it escapes that clip.
          container = "body"
        )
      )
    })

    output$remove_picker_ui <- renderUI({
      df <- metadata_base()
      choices <- if (!is.null(df)) df$isolate else character(0)
      pickerInput(
        ns("remove_picker"),
        label = NULL,
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select isolates to remove …",
          selectedTextFormat = "count > 2",
          countSelectedText = "{0} isolates selected",
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search isolates ..."
        )
      )
    })

    observeEvent(
      session_reset(),
      {
        State$data <- NULL
        State$pending <- FALSE
        reload_token(reload_token() + 1L)
      },
      ignoreInit = TRUE
    )

    output$metadata_table <- renderDT({
      df <- metadata_base()

      if (is.null(df)) {
        return(datatable(
          data.frame(
            " " = "No entries in this database yet.<br>Add isolates by typing them in the <strong>Add Isolates</strong> module.",
            check.names = FALSE
          ),
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      cols <- names(df)
      # MLST / AMR columns are derived from typing (classical_mlst / amr_summary):
      # never editable, and hidden until the user opts into their category.
      derived <- c(mlst_cols(), amr_cols())
      readonly_idx <- .dt_idx(cols, c(READONLY_COLS, derived))
      hidden_idx <- .dt_idx(cols, derived)
      # -1 when the column is absent, so the JS never matches a real column.
      date_idx <- c(.dt_idx(cols, DATE_COL), -1L)[[1]]

      column_defs <- list(
        list(className = "dt-left", targets = "_all"),
        list(className = "col-readonly", targets = readonly_idx)
      )
      # Start the MLST columns hidden; the picker toggles them. This keeps the
      # initial DT state consistent with the picker (which starts them
      # deselected).
      if (length(hidden_idx)) {
        column_defs <- c(
          column_defs,
          list(list(visible = FALSE, targets = hidden_idx))
        )
      }

      datatable(
        df,
        rownames = FALSE,
        colnames = field_labels_for(cols),
        filter = "top",
        # Drop the default "display" class's zebra striping (keep borders /
        # hover / sortable) so the cells aren't tinted per row.
        class = "row-border hover order-column",
        editable = list(
          target = "cell",
          disable = list(columns = readonly_idx)
        ),
        selection = "none",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          columnDefs = column_defs,
          initComplete = DT::JS(sprintf(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();
              var dateCol = %d;

              $(tableNode).on('keyup', 'input', function(e) {
                if (e.key === 'Enter') this.blur();
              });

              api.on('column-visibility.dt', function() {
                api.columns.adjust().draw(false);
              });

              // Swap the default text input with a native date picker for
              // the collection-date column.
              var observer = new MutationObserver(function(mutations) {
                for (var i = 0; i < mutations.length; i++) {
                  var added = mutations[i].addedNodes;
                  for (var j = 0; j < added.length; j++) {
                    var node = added[j];
                    if (node.tagName !== 'INPUT') continue;
                    var idx = api.cell(node.parentNode).index();
                    if (idx && idx.column === dateCol) {
                      node.type = 'date';
                      var v = (node.value || '').trim();
                      if (v) {
                        var d = new Date(v);
                        if (!isNaN(d.getTime()))
                          node.value = d.toISOString().split('T')[0];
                      }
                      node.focus();
                    }
                  }
                }
              });
              observer.observe(tableNode, { childList: true, subtree: true });
            }",
            date_idx
          ))
        )
      )
    })

    proxy <- dataTableProxy("metadata_table", session = session)

    # Apply cell edit to in-memory state and push to table without full re-render
    observeEvent(input$metadata_table_cell_edit, {
      req(is.data.frame(State$data))
      info <- input$metadata_table_cell_edit
      State$data <- editData(State$data, info, rownames = FALSE)
      State$pending <- TRUE
      replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
    })

    # Enable/disable both action buttons together based on pending edits
    observeEvent(State$pending, {
      if (isTRUE(State$pending)) {
        enable("save")
        addClass("save", "btn-attention")
        enable("discard")
      } else {
        disable("save")
        removeClass("save", "btn-attention")
        disable("discard")
      }
    })

    observeEvent(input$save, {
      # Drop the display-only MLST / AMR columns; they live in classical_mlst /
      # amr_summary, not the metadata table, and must never be written back.
      strip <- c(mlst_cols(), amr_cols())
      to_save <- State$data[,
        setdiff(names(State$data), strip),
        drop = FALSE
      ]
      save_metadata_table(db_path(), to_save)
      State$pending <- FALSE
      showNotification(
        "Database changes saved.",
        type = "message",
        duration = 3
      )
    })

    # Discard: re-fetch from DB and push back to the table without re-render
    observeEvent(input$discard, {
      fresh <- metadata_base()
      State$data <- fresh
      State$pending <- FALSE
      replaceData(proxy, fresh, resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(
      input$remove_picker,
      {
        if (length(input$remove_picker) > 0) {
          enable("remove_btn")
        } else {
          disable("remove_btn")
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$remove_btn, {
      isolates <- input$remove_picker
      req(length(isolates) > 0)
      showModal(modalDialog(
        title = "Remove isolates",
        if (length(isolates) == 1) {
          paste0(
            'Permanently remove "',
            isolates,
            '" from the database? This cannot be undone.'
          )
        } else {
          paste0(
            "Permanently remove ",
            length(isolates),
            " isolates from the database? This cannot be undone."
          )
        },
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_remove"), "Remove", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_remove, {
      isolates <- input$remove_picker
      removeModal()
      remove_waiter$show()
      on.exit(remove_waiter$hide())
      remove_isolates(db_path(), isolates)
      reload_token(reload_token() + 1L)
      showNotification(
        paste0(length(isolates), " isolate(s) removed from the database."),
        type = "message",
        duration = 3
      )
    })

    # Column visibility. `input$col_picker` carries raw column names, so the DT
    # indices are looked up against the frame's actual column order.
    observeEvent(
      input$col_picker,
      {
        df <- metadata_base()
        req(is.data.frame(df))

        optional <- optional_cols()
        selected <- input$col_picker
        show_idx <- .dt_idx(names(df), intersect(optional, selected))
        hide_idx <- .dt_idx(names(df), setdiff(optional, selected))

        if (length(show_idx)) {
          showCols(proxy, show_idx, reset = FALSE)
        }
        if (length(hide_idx)) hideCols(proxy, hide_idx, reset = FALSE)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    list(pending = reactive(State$pending))
  })
}
