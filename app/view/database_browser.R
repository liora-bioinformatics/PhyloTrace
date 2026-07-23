# app/view/database_browser.R

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    reactiveVal,
    isolate,
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
  bslib[as_fill_carrier, layout_sidebar, sidebar],
  shinyjs[disabled, disable, enable, addClass, removeClass],
  shinyWidgets[pickerInput, pickerOptions, virtualSelectInput],
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
    custom_fields[
      CUSTOM_TYPES,
      NUMERIC_TYPES,
      append_custom,
      coerce_custom_value,
      custom_col,
      list_custom_fields,
      save_custom_values
    ],
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

# The one date-typed field in the fixed GenEpiO metadata schema. User-defined
# date variables are *not* listed here — they are discovered from
# `phylotrace_custom_fields.type`, so this stays the schema's own constant
# rather than a hardcoded list of every date column on screen.
DATE_COL <- "sample_collection_date"

# 0-based DT column indices for `cols` within `all_cols`; NAs dropped.
.dt_idx <- function(all_cols, cols) {
  idx <- match(cols, all_cols)
  as.integer(idx[!is.na(idx)] - 1L)
}

# The same columns, in the base DT's `editable` list wants for its
# numeric/date/area fields — which is *not* the base columnDefs targets use.
# DT::makeEditableField() ends with `z - is.null(rn)`: with rownames = FALSE
# there is no rownames column, so it shifts whatever it is given one column to
# the left before handing it to the JS. Passing 0-based indices therefore opens
# the date picker on the column *before* the date one. Hand it 1-based indices
# so the shift lands on the intended column.
#
# `disable$columns` goes through no such adjustment and stays 0-based, which is
# why READONLY_COLS is indexed with .dt_idx() directly.
.dt_editable_idx <- function(all_cols, cols) .dt_idx(all_cols, cols) + 1L

#' @export
ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    padding = 0,
    border = FALSE,
    sidebar = sidebar(
      # id = ns("controls_sidebar"),
      position = "right",
      width = 300,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(
        div(
          class = "sidebar-control",
          div(
            class = "sidebar-control-group",
            div(class = "control-group-label", "Column Selection"),
            uiOutput(ns("col_picker_ui"))
          ),
          div(
            class = "sidebar-control-group",
            div(class = "control-group-label", "Edit"),
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
          ),
          div(
            class = "sidebar-control-group",
            div(class = "control-group-label", "Remove isolates"),
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
        class = "db-page_body edit-table",
        DTOutput(ns("metadata_table"), fill = TRUE)
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  db_updated = shiny::reactiveVal(0L),
  custom_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # custom_dirty buffers edits bound for phylotrace_custom_values, which the
    # metadata write path cannot carry (see the cell-edit observer).
    State <- reactiveValues(data = NULL, pending = FALSE, custom_dirty = NULL)

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

      # The user-defined custom variables, also display only, edited in the
      # Custom Variables panel. Appended *after* the NA replacement above:
      # assigning "" into a numeric column would coerce it to character and cost
      # a numeric variable its numeric sort order.
      df <- append_custom(df, path)
      appended_custom <- attr(df, "custom_cols", exact = TRUE)

      # Re-set last so they survive the NA replacement above.
      attr(df, "mlst_cols") <- appended_mlst
      attr(df, "amr_cols") <- appended_amr
      attr(df, "custom_cols") <- appended_custom
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

    # Names of the appended custom-variable columns (empty when the database has
    # none defined). Editable here as well as in the Custom Variables panel;
    # both route through coerce_custom_value() / save_custom_values(), so
    # phylotrace_custom_values still has exactly one validated write path.
    custom_cols <- reactive({
      df <- metadata_base()
      if (is.null(df)) {
        return(character(0))
      }
      cols <- attr(df, "custom_cols", exact = TRUE)
      if (is.null(cols)) character(0) else cols
    })

    # The custom-variable definitions (id / name / type / levels) behind those
    # columns, keyed by the prefixed column name they appear under. This is what
    # makes the type in `phylotrace_custom_fields` the authority here too: which
    # editor a cell gets and how its value is validated both come from `type`,
    # never from guessing at the column's contents.
    custom_defs <- reactive({
      cols <- custom_cols()
      defs <- list_custom_fields(db_path())
      if (!is.data.frame(defs) || !nrow(defs)) {
        # Still carry `column`, so every consumer can index on it without
        # first re-checking whether any variables are defined.
        return(data.frame(
          id = integer(0),
          name = character(0),
          type = character(0),
          levels = character(0),
          column = character(0),
          stringsAsFactors = FALSE
        ))
      }
      defs$column <- custom_col(defs$name)
      defs[defs$column %in% cols, , drop = FALSE]
    })

    # Custom columns of a given type, as column names. Used to hand DT the
    # right native editor per column.
    custom_cols_of_type <- function(wanted) {
      defs <- custom_defs()
      defs$column[defs$type %in% wanted]
    }

    # Every date-typed column on screen: the fixed schema's collection date plus
    # any user-defined date variable. Drives both the native date editor and the
    # `col-date` width floor, so a custom date column behaves exactly like the
    # built-in one.
    date_cols <- reactive({
      c(DATE_COL, custom_cols_of_type("date"))
    })

    observeEvent(metadata_base(), {
      State$data <- metadata_base()
      State$custom_dirty <- NULL
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
      custom <- custom_cols()

      # Group the derived MLST / AMR columns and the custom variables under
      # their own headings so the user can pick them out as categories. Without
      # any of them the picker stays flat, exactly as before.
      choices <- grouped_field_choices(cols, mlst, amr, custom)

      # virtualSelectInput rather than pickerInput: in multiple mode it puts a
      # checkbox on every optgroup header (disableOptionGroupCheckbox defaults
      # to FALSE), which is the only way to (de)select a whole category at once
      # - bootstrap-select has no equivalent. Same nested-list `choices`, and
      # the input value is still a plain character vector of column names.
      virtualSelectInput(
        ns("col_picker"),
        label = NULL,
        choices = choices,
        # Derived MLST / AMR columns start hidden - the user opts into the
        # category to show them. Custom variables, being the user's own data,
        # start visible. Keep this consistent with the DT's initial column
        # visibility.
        selected = setdiff(cols, c(mlst, amr)),
        multiple = TRUE,
        search = TRUE,
        searchPlaceholderText = "Search fields ...",
        placeholder = "Show fields …",
        optionsCount = 8,
        noOfDisplayValues = 2,
        # This tab pane is a bslib fill container (overflow: hidden), which
        # clips the dropdown once it grows taller than the remaining space.
        # Render it into <body> instead so it escapes that clip.
        dropboxWrapper = "body",
        # One column-visibility pass per dropdown session rather than one per
        # click: toggling a whole group otherwise redraws the table per option.
        updateOn = "close",
        width = "100%"
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
          liveSearchPlaceholder = "Search isolates ...",
          # Isolate ids are long, so the menu is much wider than the sidebar
          # that holds it. Rendered into <body> it is positioned by Popper,
          # which flips/shifts it back into the viewport instead of letting the
          # sidebar clip it.
          container = "body"
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

    # The Custom Variables panel changed a definition or a value: pick the new
    # columns up. Skipped while edits are pending — the same reason db_updated()
    # is not a dependency of metadata_base(): a reload would wipe them. The user
    # sees the change after they save or discard.
    observeEvent(
      custom_updated(),
      {
        if (!isTRUE(State$pending)) {
          reload_token(reload_token() + 1L)
        }
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
      # MLST / AMR columns are derived from typing (classical_mlst / amr_summary)
      # and are never editable here. Custom variables are: their edits are
      # validated and routed to phylotrace_custom_values on save. The derived
      # ones additionally start hidden, until the user opts into their category.
      derived <- c(mlst_cols(), amr_cols())
      readonly_idx <- .dt_idx(cols, c(READONLY_COLS, derived))
      hidden_idx <- .dt_idx(cols, derived)
      # Two bases on purpose — see .dt_editable_idx(). The className targets
      # below are columnDefs (0-based); the editable list is not.
      date_idx <- .dt_idx(cols, date_cols())
      edit_date_idx <- .dt_editable_idx(cols, date_cols())
      edit_numeric_idx <- .dt_editable_idx(
        cols,
        custom_cols_of_type(NUMERIC_TYPES)
      )

      column_defs <- list(
        list(className = "dt-left", targets = "_all"),
        list(className = "col-readonly", targets = readonly_idx)
      )
      # Give every date column a wider floor than the rest (see col-date in
      # main.scss): a native date input is wider than the 6rem general floor, so
      # without this DT grows the column the moment the editor opens and reflows
      # everything to its right. Guarded on length() because DT's columnDefs
      # treat an empty `targets` as "all columns".
      if (length(date_idx)) {
        column_defs <- c(
          column_defs,
          list(list(className = "col-date", targets = date_idx))
        )
      }
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
          disable = list(columns = readonly_idx),
          # Native inputs per column type, the same way database_custom.R does
          # it: DT builds a <input type="date"> / <input type="number"> itself
          # and its own blur/Escape teardown keeps working. (This replaces a
          # MutationObserver that retyped DT's text input after the fact —
          # which had to re-parse and re-focus the input, and only ever knew
          # about the one hardcoded collection-date column.)
          numeric = edit_numeric_idx,
          date = edit_date_idx
        ),
        selection = "none",
        extensions = "FixedColumns",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 1),
          columnDefs = column_defs,
          initComplete = DT::JS(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();

              $(tableNode).on('keyup', 'input', function(e) {
                if (e.key === 'Enter') this.blur();
              });

              // A native date input only accepts YYYY-MM-DD, so a stored value
              // in any other shape (legacy rows, an import) lands in the editor
              // blank — and blurring that blank would commit it, wiping the
              // date. Recover it from the cell's own data, which DT has not
              // touched yet, and offer it in the form the input can hold.
              $(tableNode).on('focusin', 'input[type=date]', function() {
                // Native date inputs have no format option; the displayed
                // component order/separator follows the browser locale of the
                // element's `lang`. ja renders yyyy/mm/dd, matching the rest
                // of the app's date display.
                this.setAttribute('lang', 'ja');
                if (this.value) return;
                var td = $(this).closest('td');
                if (!td.length) return;
                var raw = api.cell(td[0]).data();
                raw = (raw === null || raw === undefined) ? '' : String(raw).trim();
                if (!raw) return;
                var d = new Date(raw);
                if (!isNaN(d.getTime())) this.value = d.toISOString().split('T')[0];
              });

              api.on('column-visibility.dt', function() {
                api.columns.adjust().draw(false);
              });
            }"
          )
        )
      )
    })

    proxy <- dataTableProxy("metadata_table", session = session)

    # Apply cell edit to in-memory state and push to table without full re-render.
    #
    # Two destinations behind one table: a metadata column is edited in place in
    # State$data and written by save_metadata_table(), while a custom-variable
    # column is validated against its declared type first and buffered in
    # State$custom_dirty for save_custom_values(). Mirrors the same flow in
    # database_custom.R, which is why the rejection and no-op handling below
    # read the same.
    #
    # replaceData() is only called when the client actually needs correcting,
    # not on every edit unconditionally: DT's own blur handler fires cell_edit
    # for a no-op click on an empty cell too (JS null vs. the editor's blur
    # value "" never compare equal), so calling it unconditionally sent a
    # round trip on virtually every click into a not-yet-filled cell. If the
    # user had already clicked into a *different* cell to start editing it by
    # the time that round trip's replaceData() landed, its table-wide redraw
    # tore that cell's still-open editor out from under them — so the first
    # click on it appeared to do nothing and a second one was needed. See the
    # same fix in database_custom.R's values_table_cell_edit observer.
    observeEvent(input$metadata_table_cell_edit, {
      req(is.data.frame(State$data))
      info <- input$metadata_table_cell_edit
      col <- names(State$data)[info$col + 1L]

      if (!col %in% custom_cols()) {
        old_value <- State$data[[col]][[info$row]]
        State$data <- editData(State$data, info, rownames = FALSE)
        new_value <- State$data[[col]][[info$row]]

        unchanged <- if (is.na(old_value) || is.na(new_value)) {
          is.na(old_value) && is.na(new_value)
        } else {
          old_value == new_value
        }
        if (isTRUE(unchanged)) {
          return()
        }

        State$pending <- TRUE
        if (!identical(as.character(new_value), as.character(info$value))) {
          replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
        }
        return()
      }

      defs <- custom_defs()
      def <- defs[defs$column == col, , drop = FALSE]
      req(nrow(def) == 1)

      checked <- coerce_custom_value(
        info$value,
        def$type[[1]],
        def$levels[[1]]
      )

      if (!isTRUE(checked$ok)) {
        replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
        showNotification(
          paste0(
            "\"",
            info$value,
            "\" is not a valid ",
            tolower(CUSTOM_TYPES[[def$type[[1]]]]),
            " value. ",
            checked$reason
          ),
          type = "warning",
          duration = 6
        )
        return()
      }

      stored <- checked$value
      # Keep the column's R type so a numeric variable still sorts as a number.
      new_value <- if (def$type[[1]] %in% NUMERIC_TYPES) {
        suppressWarnings(as.numeric(stored))
      } else {
        stored
      }

      # DT fires cell_edit whenever the input's value differs from the cell's
      # raw JS data, and an empty custom cell is NA (`null` in JS) while the
      # editor always reads back "" on blur — so a click-in/click-out with no
      # typing would otherwise mark the table dirty. Compare the real values.
      old_value <- State$data[[col]][[info$row]]
      unchanged <- if (is.na(old_value) || is.na(new_value)) {
        is.na(old_value) && is.na(new_value)
      } else {
        old_value == new_value
      }
      if (isTRUE(unchanged)) {
        return()
      }

      State$data[[col]][info$row] <- new_value

      entry <- data.frame(
        field_id = def$id[[1]],
        isolate = State$data$isolate[[info$row]],
        value = stored,
        stringsAsFactors = FALSE
      )
      # One row per cell: re-editing a cell replaces its pending value.
      keep <- if (is.null(State$custom_dirty)) {
        NULL
      } else {
        State$custom_dirty[
          !(State$custom_dirty$field_id == entry$field_id &
            State$custom_dirty$isolate == entry$isolate),
          ,
          drop = FALSE
        ]
      }
      State$custom_dirty <- rbind(keep, entry)
      State$pending <- TRUE
      if (!identical(stored, as.character(info$value))) {
        replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
      }
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
      # Two destinations. The MLST / AMR columns are derived and never written
      # back; the custom columns are user data but live in
      # phylotrace_custom_values, so they are stripped from the metadata write
      # and sent through save_custom_values() instead.
      strip <- c(mlst_cols(), amr_cols(), custom_cols())
      to_save <- State$data[,
        setdiff(names(State$data), strip),
        drop = FALSE
      ]
      save_metadata_table(db_path(), to_save)

      if (is.data.frame(State$custom_dirty) && nrow(State$custom_dirty)) {
        save_custom_values(db_path(), State$custom_dirty)
        State$custom_dirty <- NULL
        # Same signal the Custom Variables panel raises when it writes, so the
        # two views of phylotrace_custom_values cannot drift apart.
        custom_updated(isolate(custom_updated()) + 1L)
      }

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
      State$custom_dirty <- NULL
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
