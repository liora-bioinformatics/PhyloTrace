# app/view/database_custom.R
#
# "Custom Variables" interface of the Database menu.
#
# Lets the user define their own typed, per-isolate fields (see
# app/logic/custom_fields.R) and fill them in: an overview of what is defined,
# an editable isolate x variable table, and bulk import from an isolate-keyed
# table (app/view/database_custom_import.R). Custom values are written only
# here and in Browse Entries' grid; the visualization engines merge them in
# read-only.

box::use(
  bslib,
  DT,
  jsonlite[toJSON],
  shiny,
  shinyjs[addClass, disable, disabled, enable, hidden, removeClass, toggle],
  shinyWidgets[virtualSelectInput, pickerInput],
  stats[setNames],
)

box::use(
  app / logic / custom_fields,
  app / logic / db_events,
  app / logic / db_guard[db_failed, guard_db, guard_db_read],
  app / logic / field_labels[CUSTOM_COL_PREFIX, field_labels_for],
  app / logic / functions[table_loading, table_processing],
  app / logic / pymlst[existing_strains],
  app / logic / sheet_grid,
  app / view / database_custom_import,
  app / view / help,
)

# 0-based DT column indices for `cols` within `all_cols`; NAs dropped.
# Mirrors the helper in database_browser.R.
.dt_idx <- function(all_cols, cols) {
  idx <- match(cols, all_cols)
  as.integer(idx[!is.na(idx)] - 1L)
}

# Splits comma-separated user input into a clean vector of levels.
.split_levels <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$as_fill_carrier(
    shiny$div(
      id = ns("module-container"),
      database_custom_import$ui(ns("import")),
      help$ui(ns("help")),
      bslib$as_fill_carrier(shiny$uiOutput(ns("body"), fill = TRUE))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L),
  db_rev = db_events$new_bus()
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # `data` is the displayed isolate x variable frame, `dirty` the cells edited
    # since the last save (one row per cell: field_id / isolate / value), so a
    # save writes only what actually changed.
    State <- shiny$reactiveValues(data = NULL, dirty = NULL, pending = FALSE)

    # The variable currently open in the edit dialog.
    editing_id <- shiny$reactiveVal(NULL)

    # Bumped whenever the store this panel shows has to be re-read. `reload()`
    # also announces the write outwards, so every other reader of custom
    # variables (Browse Entries' column picker, the visualization engines'
    # field lists, Export) picks the change up.
    reload_token <- shiny$reactiveVal(0L)

    reload <- function() {
      reload_token(shiny$isolate(reload_token()) + 1L)
      db_events$bump(db_rev, "custom_fields")
    }

    # The reverse direction: someone else wrote what this panel is showing.
    # `custom_fields` because Browse Entries edits these same values, and
    # `isolates` because the table has one row per isolate, so typing or a
    # removal changes its shape.
    #
    # Skipped while this panel has unsaved edits - a re-read would wipe them.
    # That is the deliberate exception to automatic refresh: the panel goes
    # stale rather than discarding work the user has not saved, and catches up
    # on the next save or discard. `reload()`'s own bump lands here too, but
    # only ever costs one redundant re-read - reload_token does not feed back
    # into db_rev.
    shiny$observeEvent(
      db_events$revision(db_rev, "custom_fields", "isolates"),
      {
        if (!isTRUE(State$pending)) {
          reload_token(shiny$isolate(reload_token()) + 1L)
        }
      },
      ignoreInit = TRUE
    )

    # -- Store reads ---------------------------------------------------------

    fields <- shiny$reactive({
      reload_token()
      guard_db_read(
        "Loading custom variables",
        custom_fields$list_custom_fields(db_path())
      )
    })

    # Column name -> type, the bridge between the DT (which knows column names)
    # and the definitions (which the type restrictions come from).
    col_types <- shiny$reactive({
      defs <- fields()
      setNames(defs$type, custom_fields$custom_col(defs$name))
    })

    isolates <- shiny$reactive({
      reload_token()
      sort(guard_db_read("Loading the isolate list", existing_strains(db_path())))
    })

    values_base <- shiny$reactive({
      reload_token()
      path <- db_path()
      if (is.null(path) || is.na(path) || !nrow(fields())) {
        return(NULL)
      }

      ids <- isolates()
      if (!length(ids)) {
        return(NULL)
      }

      guard_db_read(
        "Loading custom variables",
        custom_fields$append_custom(
          data.frame(isolate = ids, stringsAsFactors = FALSE),
          path
        )
      )
    })

    shiny$observeEvent(
      values_base(),
      {
        State$data <- values_base()
        State$dirty <- NULL
        State$pending <- FALSE
      },
      ignoreNULL = FALSE
    )

    shiny$observeEvent(
      session_reset(),
      {
        State$data <- NULL
        State$dirty <- NULL
        State$pending <- FALSE
        sheet_grid$clear_history(history)
        reload_token(reload_token() + 1L)
      },
      ignoreInit = TRUE
    )

    # -- Body ----------------------------------------------------------------

    output$body <- shiny$renderUI({
      if (!nrow(fields())) {
        return(bslib$as_fill_carrier(shiny$div(
          class = "db-transfer-body",
          empty_hint(ns)
        )))
      }

      bslib$layout_sidebar(
        padding = 0,
        border = FALSE,
        sidebar = bslib$sidebar(
          id = ns("controls_sidebar"),
          position = "right",
          width = 300,
          open = TRUE,
          fillable = TRUE,
          # Stretched to full height like Browse Entries' sidebar, so the
          # gesture hints can sit at the bottom (see main.scss).
          class = "browse-entries-sidebar",
          bslib$as_fill_carrier(
            shiny$div(
              class = "sidebar-control browse-sidebar",
              shiny$div(
                class = "sidebar-control-group",
                shiny$div(class = "control-group-label", "Variables"),
                shiny$actionButton(
                  ns("add_var"),
                  "New Variable",
                  icon = shiny$icon("plus"),
                  width = "100%"
                ),
                database_custom_import$open_button(ns("import"), width = "100%")
              ),
              shiny$div(
                class = "sidebar-control-group",
                shiny$div(class = "control-group-label", "Edit"),
                disabled(
                  shiny$actionButton(
                    ns("discard"),
                    "Discard",
                    icon = shiny$icon("rotate-left"),
                    width = "100%"
                  )
                ),
                disabled(
                  shiny$actionButton(
                    ns("save"),
                    "Save Changes",
                    class = "btn-success",
                    icon = shiny$icon("floppy-disk"),
                    width = "100%"
                  )
                )
              ),
              shiny$div(
                class = "sidebar-control-group",
                shiny$div(class = "control-group-label", "Removal"),
                shiny$uiOutput(ns("remove_picker_ui")),
                disabled(
                  shiny$actionButton(
                    ns("remove_btn"),
                    "Remove",
                    class = "btn-danger",
                    icon = shiny$icon("trash"),
                    width = "100%"
                  )
                )
              ),
              shiny$div(
                class = "sidebar-control-group selection-stats-slot",
                shiny$uiOutput(ns("values_hints_ui"))
              )
            )
          )
        ),
        bslib$navset_card_tab(
          id = ns("tabs"),
          # save()/reload() re-run this renderUI (it reads fields()), which
          # would otherwise rebuild the navset back to its first panel on
          # every save. Feeding the last-known tab back as `selected` keeps
          # whichever one the user was on; isolate() so a tab click alone
          # (which only sets input$tabs, not fields()) can't retrigger this
          # block.
          selected = if (is.null(shiny$isolate(input$tabs))) {
            "Variables"
          } else {
            shiny$isolate(input$tabs)
          },
          bslib$nav_panel(
            "Variables",
            bslib$as_fill_carrier(shiny$div(
              class = "db-page_body values-table",
              DT$DTOutput(ns("fields_table"))
            ))
          ),
          bslib$nav_panel(
            "Values",
            bslib$as_fill_carrier(shiny$div(
              class = "db-page_body custom-fields-values allow-free-text edit-table",
              # Covers the grid until its first draw (removed in
              # values_table's initComplete).
              table_loading("Loading values ..."),
              DT$DTOutput(ns("values_table"), fill = TRUE)
            ))
          )
        )
      )
    })

    # Gesture hints for the Values grid, shown while that tab is open.
    output$values_hints_ui <- shiny$renderUI({
      shiny$req(identical(input$tabs, "Values"))
      shiny$tagList(
        shiny$div(class = "control-group-label", "Table gestures"),
        sheet_grid$sheet_hints()
      )
    })

    # -- Definitions table ---------------------------------------------------

    output$fields_table <- DT$renderDT({
      defs <- fields()
      shiny$req(nrow(defs) > 0)

      n_isolates <- length(isolates())
      overview <- data.frame(
        Variable = field_labels_for(custom_fields$custom_col(defs$name)),
        Name = defs$name,
        Type = unname(custom_fields$CUSTOM_TYPES[defs$type]),
        `Allowed values` = vapply(
          seq_len(nrow(defs)),
          function(i) {
            lv <- custom_fields$listed_levels(defs$type[[i]], defs$levels[[i]])
            if (is.null(lv)) "—" else paste(lv, collapse = ", ")
          },
          character(1)
        ),
        Description = ifelse(is.na(defs$description), "", defs$description),
        Filled = paste0(defs$n_filled, " / ", n_isolates),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      DT$datatable(
        overview,
        rownames = FALSE,
        selection = "single",
        class = "row-border hover order-column",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          # A placeholder like values_table's: the real height comes from the
          # .db-page_body / one-scrollport CSS (main.scss), which is also what
          # keeps the header fixed while the body scrolls.
          scrollY = "1px",
          scrollCollapse = TRUE,
          ordering = FALSE
        )
      )
    })

    fields_proxy <- DT$dataTableProxy("fields_table", session = session)

    # A click on a definition row opens the edit dialog for that variable. The
    # selection is cleared straight away: a row that stayed selected could not
    # be clicked a second time (same value, no event), so cancelling the dialog
    # would leave that variable un-editable until the table next re-rendered.
    shiny$observeEvent(input$fields_table_rows_selected, {
      row <- input$fields_table_rows_selected
      shiny$req(length(row) == 1)
      defs <- fields()
      shiny$req(row <= nrow(defs))
      DT$selectRows(fields_proxy, NULL)
      show_field_modal(defs[row, , drop = FALSE])
    })

    # -- Values table --------------------------------------------------------

    output$values_table <- DT$renderDT({
      df <- values_base()
      if (is.null(df) || !nrow(df)) {
        return(DT$datatable(
          data.frame(
            " " = paste(
              "No isolates in this database yet.<br>Add isolates by typing",
              "them in the <strong>Add Isolates</strong> module."
            ),
            check.names = FALSE
          ),
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(
            dom = "t",
            ordering = FALSE,
            # Drawn like the real grid would be: lift the loading overlay.
            initComplete = DT$JS(
              "function() {",
              "  $(this.api().table().node())",
              "    .closest('.db-page_body').find('.loci-table-loading').remove();",
              "}"
            )
          )
        ))
      }

      cols <- names(df)
      types <- col_types()
      of_type <- function(wanted) {
        .dt_idx(cols, names(types)[types %in% wanted])
      }
      isolate_idx <- .dt_idx(cols, "isolate")
      date_idx <- of_type("date")
      numeric_idx <- of_type(custom_fields$NUMERIC_TYPES)
      # DT's own DT:::makeEditableField() silently subtracts 1 from
      # `editable$numeric`/`editable$date` (but *not* from `disable$columns` or
      # `columnDefs` targets) whenever `rownames = FALSE`, to compensate for a
      # rownames column it assumes the caller counted in and this table
      # doesn't have. `date_idx`/`numeric_idx` stay true 0-based indices for
      # the other uses below (`col-readonly`/`col-date` targets); only the
      # `editable` values need the +1 to land back on the right column.
      editable_date_idx <- date_idx + 1L
      editable_numeric_idx <- numeric_idx + 1L

      DT$datatable(
        df,
        rownames = FALSE,
        colnames = field_labels_for(cols),
        filter = "top",
        class = "row-border hover order-column",
        editable = list(
          target = "cell",
          disable = list(columns = isolate_idx),
          # Whole/decimal numbers get a native number input, dates a native date
          # picker. Both are also exempt from the app-wide charset filter in
          # app/js/index.js, which only rewrites `input[type=text]`.
          numeric = editable_numeric_idx,
          date = editable_date_idx
        ),
        selection = "none",
        # Pins the isolate column while the variable columns scroll
        # horizontally, matching Browse Entries' metadata_table — the two
        # tables edit the same underlying values and should behave alike.
        extensions = "FixedColumns",
        options = list(
          # "r": DataTables' processing indicator, shown over the grid from
          # each server request (sort, filter, an edit's replaceData()) until
          # its rows are drawn.
          dom = "rti",
          processing = TRUE,
          language = list(processing = table_processing("Updating values ...")),
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 1),
          columnDefs = c(
            list(
              list(className = "dt-left", targets = "_all"),
              list(className = "col-readonly", targets = isolate_idx)
            ),
            # Reserve the width a native date input needs, so opening the
            # editor doesn't widen the column (see col-date in main.scss).
            # Empty `targets` would mean "all columns" to DT, hence the guard.
            if (length(date_idx)) {
              list(list(className = "col-date", targets = date_idx))
            }
          ),
          initComplete = DT$JS(sprintf(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();
              // The rows are drawn: lift the loading overlay.
              $(tableNode).closest('.db-page_body').find('.loci-table-loading').remove();
              // The spreadsheet layer (app/js/dt-sheet.js): range
              // selection, clipboard, fill, undo/redo and the dropdown editor
              // for category / yes-no cells.
              %s

              // Hover text: the identifier behind each header label, as a
              // table import must head its column to fill this one.
              var titles = %s;
              api.columns().every(function(i) {
                if (titles[i]) $(this.header()).attr('title', titles[i]);
              });

              // A native date input only accepts YYYY-MM-DD, so a value stored
              // in any other shape lands in the editor blank — and blurring
              // that blank would commit it, wiping the date. Recover it from
              // the cell's own data, which DT has not touched yet.
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

              // With scrollX, DT fixes each column's width once (split into
              // the separate header/body tables the 'One scrollport per
              // table' CSS then re-unifies) and white-space: nowrap (see
              // .edit-table in main.scss) keeps a cell from wrapping to fit
              // inside that width — so a value wider than the column's
              // initial content (a long category value already stored when
              // the page loads, or the first long value entered into a
              // freshly-created, still-empty variable) just overflows its
              // cell instead of widening the column, leaving the header out
              // of step with where the body's columns actually render.
              // Recomputing widths on every future redraw keeps both in sync
              // with whatever is currently shown — but initComplete itself
              // runs after the *first* draw has already happened, before
              // this listener exists to catch it, so that initial render
              // needs its own adjust() call too.
              api.on('draw.dt', function() {
                api.columns.adjust();
              });
              api.columns.adjust();
            }",
            sheet_grid$sheet_attach_js(
              ns("values_table_sheet"),
              isolate_idx,
              sheet_grid$sheet_lists(cols, fields())
            ),
            toJSON(sub(paste0("^", CUSTOM_COL_PREFIX), "", cols))
          ))
        )
      )
    })

    proxy <- DT$dataTableProxy("values_table", session = session)

    # Undo/redo steps for this grid's edits (see app/logic/sheet_grid.R). Kept
    # across saves, dropped on discard and whenever a different database is
    # shown.
    history <- sheet_grid$new_history()

    # Validates a batch of cell edits against each variable's type and applies
    # the accepted ones, so what the user sees is always what would be saved: a
    # rejected value is rolled back in the table. `edits` holds `row` (in
    # State$data), `column` and `value`; `skipped` counts read-only cells the
    # client already left out. DT's single-cell editor, the spreadsheet layer's
    # paste/cut/delete/fill and undo/redo (`kind`, which skips validation to
    # restore what the grid held) all land here. Mirrors apply_cell_edits() in
    # database_browser.R.
    #
    # replaceData() runs only when something changed or was rejected. A no-op
    # (DT fires cell_edit for a click-in/click-out on an empty cell, since its
    # `null` never equals the editor's "") must not round-trip: the redraw
    # would tear out an editor the user has meanwhile opened in another cell.
    # An accepted edit always redraws, even when the stored form is identical
    # to what the client shows: DT's blur handler updates the cell's data
    # without draw(), leaving the sort/filter caches stale until the next
    # draw, which would blank every cell edited since.
    apply_cell_edits <- function(edits, skipped = 0L, kind = "edit") {
      df <- State$data
      defs <- fields()
      cols <- custom_fields$custom_col(defs$name)
      locked <- is.na(edits$column) | edits$column == "isolate"

      res <- sheet_grid$apply_edits(
        df,
        edits[!locked, , drop = FALSE],
        types = setNames(defs$type, cols),
        levels = setNames(defs$levels, cols),
        na_cols = cols,
        trusted = kind != "edit"
      )

      changed <- res$changed
      if (nrow(changed)) {
        isolates <- df$isolate[changed$row]
        State$data <- res$data
        # A cell put back to its saved value (typically by an undo) is no
        # longer an unsaved change.
        State$dirty <- sheet_grid$merge_dirty(
          State$dirty,
          data.frame(
            field_id = defs$id[match(changed$column, cols)],
            isolate = isolates,
            value = changed$value,
            stringsAsFactors = FALSE
          ),
          c("field_id", "isolate"),
          sheet_grid$matches_base(
            shiny$isolate(values_base()),
            isolates,
            changed$column,
            changed$value
          )
        )
        State$pending <- !is.null(State$dirty)
        sheet_grid$record_step(
          history,
          data.frame(
            isolate = isolates,
            column = changed$column,
            value = changed$old,
            stringsAsFactors = FALSE
          ),
          kind
        )
      }
      if (nrow(changed) || nrow(res$rejected)) {
        # No processing overlay over this reload: it would cover the grid
        # after every edit, and catch the click meant for the next cell.
        session$sendCustomMessage(
          "pt-dt-quiet-reload",
          list(id = session$ns("values_table"))
        )
        DT$replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
      }

      report <- sheet_grid$edit_report(res$rejected, skipped + sum(locked))
      if (!is.null(report)) {
        shiny$showNotification(
          report,
          type = if (nrow(res$rejected)) "warning" else "message",
          duration = 6
        )
      }
    }

    # DT's own single-cell editor.
    shiny$observeEvent(input$values_table_cell_edit, {
      shiny$req(is.data.frame(State$data))
      info <- input$values_table_cell_edit
      apply_cell_edits(data.frame(
        row = as.integer(info$row),
        column = names(State$data)[as.integer(info$col) + 1L],
        value = as.character(info$value),
        stringsAsFactors = FALSE
      ))
    })

    # The spreadsheet layer (app/js/dt-sheet.js): a pasted, cut, cleared or
    # filled block, or an undo/redo request.
    shiny$observeEvent(input$values_table_sheet, {
      shiny$req(is.data.frame(State$data))
      msg <- input$values_table_sheet
      if (identical(msg$op, "edit")) {
        apply_cell_edits(
          sheet_grid$decode_batch(msg, State$data),
          skipped = sum(as.integer(msg$skipped))
        )
        return()
      }
      shiny$req(msg$op %in% c("undo", "redo"))
      step <- sheet_grid$take_step(history, msg$op)
      shiny$req(step)
      apply_cell_edits(
        data.frame(
          row = match(step$isolate, State$data$isolate),
          column = step$column,
          value = step$value,
          stringsAsFactors = FALSE
        ),
        kind = msg$op
      )
    })

    shiny$observeEvent(db_path(), sheet_grid$clear_history(history))

    # Import is held back while edits are pending: its reload would discard them.
    shiny$observeEvent(State$pending, {
      if (isTRUE(State$pending)) {
        enable("save")
        addClass("save", "btn-attention")
        enable("discard")
        disable("import-open")
      } else {
        disable("save")
        removeClass("save", "btn-attention")
        disable("discard")
        enable("import-open")
      }
    })

    shiny$observeEvent(input$save, {
      shiny$req(is.data.frame(State$dirty), nrow(State$dirty) > 0)
      n <- nrow(State$dirty)
      # The edits stay pending on failure, so Save can simply be pressed again.
      saved <- guard_db(
        "Saving custom variable values",
        custom_fields$save_custom_values(db_path(), State$dirty)
      )
      if (db_failed(saved)) {
        return()
      }
      State$dirty <- NULL
      State$pending <- FALSE
      reload()
      shiny$showNotification(
        paste0(n, " value(s) saved."),
        type = "message",
        duration = 3
      )
    })

    shiny$observeEvent(input$discard, {
      sheet_grid$clear_history(history)
      fresh <- values_base()
      State$data <- fresh
      State$dirty <- NULL
      State$pending <- FALSE
      DT$replaceData(proxy, fresh, resetPaging = FALSE, rownames = FALSE)
    })

    # -- New / edit variable -------------------------------------------------

    # Displays modal dialog to create a new custom variable or edit an existing definition.
    show_field_modal <- function(def = NULL) {
      editing <- !is.null(def)
      type <- if (editing) def$type[[1]] else "text"

      levels_group <- shiny$div(
        id = ns("levels_group"),
        # allow-free-text: the levels are display values, so they take spaces
        # and the commas that separate them.
        class = "allow-free-text",
        shiny$textInput(
          ns("var_levels"),
          "Allowed values",
          value = paste(custom_fields$listed_levels(type, def$levels[[1]]), collapse = ", "),
          placeholder = "blood, urine, wound"
        ),
        shiny$div(
          class = "text-muted small mb-3",
          "Comma-separated. Only these values may be entered."
        )
      )

      editing_id(if (editing) def$id[[1]] else NULL)

      shiny$showModal(
        shiny$div(
          id = "custom-variable-modal",
          shiny$modalDialog(
            title = if (editing) {
              "Edit custom variable"
            } else {
              "New custom variable"
            },
            shiny$textInput(
              ns("var_name"),
              "Name",
              value = if (editing) def$name[[1]] else "",
              placeholder = "e.g. sampling_site"
            ),
            shiny$div(
              class = "text-muted small mb-3",
              "Letters, digits, '_' and '-' only — the name doubles as the column",
              "name when the variable is exported. It is shown prettified:",
              "\"sampling_site\" reads as \"Sampling Site\"."
            ),
            if (editing) {
              shiny$div(
                class = "mb-3",
                shiny$tags$label(class = "control-label", "Type"),
                shiny$div(
                  class = "form-control-plaintext",
                  unname(custom_fields$CUSTOM_TYPES[type])
                ),
                shiny$div(
                  class = "text-muted small",
                  "The type cannot be changed — create a new variable instead."
                )
              )
            } else {
              pickerInput(
                ns("var_type"),
                "Type",
                choices = setNames(
                  names(custom_fields$CUSTOM_TYPES),
                  unname(custom_fields$CUSTOM_TYPES)
                )
              )
            },
            # Hidden up front rather than toggled after showModal, so the field
            # never flashes into view for a non-category variable.
            if (identical(type, "category")) {
              levels_group
            } else {
              hidden(levels_group)
            },
            shiny$textAreaInput(
              ns("var_description"),
              "Description (optional)",
              value = if (editing && !is.na(def$description[[1]])) {
                def$description[[1]]
              } else {
                ""
              },
              placeholder = "What this variable records, and how it is filled in."
            ),
            footer = shiny$tagList(
              shiny$modalButton("Cancel"),
              shiny$actionButton(
                ns(if (editing) "confirm_edit" else "confirm_new"),
                if (editing) "Save" else "Create",
                class = "btn-success"
              )
            ),
            easyClose = TRUE
          )
        )
      )
    }

    shiny$observeEvent(input$add_var, {
      show_field_modal(NULL)
    })

    shiny$observeEvent(input$add_var_empty, {
      show_field_modal(NULL)
    })

    # The levels field only applies to a category variable. shinyjs rather than
    # conditionalPanel: this panel is injected into a navset_hidden after load,
    # where conditionalPanel does not bind reliably.
    shiny$observeEvent(input$var_type, {
      toggle("levels_group", condition = identical(input$var_type, "category"))
    })

    shiny$observeEvent(input$confirm_new, {
      failure <- tryCatch(
        {
          custom_fields$create_custom_field(
            db_path(),
            name = input$var_name,
            type = input$var_type,
            description = input$var_description,
            levels = .split_levels(input$var_levels)
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )

      if (!is.null(failure)) {
        shiny$showNotification(failure, type = "error", duration = 6)
        return()
      }

      shiny$removeModal()
      reload()
      shiny$showNotification(
        paste0("Custom variable \"", input$var_name, "\" created."),
        type = "message",
        duration = 3
      )
    })

    shiny$observeEvent(input$confirm_edit, {
      id <- editing_id()
      shiny$req(!is.null(id))

      failure <- tryCatch(
        {
          custom_fields$update_custom_field(
            db_path(),
            id,
            name = input$var_name,
            description = input$var_description,
            levels = .split_levels(input$var_levels)
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )

      if (!is.null(failure)) {
        shiny$showNotification(failure, type = "error", duration = 6)
        return()
      }

      shiny$removeModal()
      editing_id(NULL)
      reload()
    })

    help$server("help", topic = "custom_variables")

    # -- Import from table ---------------------------------------------------

    # A merge into a standard metadata field changes the metadata table too, so
    # that domain is announced alongside reload()'s custom_fields bump.
    database_custom_import$server(
      "import",
      db_path = db_path,
      blocked = shiny$reactive(isTRUE(State$pending)),
      on_imported = function(counts) {
        reload()
        if (isTRUE(counts$metadata)) {
          db_events$bump(db_rev, "metadata")
        }
      }
    )

    # -- Remove --------------------------------------------------------------

    output$remove_picker_ui <- shiny$renderUI({
      defs <- fields()
      choices <- if (nrow(defs)) {
        setNames(
          defs$id,
          field_labels_for(custom_fields$custom_col(defs$name))
        )
      } else {
        character(0)
      }

      virtualSelectInput(
        ns("remove_picker"),
        label = NULL,
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        search = TRUE,
        # Without this, the header checkbox selects every variable in the
        # list, not just the ones the search term currently matches.
        selectAllOnlyVisible = TRUE,
        searchPlaceholderText = "Search variables ...",
        placeholder = "Select variables to remove …",
        noOfDisplayValues = 2,
        dropboxWrapper = "body",
        showDropboxAsPopup = TRUE,
        popupDropboxBreakpoint = "10000px",
        width = "100%"
      )
    })

    shiny$observeEvent(
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

    shiny$observeEvent(input$remove_btn, {
      selected <- input$remove_picker
      shiny$req(length(selected) > 0)
      defs <- fields()
      names_sel <- defs$name[defs$id %in% as.integer(selected)]

      shiny$showModal(shiny$modalDialog(
        title = "Remove custom variables",
        paste0(
          if (length(names_sel) == 1) {
            paste0("Permanently remove \"", names_sel, "\"")
          } else {
            paste0("Permanently remove ", length(names_sel), " variables")
          },
          " and every value stored for them? This cannot be undone."
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(
            ns("confirm_remove"),
            "Remove",
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    })

    shiny$observeEvent(input$confirm_remove, {
      selected <- as.integer(input$remove_picker)
      shiny$removeModal()
      removed <- guard_db(
        "Removing custom variables",
        custom_fields$delete_custom_field(db_path(), selected)
      )
      if (db_failed(removed)) {
        return()
      }
      reload()
      shiny$showNotification(
        paste0(length(selected), " custom variable(s) removed."),
        type = "message",
        duration = 3
      )
    })

    # Nothing to return: this panel's writes are announced on the shared
    # revision bus, which every other reader of custom variables follows
    # directly. The `changed` reactive it used to hand back had no consumer.
    invisible(NULL)
  })
}

# Renders empty state guidance UI when no custom variables exist.
empty_hint <- function(ns) {
  example <- function(name, type, what) {
    shiny$tags$li(
      shiny$tags$code(name),
      shiny$tags$span(class = "text-muted", paste0(" — ", type, ", ", what))
    )
  }

  shiny$div(
    class = "custom-fields-hint",
    shiny$h5("No custom variables yet"),
    shiny$p(
      "Custom variables are your own metadata fields, on top of the standard",
      "ones. Each has a name, a type and an optional description, and holds",
      "one value per isolate."
    ),
    shiny$p(
      "They live inside this database file, so they travel with it and can be",
      "carried through Export and Import. Once defined they can be shown in",
      "Browse Entries and used to colour, label and group plots in the",
      "Visualization module."
    ),
    shiny$tags$ul(
      class = "custom-fields-hint_examples",
      example("ward", "Text", "where the isolate came from"),
      example("ct_value", "Decimal number", "a measurement"),
      example("collected_on", "Date", "an extra date, entered as YYYY-MM-DD"),
      example("is_index_case", "Yes / No", "a flag"),
      example("specimen", "Category", "one of a fixed list you define")
    ),
    shiny$p(
      class = "text-muted",
      "The type decides what may be entered: a whole-number cell refuses",
      "\"12.5\", a category cell only accepts one of its allowed values."
    ),
    shiny$p(
      class = "text-muted",
      "Already keep this information in a spreadsheet? Import it: a table with",
      "one row per isolate, the isolate name in a column headed \"isolate\"",
      "(or the first column), and one column per variable."
    ),
    shiny$div(
      class = "custom-fields-hint_actions",
      shiny$actionButton(
        ns("add_var_empty"),
        "Create the first variable",
        icon = shiny$icon("plus"),
        class = "btn-success"
      ),
      database_custom_import$open_button(ns("import"))
    )
  )
}
