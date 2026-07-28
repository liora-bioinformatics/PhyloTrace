# app/view/database_custom.R
#
# "Custom Variables" interface of the Database menu.
#
# Lets the user define their own typed, per-isolate fields (see
# app/logic/custom_fields.R) and fill them in: an overview of what is defined
# and an editable isolate x variable table. This is the *only* place custom
# values are written — every other view (Browse Entries, the visualization
# engines) merges them in read-only.

box::use(
  shiny,
  bslib[as_fill_carrier, navset_card_tab, nav_panel, layout_sidebar, sidebar],
  jsonlite[toJSON],
  shinyjs[addClass, disable, disabled, enable, hidden, removeClass, toggle],
  shinyWidgets[virtualSelectInput],
  DT[
    DTOutput,
    JS,
    dataTableProxy,
    datatable,
    renderDT,
    replaceData,
    selectRows
  ],
)

box::use(
  app / logic / custom_fields,
  app / logic / field_labels[field_labels_for],
  app / logic / functions[panel_card],
  app / logic / pymlst[existing_strains],
)

# The allowed values a `boolean` cell offers; `category` takes its own levels.
BOOLEAN_LEVELS <- c("yes", "no")

# 0-based DT column indices for `cols` within `all_cols`; NAs dropped.
# Mirrors the helper in database_browser.R.
.dt_idx <- function(all_cols, cols) {
  idx <- match(cols, all_cols)
  as.integer(idx[!is.na(idx)] - 1L)
}

# The values a cell of `type` may take, or NULL when it is free-form.
.type_levels <- function(type, levels_json) {
  if (identical(type, "boolean")) {
    return(BOOLEAN_LEVELS)
  }
  if (identical(type, "category")) {
    return(custom_fields$field_levels(levels_json))
  }
  NULL
}

# Comma-separated user input -> level vector.
.split_levels <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  as_fill_carrier(
    shiny$div(
      id = ns("module-container"),
      as_fill_carrier(shiny$uiOutput(ns("body"), fill = TRUE))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L),
  custom_updated = shiny$reactiveVal(0L)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # `data` is the displayed isolate x variable frame, `dirty` the cells edited
    # since the last save (one row per cell: field_id / isolate / value), so a
    # save writes only what actually changed.
    State <- shiny$reactiveValues(data = NULL, dirty = NULL, pending = FALSE)

    # The variable currently open in the edit dialog.
    editing_id <- shiny$reactiveVal(NULL)

    # Bumped whenever this panel changes the store, to re-read it. `reload()`
    # also signals the change outwards, so Browse Entries can pick the new
    # columns up (see database.R's custom_updated).
    reload_token <- shiny$reactiveVal(0L)

    reload <- function() {
      reload_token(shiny$isolate(reload_token()) + 1L)
      custom_updated(shiny$isolate(custom_updated()) + 1L)
    }

    # Browse Entries edits these same values now, so the signal runs both ways:
    # re-read when someone else writes the store. Skipped while this panel has
    # unsaved edits — a reload would wipe them — exactly as Browse Entries
    # guards its own reload. The bump `reload()` raises is caught here too, but
    # only ever costs one redundant re-read: reload_token does not feed back
    # into custom_updated.
    shiny$observeEvent(
      custom_updated(),
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
      custom_fields$list_custom_fields(db_path())
    })

    # Column name -> type, the bridge between the DT (which knows column names)
    # and the definitions (which the type restrictions come from).
    col_types <- shiny$reactive({
      defs <- fields()
      stats::setNames(defs$type, custom_fields$custom_col(defs$name))
    })

    isolates <- shiny$reactive({
      reload_token()
      sort(existing_strains(db_path()))
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

      custom_fields$append_custom(
        data.frame(isolate = ids, stringsAsFactors = FALSE),
        path
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
        reload_token(reload_token() + 1L)
      },
      ignoreInit = TRUE
    )

    # -- Body ----------------------------------------------------------------

    output$body <- shiny$renderUI({
      if (!nrow(fields())) {
        return(as_fill_carrier(shiny$div(
          class = "db-transfer-body",
          empty_hint(ns)
        )))
      }

      layout_sidebar(
        padding = 0,
        border = FALSE,
        sidebar = sidebar(
          id = ns("controls_sidebar"),
          position = "right",
          width = 300,
          open = TRUE,
          fillable = TRUE,
          as_fill_carrier(
            shiny$div(
              class = "sidebar-control",
              shiny$div(
                class = "sidebar-control-group",
                shiny$div(class = "control-group-label", "Variables"),
                shiny$actionButton(
                  ns("add_var"),
                  "New Variable",
                  icon = shiny$icon("plus"),
                  width = "100%"
                )
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
              )
            )
          )
        ),
        navset_card_tab(
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
          nav_panel(
            "Variables",
            as_fill_carrier(shiny$div(
              class = "db-page_body values-table",
              DTOutput(ns("fields_table"))
            ))
          ),
          nav_panel(
            "Values",
            as_fill_carrier(shiny$div(
              class = "db-page_body custom-fields-values allow-free-text edit-table",
              DTOutput(ns("values_table"), fill = TRUE)
            ))
          )
        )
      )
    })

    # -- Definitions table ---------------------------------------------------

    output$fields_table <- renderDT({
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
            lv <- .type_levels(defs$type[[i]], defs$levels[[i]])
            if (is.null(lv)) "—" else paste(lv, collapse = ", ")
          },
          character(1)
        ),
        Description = ifelse(is.na(defs$description), "", defs$description),
        Filled = paste0(defs$n_filled, " / ", n_isolates),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      datatable(
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

    fields_proxy <- dataTableProxy("fields_table", session = session)

    # A click on a definition row opens the edit dialog for that variable. The
    # selection is cleared straight away: a row that stayed selected could not
    # be clicked a second time (same value, no event), so cancelling the dialog
    # would leave that variable un-editable until the table next re-rendered.
    shiny$observeEvent(input$fields_table_rows_selected, {
      row <- input$fields_table_rows_selected
      shiny$req(length(row) == 1)
      defs <- fields()
      shiny$req(row <= nrow(defs))
      selectRows(fields_proxy, NULL)
      show_field_modal(defs[row, , drop = FALSE])
    })

    # -- Values table --------------------------------------------------------

    output$values_table <- renderDT({
      df <- values_base()
      if (is.null(df) || !nrow(df)) {
        return(datatable(
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
          options = list(dom = "t", ordering = FALSE)
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

      # Values drawn from a fixed list get a native <select> swapped in for
      # DT's own text input the moment it's created (see initComplete below),
      # instead of a free-text box hinting at the allowed values. coerce_custom_value()
      # stays the authority on what may actually be stored.
      defs <- fields()
      lists <- list()
      for (i in seq_len(nrow(defs))) {
        lv <- .type_levels(defs$type[[i]], defs$levels[[i]])
        idx <- .dt_idx(cols, custom_fields$custom_col(defs$name[[i]]))
        if (!is.null(lv) && length(lv) && length(idx)) {
          lists[[as.character(idx)]] <- lv
        }
      }

      datatable(
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
          dom = "ti",
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
          initComplete = JS(sprintf(
            "function(settings) {
              var api = this.api();
              var tableNode = api.table().node();
              var lists = %s;

              // A category/boolean cell may only ever hold one of a fixed
              // set of values, so the moment DT creates its plain text input
              // for such a cell, swap in a native <select> offering exactly
              // those options — instead of hinting at them on a free-text
              // box. The original input is kept in the DOM (merely hidden)
              // so DT's own blur handler, and the Shiny wiring behind it,
              // keep doing the actual commit: this code only ever writes the
              // chosen option back into that input and triggers blur.
              $(tableNode).on('focusin', 'input', function() {
                var $input = $(this);
                if ($input.data('categorySwapped')) return;
                var cell = $input.closest('td');
                if (!cell.length) return;
                var idx = api.cell(cell[0]).index();
                if (!idx) return;
                var options = lists[idx.column];
                if (!options || !options.length) return;

                $input.data('categorySwapped', true);
                var current = $input.val();
                var $select = $('<select class=\"dt-category-select\"></select>');
                options.forEach(function(v) {
                  $select.append($('<option></option>').val(v).text(v));
                });
                if (options.indexOf(current) === -1) {
                  $select.prepend($('<option></option>').val(current).text(current));
                }
                $select.val(current);

                // Guarded so the natural change -> blur sequence (selecting
                // an option fires 'change' without blurring the <select>
                // first) can't run this twice: the handlers are unbound and
                // the element removed on first entry. Blur must commit
                // synchronously, not deferred: a click on a *different* cell
                // blurs this select before dispatching its own click event
                // (see index.js's click -> dblclick promotion), and a
                // deferred commit would still be pending when that other
                // cell's editor opens — the table.draw() it triggers a tick
                // later would then wipe out the new editor, so the click
                // would appear to do nothing and a second one would be
                // needed.
                var commit = function() {
                  var chosen = $select.val();
                  $select.off().remove();
                  $input.val(chosen);
                  $input.trigger('blur');
                };
                $select.on('change', commit);
                $select.on('blur', commit);
                $select.on('keyup', function(e) {
                  if (e.key === 'Escape') commit();
                });

                $input.hide().after($select);
                $select.trigger('focus');
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

              $(tableNode).on('keyup', 'input', function(e) {
                if (e.key === 'Enter') this.blur();
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
            as.character(toJSON(lists, auto_unbox = FALSE))
          ))
        )
      )
    })

    proxy <- dataTableProxy("values_table", session = session)

    # Every edit is validated against its variable's type before it reaches the
    # in-memory frame: a rejected value is rolled back in the table, so what the
    # user sees is always what would be saved.
    shiny$observeEvent(input$values_table_cell_edit, {
      shiny$req(is.data.frame(State$data))
      info <- input$values_table_cell_edit
      col <- names(State$data)[info$col + 1L]
      defs <- fields()
      def <- defs[custom_fields$custom_col(defs$name) == col, , drop = FALSE]
      shiny$req(nrow(def) == 1)

      checked <- custom_fields$coerce_custom_value(
        info$value,
        def$type[[1]],
        def$levels[[1]]
      )

      if (!isTRUE(checked$ok)) {
        replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
        shiny$showNotification(
          paste0(
            "\"",
            info$value,
            "\" is not a valid ",
            tolower(custom_fields$CUSTOM_TYPES[[def$type[[1]]]]),
            " value. ",
            checked$reason
          ),
          type = "warning",
          duration = 6
        )
        return()
      }

      stored <- checked$value
      # The displayed frame keeps the column's R type, so a numeric variable
      # keeps sorting as a number rather than as a string.
      new_value <- if (def$type[[1]] %in% custom_fields$NUMERIC_TYPES) {
        suppressWarnings(as.numeric(stored))
      } else {
        stored
      }

      # DT's own inline editor fires a cell_edit event whenever the input's
      # value differs from the cell's raw JS data even when nothing was
      # typed: an empty (NA) cell serialises to `null`, but the editor input
      # always reads back as `""` on blur, so `"" !== null` trips DT's change
      # check on a no-op click. Re-check the actual value here so that
      # doesn't mark the panel dirty (compare, not `df[is.na(df)] <- ""` as
      # database_browser.R's metadata_table does — that would coerce this
      # table's numeric columns to character and break their sort order).
      old_value <- State$data[[col]][[info$row]]
      unchanged <- if (is.na(old_value) || is.na(new_value)) {
        is.na(old_value) && is.na(new_value)
      } else {
        old_value == new_value
      }
      if (isTRUE(unchanged)) {
        # No replaceData() here: nothing in State$data actually changed, so
        # there is nothing to correct on the client. Calling it anyway (as
        # this used to) sends a round trip on *every* no-op blur — and
        # because the NA/"" mismatch above means a never-yet-filled cell
        # always takes this branch, that fired on virtually every click in a
        # freshly-created variable's empty column. If the user had already
        # clicked into a *different* cell to start editing it by the time
        # that round trip's replaceData() landed, its table-wide redraw tore
        # out the new cell's still-open editor out from under them — so the
        # first click appeared to do nothing and a second one was needed.
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
      keep <- if (is.null(State$dirty)) {
        NULL
      } else {
        State$dirty[
          !(State$dirty$field_id == entry$field_id &
            State$dirty$isolate == entry$isolate),
          ,
          drop = FALSE
        ]
      }
      State$dirty <- rbind(keep, entry)
      State$pending <- TRUE
      # Unlike the no-op case above, an *accepted* edit always gets a
      # replaceData() round trip, even when the canonical stored form is
      # byte-identical to what the user typed: DT's blur handler updates the
      # cell's data client-side without ever calling draw(), so the table's
      # per-row sort/filter caches are never rebuilt for that edit and stay
      # built from the pre-edit data. Skipping replaceData() here left the
      # next column-header sort click as the first thing to force a draw -
      # at which point DataTables rebuilt those stale caches and blanked
      # every cell edited since the table last rendered, even though the
      # value was already saved correctly (a full reload always showed it
      # right, since that goes through a real render).
      replaceData(proxy, State$data, resetPaging = FALSE, rownames = FALSE)
    })

    shiny$observeEvent(State$pending, {
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

    shiny$observeEvent(input$save, {
      shiny$req(is.data.frame(State$dirty), nrow(State$dirty) > 0)
      n <- nrow(State$dirty)
      custom_fields$save_custom_values(db_path(), State$dirty)
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
      fresh <- values_base()
      State$data <- fresh
      State$dirty <- NULL
      State$pending <- FALSE
      replaceData(proxy, fresh, resetPaging = FALSE, rownames = FALSE)
    })

    # -- New / edit variable -------------------------------------------------

    # One dialog for both: `def` is NULL when creating. The type is only
    # choosable on creation — stored values are canonicalised for the type they
    # were entered under, so changing it later would silently invalidate them.
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
          value = paste(.type_levels(type, def$levels[[1]]), collapse = ", "),
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
              shiny$selectInput(
                ns("var_type"),
                "Type",
                choices = stats::setNames(
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

    # -- Remove --------------------------------------------------------------

    output$remove_picker_ui <- shiny$renderUI({
      defs <- fields()
      choices <- if (nrow(defs)) {
        stats::setNames(
          defs$id,
          field_labels_for(custom_fields$custom_col(defs$name))
        )
      } else {
        character(0)
      }
      # virtualSelectInput, same as Browse Entries' col_picker / remove_picker
      # (see database_browser.R): opens centered over the viewport via the
      # popup config below instead of anchored under this 300px sidebar
      # control, and dropboxWrapper = "body" still escapes the sidebar's own
      # clipping ancestor for the (unlikely) non-popup fallback.
      virtualSelectInput(
        ns("remove_picker"),
        label = NULL,
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        search = TRUE,
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
      custom_fields$delete_custom_field(db_path(), selected)
      reload()
      shiny$showNotification(
        paste0(length(selected), " custom variable(s) removed."),
        type = "message",
        duration = 3
      )
    })

    list(changed = shiny$reactive(custom_updated()))
  })
}

# Shown instead of the two tables while no variable is defined: what custom
# variables are for, and how to make one.
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
    shiny$actionButton(
      ns("add_var_empty"),
      "Create the first variable",
      icon = shiny$icon("plus"),
      class = "btn-success"
    )
  )
}
