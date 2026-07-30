# app/view/database_export.R
#
# "Export" interface of the Database menu. UI and backend live here so
# the panel computes its own state independently of the other menu entries.
#
# Writes a new `.db` holding a chosen set of isolates and a chosen set of
# metadata columns. Withheld columns are never written to the destination — see
# app/logic/db_export.R for why the file is built fresh rather than pruned.

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    observe,
    reactive,
    reactiveVal,
    req,
    div,
    span,
    strong,
    tags,
    icon,
    actionButton,
    checkboxInput,
    updateCheckboxInput,
    uiOutput,
    renderUI,
    showNotification,
    withProgress,
    setProgress,
    tagList,
    h5,
    showModal,
    modalDialog,
    modalButton,
    removeModal,
    dateRangeInput,
    updateDateRangeInput,
    isolate,
  ],
  bslib[as_fill_carrier, tooltip, layout_sidebar, sidebar],
  shinyjs[disabled, disable, enable],
  shinyWidgets[pickerInput, pickerOptions],
  shinyFiles[shinySaveButton, shinyFileSave, parseSavePath],
  fs[path_home],
  waiter[Waiter, spin_flower, useWaiter],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows, JS],
)

box::use(
  app / logic / custom_fields[append_custom, custom_col],
  app /
    logic /
    db_export[
      export_preview,
      export_database,
      available_result_tables,
      exportable_custom_fields,
      METADATA_FIXED_COLS
    ],
  app / logic / database_functions[metadata_columns, make_metadata_table],
  app / logic / db_sources[SOURCE_COL],
  app / logic / pymlst[existing_strains],
  app / logic / field_labels[field_chips, field_labels_for],
  app / logic / field_types[as_date_safe, date_fields],
  app / logic / functions[panel_card, stat_tile, transfer_cards],
  app /
    logic /
    profile_io[
      export_typing_results,
      typing_export_target,
      typing_preview,
      replace_ext
    ],
)

# Deliverables. A `.db` is a complete PhyloTrace database; a profile table is the
# interoperable currency every other cgMLST tool reads.
EXPORT_TYPES <- c(
  "PhyloTrace database (.db)" = "database",
  "Typing results (table)" = "typing"
)

FILE_FORMATS <- c(
  "Excel workbook (.xlsx)" = "xlsx",
  "Tab-separated (.tsv)" = "tsv",
  "Comma-separated (.csv)" = "csv"
)

SEQUENCE_MODES <- c(
  "None" = "none",
  "Single FASTA (>locus|hash)" = "fasta",
  "One FASTA per locus (zip)" = "per_locus"
)

# One save button per *output extension*, keyed by that extension. Exactly one is
# shown at a time: the one that matches the file the current selection will really
# produce, so the dialog never offers a type the export cannot write.
#
# They must be separate, statically-mounted buttons. shinyFiles binds a file
# dialog to the button *element*, so a single button re-rendered through renderUI
# (to follow the export type) swaps that element out and leaves the previous
# dialog orphaned in the DOM — an empty window behind the real one.
DEST_BUTTONS <- list(
  db = list(
    label = "Choose .db file",
    title = "Export database as",
    filetype = list("PhyloTrace database" = "db")
  ),
  xlsx = list(
    label = "Choose .xlsx file",
    title = "Export typing results as",
    filetype = list("Excel workbook" = "xlsx")
  ),
  tsv = list(
    label = "Choose .tsv file",
    title = "Export typing results as",
    filetype = list("Tab-separated" = "tsv")
  ),
  csv = list(
    label = "Choose .csv file",
    title = "Export typing results as",
    filetype = list("Comma-separated" = "csv")
  ),
  # Metadata and/or sequences alongside the profile cannot live in one table
  # file, so that selection is delivered as a bundle.
  zip = list(
    label = "Choose .zip file",
    title = "Export typing results as a bundle …",
    filetype = list("Bundle" = "zip")
  )
)

.roots <- function() c(Home = path_home(), Root = "/")

# shinyFiles renders a Root-relative pick as "//tmp/x.db"; collapse the leading
# slashes so the path displays cleanly and compares equal to the real one.
.clean_path <- function(p) sub("^/{2,}", "/", as.character(p))

typing_note <- function(sequences) {
  tagList(
    tags$p(tagList(
      icon("circle-info"),
      " Alleles are written as ",
      strong("sha256 hashes"),
      " of their DNA sequence — the same allele hashes the same in any",
      " database, so a peer can compare these isolates against their own."
    )),
    if (!identical(sequences, "none")) {
      tags$p(tagList(
        icon("dna"),
        " The allele sequences are included, so the recipient can link these",
        " calls to their own alleles whatever identifiers they use."
      ))
    }
  )
}

.fmt_bytes <- function(b) {
  if (!length(b) || is.na(b)) {
    return("—")
  }
  if (b >= 1e9) {
    sprintf("%.1f GB", b / 1e9)
  } else if (b >= 1e6) {
    sprintf("%.0f MB", b / 1e6)
  } else {
    sprintf("%.0f kB", max(b / 1e3, 1))
  }
}

# A labelled cluster of controls in the sidebar. The id sits on the group
# itself, so shinyjs::toggle() takes the label away with the widgets it names.
control_group <- function(label, ..., id = NULL, class = NULL) {
  div(
    id = id,
    class = paste("io-control-group", class),
    div(class = "control-group-label", label),
    ...
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      id = ns("module-container"),
      layout_sidebar(
        padding = 0,
        border = FALSE,
        # The choices read top to bottom: decide WHAT to export, then HOW it
        # should be written, then WHERE. Steps that do not apply to the chosen
        # export type are hidden (server-driven, via shinyjs — this panel is
        # injected into a navset_hidden after load, where conditionalPanel
        # never binds).
        sidebar = sidebar(
          id = ns("controls_sidebar"),
          position = "right",
          width = 350,
          open = TRUE,
          fillable = TRUE,
          as_fill_carrier(
            div(
              class = "io-control",

              # -- 1. What ---------------------------------------------------
              control_group(
                "Export type",
                pickerInput(
                  ns("export_type"),
                  label = NULL,
                  choices = EXPORT_TYPES
                )
              ),
              control_group("Isolates", uiOutput(ns("isolate_picker_ui"))),

              # `isolate` and `organism` travel with every export, so the
              # metadata table is never fully withheld — the field picker alone
              # says which of the remaining fields come along.
              control_group("Metadata", uiOutput(ns("meta_picker_ui"))),

              # The user's own fields, offered per variable
              control_group(
                "Custom variables",
                id = ns("custom_group"),
                class = "d-none", # Start hidden so flexbox gap ignores it completely
                uiOutput(ns("custom_picker_ui"))
              ),

              # -- 2. How ----------------------------------------------------
              # Only a profile table has an allele encoding or a file format to
              # choose; a `.db` has exactly one representation.
              shinyjs::hidden(control_group(
                "Sequence data",
                id = ns("content_group"),
                pickerInput(
                  ns("sequences"),
                  label = NULL,
                  choices = SEQUENCE_MODES
                )
              )),
              shinyjs::hidden(control_group(
                "Table format",
                id = ns("format_group"),
                pickerInput(
                  ns("file_format"),
                  label = NULL,
                  choices = FILE_FORMATS
                )
              )),

              # Optional analysis-result tables, carried only for a `.db` export
              # and only when the loaded database actually holds them (the boxes
              # are disabled otherwise, driven server-side).
              shinyjs::hidden(control_group(
                "Analysis results",
                id = ns("results_group"),
                checkboxInput(
                  ns("include_classical"),
                  "Classical MLST results",
                  value = TRUE
                ),
                checkboxInput(
                  ns("include_amr"),
                  "AMR results",
                  value = TRUE
                )
              )),

              # -- 3. Where --------------------------------------------------
              # The closing step: pick a file, see the file you will get, write
              # it.
              #
              # One save button per possible output extension, each mounted once
              # so shinyFiles binds its dialog exactly once. Exactly one is ever
              # shown: the one matching the file this selection will produce.
              control_group(
                "Destination",
                lapply(names(DEST_BUTTONS), function(btn) {
                  spec <- DEST_BUTTONS[[btn]]
                  shinyjs::hidden(div(
                    id = ns(paste0("slot_", btn)),
                    class = "dest-slot",
                    shinySaveButton(
                      ns(btn),
                      spec$label,
                      title = spec$title,
                      filetype = spec$filetype,
                      icon = icon("folder-open"),
                      buttonType = "default"
                    )
                  ))
                }),
                uiOutput(ns("dest_label"), inline = TRUE)
              ),
              control_group(
                "Export",
                disabled(actionButton(
                  ns("export_btn"),
                  "Export",
                  class = "btn-success",
                  icon = icon("share-from-square")
                ))
              )
            )
          )
        ),
        as_fill_carrier(
          div(
            class = "db-page_body db-transfer-body",
            useWaiter(),
            uiOutput(ns("summary"), fill = TRUE)
          )
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
  custom_updated = shiny::reactiveVal(0L),
  # Advances every time this panel's markup is (re)inserted into the page —
  # i.e. on every database load, since the Database panel is rebuilt from
  # scratch each time while this server keeps running. Every observer below
  # that puts state into the DOM depends on it, because that state does not
  # survive the rebuild and nothing else would make them fire again. See
  # main.R's ui_mounted.
  ui_mounted = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define spinner
    db_waiter <- Waiter$new(
      id = ns("module-container"),
      html = div(
        class = "spinner-custom",
        spin_flower(),
        h5("Exporting ...")
      )
    )

    dest_path <- reactiveVal(NULL)

    typing <- reactive(identical(input$export_type, "typing"))

    # Each button gets its own registration and its own observer; whichever one
    # the user actually used feeds the single destination.
    lapply(names(DEST_BUTTONS), function(ext) {
      shinyFileSave(
        input,
        ext,
        roots = .roots(),
        filetypes = unlist(DEST_BUTTONS[[ext]]$filetype, use.names = FALSE),
        session = session
      )
      observeEvent(input[[ext]], {
        path <- parseSavePath(.roots(), input[[ext]])$datapath
        if (length(path) && nzchar(path)) {
          dest_path(.clean_path(path))
        }
      })
    })

    # Reveal the steps that apply to the chosen export type. shinyjs rather than
    # conditionalPanel: the Database panels are injected into a navset_hidden
    # after the database loads, and Shiny's conditional-panel binding does not
    # take there — the panels simply stay visible. The rest of this app (see
    # visualization.R) toggles the same way.
    observeEvent(
      list(input$export_type, ui_mounted()),
      {
        shinyjs::toggle("content_group", condition = typing())
        shinyjs::toggle("format_group", condition = typing())
        # Result-table toggles apply only to a `.db` export.
        shinyjs::toggle("results_group", condition = !typing())
      },
      ignoreInit = FALSE
    )

    # Enable each result-table toggle only when the loaded database holds that
    # table; uncheck a box the moment its table is unavailable so the summary
    # never implies data that will not travel. Re-run on remount as well: the
    # rebuilt markup brings both boxes back enabled and checked, whatever this
    # observer had made of them.
    observeEvent(
      list(db_path(), ui_mounted()),
      {
        present <- available_result_tables(db_path())
        shinyjs::toggleState("include_classical", condition = present$classical)
        shinyjs::toggleState("include_amr", condition = present$amr)
        if (!present$classical) {
          updateCheckboxInput(session, "include_classical", value = FALSE)
        }
        if (!present$amr) {
          updateCheckboxInput(session, "include_amr", value = FALSE)
        }
      },
      ignoreNULL = FALSE
    )

    # The file the current selection will land in, named in the toolbar itself so
    # the destination step reads as complete without a trip to the summary.
    output$dest_label <- renderUI({
      if (is.null(dest_path())) {
        return(span(class = "dest-label is-empty", "No file chosen"))
      }
      path <- resolved_target()$path
      tooltip(
        span(class = "dest-label", icon("file-arrow-down"), basename(path)),
        path
      )
    })

    # The file this selection will actually produce. Everything the user has
    # decided — export type, table format, metadata, sequences — collapses into
    # one extension, and only that destination button is offered.
    target_ext <- reactive({
      if (!typing()) {
        return("db")
      }
      typing_export_target(
        "x",
        input$file_format %||% "xlsx",
        !is.null(typing_metadata()),
        input$sequences %||% "none"
      )$ext
    })

    observeEvent(
      list(target_ext(), ui_mounted()),
      {
        for (ext in names(DEST_BUTTONS)) {
          shinyjs::toggle(
            paste0("slot_", ext),
            condition = identical(ext, target_ext())
          )
        }
      },
      ignoreInit = FALSE
    )

    # The metadata the profile export ships, built from the panel's existing
    # column picker so both export types honour the same confidentiality choice.
    typing_metadata <- reactive({
      selected_meta <- input$meta_cols %||% character(0)
      selected_custom <- input$custom_fields %||% character(0)
      # Nothing optional selected: ship the profile table on its own rather than
      # forcing an isolate/organism-only metadata sheet nobody asked for.
      if (!length(selected_meta) && !length(selected_custom)) {
        return(NULL)
      }

      path <- db_path()
      req(!is.null(path), !is.na(path))

      md <- make_metadata_table(path)
      if (is.null(md) || !nrow(md)) {
        return(NULL)
      }

      keep <- union(
        intersect(METADATA_FIXED_COLS, names(md)),
        selected_meta
      )
      md <- md[
        md$isolate %in% resolved_isolates(),
        names(md)[names(md) %in% keep],
        drop = FALSE
      ]
      if (!nrow(md) || !ncol(md)) {
        return(NULL)
      }

      # A tabular export has no place to put the custom tables, so the selected
      # variables ride along as ordinary columns of the metadata sheet.
      if (length(selected_custom)) {
        md <- append_custom(md, path, fields = selected_custom)
      }
      md
    })

    # Where the file will actually land: a bare table when that is all there is,
    # a zip when the selection needs several artefacts.
    resolved_target <- reactive({
      dest <- dest_path()
      req(!is.null(dest))
      if (!typing()) {
        # The save dialog does not force an extension, so settle it here rather
        # than writing a database to whatever the user happened to type.
        return(list(path = replace_ext(dest, "db"), bundled = FALSE))
      }
      typing_export_target(
        dest,
        input$file_format %||% "xlsx",
        !is.null(typing_metadata()),
        input$sequences %||% "none"
      )
    })

    isolates <- reactive({
      path <- db_path()
      if (is.null(path) || is.na(path)) character(0) else existing_strains(path)
    })

    # `isolate` and `organism` always travel with the table; everything else is
    # the user's to withhold. `source` is not offered at all: it describes how
    # an isolate entered *this* database (which peer it was merged from), which
    # is this lab's own bookkeeping - the receiving side stamps its own label on
    # import and would ignore ours anyway.
    optional_meta <- reactive({
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(character(0))
      }
      setdiff(metadata_columns(path), c(METADATA_FIXED_COLS, SOURCE_COL))
    })

    # ------------------------------------------------- isolate selection ---
    # Same modal/table isolate picker as the Visualization module's plot setup
    # and the Analysis Dashboard's wizard (see app/view/visualization_plot.R,
    # sel_* — this mirrors it almost line for line). NULL = no restriction
    # (every export-eligible isolate); otherwise the isolate names confirmed
    # in the modal.
    selected_isolates <- reactiveVal(NULL)

    # A new database means the previous isolate names may not exist any more.
    observeEvent(isolates(), selected_isolates(NULL), ignoreInit = TRUE)

    # The concrete isolate list every downstream export step reads — resolves
    # the NULL "no restriction" sentinel to the full export-eligible set, so
    # callers never have to special-case NULL themselves the way input$isolates
    # never needed to (it always held a concrete vector).
    resolved_isolates <- reactive({
      sel <- selected_isolates()
      if (is.null(sel)) isolates() else sel
    })

    # The metadata table backing the modal, restricted to isolates() (the
    # export-eligible set the mlst table actually holds calls for) so the
    # modal can never let the user pick something export_database()/
    # export_typing_results() would reject.
    export_meta <- reactive({
      path <- db_path()
      req(path)
      iso <- isolates()
      req(length(iso) > 0)
      md <- make_metadata_table(path)
      req(md)
      md[md$isolate %in% iso, , drop = FALSE]
    })

    export_date_choices <- reactive({
      meta <- export_meta()
      if (is.null(meta)) {
        return(character(0))
      }
      cols <- date_fields(db_path(), names(meta))
      stats::setNames(cols, field_labels_for(cols))
    })

    export_dates <- reactive({
      meta <- export_meta()
      col <- input$export_date_field
      if (is.null(meta) || is.null(col) || !nzchar(col)) {
        return(NULL)
      }
      if (!col %in% names(meta)) {
        return(NULL)
      }
      as_date_safe(meta[[col]])
    })

    export_filtered <- reactive({
      meta <- export_meta()
      req(meta)
      d <- export_dates()
      rng <- input$export_date_range
      if (is.null(d) || is.null(rng) || length(rng) != 2 || anyNA(rng)) {
        return(meta)
      }
      keep <- !is.na(d) & d >= as.Date(rng[1]) & d <= as.Date(rng[2])
      meta[keep, , drop = FALSE]
    })

    export_checked <- reactiveVal(character(0))

    output$isolate_picker_ui <- renderUI({
      div(
        actionButton(
          ns("isolates_button"),
          "Choose isolates",
          icon = icon("list-check"),
          width = "100%"
        ),
        uiOutput(ns("isolates_selection_info"))
      )
    })

    output$isolates_selection_info <- renderUI({
      meta <- export_meta()
      if (is.null(meta)) {
        return(div(class = "text-muted small mt-2", "No database loaded"))
      }
      total <- nrow(meta)
      sel <- selected_isolates()
      if (is.null(sel)) {
        div(
          class = "small mt-2 text-muted",
          sprintf("All %d isolates selected", total)
        )
      } else {
        div(
          class = "small mt-2",
          sprintf("%d of %d isolates selected", length(sel), total)
        )
      }
    })

    observeEvent(input$isolates_button, {
      meta <- export_meta()
      req(meta)
      # Sticky across reopens, like the other isolate pickers: the checked set
      # carries the last confirmed selection, and the date controls come back
      # holding whatever they were last set to.
      export_checked(selected_isolates() %||% character(0))
      dates <- export_date_choices()
      field <- isolate(input$export_date_field) %||% ""
      rng <- isolate(input$export_date_range)

      showModal(div(
        class = "selection-modal",
        modalDialog(
          title = NULL,
          div(
            class = "selection-modal-toolbar",
            actionButton(
              ns("isolates_sel_all"),
              "Select all",
              icon = icon("check-double")
            ),
            actionButton(
              ns("isolates_sel_none"),
              "Select none",
              icon = icon("xmark")
            ),
            uiOutput(ns("isolates_sel_count"), class = "selection-modal-count")
          ),
          div(
            class = "isolate-selection-table",
            DTOutput(ns("isolates_table"), fill = FALSE)
          ),
          footer = tagList(
            if (length(dates)) {
              div(
                class = "selection-modal-filter",
                pickerInput(
                  ns("export_date_field"),
                  label = NULL,
                  choices = c("No time filter" = "", dates),
                  selected = if (field %in% dates) field else "",
                  width = "fit"
                ),
                div(
                  id = ns("export_date_range_wrap"),
                  dateRangeInput(
                    ns("export_date_range"),
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
            actionButton(
              ns("isolates_confirm"),
              "Confirm selection",
              class = "btn-primary"
            )
          ),
          easyClose = TRUE
        )
      ))
      shinyjs::toggleState(
        "export_date_range",
        condition = nzchar(field) && field %in% dates
      )
    })

    observeEvent(input$export_date_field, {
      d <- export_dates()
      shinyjs::toggleState("export_date_range", condition = !is.null(d))
      if (is.null(d) || all(is.na(d))) {
        return()
      }
      updateDateRangeInput(
        session,
        "export_date_range",
        start = min(d, na.rm = TRUE),
        end = max(d, na.rm = TRUE),
        min = min(d, na.rm = TRUE),
        max = max(d, na.rm = TRUE)
      )
    })

    observeEvent(
      input$isolates_table_rows_selected,
      {
        rows <- input$isolates_table_rows_selected
        tbl <- export_filtered()
        export_checked(if (length(rows)) tbl$isolate[rows] else character(0))
      },
      ignoreNULL = FALSE
    )

    output$isolates_table <- renderDT(
      {
        tbl <- export_filtered()
        req(tbl)
        keep <- match(isolate(export_checked()), tbl$isolate)

        date_col <- input$export_date_field
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
          selection = list(mode = "multiple", selected = keep[!is.na(keep)]),
          extensions = "FixedColumns",
          options = list(
            dom = "tip",
            pageLength = 20,
            scrollX = TRUE,
            scrollY = "1px",
            scrollCollapse = TRUE,
            fixedColumns = list(leftColumns = length(pin_cols)),
            # See the identical comment in visualization_plot.R, where this
            # FixedColumns/filter-row fix first landed.
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

    output$isolates_sel_count <- renderUI({
      meta <- export_meta()
      req(meta)
      shown <- nrow(export_filtered())
      n <- length(input$isolates_table_rows_selected)
      if (shown == nrow(meta)) {
        return(span(sprintf("%d of %d selected", n, shown)))
      }
      d <- export_dates()
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

    isolates_proxy <- dataTableProxy("isolates_table")
    observeEvent(
      input$isolates_sel_all,
      selectRows(isolates_proxy, input$isolates_table_rows_all)
    )
    observeEvent(input$isolates_sel_none, selectRows(isolates_proxy, NULL))

    observeEvent(input$isolates_confirm, {
      meta <- export_meta()
      tbl <- export_filtered()
      req(meta, tbl)
      rows <- input$isolates_table_rows_selected
      picked <- if (length(rows)) tbl$isolate[rows] else tbl$isolate
      if (!length(picked)) {
        showNotification(
          "No isolates left to select — widen the time filter.",
          type = "warning"
        )
        return()
      }
      selected_isolates(if (setequal(picked, meta$isolate)) NULL else picked)
      removeModal()
    })

    # The custom variables this database defines. Empty for a database that has
    # never had one, which is what hides the whole control group. Re-read
    # whenever the Custom Variables panel adds, renames or removes a field —
    # db_path() alone does not change on that edit, and without this dependency
    # the picker would keep showing whatever was true when the Export panel was
    # first opened (see database.R's custom_updated).
    custom_choices <- reactive({
      custom_updated()
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(character(0))
      }
      exportable_custom_fields(path)
    })

    output$custom_picker_ui <- renderUI({
      cols <- custom_choices()
      req(length(cols) > 0)

      pickerInput(
        ns("custom_fields"),
        label = NULL,
        choices = stats::setNames(cols, field_labels_for(cols)),
        selected = cols,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select variables …",
          selectedTextFormat = "count > 3",
          countSelectedText = paste0("{0} / ", length(cols), " variables"),
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search variables ...",
          container = "body"
        )
      )
    })

    observeEvent(
      list(input$export_type, custom_choices(), ui_mounted()),
      {
        req(input$export_type)

        # Adds 'd-none' (hides + removes flex gap) when no custom choices exist
        # Removes 'd-none' (shows group) when custom choices exist
        shinyjs::toggleClass(
          id = "custom_group",
          class = "d-none",
          condition = length(custom_choices()) == 0
        )
      },
      ignoreInit = FALSE
    )

    output$meta_picker_ui <- renderUI({
      cols <- optional_meta()
      pickerInput(
        ns("meta_cols"),
        label = NULL,
        choices = stats::setNames(cols, field_labels_for(cols)),
        selected = cols,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select fields …",
          selectedTextFormat = "count > 3",
          countSelectedText = paste0("{0} / ", length(cols), " fields"),
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search fields ...",
          container = "body"
        )
      )
    })

    preview <- reactive({
      path <- db_path()
      req(!is.null(path), !is.na(path))
      sel <- resolved_isolates()
      if (!length(sel)) {
        return(NULL)
      }

      # The headline figures always describe the *isolates* you are exporting, so
      # they do not jump when the export type changes. A `.db` additionally
      # carries the scheme reference, noted separately below rather than
      # silently inflating these counts.
      p <- typing_preview(path, sel)

      if (typing()) {
        md <- typing_metadata()
        p$columns <- if (is.null(md)) character(0) else names(md)
        return(p)
      }

      full <- export_preview(
        path,
        sel,
        input$meta_cols %||% character(0),
        custom_fields = input$custom_fields %||% character(0)
      )
      p$columns <- full$columns
      p$custom_fields <- full$custom_fields
      p
    })

    output$summary <- renderUI({
      p <- preview()
      if (is.null(p)) {
        return(div(
          class = "db-empty-hint",
          "Select at least one isolate to export."
        ))
      }

      cols <- p$columns
      transfer_cards(
        # The mirror of the Import panel: the figures first describe the
        # selection, the card under them what it will be written as. A plain
        # fill carrier, not a `layout_column_wrap()` grid — a grid gives each
        # row an equal `1fr` share of the fillable height regardless of
        # content, which padded the shorter card with dead space.
        as_fill_carrier(div(
          class = "transfer-row",
          panel_card(
            "Export summary",
            div(
              class = "export-stats",
              stat_tile("Isolates", p$n_isolates, "vial"),
              stat_tile("Loci", p$n_loci, "table-columns"),
              stat_tile("Alleles", p$n_alleles, "dna"),
              stat_tile(
                "Allele calls",
                format(p$n_calls, big.mark = ","),
                "hashtag"
              ),
              if (typing()) {
                stat_tile(
                  "Missing calls",
                  format(p$n_missing, big.mark = ","),
                  "circle-question"
                )
              }
            ),
            if (typing()) {
              typing_note(input$sequences %||% "none")
            } else {
              tags$p(
                class = "export-note mb-0",
                tagList(
                  icon("circle-info"),
                  " The scheme reference, loci and scheme info are always",
                  " included, so the exported file can be typed against and",
                  " imported by a peer."
                )
              )
            }
          ),
          panel_card(
            "Output",
            fill = TRUE,
            div(
              class = "export-note",
              # What the selection will actually produce — stated *before* a
              # destination is picked, so the file type is never a surprise.
              tags$p(tagList(
                strong("File: "),
                tags$code(paste0(".", target_ext())),
                span(class = "dest-label is-empty", " — "),
                span(deliverable_text())
              )),
              if (length(cols)) {
                tags$p(tagList(
                  strong("Metadata fields: "),
                  div(
                    class = "species-details_lineage",
                    field_chips(cols)
                  )
                ))
              } else {
                tags$p(tagList(
                  strong("No metadata table"),
                  " will be written — only allele data and the scheme."
                ))
              },
              # Named separately from the metadata fields: they are the user's
              # own definitions, and for a `.db` they travel as their own
              # tables rather than as metadata columns.
              if (length(p$custom_fields)) {
                tags$p(tagList(
                  strong("Custom variables: "),
                  div(
                    class = "species-details_lineage",
                    field_chips(custom_col(p$custom_fields))
                  )
                ))
              },
              if (!is.null(dest_path())) {
                tags$p(
                  class = "mb-0",
                  tagList(
                    strong("Destination: "),
                    tags$code(resolved_target()$path)
                  )
                )
              } else {
                tags$p(
                  class = "text-muted mb-0",
                  tagList(
                    strong("Destination: "),
                    span(
                      class = "dest-label is-empty",
                      "Choose a destination file to enable Export."
                    )
                  )
                )
              }
            )
          )
        ))
      )
    })

    # Plain-language description of the artefact the current selection produces.
    deliverable_text <- reactive({
      if (!typing()) {
        return(
          "a complete PhyloTrace database a peer can import or type against"
        )
      }

      fmt <- input$file_format %||% "xlsx"
      parts <- c(
        if (identical(fmt, "xlsx")) {
          "a profile sheet"
        } else {
          paste0("profile.", fmt)
        },
        if (!is.null(typing_metadata())) {
          if (identical(fmt, "xlsx")) {
            "a metadata sheet"
          } else {
            paste0("metadata.", fmt)
          }
        },
        switch(
          input$sequences %||% "none",
          fasta = "alleles.fasta",
          per_locus = "one FASTA per locus",
          NULL
        )
      )

      paste0(
        if (identical(target_ext(), "zip")) "a bundle containing " else "",
        paste(parts, collapse = " + ")
      )
    })

    observe({
      # ui_mounted(): the rebuilt button always comes back disabled, so state it
      # again rather than leave a ready selection looking blocked.
      ui_mounted()
      ready <- !is.null(dest_path()) && length(resolved_isolates()) > 0
      if (ready) enable("export_btn") else disable("export_btn")
    })

    observeEvent(input$export_btn, {
      db_waiter$show()
      on.exit(db_waiter$hide())

      path <- db_path()
      dest <- dest_path()
      req(!is.null(path), !is.null(dest), length(resolved_isolates()))

      as_typing <- typing()
      md <- if (as_typing) typing_metadata() else NULL
      dest <- resolved_target()$path

      title <- if (as_typing) {
        "Exporting typing results"
      } else {
        "Exporting database"
      }

      # Feed progress into the db_waiter spinner instead of a Shiny progress bar.
      step <- function(frac, msg) {
        db_waiter$update(
          html = div(
            class = "spinner-custom",
            spin_flower(),
            h5(title),
            tags$p(sprintf("%d%% — %s", round(frac * 100), msg))
          )
        )
      }

      result <- tryCatch(
        if (as_typing) {
          export_typing_results(
            db_path = path,
            dest_path = dest,
            isolates = resolved_isolates(),
            metadata = md,
            format = input$file_format %||% "xlsx",
            value_kind = "hash",
            preset = "phylotrace",
            sequences = input$sequences %||% "none",
            progress = step
          )
        } else {
          export_database(
            src_path = path,
            dest_path = dest,
            isolates = resolved_isolates(),
            metadata_cols = input$meta_cols %||% character(0),
            include_classical = isTRUE(input$include_classical),
            include_amr = isTRUE(input$include_amr),
            custom_fields = input$custom_fields %||% character(0),
            progress = step
          )
        },
        error = function(e) e
      )

      if (inherits(result, "error")) {
        showNotification(
          tagList(strong("Export failed. "), conditionMessage(result)),
          type = "error",
          duration = NULL
        )
        return()
      }

      showNotification(
        tagList(
          icon("share-from-square"),
          " ",
          strong(
            if (as_typing) "Typing results exported." else "Database exported."
          ),
          tags$br(),
          sprintf(
            "%d isolate(s), %s to %s",
            result$n_isolates,
            .fmt_bytes(result$bytes),
            basename(result$path)
          )
        ),
        type = "message",
        duration = 8
      )
    })

    # Reset module state when the user returns to the landing screen.
    observeEvent(
      session_reset(),
      {
        dest_path(NULL)
      },
      ignoreInit = TRUE
    )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
