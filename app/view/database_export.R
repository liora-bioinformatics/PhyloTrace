# app/view/database_export.R
#
# "Export" interface of the Database menu.
#
# Writes a new `.db` holding a chosen set of isolates and a chosen set of
# metadata columns. Withheld columns are never written to the destination — see
# app/logic/db_export.R for why the file is built fresh rather than pruned.

box::use(
  bslib[as_fill_carrier, layout_sidebar, sidebar, tooltip],
  DT[datatable, dataTableProxy, DTOutput, JS, renderDT, selectRows],
  fs[path_home],
  shiny,
  shinyFiles[parseSavePath, shinyFileSave, shinySaveButton],
  shinyjs[disable, disabled, enable, hidden, toggle, toggleClass, toggleState],
  shinyWidgets[pickerInput, pickerOptions],
  stats[setNames],
  waiter[spin_flower, useWaiter, Waiter]
)

box::use(
  app / logic / custom_fields[append_custom, custom_col],
  app / logic / database_functions[metadata_columns],
  app /
    logic /
    db_export[
      available_result_tables,
      export_database,
      export_preview,
      exportable_custom_fields,
      METADATA_FIXED_COLS
    ],
  app / logic / db_events,
  app / logic / db_sources[SOURCE_COL],
  app / logic / db_store,
  app / logic / field_labels[field_chips, field_labels_for],
  app / logic / field_types[as_date_safe, date_fields],
  app / logic / functions[panel_card, stat_tile, transfer_cards],
  app /
    logic /
    profile_io[
      export_typing_results,
      replace_ext,
      typing_export_target,
      typing_preview
    ],
  app / logic / pymlst[existing_strains]
)

`%||%` <- function(a, b) if (is.null(a)) b else a

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
# shown at a time matching the file the current selection will produce.
# Separated static buttons are required because shinyFiles binds a file dialog to the
# element itself, so re-rendering standard UI buttons leaves orphaned dialogs in DOM.
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
  zip = list(
    label = "Choose .zip file",
    title = "Export typing results as a bundle …",
    filetype = list("Bundle" = "zip")
  )
)

# Returns the available root directory choices for shinyFiles dialogs.
.roots <- function() c(Home = path_home(), Root = "/")

# Collapses leading duplicate slashes produced by shinyFiles relative path parsing.
.clean_path <- function(p) sub("^/{2,}", "/", as.character(p))

# Renders explanatory tags regarding sha256 allele hash encoding and DNA sequences.
typing_note <- function(sequences) {
  shiny$tagList(
    shiny$tags$p(shiny$tagList(
      shiny$icon("circle-info"),
      " Alleles are written as ",
      shiny$strong("sha256 hashes"),
      " of their DNA sequence — the same allele hashes the same in any",
      " database, so a peer can compare these isolates against their own."
    )),
    if (!identical(sequences, "none")) {
      shiny$tags$p(shiny$tagList(
        shiny$icon("dna"),
        " The allele sequences are included, so the recipient can link these",
        " calls to their own alleles whatever identifiers they use."
      ))
    }
  )
}

# Formats byte counts into readable size units (kB, MB, GB).
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

# Groups sidebar UI controls under a labelled container element.
control_group <- function(label, ..., id = NULL, class = NULL) {
  shiny$div(
    id = id,
    class = paste("io-control-group", class),
    shiny$div(class = "control-group-label", label),
    ...
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  as_fill_carrier(
    shiny$div(
      id = ns("module-container"),
      layout_sidebar(
        padding = 0,
        border = FALSE,
        sidebar = sidebar(
          id = ns("controls_sidebar"),
          position = "right",
          width = 350,
          open = TRUE,
          fillable = TRUE,
          as_fill_carrier(
            shiny$div(
              class = "io-control",
              control_group(
                "Export type",
                pickerInput(
                  ns("export_type"),
                  label = NULL,
                  choices = EXPORT_TYPES
                )
              ),
              control_group(
                "Isolates",
                shiny$uiOutput(ns("isolate_picker_ui"))
              ),
              control_group("Metadata", shiny$uiOutput(ns("meta_picker_ui"))),
              control_group(
                "Custom variables",
                id = ns("custom_group"),
                class = "d-none",
                shiny$uiOutput(ns("custom_picker_ui"))
              ),
              hidden(control_group(
                "Sequence data",
                id = ns("content_group"),
                pickerInput(
                  ns("sequences"),
                  label = NULL,
                  choices = SEQUENCE_MODES
                )
              )),
              hidden(control_group(
                "Table format",
                id = ns("format_group"),
                pickerInput(
                  ns("file_format"),
                  label = NULL,
                  choices = FILE_FORMATS
                )
              )),
              hidden(control_group(
                "Analysis results",
                id = ns("results_group"),
                shiny$checkboxInput(
                  ns("include_classical"),
                  "Classical MLST results",
                  value = TRUE
                ),
                shiny$checkboxInput(
                  ns("include_amr"),
                  "AMR results",
                  value = TRUE
                )
              )),
              control_group(
                "Destination",
                lapply(names(DEST_BUTTONS), function(btn) {
                  spec <- DEST_BUTTONS[[btn]]
                  hidden(shiny$div(
                    id = ns(paste0("slot_", btn)),
                    class = "dest-slot",
                    shinySaveButton(
                      ns(btn),
                      spec$label,
                      title = spec$title,
                      filetype = spec$filetype,
                      icon = shiny$icon("folder-open"),
                      buttonType = "default"
                    )
                  ))
                }),
                shiny$uiOutput(ns("dest_label"), inline = TRUE)
              ),
              control_group(
                "Export",
                disabled(shiny$actionButton(
                  ns("export_btn"),
                  "Export",
                  class = "btn-success",
                  icon = shiny$icon("share-from-square")
                ))
              )
            )
          )
        ),
        as_fill_carrier(
          shiny$div(
            class = "db-page_body db-transfer-body",
            useWaiter(),
            shiny$uiOutput(ns("summary"), fill = TRUE)
          )
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L),
  db_rev = db_events$new_bus(),
  ui_mounted = shiny$reactive(0L),
  # Wired to whatever db_path/db_rev this call actually received - see
  # database.R's server() for why the default can't just be
  # `db_store$new_store()`.
  store = db_store$new_store(db_path = db_path, db_rev = db_rev)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db_waiter <- Waiter$new(
      id = ns("module-container"),
      html = shiny$div(
        class = "spinner-custom",
        spin_flower(),
        shiny$h5("Exporting ...")
      )
    )

    dest_path <- shiny$reactiveVal(NULL)

    typing <- shiny$reactive(identical(input$export_type, "typing"))

    lapply(names(DEST_BUTTONS), function(ext) {
      shinyFileSave(
        input,
        ext,
        roots = .roots(),
        filetypes = unlist(DEST_BUTTONS[[ext]]$filetype, use.names = FALSE),
        session = session
      )
      shiny$observeEvent(input[[ext]], {
        path <- parseSavePath(.roots(), input[[ext]])$datapath
        if (length(path) && nzchar(path)) {
          dest_path(.clean_path(path))
        }
      })
    })

    # Toggle options dependent on export type via shinyjs due to navset injection issues.
    shiny$observeEvent(
      list(input$export_type, ui_mounted()),
      {
        toggle("content_group", condition = typing())
        toggle("format_group", condition = typing())
        toggle("results_group", condition = !typing())
      },
      ignoreInit = FALSE
    )

    # Disable unavailable result table options dynamically when switching databases.
    shiny$observeEvent(
      list(db_path(), ui_mounted()),
      {
        present <- available_result_tables(db_path())
        toggleState("include_classical", condition = present$classical)
        toggleState("include_amr", condition = present$amr)
        if (!present$classical) {
          shiny$updateCheckboxInput(session, "include_classical", value = FALSE)
        }
        if (!present$amr) {
          shiny$updateCheckboxInput(session, "include_amr", value = FALSE)
        }
      },
      ignoreNULL = FALSE
    )

    output$dest_label <- shiny$renderUI({
      if (is.null(dest_path())) {
        return(shiny$span(class = "dest-label is-empty", "No file chosen"))
      }
      path <- resolved_target()$path
      tooltip(
        shiny$span(
          class = "dest-label",
          shiny$icon("file-arrow-down"),
          basename(path)
        ),
        path
      )
    })

    target_ext <- shiny$reactive({
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

    shiny$observeEvent(
      list(target_ext(), ui_mounted()),
      {
        for (ext in names(DEST_BUTTONS)) {
          toggle(
            paste0("slot_", ext),
            condition = identical(ext, target_ext())
          )
        }
      },
      ignoreInit = FALSE
    )

    typing_metadata <- shiny$reactive({
      selected_meta <- input$meta_cols %||% character(0)
      selected_custom <- input$custom_fields %||% character(0)

      if (!length(selected_meta) && !length(selected_custom)) {
        return(NULL)
      }

      path <- db_path()
      shiny$req(!is.null(path), !is.na(path))

      # `custom_fields` for append_custom() below; store$metadata() carries its
      # own dependency on the domains that move the metadata table itself.
      db_events$depend(db_rev, "custom_fields")
      md <- store$metadata()
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

      if (length(selected_custom)) {
        md <- append_custom(md, path, fields = selected_custom)
      }
      md
    })

    resolved_target <- shiny$reactive({
      dest <- dest_path()
      shiny$req(!is.null(dest))
      if (!typing()) {
        return(list(path = replace_ext(dest, "db"), bundled = FALSE))
      }
      typing_export_target(
        dest,
        input$file_format %||% "xlsx",
        !is.null(typing_metadata()),
        input$sequences %||% "none"
      )
    })

    isolates <- shiny$reactive({
      db_events$depend(db_rev, "isolates")
      path <- db_path()
      if (is.null(path) || is.na(path)) character(0) else existing_strains(path)
    })

    optional_meta <- shiny$reactive({
      db_events$depend(db_rev, "metadata")
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(character(0))
      }
      setdiff(metadata_columns(path), c(METADATA_FIXED_COLS, SOURCE_COL))
    })

    selected_isolates <- shiny$reactiveVal(NULL)

    # The isolate pool moved (typing, a removal, a merge). A narrowed export
    # selection is reconciled rather than reset: NULL here means "export
    # everything", so clearing it would silently widen the export instead of
    # narrowing it, and the user would not necessarily notice before the file
    # was written. Dropping to NULL is right only when nothing survives.
    shiny$observeEvent(
      isolates(),
      {
        live <- db_events$reconcile_names(
          shiny$isolate(selected_isolates()),
          isolates()
        )
        if (live$changed) {
          selected_isolates(if (length(live$kept)) live$kept else NULL)
          shiny$showNotification(
            sprintf(
              paste(
                "%d selected isolate(s) are no longer in the database and",
                "were removed from the export selection."
              ),
              length(live$dropped)
            ),
            type = "warning",
            duration = 8
          )
        }
      },
      ignoreInit = TRUE
    )

    resolved_isolates <- shiny$reactive({
      sel <- selected_isolates()
      if (is.null(sel)) isolates() else sel
    })

    export_meta <- shiny$reactive({
      path <- db_path()
      shiny$req(path)
      iso <- isolates()
      shiny$req(length(iso) > 0)
      md <- store$metadata()
      shiny$req(md)
      md[md$isolate %in% iso, , drop = FALSE]
    })

    export_date_choices <- shiny$reactive({
      meta <- export_meta()
      if (is.null(meta)) {
        return(character(0))
      }
      cols <- date_fields(db_path(), names(meta))
      setNames(cols, field_labels_for(cols))
    })

    export_dates <- shiny$reactive({
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

    export_filtered <- shiny$reactive({
      meta <- export_meta()
      shiny$req(meta)
      d <- export_dates()
      rng <- input$export_date_range
      if (is.null(d) || is.null(rng) || length(rng) != 2 || anyNA(rng)) {
        return(meta)
      }
      keep <- !is.na(d) & d >= as.Date(rng[1]) & d <= as.Date(rng[2])
      meta[keep, , drop = FALSE]
    })

    export_checked <- shiny$reactiveVal(character(0))

    output$isolate_picker_ui <- shiny$renderUI({
      shiny$div(
        shiny$actionButton(
          ns("isolates_button"),
          "Choose isolates",
          icon = shiny$icon("list-check"),
          width = "100%"
        ),
        shiny$uiOutput(ns("isolates_selection_info"))
      )
    })

    output$isolates_selection_info <- shiny$renderUI({
      meta <- export_meta()
      if (is.null(meta)) {
        return(shiny$div(class = "text-muted small mt-2", "No database loaded"))
      }
      total <- nrow(meta)
      sel <- selected_isolates()
      if (is.null(sel)) {
        shiny$div(
          class = "small mt-2 text-muted",
          sprintf("All %d isolates selected", total)
        )
      } else {
        shiny$div(
          class = "small mt-2",
          sprintf("%d of %d isolates selected", length(sel), total)
        )
      }
    })

    shiny$observeEvent(input$isolates_button, {
      meta <- export_meta()
      shiny$req(meta)

      export_checked(selected_isolates() %||% character(0))
      dates <- export_date_choices()
      field <- shiny$isolate(input$export_date_field) %||% ""
      rng <- shiny$isolate(input$export_date_range)

      shiny$showModal(shiny$div(
        class = "selection-modal",
        shiny$modalDialog(
          title = NULL,
          shiny$div(
            class = "selection-modal-toolbar",
            shiny$actionButton(
              ns("isolates_sel_all"),
              "Select all",
              icon = shiny$icon("check-double")
            ),
            shiny$actionButton(
              ns("isolates_sel_none"),
              "Select none",
              icon = shiny$icon("xmark")
            ),
            shiny$uiOutput(
              ns("isolates_sel_count"),
              class = "selection-modal-count"
            )
          ),
          shiny$div(
            class = "isolate-selection-table",
            DTOutput(ns("isolates_table"), fill = FALSE)
          ),
          footer = shiny$tagList(
            if (length(dates)) {
              shiny$div(
                class = "selection-modal-filter",
                pickerInput(
                  ns("export_date_field"),
                  label = NULL,
                  choices = c("No time filter" = "", dates),
                  selected = if (field %in% dates) field else "",
                  width = "fit"
                ),
                shiny$div(
                  id = ns("export_date_range_wrap"),
                  shiny$dateRangeInput(
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
            shiny$modalButton("Cancel"),
            shiny$actionButton(
              ns("isolates_confirm"),
              "Confirm selection",
              class = "btn-primary"
            )
          ),
          easyClose = TRUE
        )
      ))
      toggleState(
        "export_date_range",
        condition = nzchar(field) && field %in% dates
      )
    })

    shiny$observeEvent(input$export_date_field, {
      d <- export_dates()
      toggleState("export_date_range", condition = !is.null(d))
      if (is.null(d) || all(is.na(d))) {
        return()
      }
      shiny$updateDateRangeInput(
        session,
        "export_date_range",
        start = min(d, na.rm = TRUE),
        end = max(d, na.rm = TRUE),
        min = min(d, na.rm = TRUE),
        max = max(d, na.rm = TRUE)
      )
    })

    shiny$observeEvent(
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
        shiny$req(tbl)
        keep <- match(shiny$isolate(export_checked()), tbl$isolate)

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

    output$isolates_sel_count <- shiny$renderUI({
      meta <- export_meta()
      shiny$req(meta)
      shown <- nrow(export_filtered())
      n <- length(input$isolates_table_rows_selected)
      if (shown == nrow(meta)) {
        return(shiny$span(sprintf("%d of %d selected", n, shown)))
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
      shiny$span(
        sprintf("%d of %d selected", n, shown),
        shiny$span(class = "selection-modal-count-total", detail)
      )
    })

    isolates_proxy <- dataTableProxy("isolates_table")
    shiny$observeEvent(
      input$isolates_sel_all,
      selectRows(isolates_proxy, input$isolates_table_rows_all)
    )
    shiny$observeEvent(
      input$isolates_sel_none,
      selectRows(isolates_proxy, NULL)
    )

    shiny$observeEvent(input$isolates_confirm, {
      meta <- export_meta()
      tbl <- export_filtered()
      shiny$req(meta, tbl)
      rows <- input$isolates_table_rows_selected
      picked <- if (length(rows)) tbl$isolate[rows] else tbl$isolate
      if (!length(picked)) {
        shiny$showNotification(
          "No isolates left to select — widen the time filter.",
          type = "warning"
        )
        return()
      }
      selected_isolates(if (setequal(picked, meta$isolate)) NULL else picked)
      shiny$removeModal()
    })

    custom_choices <- shiny$reactive({
      db_events$depend(db_rev, "custom_fields")
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(character(0))
      }
      exportable_custom_fields(path)
    })

    output$custom_picker_ui <- shiny$renderUI({
      cols <- custom_choices()
      shiny$req(length(cols) > 0)

      pickerInput(
        ns("custom_fields"),
        label = NULL,
        choices = setNames(cols, field_labels_for(cols)),
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

    shiny$observeEvent(
      list(input$export_type, custom_choices(), ui_mounted()),
      {
        shiny$req(input$export_type)
        toggleClass(
          id = "custom_group",
          class = "d-none",
          condition = length(custom_choices()) == 0
        )
      },
      ignoreInit = FALSE
    )

    output$meta_picker_ui <- shiny$renderUI({
      cols <- optional_meta()
      pickerInput(
        ns("meta_cols"),
        label = NULL,
        choices = setNames(cols, field_labels_for(cols)),
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

    preview <- shiny$reactive({
      path <- db_path()
      shiny$req(!is.null(path), !is.na(path))
      sel <- resolved_isolates()
      if (!length(sel)) {
        return(NULL)
      }

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

    output$summary <- shiny$renderUI({
      p <- preview()
      if (is.null(p)) {
        return(shiny$div(
          class = "db-empty-hint",
          "Select at least one isolate to export."
        ))
      }

      cols <- p$columns
      transfer_cards(
        as_fill_carrier(shiny$div(
          class = "transfer-row",
          panel_card(
            "Export summary",
            shiny$div(
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
              shiny$tags$p(
                class = "export-note mb-0",
                shiny$tagList(
                  shiny$icon("circle-info"),
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
            shiny$div(
              class = "export-note",
              shiny$tags$p(shiny$tagList(
                shiny$strong("File: "),
                shiny$tags$code(paste0(".", target_ext())),
                shiny$span(class = "dest-label is-empty", " — "),
                shiny$span(deliverable_text())
              )),
              if (length(cols)) {
                shiny$tags$p(shiny$tagList(
                  shiny$strong("Metadata fields: "),
                  shiny$div(
                    class = "species-details_lineage",
                    field_chips(cols)
                  )
                ))
              } else {
                shiny$tags$p(shiny$tagList(
                  shiny$strong("No metadata table"),
                  " will be written — only allele data and the scheme."
                ))
              },
              if (length(p$custom_fields)) {
                shiny$tags$p(shiny$tagList(
                  shiny$strong("Custom variables: "),
                  shiny$div(
                    class = "species-details_lineage",
                    field_chips(custom_col(p$custom_fields))
                  )
                ))
              },
              if (!is.null(dest_path())) {
                shiny$tags$p(
                  class = "mb-0",
                  shiny$tagList(
                    shiny$strong("Destination: "),
                    shiny$tags$code(resolved_target()$path)
                  )
                )
              } else {
                shiny$tags$p(
                  class = "text-muted mb-0",
                  shiny$tagList(
                    shiny$strong("Destination: "),
                    shiny$span(
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

    deliverable_text <- shiny$reactive({
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

    shiny$observe({
      ui_mounted()
      ready <- !is.null(dest_path()) && length(resolved_isolates()) > 0
      if (ready) enable("export_btn") else disable("export_btn")
    })

    shiny$observeEvent(input$export_btn, {
      db_waiter$show()
      on.exit(db_waiter$hide())

      path <- db_path()
      dest <- dest_path()
      shiny$req(!is.null(path), !is.null(dest), length(resolved_isolates()))

      as_typing <- typing()
      md <- if (as_typing) typing_metadata() else NULL
      dest <- resolved_target()$path

      title <- if (as_typing) {
        "Exporting typing results"
      } else {
        "Exporting database"
      }

      step <- function(frac, msg) {
        db_waiter$update(
          html = shiny$div(
            class = "spinner-custom",
            spin_flower(),
            shiny$h5(title),
            shiny$tags$p(sprintf("%d%% — %s", round(frac * 100), msg))
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
        shiny$showNotification(
          shiny$tagList(
            shiny$strong("Export failed. "),
            conditionMessage(result)
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      shiny$showNotification(
        shiny$tagList(
          shiny$icon("share-from-square"),
          " ",
          shiny$strong(
            if (as_typing) "Typing results exported." else "Database exported."
          ),
          shiny$tags$br(),
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

    shiny$observeEvent(
      session_reset(),
      {
        dest_path(NULL)
      },
      ignoreInit = TRUE
    )
  })
}
