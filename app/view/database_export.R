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
    selectInput,
    uiOutput,
    renderUI,
    showNotification,
    withProgress,
    setProgress,
    tagList,
    h5,
  ],
  bslib[as_fill_carrier, layout_columns, tooltip],
  shinyjs[disabled, disable, enable],
  shinyWidgets[pickerInput, pickerOptions],
  shinyFiles[shinySaveButton, shinyFileSave, parseSavePath],
  fs[path_home],
  waiter[Waiter, spin_flower, useWaiter],
)

box::use(
  app / logic / db_export[export_preview, export_database, METADATA_FIXED_COLS],
  app / logic / database_functions[metadata_columns, make_metadata_table],
  app / logic / pymlst[existing_strains],
  app / logic / field_labels[field_chips, field_labels_for],
  app / logic / functions[panel_card, stat_tile],
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

# A labelled cluster of controls in the header row.
control_group <- function(label, ..., id = NULL, class = NULL) {
  div(
    id = id,
    class = paste("control-group", class),
    div(class = "control-group-label", label),
    div(class = "control-group-items", ...)
  )
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
        # The flow reads left to right: decide WHAT to export, then HOW it should
        # be written, then WHERE. Steps that do not apply to the chosen export
        # type are hidden (server-driven, via shinyjs — this panel is injected
        # into a navset_hidden after load, where conditionalPanel never binds).
        div(
          class = "db-browse-controls import-export-controls",

          # -- 1. What -------------------------------------------------------
          control_group(
            "Export type",
            selectInput(
              ns("export_type"),
              label = NULL,
              choices = EXPORT_TYPES
            )
          ),
          control_group("Isolates", uiOutput(ns("isolate_picker_ui"))),

          # `isolate` and `organism` travel with every export, so the metadata
          # table is never fully withheld — the field picker alone says which of
          # the remaining fields come along.
          control_group("Metadata", uiOutput(ns("meta_picker_ui"))),

          # -- 2. How --------------------------------------------------------
          # Only a profile table has an allele encoding or a file format to
          # choose; a `.db` has exactly one representation.
          shinyjs::hidden(control_group(
            "Sequence data",
            id = ns("content_group"),
            selectInput(
              ns("sequences"),
              label = NULL,
              choices = SEQUENCE_MODES
            )
          )),
          shinyjs::hidden(control_group(
            "Table format",
            id = ns("format_group"),
            selectInput(
              ns("file_format"),
              label = NULL,
              choices = FILE_FORMATS
            )
          )),

          # -- 3. Where ------------------------------------------------------
          # The closing step, set apart from the choices that feed it: pick a
          # file, see the file you will get, write it.
          #
          # One save button per possible output extension, each mounted once so
          # shinyFiles binds its dialog exactly once. Exactly one is ever shown:
          # the one matching the file this selection will actually produce.
          control_group(
            "Destination",
            class = "control-group",
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
            uiOutput(ns("dest_label"), inline = TRUE),
            disabled(actionButton(
              ns("export_btn"),
              "Export",
              class = "btn-success",
              icon = icon("share-from-square")
            ))
          )
        )
      ),
      as_fill_carrier(div(
        class = "db-page_body db-transfer-body",
        uiOutput(ns("summary"))
      ))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
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
      input$export_type,
      {
        shinyjs::toggle("content_group", condition = typing())
        shinyjs::toggle("format_group", condition = typing())
      },
      ignoreInit = FALSE
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
      target_ext(),
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
      # No optional field selected: ship the profile table on its own rather
      # than forcing an isolate/organism-only metadata sheet nobody asked for.
      if (!length(input$meta_cols %||% character(0))) {
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
        input$meta_cols
      )
      md <- md[
        md$isolate %in% input$isolates,
        names(md)[names(md) %in% keep],
        drop = FALSE
      ]
      if (!nrow(md) || !ncol(md)) NULL else md
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
    # the user's to withhold.
    optional_meta <- reactive({
      path <- db_path()
      if (is.null(path) || is.na(path)) {
        return(character(0))
      }
      setdiff(metadata_columns(path), METADATA_FIXED_COLS)
    })

    output$isolate_picker_ui <- renderUI({
      choices <- isolates()
      pickerInput(
        ns("isolates"),
        label = NULL,
        choices = choices,
        selected = choices,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select isolates …",
          selectedTextFormat = "count > 2",
          countSelectedText = paste0("{0} / ", length(choices), " isolates"),
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search isolates ..."
        )
      )
    })

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
          liveSearchPlaceholder = "Search fields ..."
        )
      )
    })

    preview <- reactive({
      path <- db_path()
      req(!is.null(path), !is.na(path))
      sel <- input$isolates
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
        input$meta_cols %||% character(0)
      )
      p$columns <- full$columns
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
      div(
        class = "transfer-cards",
        # The mirror of the Import panel: the figures on the left describe the
        # selection, the card on the right what it will be written as.
        layout_columns(
          col_widths = c(7, 5),
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
            div(
              class = "export-note",
              # What the selection will actually produce — stated *before* a
              # destination is picked, so the file type is never a surprise.
              tags$p(tagList(
                strong("File: "),
                tags$code(paste0(".", target_ext())),
                span(class = "text-muted", " — "),
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
                    "Choose a destination file to enable Export."
                  )
                )
              }
            )
          )
        )
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
      ready <- !is.null(dest_path()) && length(input$isolates) > 0
      if (ready) enable("export_btn") else disable("export_btn")
    })

    observeEvent(input$export_btn, {
      db_waiter$show()
      on.exit(db_waiter$hide())

      path <- db_path()
      dest <- dest_path()
      req(!is.null(path), !is.null(dest), length(input$isolates))

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
            isolates = input$isolates,
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
            isolates = input$isolates,
            metadata_cols = input$meta_cols %||% character(0),
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
