# app/view/database_loci_info.R

box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    reactiveVal,
    observeEvent,
    req,
    div,
    span,
    icon,
    actionButton,
    downloadButton,
    downloadHandler,
    renderUI,
    uiOutput,
    outputOptions,
    p,
    HTML
  ],
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_header,
    card_body
  ],
  DT[DTOutput, renderDT, datatable, JS],
  shinyWidgets[pickerInput, pickerOptions, updatePickerInput],
  shinyjs[runjs],
  jsonlite[toJSON],
  utils[write.csv]
)

box::use(
  app /
    logic /
    database_functions[
      load_loci_info,
      load_locus_alleles,
      load_allele_sequence,
      locus_fasta
    ],
  app / logic / functions[render_info]
)

# Columns of the loci table shown to the user (the internal `.gene` column that
# `load_loci_info` adds is intentionally excluded).
display_cols <- c("Locus", "Gene", "Start", "Length", "Product", "Allele Count")

# Wrap each nucleotide in a span so the viewer can color it via SCSS.
color_sequence <- function(sequence) {
  sequence <- gsub("A", "<span class='base-a'>A</span>", sequence)
  sequence <- gsub("T", "<span class='base-t'>T</span>", sequence)
  sequence <- gsub("G", "<span class='base-g'>G</span>", sequence)
  sequence <- gsub("C", "<span class='base-c'>C</span>", sequence)
  sequence
}

# Push text to the client-side clipboard. toJSON turns the value into a safely
# escaped JS string literal.
copy_to_clipboard <- function(text) {
  runjs(sprintf(
    "navigator.clipboard.writeText(%s);",
    toJSON(text, auto_unbox = TRUE)
  ))
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      class = "loci-info-layout",
      as_fill_carrier(
        div(
          class = "loci-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                class = "loci-info-header",
                "Loci Table",
                downloadButton(
                  ns("export_csv"),
                  "Export CSV",
                  icon = icon("file-csv"),
                  class = "btn-sm loci-export-btn"
                )
              ),
              card_body(
                class = "loci-table-body",
                # Loading overlay shown until the DataTable has drawn
                div(
                  class = "loci-table-loading",
                  div(
                    class = "loci-loading-inner",
                    div(class = "loci-spinner"),
                    p("Loading loci table ...", class = "loci-loading_text")
                  )
                ),
                DTOutput(ns("db_loci"), fill = TRUE)
              )
            )
          )
        )
      ),
      as_fillable_container(
        div(
          class = "loci-controls",
          pickerInput(
            ns("allele_select"),
            label = "Select locus from table",
            choices = character(0),
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10,
              liveSearchPlaceholder = "Search alleles ..."
            )
          ),
          as_fill_carrier(
            div(
              class = "loci-allele-panel",
              div(
                class = "loci-allele-header",
                div(
                  class = "loci-allele-actions",
                  actionButton(
                    ns("copy_seq"),
                    "Sequence",
                    icon = icon("copy"),
                    class = "btn-sm"
                  ),
                  actionButton(
                    ns("copy_idx"),
                    "Index",
                    icon = icon("hashtag"),
                    class = "btn-sm"
                  ),
                  downloadButton(
                    ns("download_locus"),
                    "Locus",
                    icon = icon("download"),
                    class = "btn-sm"
                  )
                )
              ),
              as_fill_item(
                div(class = "sequence", uiOutput(ns("allele_sequence")))
              )
            )
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
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sequence of the currently displayed allele, cached so the "Sequence" copy
    # button does not have to re-query the database.
    seq_cache <- reactiveVal(NULL)

    # Reset module state when the user returns to the landing screen.
    observeEvent(session_reset(), seq_cache(NULL), ignoreInit = TRUE)

    # Re-arm the table loading overlay whenever a new database is loaded, so the
    # spinner shows again while the (new) table redraws (initComplete clears it).
    observeEvent(db_path(), {
      runjs("$('.loci-table-body').removeClass('is-loaded');")
    })

    # Loci table (targets enriched with per-locus allele counts).
    loci_info <- reactive({
      req(db_path())
      load_loci_info(db_path())
    })

    output$db_loci <- renderDT({
      render_info("output$db_loci")

      li <- loci_info()

      # Clear the loading overlay once the table has drawn
      draw_signal <- JS(
        "function(settings) {",
        "  $(this.api().table().node())",
        "    .closest('.loci-table-body').addClass('is-loaded');",
        "}"
      )

      if (is.null(li)) {
        return(datatable(
          data.frame(
            " " = paste(
              "No 'targets' table found. Re-download the scheme to",
              "populate loci info."
            ),
            check.names = FALSE
          ),
          rownames = FALSE,
          colnames = "",
          selection = "none",
          options = list(
            dom = "t",
            ordering = FALSE,
            paging = FALSE,
            initComplete = draw_signal
          )
        ))
      }

      datatable(
        li[, display_cols, drop = FALSE],
        rownames = FALSE,
        filter = "top",
        selection = list(mode = "single", selected = 1),
        class = "stripe row-border order-column",
        options = list(
          dom = "ti",
          paging = FALSE,
          scrollX = TRUE,
          scrollY = "1px",
          scrollCollapse = TRUE,
          columnDefs = list(list(className = "dt-left", targets = "_all")),
          # Signal the server that the table has finished drawing so the loading
          # waiter can be hidden (plain output events fire before this render).
          initComplete = draw_signal
        )
      )
    })

    # The selected row's `mlst` gene name (used to key the detail queries) and
    # its display Locus (used for the FASTA download name).
    selected_row <- reactive({
      li <- loci_info()
      req(li)
      row <- input$db_loci_rows_selected
      req(length(row) == 1)
      li[row, , drop = FALSE]
    })

    # Distinct alleles of the selected locus with in-database usage.
    alleles <- reactive({
      req(db_path())
      load_locus_alleles(db_path(), selected_row()$.gene)
    })

    # Refresh the allele dropdown in place whenever the selected locus changes
    observeEvent(alleles(), {
      df <- alleles()
      req(nrow(df) > 0)

      total <- sum(df$count)
      labels <- ifelse(
        df$present,
        sprintf(
          "Allele %s - %d times in DB (%.1f%%)",
          df$seqid,
          df$count,
          100 * df$count / total
        ),
        sprintf("Allele %s - not present", df$seqid)
      )

      updatePickerInput(
        session,
        "allele_select",
        label = paste("Selected Locus:", selected_row()$.gene),
        choices = stats::setNames(as.character(df$seqid), labels),
        selected = as.character(df$seqid[1])
      )
    })

    # color-coded sequence in FASTA form with allele index header
    output$allele_sequence <- renderUI({
      req(db_path(), input$allele_select)

      sequence <- load_allele_sequence(
        db_path(),
        as.integer(input$allele_select)
      )
      req(sequence)
      seq_cache(sequence)

      render_info("output$allele_sequence")

      HTML(paste0(
        "<div class=\"fasta-header\">&gt;",
        input$allele_select,
        "</div>",
        color_sequence(sequence)
      ))
    })

    # Copy actions -----------------------------------------------------------
    observeEvent(input$copy_seq, {
      req(seq_cache())
      copy_to_clipboard(seq_cache())
    })

    observeEvent(input$copy_idx, {
      req(input$allele_select)
      copy_to_clipboard(input$allele_select)
    })

    # Downloads --------------------------------------------------------------
    output$download_locus <- downloadHandler(
      filename = function() paste0(selected_row()$Locus, ".fasta"),
      content = function(file) {
        writeLines(locus_fasta(db_path(), selected_row()$.gene), file)
      }
    )

    output$export_csv <- downloadHandler(
      filename = function() "loci_info.csv",
      content = function(file) {
        li <- loci_info()
        req(li)
        write.csv(li[, display_cols, drop = FALSE], file, row.names = FALSE)
      }
    )

    # NB: db_loci is deliberately NOT pre-rendered (suspendWhenHidden left at its
    # default TRUE). Initialising this scrollX/scrollY DataTable while the Loci
    # Info sub-tab is hidden makes DataTables throw during its width calculation;
    # that error breaks the client message loop mid-load, so the sibling navbar
    # panels never finish inserting and open blank. Letting it render on first
    # visit (tab visible) is correct — the loading overlay covers that first
    # draw via initComplete above.
    outputOptions(output, "allele_sequence", suspendWhenHidden = FALSE)
  })
}
