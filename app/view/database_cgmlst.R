# app/view/database_cgmlst.R

#' "cgMLST" interface of the Database menu: Scheme Info and Loci Info tabs.

box::use(
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_body,
    card_header,
    nav_panel,
    navset_card_tab
  ],
  DT[DTOutput, JS, datatable, renderDT],
  htmltools[HTML],
  jsonlite[toJSON],
  shiny[
    NS,
    a,
    actionButton,
    div,
    downloadButton,
    downloadHandler,
    em,
    icon,
    imageOutput,
    moduleServer,
    observeEvent,
    outputOptions,
    p,
    reactive,
    reactiveVal,
    renderImage,
    renderUI,
    req,
    span,
    tagList,
    uiOutput
  ],
  shinyjs[runjs],
  shinyWidgets[pickerInput, pickerOptions, updatePickerInput],
  stats[setNames],
  utils[write.csv]
)

box::use(
  app /
    logic /
    database_functions[
      load_allele_sequence,
      load_db_scheme_overview,
      load_db_species,
      load_loci_info,
      load_locus_alleles,
      locus_fasta
    ],
  app / logic / functions[render_info],
  app / logic / scheme_browser[get_species_details, get_species_img]
)

# Columns of the loci table shown to the user (the internal `.gene` column
# that `load_loci_info` adds is intentionally excluded).
display_cols <- c("Locus", "Gene", "Start", "Length", "Product", "Allele Count")

# Wrap each nucleotide in a span so the viewer can color it via SCSS.
color_sequence <- function(sequence) {
  sequence <- gsub("A", "<span class='base-a'>A</span>", sequence)
  sequence <- gsub("T", "<span class='base-t'>T</span>", sequence)
  sequence <- gsub("G", "<span class='base-g'>G</span>", sequence)
  sequence <- gsub("C", "<span class='base-c'>C</span>", sequence)
  sequence
}

# Push text to the client-side clipboard.
copy_to_clipboard <- function(text) {
  runjs(sprintf(
    "navigator.clipboard.writeText(%s);",
    toJSON(text, auto_unbox = TRUE)
  ))
}

# Renders the UI layout for the Scheme Info tab.
scheme_info_ui <- function(ns) {
  as_fill_carrier(
    div(
      class = "scheme-info-layout",
      as_fill_carrier(
        div(
          class = "scheme-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                "Scheme Metadata"
              ),
              card_body(DTOutput(ns("local_scheme_table"), fill = TRUE))
            )
          )
        )
      ),
      as_fill_carrier(
        div(
          class = "scheme-aside",
          div(
            class = "species-photo",
            imageOutput(ns("species_img"), height = "auto"),
            uiOutput(ns("species_caption"))
          ),
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header(
                "Details"
              ),
              card_body(
                div(
                  class = "species-card_article",
                  uiOutput(ns("species_details")),
                  uiOutput(ns("species_summary"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Renders the UI layout for the Loci Info tab.
loci_info_ui <- function(ns) {
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
              liveSearchPlaceholder = "Search alleles ...",
              container = "body"
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
ui <- function(id) {
  ns <- NS(id)

  navset_card_tab(
    nav_panel("Scheme Info", scheme_info_ui(ns)),
    nav_panel("Loci Info", loci_info_ui(ns))
  )
}

# Handles server logic for displaying scheme overview and species metadata.
scheme_info_server <- function(input, output, session, db_path) {
  scheme_overview <- reactive({
    req(db_path())
    load_db_scheme_overview(db_path())
  })

  scheme_species <- reactive({
    req(db_path())
    load_db_species(db_path())
  })

  output$local_scheme_table <- renderDT({
    overview <- scheme_overview()

    if (is.null(overview) || isFALSE(is.data.frame(overview))) {
      overview <- data.frame(
        " " = paste(
          "No 'Scheme Overview' table found. <br> Try rebuilding",
          "the schema in the <strong>Create Scheme</strong> module"
        ),
        check.names = FALSE
      )
    }

    render_info("output$local_scheme_table")

    datatable(
      overview,
      class = 'stripe row-border order-column',
      colnames = rep("", ncol(overview)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    )
  })

  output$species_img <- renderImage(
    {
      species <- scheme_species()
      req(species)

      render_info("output$species_img")

      list(src = get_species_img(species))
    },
    deleteFile = FALSE
  )

  species_record <- reactive({
    species <- scheme_species()
    req(species)

    get_species_details(species)
  })

  output$species_caption <- renderUI({
    render_info("output$species_caption")

    species <- scheme_species()
    req(species)

    details <- species_record()

    tagList(
      div(
        class = "species-photo_badges",
        if (!is.null(details) && !is.null(details$rank)) {
          span(details$rank, class = "species-details_rank")
        },
        if (!is.null(details) && !is.null(details$ncbi_taxid)) {
          a(
            href = paste0(
              "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
              details$ncbi_taxid
            ),
            target = "_blank",
            class = "species-details_taxid",
            paste0("NCBI:txid", details$ncbi_taxid)
          )
        }
      ),
      div(
        class = "species-photo_caption",
        span(em(species), class = "species-photo_name")
      )
    )
  })

  output$species_details <- renderUI({
    render_info("output$species_details")

    req(scheme_species())

    details <- species_record()

    if (is.null(details)) {
      return(div(
        class = "species-details_empty",
        "No metadata available for this species."
      ))
    }

    ranks <- c("phylum", "class", "order", "family", "genus")
    chips <- lapply(ranks, function(rank) {
      value <- details$lineage[[rank]]
      if (is.null(value)) {
        return(NULL)
      }
      div(
        class = "species-details_chip",
        span(toupper(rank), class = "species-details_chip-rank"),
        span(value, class = "species-details_chip-value")
      )
    })

    div(
      class = "species-details",
      div(class = "species-details_lineage", chips)
    )
  })

  output$species_summary <- renderUI({
    render_info("output$species_summary")

    details <- species_record()
    req(!is.null(details), !is.null(details$summary))

    p(details$summary, class = "species-details_summary")
  })
}

# Handles server logic for browsing loci, inspecting sequence alleles, and exporting data.
loci_info_server <- function(input, output, session, db_path, session_reset) {
  ns <- session$ns

  # Sequence of the currently displayed allele, cached so the "Sequence" copy
  # button does not have to re-query the database.
  seq_cache <- reactiveVal(NULL)

  # Reset module state when the user returns to the landing screen.
  observeEvent(session_reset(), seq_cache(NULL), ignoreInit = TRUE)

  observeEvent(db_path(), {
    runjs("$('.loci-table-body').removeClass('is-loaded');")
  })

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
      choices = setNames(as.character(df$seqid), labels),
      selected = as.character(df$seqid[1])
    )
  })

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

  outputOptions(output, "allele_sequence", suspendWhenHidden = FALSE)
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    scheme_info_server(input, output, session, db_path)
    loci_info_server(input, output, session, db_path, session_reset)
  })
}
