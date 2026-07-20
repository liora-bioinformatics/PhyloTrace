# app/view/scheme_browser.R

box::use(
  shiny[
    NS,
    reactive,
    moduleServer,
    renderUI,
    uiOutput,
    req,
    div,
    p,
    span,
    em,
    a,
    imageOutput,
    renderImage,
    actionButton,
    icon,
    reactiveValues,
    observeEvent,
    tagList,
    renderText,
    h5,
    observe,
    bindEvent,
    textInput,
    verbatimTextOutput,
    showNotification
  ],
  shinyjs[disabled, useShinyjs, enable, disable, addClass, removeClass],
  bslib[
    navset_card_tab,
    page_fillable,
    nav_panel,
    sidebar,
    layout_columns,
    card,
    card_header,
    card_body,
    card_title,
    tooltip,
    as_fill_item,
    as_fillable_container,
    as_fill_carrier
  ],
  shinyWidgets[pickerInput, pickerOptions],
  DT[DTOutput, renderDT, datatable],
  waiter[autoWaiter, spin_flower, Waiter, transparent],
  fs[path_home],
  shinyFiles[shinyDirButton, shinyDirChoose, parseDirPath]
)

box::use(
  app / logic / functions[render_info],
  app / logic / schemes[cgmlst_org_schemes],
  app / logic / pymlst[download_cgmlst_scheme, conda_env],
  app /
    logic /
    scheme_browser[
      download_scheme_overview,
      download_scheme_targets,
      get_scheme_overview,
      get_species_img,
      get_species_details,
      assemble_db_location
    ]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    useShinyjs(),
    autoWaiter(
      id = ns("scheme_table"),
      html = div(
        class = "scheme-waiter",
        spin_flower(),
        p("Fetching metadata", class = "scheme-waiter_text")
      ),
      color = "black"
    ),
    as_fill_carrier(
      div(
        id = ns("scheme-download-container"),
        navset_card_tab(
          full_screen = FALSE,
          title = NULL,
          nav_panel(
            "Scheme Download",
            layout_columns(
              as_fill_carrier(
                div(
                  div(
                    id = "scheme-selection",
                    uiOutput(ns("scheme_selection"))
                  ),
                  as_fill_item(
                    card(
                      fill = TRUE,
                      full_screen = TRUE,
                      card_header(
                        class = "bg-dark",
                        "Scheme Metadata"
                      ),
                      card_body(as_fill_item(uiOutput(ns("scheme_overview"))))
                    )
                  )
                )
              ),
              as_fill_carrier(
                div(
                  card(
                    fill = FALSE,
                    card_header(
                      class = "bg-dark help-header",
                      "Initiate New Database",
                      tooltip(
                        div(
                          class = "tooltip-bttn",
                          actionButton(
                            ns("initiate_db_tooltip_bttn"),
                            label = NULL,
                            icon = icon("circle-question")
                          )
                        ),
                        paste(
                          "Pick a target folder and enter a name for the new",
                          "database, then 'Download Scheme' to fetch the selected",
                          "cgMLST scheme into it. Once the download completes,",
                          "'Load Database' opens it for typing and analysis."
                        ),
                        placement = "bottom"
                      )
                    ),
                    card_body(
                      div(
                        id = "location-define-ui",
                        shinyDirButton(
                          ns("download_location"),
                          "Select Location",
                          icon = icon("folder-open"),
                          title = "Choose a location for a new database",
                          buttonType = "default",
                          root = path_home(),
                          multiple = FALSE
                        ),
                        uiOutput(ns("db_name_input")),
                      ),
                      div(
                        id = "location-selected-ui",
                        div(id = "target-location", "Target location:"),
                        verbatimTextOutput(ns("selected_dir"))
                      ),
                      div(
                        id = "download-buttons",
                        disabled(
                          actionButton(
                            ns("scheme_download"),
                            "Download Scheme",
                            icon = icon("download")
                          )
                        ),
                        disabled(
                          actionButton(
                            ns("load_db"),
                            "Load Database",
                            icon = icon("angles-right")
                          )
                        )
                      )
                    )
                  ),
                  as_fill_item(
                    card(
                      fill = TRUE,
                      full_screen = TRUE,
                      card_header(
                        class = "bg-dark",
                        "Details"
                      ),
                      card_body(
                        div(
                          class = "species-card_article",
                          div(
                            class = "species-card_media",
                            imageOutput(ns("species_img"), height = "auto")
                          ),
                          uiOutput(ns("species_details")),
                          uiOutput(ns("species_summary"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          nav_panel(
            "Custom Scheme",
            "Coming soon ..."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, session_reset = shiny::reactive(0L)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    Scheme_Browser <- reactiveValues(download_status = "", last_download = NULL)

    # Reset module state when the user returns to the landing screen.
    # Clears the last download path so the "Load Database" button is disabled.
    observeEvent(
      session_reset(),
      {
        Scheme_Browser$download_status <- ""
        Scheme_Browser$last_download <- NULL
      },
      ignoreInit = TRUE
    )

    # Render scheme selector
    output$scheme_selection <- renderUI({
      render_info("output$scheme_selection")

      pickerInput(
        ns("scheme_selector"),
        "Select Scheme",
        choices = sort(gsub("_", " ", cgmlst_org_schemes$species)),
        choicesOpt = list(
          subtext = rep("cgMLST", nrow(cgmlst_org_schemes))
        ),
        options = pickerOptions(
          liveSearch = TRUE,
          size = 10,
          showSubtext = TRUE,
          liveSearchPlaceholder = "Search loci ..."
        )
      )
    })

    # Fetch scheme metadata from cgmlst.org
    scheme_overview <- reactive({
      req(input$scheme_selector)

      get_scheme_overview(input$scheme_selector)
    })

    # Render scheme metadata
    output$scheme_overview <- renderUI({
      render_info("output$scheme_overview")

      overview <- scheme_overview()

      if (is.character(overview)) {
        div(overview)
      } else if (is.data.frame(overview)) {
        DTOutput(ns("scheme_table"))
      }
    })

    # Render scheme info table
    output$scheme_table <- renderDT({
      req(is.data.frame(scheme_overview()))

      render_info("output$scheme_table")

      datatable(
        scheme_overview(),
        class = 'stripe row-border order-column',
        colnames = c("", ""),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })

    # Render species img
    output$species_img <- renderImage(
      {
        req(input$scheme_selector)

        render_info("output$species_img")

        list(src = get_species_img(input$scheme_selector))
      },
      deleteFile = FALSE
    )

    # Enriched species metadata (taxonomy + description)
    species_record <- reactive({
      req(input$scheme_selector)

      get_species_details(input$scheme_selector)
    })

    # Render title + taxonomy
    output$species_details <- renderUI({
      render_info("output$species_details")

      details <- species_record()

      if (is.null(details)) {
        return(div(
          class = "species-details_empty",
          "No metadata available for this species."
        ))
      }

      # Taxonomy ladder: one chip per known rank (skips missing ranks)
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
        div(
          class = "species-details_header",
          span(em(input$scheme_selector), class = "species-details_name"),
          span(details$rank, class = "species-details_rank"),
          a(
            href = paste0(
              "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
              details$ncbi_taxid
            ),
            target = "_blank",
            class = "species-details_taxid",
            paste0("NCBI:txid", details$ncbi_taxid)
          )
        ),
        # Taxonomy ladder
        div(class = "species-details_lineage", chips)
      )
    })

    # Render description in its own full-width row below the title/image
    output$species_summary <- renderUI({
      render_info("output$species_summary")

      details <- species_record()
      req(!is.null(details), !is.null(details$summary))

      p(details$summary, class = "species-details_summary")
    })

    # Download location directory chooser
    shinyDirChoose(
      input,
      "download_location",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      session = session
    )

    output$db_name_input <- renderUI({
      render_info("output$db_name_input")

      textInput(
        ns("db_name"),
        "Define Database Name",
        value = gsub(" ", "_", gsub("/", "_", input$scheme_selector)),
        placeholder = "No database name defined ..."
      )
    })

    output$selected_dir <- renderText({
      render_info("output$selected_dir")

      download_path <- parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$download_location
      )

      if (
        !length(download_path) ||
          !is.character(download_path)
      ) {
        "No location selected ..."
      } else if (
        is.null(input$db_name) || !length(input$db_name) || input$db_name == ""
      ) {
        "No database name ..."
      } else {
        paste(
          file.path(download_path, paste0(input$db_name, ".db"))
        )
      }
    }) |>
      bindEvent(list(input$db_name, input$download_location))

    # Observe download button status
    observe({
      download_path <- parseDirPath(
        roots = c(Home = path_home(), Root = "/"),
        input$download_location
      )

      if (
        !is.null(input$db_name) &&
          length(input$db_name) &&
          input$db_name != "" &&
          length(download_path) &&
          is.character(download_path)
      ) {
        enable("scheme_download")
      } else {
        disable("scheme_download")
      }
    }) |>
      shiny::bindEvent(list(input$db_name, input$download_location))

    # Event scheme download
    observeEvent(input$scheme_download, {
      db_location <- assemble_db_location(
        input$download_location,
        input$db_name
      )
      req(db_location)

      # If database already exists exit
      if (file.exists(db_location)) {
        showNotification(
          paste(db_location, "already exists"),
          type = "error",
          duration = 5
        )
        return()
      }

      Scheme_Browser$download_status <- "Downloading ..."

      waiting_screen <- div(
        class = "spinner-custom",
        spin_flower(),
        div(
          h5("Downloading ..."),
          div(id = "scheme-load", input$scheme_selector)
        )
      )

      # Define spinner
      w <- Waiter$new(
        id = ns("scheme-download-container"),
        html = waiting_screen
      )
      w$show()
      on.exit(w$hide())

      # Run download
      status <- download_cgmlst_scheme(
        input$scheme_selector,
        db_location,
        env_name = conda_env
      )

      # Check download process status
      if (status$status == 1 | isFALSE(file.exists(db_location))) {
        # Case download has exit status 1
        download_status <- paste(
          "Download of",
          input$scheme_selector,
          "failed"
        )
      } else if (status$status == 0) {
        if (!is.null(scheme_overview())) {
          download_scheme_overview(scheme_overview(), db_location)
        }

        # Store the scheme's target/locus table from cgmlst.org as a `targets`
        # table. Self-contained and non-fatal: a fetch failure leaves the
        # database otherwise intact.
        download_scheme_targets(input$scheme_selector, db_location)

        # Case download has exit status 0
        download_status <- paste(
          "Download of",
          input$scheme_selector,
          "was successful."
        )

        # Remember the path of the last successful download so the
        # "Load Database" click hands over this database rather than the
        # current (possibly changed) input selection.
        Scheme_Browser$last_download <- db_location

        enable("load_db")
        addClass("load_db", "btn-attention")
      }

      # Return status
      showNotification(
        download_status,
        type = ifelse(status$status == 0, "message", "error"),
        duration = 5
      )
    })

    # Disable load_db button on each scheme change
    observeEvent(input$scheme_selector, {
      disable("load_db")
      removeClass("load_db", "btn-attention")
    })

    # Server return values
    reactiveValues(
      load_db = reactive(input$load_db),
      db_location = reactive(Scheme_Browser$last_download)
    )
  })
}
