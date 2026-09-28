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
    showNotification,
    invalidateLater,
    tags
  ],
  shinyjs[disabled, useShinyjs, enable, disable, addClass, removeClass, html],
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
  shinyWidgets[virtualSelectInput],
  DT[DTOutput, renderDT, datatable],
  waiter[autoWaiter, spin_flower, Waiter, transparent],
  fs[path_home],
  shinyFiles[shinyDirButton, shinyDirChoose, parseDirPath]
)

box::use(
  app / logic / db_guard[db_failed, guard_db],
  app / logic / functions[render_info],
  app / logic / schemes[cgmlst_org_schemes],
  app /
    logic /
    pymlst[
      conda_env,
      finish_scheme_download,
      scheme_download_error,
      start_scheme_download,
      stop_scheme_download
    ],
  app /
    logic /
    scheme_browser[
      download_scheme_overview,
      download_scheme_targets,
      get_scheme_overview,
      get_species_img,
      get_species_details,
      assemble_db_location,
      scheme_url
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
                      class = "help-header",
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
                          "'Load Database' opens it for typing and analysis.",
                          "Choose a folder on this computer's own disk: SQLite",
                          "relies on file locking that network and shared drives",
                          "(NFS, SMB/Windows shares, cloud-synced folders) do not",
                          "implement reliably, so a database kept there can be",
                          "corrupted by a typing run - and the app will freeze if",
                          "the connection drops while a database is open."
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
                        class = "storage-location-hint",
                        icon("circle-info"),
                        " ",
                        "Choose a folder on this computer. Databases kept on",
                        " network or shared drives (NFS, SMB, cloud-synced",
                        " folders) can be corrupted."
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

    Scheme_Browser <- reactiveValues(
      download_status = "",
      last_download = NULL,
      # The running download: process, target file, log, scheme and start time.
      download = NULL
    )

    # Plain mirror of the running download, so session teardown - outside any
    # reactive context - can still stop it and remove its partial database.
    live_download <- NULL
    session$onSessionEnded(function() {
      if (!is.null(live_download)) {
        stop_scheme_download(live_download$proc, live_download$db)
      }
    })

    # Reset module state when the user returns to the landing screen.
    # Clears the last download path so the "Load Database" button is disabled,
    # and abandons a download still in progress.
    observeEvent(
      session_reset(),
      {
        if (!is.null(live_download)) {
          stop_scheme_download(live_download$proc, live_download$db)
          live_download$waiter$hide()
          live_download <<- NULL
        }
        Scheme_Browser$download <- NULL
        Scheme_Browser$download_status <- ""
        Scheme_Browser$last_download <- NULL
      },
      ignoreInit = TRUE
    )

    # Render scheme selector
    output$scheme_selection <- renderUI({
      render_info("output$scheme_selection")

      # The picker's value is the species name with spaces (not underscores) -
      # get_scheme_overview() / download_scheme_targets() etc. all re-derive
      # the underscore form themselves, so label and value stay identical here.
      virtualSelectInput(
        ns("scheme_selector"),
        "Select Scheme",
        choices = sort(gsub("_", " ", cgmlst_org_schemes$species)),
        search = TRUE,
        searchPlaceholderText = "Search scheme ...",
        optionsCount = 10,
        # Escapes any clipping ancestor, same contract as every other picker in
        # the app (see the "Dropdown overflow" rules in main.scss).
        dropboxWrapper = "body",
        # Centers the dropbox over the whole viewport with a backdrop instead
        # of anchoring it under the toggle - same as col_picker / remove_picker
        # in database_browser.R.
        showDropboxAsPopup = TRUE,
        popupDropboxBreakpoint = "10000px",
        width = "100%"
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

    # Start a scheme download in the background. A large scheme can take many
    # minutes; running it synchronously would freeze every session and leave no
    # way to cancel it.
    observeEvent(input$scheme_download, {
      req(is.null(Scheme_Browser$download))

      db_location <- assemble_db_location(
        input$download_location,
        input$db_name
      )
      req(db_location)

      # Refusing an existing file also guarantees that whatever sits at
      # db_location after a failed or cancelled download is this download's own
      # partial file, safe to remove.
      if (file.exists(db_location)) {
        showNotification(
          paste(db_location, "already exists"),
          type = "error",
          duration = 5
        )
        return()
      }

      scheme <- input$scheme_selector
      log_file <- tempfile("scheme_download_", fileext = ".log")
      file.create(log_file)

      proc <- tryCatch(
        start_scheme_download(
          scheme,
          db_location,
          log_file,
          env_name = conda_env,
          scheme_url = scheme_url(scheme)
        ),
        error = function(e) e
      )
      if (inherits(proc, "error")) {
        unlink(log_file)
        showNotification(
          paste("Could not start the download:", conditionMessage(proc)),
          type = "error",
          duration = 8
        )
        return()
      }

      waiter <- Waiter$new(
        id = ns("scheme-download-container"),
        html = div(
          class = "spinner-custom",
          spin_flower(),
          div(
            h5("Downloading ..."),
            div(id = "scheme-load", scheme),
            div(id = ns("download_elapsed"), "0:00 elapsed"),
            tags$button(
              type = "button",
              class = "btn btn-primary",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
                ns("cancel_download")
              ),
              "Cancel"
            )
          )
        )
      )
      waiter$show()

      live_download <<- list(proc = proc, db = db_location, waiter = waiter)
      Scheme_Browser$download_status <- "Downloading ..."
      Scheme_Browser$download <- list(
        proc = proc,
        db = db_location,
        log = log_file,
        scheme = scheme,
        overview = scheme_overview(),
        started = Sys.time(),
        cancelled = FALSE
      )
    })

    # Cancel the running download: its process tree is killed and its partial
    # database removed; the poll below then reports the cancellation.
    observeEvent(input$cancel_download, {
      req(!is.null(Scheme_Browser$download))
      Scheme_Browser$download$cancelled <- TRUE
      stop_scheme_download(
        Scheme_Browser$download$proc,
        Scheme_Browser$download$db
      )
    })

    # Poll the running download once a second: show the elapsed time while it
    # runs, then finalise the database and report the outcome once it exits.
    observe({
      dl <- Scheme_Browser$download
      if (is.null(dl)) {
        return(NULL)
      }

      if (dl$proc$is_alive()) {
        secs <- as.integer(difftime(Sys.time(), dl$started, units = "secs"))
        html(
          "download_elapsed",
          sprintf("%d:%02d elapsed", secs %/% 60, secs %% 60)
        )
        invalidateLater(1000, session)
        return(NULL)
      }

      Scheme_Browser$download <- NULL
      live_download$waiter$hide()
      live_download <<- NULL

      log_lines <- if (file.exists(dl$log)) {
        readLines(dl$log, warn = FALSE)
      } else {
        character(0)
      }
      unlink(dl$log)

      if (isTRUE(dl$cancelled)) {
        showNotification(
          paste("Download of", dl$scheme, "cancelled"),
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      if (!identical(dl$proc$get_exit_status(), 0L) || !file.exists(dl$db)) {
        stop_scheme_download(NULL, dl$db)
        reason <- scheme_download_error(log_lines)
        showNotification(
          paste0(
            "Download of ",
            dl$scheme,
            " failed",
            if (!is.na(reason)) paste0(": ", reason) else ""
          ),
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # Writes to the new database file (source and species repair), so it is
      # guarded like any other database write.
      finished <- guard_db(
        "Downloading the scheme",
        finish_scheme_download(dl$db, log_lines)
      )
      if (db_failed(finished)) {
        return(NULL)
      }

      # Store the scheme overview and its target/locus table from cgmlst.org
      # (`targets`). Both are supplementary: a failed write is reported but
      # leaves the downloaded database loadable.
      guard_db("Storing the scheme details", {
        if (is.data.frame(dl$overview)) {
          download_scheme_overview(dl$overview, dl$db)
        }
        download_scheme_targets(dl$scheme, dl$db)
      })

      # Remember the path of the last successful download so the
      # "Load Database" click hands over this database rather than the
      # current (possibly changed) input selection.
      Scheme_Browser$last_download <- dl$db

      enable("load_db")
      addClass("load_db", "btn-attention")

      showNotification(
        paste("Download of", dl$scheme, "was successful."),
        type = "message",
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
