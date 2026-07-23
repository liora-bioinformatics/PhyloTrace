# app/view/landing_page.R

box::use(
  DT[DTOutput, datatable, renderDT],
  bslib[page_fillable],
  fs[path_home],
  jsonlite[fromJSON],
  shiny,
  shinyFiles[parseFilePaths, shinyFileChoose, shinyFilesButton],
  shinyjs[disabled, useShinyjs],
)

box::use(
  app / logic / functions[render_info],
  app / logic / paths[app_local_share_path],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  page_fillable(
    useShinyjs(),
    shiny$imageOutput(ns("phylotrace_large")),
    shiny$div(
      class = "landing-page-ui",
      shiny$div(
        id = "loading-instructions",
        shiny$div(
          id = "instructions",
          "Proceed by loading a compatible local database or create a new one."
        ),
        shiny$div(
          id = "loading-inputs",
          shinyFilesButton(
            ns("db_location"),
            "Select Database",
            icon = shiny$icon("folder-open"),
            title = "Choose a database",
            buttonType = "default",
            root = path_home(),
            multiple = FALSE
          ),
          shiny$actionButton(
            ns("create_new_db"),
            "Create New",
            icon = shiny$icon("plus"),
            title = "Choose location for new database",
            buttonType = "default",
            root = path_home()
          )
        ),
        shiny$uiOutput(ns("database_selection"))
      )
    )
  )
}

#' @export
server <- function(
  id,
  external_db = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Currently selected database location, as c(label, path) or c(label, NA).
    db_location <- shiny$reactiveVal(NULL)

    # Combined load trigger: incremented by the UI button and by the external
    # db observer so that both paths share one downstream observeEvent.
    load_trigger <- shiny$reactiveVal(NULL)
    fire_load <- function() {
      n <- load_trigger()
      load_trigger(if (is.null(n)) 1L else n + 1L)
    }

    # Incrementing this forces the db-location observe() to re-run after reset,
    # even when input$db_location hasn't changed.
    db_location_trigger <- shiny$reactiveVal(0L)

    shiny$observeEvent(
      session_reset(),
      {
        load_trigger(NULL)
        db_location_trigger(db_location_trigger() + 1L)
      },
      ignoreInit = TRUE
    )

    # Observe a database location
    shiny$observeEvent(external_db(), {
      db_path <- external_db()
      shiny$req(!is.null(db_path), length(db_path), is.character(db_path))

      loc <- if (file.exists(db_path)) {
        c("Currently selected:", db_path)
      } else {
        c("Currently selected:", NA)
      }
      db_location(loc)

      if (!is.na(loc[2])) fire_load()
    })

    shiny$observeEvent(input$load_database, {
      fire_load()
    })

    # Render PhyloTrace logo
    output$phylotrace_large <- shiny$renderImage(
      {
        render_info("output$phylotrace_large")

        list(
          src = file.path(getwd(), "app/static/images/PhyloTrace_flat_512.png")
        )
      },
      deleteFile = FALSE
    )

    # Present DB location dir choose
    shinyFileChoose(
      input,
      "db_location",
      roots = c(Home = path_home(), Root = "/"),
      defaultRoot = "Home",
      filetypes = c("db"),
      session = session
    )

    # Observe current database path.
    # Depends on db_location_trigger so it also re-runs after a session reset,
    # even when input$db_location has not changed.
    # Falls back to disk (not in-memory) so it picks up whatever main.R wrote
    # during this session.
    shiny$observe({
      db_location_trigger()

      loc <- NULL

      location_input <- parseFilePaths(
        roots = c(Home = path_home(), Root = "/"),
        input$db_location
      )$datapath

      if (length(location_input)) {
        if (is.character(location_input) && file.exists(location_input)) {
          loc <- c("Currently selected:", location_input)
        } else {
          loc <- c("Currently selected:", NA)
        }
      } else {
        state_file <- file.path(app_local_share_path, "state.json")
        disk_stat <- tryCatch(
          if (file.exists(state_file)) fromJSON(state_file) else NULL,
          error = function(e) NULL
        )
        if (
          !is.null(disk_stat) &&
            length(disk_stat$last_db) &&
            file.exists(disk_stat$last_db) &&
            endsWith(disk_stat$last_db, ".db")
        ) {
          loc <- c("Last used:", disk_stat$last_db)
        }
      }

      db_location(loc)
    })

    # Render database selection interface
    output$database_selection <- shiny$renderUI({
      shiny$req(db_location())

      render_info("output$database_selection")

      db_unselected <- is.na(db_location()[2])

      load_button <- shiny$actionButton(
        ns("load_database"),
        "Load Database"
      )

      if (db_unselected) {
        # Case no database selected
        table <- "No database selected"
        load_button <- disabled(load_button)
      } else {
        # Case valid database selected
        table <- DTOutput(ns("selected_database"))
      }

      shiny$div(
        shiny$p(db_location()[1]),
        table,
        load_button
      )
    })

    # Render selected database
    output$selected_database <- renderDT({
      render_info("output$selected_database")

      # Get database metadata
      db_path <- db_location()[2]
      shiny$req(is.character(db_path) && !is.na(db_path) && file.exists(db_path))
      db_metadata <- file.info(db_path)
      db_name <- basename(db_path)
      db_time <- format(db_metadata$mtime, "%Y-%m-%d %H:%M:%S")
      db_size <- if (db_metadata$size >= 1000^3) {
        paste0(round(db_metadata$size / 1000^3, 2), " GB")
      } else if (db_metadata$size >= 1000^2) {
        paste0(round(db_metadata$size / 1000^2, 2), " MB")
      } else if (db_metadata$size >= 1000) {
        paste0(round(db_metadata$size / 1000, 2), " KB")
      } else {
        paste0(db_metadata$size, " Bytes")
      }

      # Render database metadata table
      datatable(
        data.frame(
          Property = c("Name", "Location", "Size", "Last Changed"),
          Value = c(
            db_name,
            db_path,
            db_size,
            db_time
          )
        ),
        class = "stripe row-border order-column",
        colnames = c("", ""),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", ordering = FALSE)
      )
    })

    # Return values
    #
    # `db_path` is the *loaded* database, not merely the selected or remembered
    # one. `db_location()` is populated at startup from state.json so the landing
    # page can offer "Last used: …", and it moves as soon as the user picks a
    # file - but neither of those means the database is open. Gating on
    # `load_trigger()` (NULL until Load is pressed, NULL again after a session
    # reset) keeps every downstream module from querying - or migrating - a file
    # the user has not asked for yet. Without the gate the Database Browser and
    # Custom Variables panels read the remembered database on the first reactive
    # flush, i.e. before main.R has run its on-load migrations.
    list(
      create_scheme = shiny$reactive(input$create_new_db),
      load_database = shiny$reactive(load_trigger()),
      db_path = shiny$reactive(
        if (is.null(load_trigger())) NULL else db_location()[2]
      )
    )
  })
}
