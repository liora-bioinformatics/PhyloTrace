# app/view/database.R

box::use(
  shiny[NS, moduleServer, observeEvent, reactive, reactiveVal],
  bslib[page_sidebar, sidebar, navset_hidden, nav_panel, as_fill_carrier],
  shinyjs[useShinyjs, addClass, removeClass],
)
box::use(
  app / logic / functions[sidebar_menu],
  app / view / database_browser,
  app / view / database_custom,
  app / view / database_cgmlst,
  app / view / database_import,
  app / view / database_export,
)

# Sidebar menu definition
db_menu <- list(
  list(
    value = "browse_entries",
    label = "Browse Entries",
    module = database_browser
  ),
  list(
    value = "custom_fields",
    label = "Custom Variables",
    module = database_custom
  ),
  list(
    value = "cgmlst_overview",
    label = "cgMLST Overview",
    module = database_cgmlst
  ),
  list(
    value = "import",
    label = "Import",
    module = database_import
  ),
  list(
    value = "export",
    label = "Export",
    module = database_export
  )
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    useShinyjs(),
    fillable = TRUE,
    sidebar = sidebar(
      title = "Database Browser",
      sidebar_menu(ns, db_menu)
    ),
    # Hidden tabset: one panel per interface, swapped via nav_select(). Each
    # panel hosts its module's UI under the module's own namespace.
    do.call(
      navset_hidden,
      c(
        list(id = ns("pages")),
        lapply(db_menu, function(item) {
          as_fill_carrier(nav_panel(
            title = item$label,
            value = item$value,
            item$module$ui(ns(item$value))
          ))
        })
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  ui_mounted = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reset module state when the user returns to the landing screen.
    # Switches the visible sub-panel back to Browse Entries and clears the
    # active highlight from all other sidebar buttons.
    observeEvent(
      session_reset(),
      {
        bslib::nav_select("pages", selected = "browse_entries")
        for (item in db_menu) {
          removeClass(paste0("menu_", item$value), "active")
        }
        addClass("menu_browse_entries", "active")
      },
      ignoreInit = TRUE
    )

    # Bumped by the Custom Variables panel whenever it changes the store, and
    # watched by Browse Entries so a new variable shows up in its column picker
    # without a full database reload.
    custom_updated <- reactiveVal(0L)

    # Start each interface module's server under its own namespace and forward
    # session_reset so each sub-module can reset its own state.
    # db_updated is passed only to browse_entries; its return value (pending)
    # is captured and re-exposed so main.R can check for unsaved edits. The
    # import module signals a completed merge/restore through `imported`, which
    # main.R turns into a reload prompt.
    browse_entries_vals <- NULL
    import_vals <- NULL
    for (item in db_menu) {
      extra <- switch(
        item$value,
        browse_entries = list(
          db_updated = db_updated,
          custom_updated = custom_updated
        ),
        custom_fields = list(custom_updated = custom_updated),
        # Import and Export drive part of their sidebar from the server
        # (shinyjs::toggle on the control groups that only one source/export
        # type needs), so both need to know when their markup was rebuilt.
        import = list(
          custom_updated = custom_updated,
          ui_mounted = ui_mounted
        ),
        export = list(
          custom_updated = custom_updated,
          ui_mounted = ui_mounted
        ),
        list()
      )
      result <- do.call(
        item$module$server,
        c(
          list(item$value, db_path = db_path, session_reset = session_reset),
          extra
        )
      )
      if (identical(item$value, "browse_entries")) {
        browse_entries_vals <- result
      }
      if (identical(item$value, "import")) {
        import_vals <- result
      }
    }

    # Each menu button switches the main field to its panel and keeps the
    # `active` highlight (hover color) on the current selection.
    lapply(db_menu, function(item) {
      observeEvent(input[[paste0("menu_", item$value)]], {
        bslib::nav_select("pages", selected = item$value)

        for (other in db_menu) {
          removeClass(paste0("menu_", other$value), "active")
        }
        addClass(paste0("menu_", item$value), "active")
      })
    })

    list(
      pending = browse_entries_vals$pending,
      imported = import_vals$imported
    )
  })
}
