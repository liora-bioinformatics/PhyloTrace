# app/view/database.R

box::use(
  shiny[NS, moduleServer, observeEvent],
  bslib[page_sidebar, sidebar, navset_hidden, nav_panel, as_fill_carrier],
  shinyjs[useShinyjs, addClass, removeClass],
)
box::use(
  app / logic / db_events,
  app / logic / db_store,
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
  show_browse = shiny::reactive(0L),
  ui_mounted = shiny::reactive(0L),
  db_rev = db_events$new_bus(),
  # Defaults to a store wired to whatever db_path/db_rev this call actually
  # received (R evaluates default expressions in the function's own frame, so
  # this sees the bound values, not new_store()'s own defaults) - a plain
  # `db_store$new_store()` default would silently construct a store pinned to
  # its own reactive(NULL), never reflecting the real database regardless of
  # what db_path was passed.
  store = db_store$new_store(db_path = db_path, db_rev = db_rev)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Switch the visible sub-panel back to Browse Entries and move the sidebar
    # highlight along with it.
    select_browse <- function() {
      bslib::nav_select("pages", selected = "browse_entries")
      for (item in db_menu) {
        removeClass(paste0("menu_", item$value), "active")
      }
      addClass("menu_browse_entries", "active")
    }

    # Reset module state when the user returns to the landing screen.
    observeEvent(session_reset(), select_browse(), ignoreInit = TRUE)

    # main.R asks for Browse Entries *before* it starts a database reload, so
    # the reload overlay has a visible element to cover. The reset observer
    # above would get there eventually (main.R's reload bumps session_reset
    # too), but only at the very end of the reload - by which time the overlay
    # has already been hidden again, and the user has been staring at the panel
    # they started from (Import, say) for the whole reload.
    observeEvent(show_browse(), select_browse(), ignoreInit = TRUE)

    # Start each interface module's server under its own namespace and forward
    # session_reset so each sub-module can reset its own state.
    # Browse Entries' return value (pending) is captured and re-exposed so
    # main.R can check for unsaved edits. The import module signals a completed
    # merge/restore through `imported`, which main.R turns into a reload prompt.
    #
    # db_rev goes to every sub-module: each writes some corner of the database
    # that at least one of the others reads. It replaces the local
    # `custom_updated` signal that used to carry Custom Variables -> Browse
    # Entries / Import / Export, which covered that one edge and no others.
    #
    # `store` (the shared metadata cache, app/logic/db_store.R) goes only to
    # the two sub-modules that display the metadata table.
    browse_entries_vals <- NULL
    import_vals <- NULL
    for (item in db_menu) {
      extra <- switch(
        item$value,
        browse_entries = list(store = store),
        # Import and Export drive part of their sidebar from the server
        # (shinyjs::toggle on the control groups that only one source/export
        # type needs), so both need to know when their markup was rebuilt.
        import = list(ui_mounted = ui_mounted),
        export = list(ui_mounted = ui_mounted, store = store),
        list()
      )
      result <- do.call(
        item$module$server,
        c(
          list(
            item$value,
            db_path = db_path,
            session_reset = session_reset,
            db_rev = db_rev
          ),
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
