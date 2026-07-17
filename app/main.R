box::use(
  shiny[
    stopApp,
    moduleServer,
    NS,
    observeEvent,
    tags,
    div,
    actionButton,
    showModal,
    removeModal,
    modalDialog,
    modalButton,
    icon,
    tagList,
    reactive,
    reactiveVal,
    isolate,
    showNotification,
    removeNotification,
  ],
  bslib[
    page_sidebar,
    page_fillable,
    navbar_options,
    bs_theme,
    nav_panel,
    page_navbar,
    nav_spacer,
    nav_item,
    nav_select,
    nav_insert,
    nav_remove,
    nav_hide,
    nav_show,
    as_fill_carrier,
    toggle_dark_mode
  ],
  shinyjs[runjs],
  htmltools[tagQuery],
  waiter[useWaiter, waiterShowOnLoad, Waiter, spin_flower],
)
box::use(
  app / logic / functions[render_info],
  app / logic / paths[stat_json, app_local_share_path],
  app / logic / pymlst[hash_database],
  app / view / landing_page,
  app / view / scheme_browser,
  app / view / database,
  app / view / typing,
  app / view / analysis_dashboard,
  app / view / visualization,
  app / view / resistance_screening,
)

fillable_panel <- function(...) {
  as_fill_carrier(nav_panel(...))
}

# shinyFiles asset stripping logic
strip_shinyfiles_assets <- function(ui) {
  tagQuery(ui)$find("script")$filter(function(node, i) {
    src <- node$attribs$src
    !is.null(src) && grepl("^sF/", src)
  })$remove()$resetSelected()$find("link")$filter(function(node, i) {
    href <- node$attribs$href
    !is.null(href) && grepl("^sF/", href)
  })$remove()$allTags()
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    useWaiter(),
    waiterShowOnLoad(
      html = div(
        class = "waiter-splash",
        tags$img(
          src = "static/images/PhyloTrace_flat_256.png",
          width = "200px",
          height = "200px"
        ),
        div(class = "waiter-splash-title", "PhyloTrace")
      )
    ),
    page_navbar(
      id = ns("tabs"),
      title = div(
        id = "navbar-title",
        tags$img(
          src = "static/images/PhyloTrace_flat_128.png",
        ),
        div("PhyloTrace")
      ),
      window_title = "PhyloTrace",
      # Baseline Bootstrap 5 theme; the navbar button toggles its light/dark mode.
      theme = bs_theme(version = 5, preset = "shiny"),
      navbar_options = navbar_options(underline = TRUE),
      nav_panel(
        title = "Load Database",
        value = "landing_page_panel",
        landing_page$ui(ns("landing_page"))
      ),
      nav_panel(
        title = "Scheme Browser",
        value = "scheme_browser_panel",
        scheme_browser$ui(ns("scheme_browser"))
      ),
      nav_spacer(),
      nav_item("v1.6.1"),
      nav_item(
        actionButton(
          inputId = ns("toggle_dark"),
          label = NULL,
          icon = icon("moon"),
          title = "Toggle Light/Dark Mode"
        )
      ),
      nav_item(
        actionButton(
          inputId = ns("quit"),
          label = NULL,
          icon = icon("power-off"),
          title = "Turn Off"
        )
      )
    )
  ) # close tagList
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # [A/B TEST — TEMPORARY, REVERT AFTER CONFIRMING] The real fix (the
    # RAM-scaled heap cap) is DISABLED via `if (FALSE)` below so the runaway can
    # be reproduced. A single high safety ceiling (14 GB) is left active so the
    # balloon throws `cannot allocate vector` near the top instead of hard-OOMing
    # the machine (which crashes Positron). Expect: RAM climbs from ~130 MB
    # toward 14 GB and the DataTable goes blank on reload-after-typing — that is
    # the bug returning.
    # TO RESTORE THE FIX: delete the mem.maxVSize(14000) line and change
    # `if (FALSE)` to `if (TRUE)`.
    mem.maxVSize(14000) # [A/B TEST] safety ceiling ONLY — not the fix
    # ========================================================================

    # Bound R's vector heap so its garbage-collection trigger cannot balloon.
    # R amortises GC by letting the heap grow before collecting; left unbounded
    # on this app the trigger grew into the tens of GB, so *reclaimable* garbage
    # (the live working set is only ~130 MB) piled up to ~19 GB between
    # collections. A reload's reset cascade on a memory-tight machine then tipped
    # it into swap — RAM spiked and the freshly shown Database Browser rendered
    # its DataTable blank. Capping the heap forces R to collect before it
    # balloons; the cap is far above any real working set so it never limits
    # actual work. Scaled to physical RAM (Linux /proc/meminfo) so it adapts
    # across machines, with an 8 GB fallback when that can't be read.
    if (FALSE) local({
      mem_total_mb <- tryCatch(
        {
          line <- grep(
            "^MemTotal",
            readLines("/proc/meminfo"),
            value = TRUE
          )[1]
          as.numeric(gsub("\\D", "", line)) / 1024
        },
        error = function(e) NA_real_
      )
      cap <- if (is.finite(mem_total_mb)) {
        max(4096, floor(mem_total_mb * 0.5))
      } else {
        8192
      }
      suppressWarnings(try(mem.maxVSize(cap), silent = TRUE))
    })

    # Kill server on session end
    session$onSessionEnded(function() {
      stopApp()
    })

    # Light/dark mode toggle
    observeEvent(input$toggle_dark, {
      toggle_dark_mode()
    })

    # Close application
    observeEvent(input$quit, {
      showModal(
        div(
          class = "start-modal",
          modalDialog(
            "Are you sure you want to stop the application?",
            title = "Close PhyloTrace",
            easyClose = TRUE,
            footer = tagList(
              modalButton("No"),
              actionButton(
                ns("conf_shutdown"),
                "Yes",
                width = "100px"
              )
            )
          )
        )
      )
    })

    observeEvent(input$conf_shutdown, {
      removeModal()
      runjs("window.close();")
      later::later(stopApp, delay = 0.5)
    })

    # Shared reset signal: incremented each time the user resets the session.
    # Modules observe it with ignoreInit = TRUE and tear down their own state.
    session_reset <- reactiveVal(0L)

    SCHEME_BROWSER_vals <- scheme_browser$server(
      "scheme_browser",
      session_reset = session_reset
    )

    scheme_browser_db <- reactive({
      SCHEME_BROWSER_vals$load_db()
      isolate(SCHEME_BROWSER_vals$db_location())
    })

    LANDING_PAGE_vals <- landing_page$server(
      "landing_page",
      external_db = scheme_browser_db,
      session_reset = session_reset
    )

    TYPING_vals <- typing$server(
      "typing",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = session_reset
    )

    # ID of the most-recently shown "new isolates" notification; used to
    # remove it programmatically when the user clicks Reload or resets.
    db_notification_id <- NULL

    DATABASE_vals <- database$server(
      "database",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = session_reset,
      typing_status = TYPING_vals$typing_status,
      db_updated = TYPING_vals$db_updated
    )

    # The database changed underneath the open session, so offer a reload —
    # warning first about metadata edits the reload would discard. `headline`
    # and `detail` name the actual cause: typing and import both land here, and
    # a notification that blamed the wrong one would be worse than none.
    notify_db_changed <- function(db_icon, headline, detail) {
      has_pending <- isTRUE(DATABASE_vals$pending())

      db_notification_id <<- showNotification(
        ui = tagList(
          icon(db_icon),
          " ",
          tags$strong(headline),
          tags$br(),
          detail,
          if (has_pending) {
            div(
              class = "alert alert-warning p-2 mt-2 mb-1 small",
              icon("triangle-exclamation"),
              " ",
              tags$strong("Unsaved metadata edits detected."),
              " Save or discard them in the Database Browser before reloading, or they will be lost."
            )
          },
          div(
            class = "mt-1",
            actionButton(
              ns("reload_db"),
              label = "Reload Database",
              icon = icon("rotate"),
              class = "btn-sm btn-primary w-100"
            )
          )
        ),
        duration = NULL,
        closeButton = TRUE,
        type = if (has_pending) "warning" else "message",
        session = session
      )
    }

    observeEvent(
      TYPING_vals$db_updated(),
      {
        notify_db_changed(
          "database",
          "New isolates added.",
          "Typing wrote new entries to the database."
        )
      },
      ignoreInit = TRUE
    )

    # Bumped by the Import panel after a merge or a backup restore.
    observeEvent(
      DATABASE_vals$imported(),
      {
        notify_db_changed(
          "code-merge",
          "Database changed.",
          "An import or a backup restore rewrote the database."
        )
      },
      ignoreInit = TRUE
    )
    analysis_dashboard$server(
      "analysis_dashboard",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = session_reset
    )
    visualization$server(
      "visualization",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = session_reset,
      typing_status = TYPING_vals$typing_status,
      db_updated = TYPING_vals$db_updated
    )
    resistance_screening$server(
      "resistance_screening",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = session_reset,
      typing_status = TYPING_vals$typing_status,
      db_updated = TYPING_vals$db_updated
    )

    observeEvent(LANDING_PAGE_vals$create_scheme(), {
      nav_select(id = "tabs", selected = "scheme_browser_panel")
    })

    observeEvent(LANDING_PAGE_vals$load_database(), {
      db_path <- LANDING_PAGE_vals$db_path()

      # Full-page loading overlay. The panel HTML below is built synchronously
      # here; the outputs inside those panels that opt out of suspendWhenHidden
      # pre-render on the following flushes, i.e. underneath this overlay.
      w <- Waiter$new(
        id = NULL,
        html = div(
          class = "spinner-custom",
          spin_flower(),
          div(
            tags$h5("Hashing Database ..."),
            if (length(db_path) && !is.na(db_path)) {
              div(id = "db-load", basename(db_path))
            }
          )
        )
      )
      w$show()

      # Hash database
      # TODO: a hashed databse takes currently much longer to perform allelic typing on
      # maybe hash_database() should be appled after each isolate addition?
      # check underlying pymlst implementation and why the hash changes slow typing down
      hash_database(db_path)

      app_panels <- list(
        fillable_panel(
          "Database Browser",
          value = "database_panel",
          strip_shinyfiles_assets(database$ui(ns("database")))
        ),
        fillable_panel(
          "Analysis Dashboard",
          value = "analysis_dashboard_panel",
          strip_shinyfiles_assets(analysis_dashboard$ui(ns(
            "analysis_dashboard"
          )))
        ),
        fillable_panel(
          "Visualization",
          value = "visualization_panel",
          strip_shinyfiles_assets(visualization$ui(ns("visualization")))
        ),
        fillable_panel(
          "Allelic Typing",
          value = "typing_panel",
          strip_shinyfiles_assets(typing$ui(ns("typing")))
        ),
        fillable_panel(
          "Resistance Screening",
          value = "resistance_screening_panel",
          strip_shinyfiles_assets(resistance_screening$ui(ns(
            "resistance_screening"
          )))
        )
      )

      targets <- c(
        "scheme_browser_panel",
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel"
      )

      for (i in seq_along(app_panels)) {
        nav_insert(
          id = "tabs",
          nav = app_panels[[i]],
          target = targets[i],
          position = "after",
          select = i == 1L
        )
      }

      nav_insert(
        id = "tabs",
        nav = nav_item(
          actionButton(
            inputId = ns("reset"),
            label = NULL,
            icon = icon("arrow-rotate-left"),
            title = "Return to the start screen"
          )
        ),
        target = "resistance_screening_panel",
        position = "after"
      )

      nav_insert(
        id = "tabs",
        nav = nav_item(
          style = "margin-left: auto;",
          if (length(db_path) && !is.na(db_path)) {
            div(
              id = "loaded-db-path",
              title = db_path,
              basename(db_path)
            )
          }
        ),
        target = "resistance_screening_panel",
        position = "after"
      )

      nav_hide(id = "tabs", target = "landing_page_panel")
      nav_hide(id = "tabs", target = "scheme_browser_panel")

      if (!is.null(stat_json$last_db) && file.exists(stat_json$last_db)) {
        stat_json$last_db <- db_path
      } else {
        stat_json <- list(last_db = db_path)
      }
      jsonlite::write_json(
        stat_json,
        file.path(app_local_share_path, "state.json"),
        pretty = TRUE,
        auto_unbox = TRUE
      )

      # The load is near-instant, so hold the overlay for a short buffer to read
      # as a deliberate loading step. Hide asynchronously via later() so the show
      # and hide land in separate flushes and the overlay is actually painted.
      # withReactiveDomain() restores the session inside the later callback so
      # waiter can resolve it (otherwise w$hide() errors on a NULL session).
      later::later(
        function() shiny::withReactiveDomain(session, w$hide()),
        delay = 1
      )
    })

    # Increment the shared reset signal so every subscribed module observer
    # fires and tears down its own reactive state (files, results, processes).
    reset_session <- function() {
      # Reclaim accumulated garbage and return it to the OS at the reset point,
      # before the teardown/rebuild cascade runs. The vector-heap cap above keeps
      # the session from ballooning; this full collection additionally hands the
      # freed pages back to the OS promptly (Linux malloc_trim) so a reload on a
      # memory-tight machine renders with headroom rather than in swap. Cheap on
      # the app's small live set (~130 MB); synchronous so it finishes before
      # session_reset() triggers the rebuild flush.
      # gc(full = TRUE) # [A/B TEST — TEMPORARY] disabled; re-enable to restore fix
      session_reset(session_reset() + 1L)
    }

    hide_db_notification <- function() {
      if (!is.null(db_notification_id)) {
        removeNotification(db_notification_id, session = session)
        db_notification_id <<- NULL
      }
    }

    # Reload the database: reset all module-internal state (so every module
    # re-queries the updated DB) without removing panels or returning to the
    # landing screen, then navigate directly to the Database Browser.
    observeEvent(input$reload_db, {
      hide_db_notification()
      reset_session()
      nav_select(id = "tabs", selected = "database_panel")
    })

    observeEvent(input$reset, {
      hide_db_notification()
      nav_show(id = "tabs", target = "landing_page_panel", select = TRUE)
      nav_show(id = "tabs", target = "scheme_browser_panel")

      for (panel in c(
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel",
        "resistance_screening_panel"
      )) {
        nav_remove(id = "tabs", target = panel)
      }

      # Remove the two dynamically inserted navbar items (reset button and
      # db-path display) by finding their known HTML element ids and walking
      # up to the enclosing <li class="nav-item"> Bootstrap generates.
      runjs(sprintf(
        "['%s','loaded-db-path'].forEach(function(id){
           var el=document.getElementById(id);
           if(el){var li=el.closest('li.nav-item');if(li)li.remove();}
         });",
        ns("reset")
      ))

      reset_session()
    })
  })
}
