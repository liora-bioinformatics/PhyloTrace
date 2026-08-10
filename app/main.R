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
    selectInput
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
  htmltools[attachDependencies, findDependencies],
)
box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / database_functions[migrate_species_name, sync_metadata_table],
  app / logic / db_events[bump_all, new_bus],
  app / logic / db_store[new_store],
  app / logic / functions[render_info],
  app / logic / logging[log_event, start_session_log],
  app / logic / paths[stat_json, app_local_share_path],
  app / logic / pymlst[hash_database, hashes_pending],
  app / view / landing_page,
  app / view / scheme_browser,
  app / view / database,
  app / view / typing,
  app / view / analysis_dashboard,
  app / view / visualization,
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

  main_layout <- tagList(
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
      nav_item(paste0("v", APP_VERSION)),
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

  # Force Shiny to load Selectize JS assets into the initial HTML <head> at
  # startup. Prevents missing 'selectize-plugin-a11y' errors when dropdowns
  # load dynamically in modules/modals.
  attachDependencies(
    main_layout,
    findDependencies(selectInput(
      ns("selectize_deps"),
      label = NULL,
      choices = NULL
    ))
  )
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
    # mem.maxVSize(14000) # [A/B TEST] safety ceiling ONLY — not the fix
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
    if (FALSE) {
      local({
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
    }

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

    # Shared reset signal: incremented only when the user returns to the
    # landing screen (input$reset). Landing Page and Scheme Browser are the
    # only listeners - Landing Page nulls its load_trigger on this signal so
    # a stale remembered db_path isn't picked up before the user explicitly
    # loads one again.
    session_reset <- reactiveVal(0L)

    # Bumped whenever every other module should tear down / re-query its
    # state: on a full session_reset AND on "Reload Database" (input$reload_db).
    # Deliberately NOT observed by Landing Page/Scheme Browser - if it were,
    # "Reload Database" would null out Landing Page's load_trigger (hence
    # db_path) without ever re-firing it, since that lightweight reload never
    # sends the user back to the landing screen to press Load again.
    data_reset <- reactiveVal(0L)

    # Bumped once a freshly built set of app panels is actually in the DOM.
    #
    # The panels are rebuilt from scratch on every database load (see the
    # load_database handler below) while the module servers live for the whole
    # session. Shiny replays an output's last value into a re-bound element, so
    # everything rendered through renderUI() comes back on its own - but a
    # one-shot shinyjs message (toggle(), toggleClass(), toggleState()) has
    # nothing to replay, and the observer that sent it does not re-fire because
    # none of the reactives it watches changed. The new markup therefore arrives
    # in its static state: controls the module had revealed are hidden again.
    # Modules re-apply that DOM state when this signal advances.
    #
    # Deferred through later() on purpose - see the bump site for why.
    ui_mounted <- reactiveVal(0L)

    # Per-domain revision counters for the loaded database (app/logic/db_events.R).
    #
    # A SQLite write is invisible to Shiny's reactive graph: db_path() does not
    # change, so a reader keyed only on it never re-queries and quietly shows
    # data from before the write. Every module that writes bumps the domains it
    # touched; every module that reads names the domains it depends on. This
    # supersedes the per-pair signals the app grew one at a time (Analyses/plots,
    # custom variables, staged sets, typed isolates), which each covered exactly
    # one edge of that graph and left the rest stale.
    #
    # Distinct from data_reset below: this says "re-read, your query is out of
    # date", not "tear down, the session moved". Modules holding unsaved user
    # input (the Database Browser's edit grid) deliberately treat a bump as a
    # staleness marker rather than a cue to reload over the user's work.
    db_rev <- new_bus()

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

    # The shared metadata cache (app/logic/db_store.R): one reactive read of
    # the `metadata` table, passed to every module that displays it, so the
    # question "does the whole app agree on the isolate table" has exactly one
    # answer rather than one per consumer.
    store <- new_store(db_path = LANDING_PAGE_vals$db_path, db_rev = db_rev)

    TYPING_vals <- typing$server(
      "typing",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = data_reset,
      db_rev = db_rev
    )

    # IDs of currently shown "database changed" notifications; used to remove
    # them programmatically when the user clicks Reload or resets. A vector
    # because typing/import can fire more than once before either is
    # acknowledged, stacking multiple notifications.
    db_notification_ids <- character(0)

    # Asks the Database module to bring its Browse Entries sub-panel to the
    # front. Bumped by the reload handler below, which needs that panel visible
    # before it covers it with the reload overlay.
    show_browse <- reactiveVal(0L)

    DATABASE_vals <- database$server(
      "database",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = data_reset,
      show_browse = show_browse,
      ui_mounted = ui_mounted,
      db_rev = db_rev,
      store = store
    )

    # The database changed underneath the open session, so offer a reload —
    # warning first about metadata edits the reload would discard. `headline`
    # and `detail` name the actual cause: typing and import both land here, and
    # a notification that blamed the wrong one would be worse than none.
    notify_db_changed <- function(db_icon, headline, detail) {
      has_pending <- isTRUE(DATABASE_vals$pending())

      db_notification_ids <<- c(
        db_notification_ids,
        showNotification(
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
      )
    }

    observeEvent(
      TYPING_vals$db_updated(),
      {
        notify_db_changed(
          "database",
          "New isolates added",
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

    ANALYSIS_DASHBOARD_vals <- analysis_dashboard$server(
      "analysis_dashboard",
      db_path = LANDING_PAGE_vals$db_path,
      session_reset = data_reset,
      db_rev = db_rev,
      store = store
    )
    visualization$server(
      "visualization",
      db_path = LANDING_PAGE_vals$db_path,
      # The true session_reset, NOT data_reset — the one module wired this way.
      #
      # Visualization tears its plot tabs down on this signal, and does so
      # without removing their nav markup, because the only caller that raises
      # it (input$reset) has already removed the whole Visualization panel.
      # data_reset does not: "Reload Database" leaves every panel in place, so
      # feeding it here destroyed each tab's server behind nav markup that
      # stayed on screen — a blank plot tab that could not be used or closed.
      #
      # It no longer needs data_reset either. Everything it would have re-read
      # on a reload it now re-reads through db_rev, which reload_data() bumps.
      session_reset = session_reset,
      # Dashboard -> Visualization: "Add Plot" for an Analysis and "open a saved
      # plot" both route here and preselect / restore in the Save panel.
      launch_ctx = ANALYSIS_DASHBOARD_vals$request_add_plot,
      open_ctx = ANALYSIS_DASHBOARD_vals$request_open_plot,
      db_rev = db_rev,
      store = store,
      # Blocks Generate while a typing run is writing the database. The plot
      # engines are the only readers that go to `mlst` for allele profiles
      # instead of through the store, so they are the only ones that can see a
      # run half-written.
      typing_active = TYPING_vals$typing_active
    )

    # Dashboard buttons that hand off to the Visualization tab.
    observeEvent(ANALYSIS_DASHBOARD_vals$request_add_plot(), {
      nav_select(id = "tabs", selected = "visualization_panel")
    })
    observeEvent(ANALYSIS_DASHBOARD_vals$request_open_plot(), {
      nav_select(id = "tabs", selected = "visualization_panel")
    })

    observeEvent(LANDING_PAGE_vals$create_scheme(), {
      nav_select(id = "tabs", selected = "scheme_browser_panel")
    })

    # Clear the two navbar items the load handler inserts (the reset button and
    # the db-path display), by their known element ids.
    #
    # Called from both ends — before inserting and on reset — because the two
    # nav_insert() calls are the only writers of the `app-reset` element id and
    # nothing else keeps count of them. Sweeping first makes the insertion
    # idempotent: however many times load_database() fires (the landing page
    # fires it from the Load button *and* from its external-db observer), the
    # navbar ends up with exactly one of each rather than a second element
    # quietly sharing a Shiny input id with the first.
    remove_navbar_items <- function() {
      # querySelectorAll rather than getElementById: it takes every copy, so a
      # session that already accumulated some is repaired rather than merely
      # kept from growing. The fallback to the element itself keeps this
      # correct if bslib's wrapper markup ever changes — the id is the part
      # that has to go.
      runjs(sprintf(
        "['%s','loaded-db-path'].forEach(function(id){
           document.querySelectorAll('[id=\"' + id + '\"]').forEach(function(el){
             var item = el.closest('li.nav-item') || el;
             if (window.Shiny && Shiny.unbindAll) { Shiny.unbindAll(item); }
             item.remove();
           });
         });",
        ns("reset")
      ))
    }

    observeEvent(LANDING_PAGE_vals$load_database(), {
      db_path <- LANDING_PAGE_vals$db_path()

      # Open a fresh session log for this database load. Its header names the
      # database, so every line written afterwards (DB writes, typing, ...) can
      # omit it. Must run first, so the DB writes triggered further down this
      # handler land in the new file.
      start_session_log(if (length(db_path)) db_path else NA_character_)

      # Full-page loading overlay. The panel HTML below is built synchronously
      # here; the outputs inside those panels that opt out of suspendWhenHidden
      # pre-render on the following flushes, i.e. underneath this overlay.
      w <- Waiter$new(
        id = NULL,
        html = div(
          class = "spinner-custom",
          spin_flower(),
          div(
            tags$h5("Loading Database ..."),
            if (length(db_path) && !is.na(db_path)) {
              div(id = "db-load", basename(db_path))
            }
          )
        )
      )
      w$show()

      # Bring the scheme species up to the current spelling before anything
      # reads the database, then fill in any missing allele hashes, then sync
      # the metadata table - creating it and backfilling a row for every
      # isolate `mlst` already has (this is only a no-op past the first load;
      # a database opened before ever having a metadata table gets one here).
      #
      # This is the one explicit, deterministic sync call every module's read
      # can rely on instead of triggering its own: see
      # `sync_metadata_table()`'s docs for why performing it lazily, inside
      # whichever reactive happened to read first, was the actual bug behind
      # modules disagreeing about which isolates existed.
      migrate_species_name(db_path)
      hash_database(db_path)
      sync_metadata_table(db_path)

      # All three of the above write. Readers keyed on db_path() invalidate
      # anyway when it changes, but a reload of the *same* path (the landing
      # page can re-fire load for the database already open) leaves db_path()
      # untouched, so the revisions are what makes those reads re-run.
      bump_all(db_rev)

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
          "Add Isolates",
          value = "typing_panel",
          strip_shinyfiles_assets(typing$ui(ns("typing")))
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

      # Both items below carry fixed element ids, so any copy left over from an
      # earlier load has to go before another is inserted beside it.
      remove_navbar_items()

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
        target = "typing_panel",
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
        target = "typing_panel",
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

      # Tell the modules their UI is (back) in the page, so each can re-apply
      # the DOM state it drives from the server (see ui_mounted's definition).
      #
      # Deferred rather than bumped inline: an inline bump would run those
      # modules' observers inside this same flush, putting their shinyjs
      # messages in the same websocket batch as the panel HTML - and the client
      # dispatches "custom" messages *before* "shiny-insert-tab", so the toggles
      # would target elements that do not exist yet. A later() bump lands in the
      # next flush, hence a separate batch, which the client processes only
      # after this one's insertion has finished.
      later::later(function() {
        shiny::withReactiveDomain(
          session,
          ui_mounted(shiny::isolate(ui_mounted()) + 1L)
        )
      })

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

    # Increment data_reset so every subscribed module observer fires and
    # tears down its own reactive state (files, results, processes), without
    # touching Landing Page's load_trigger / db_path.
    reload_data <- function() {
      # Reclaim accumulated garbage and return it to the OS at the reset point,
      # before the teardown/rebuild cascade runs. The vector-heap cap above keeps
      # the session from ballooning; this full collection additionally hands the
      # freed pages back to the OS promptly (Linux malloc_trim) so a reload on a
      # memory-tight machine renders with headroom rather than in swap. Cheap on
      # the app's small live set (~130 MB); synchronous so it finishes before
      # data_reset() triggers the rebuild flush.
      # gc(full = TRUE) # [A/B TEST — TEMPORARY] disabled; re-enable to restore fix
      data_reset(data_reset() + 1L)
      # "Reload Database" means every query is suspect, including the ones whose
      # module opted out of automatic refresh (the Browser's edit grid) - the
      # user asking for a reload is the explicit consent that opt-out waits for.
      bump_all(db_rev)
    }

    # Full reset: everything reload_data() does, plus the true session_reset
    # signal that sends Landing Page/Scheme Browser back to their pre-load
    # state (see session_reset's definition above for why they're excluded
    # from data_reset).
    reset_session <- function() {
      reload_data()
      session_reset(session_reset() + 1L)
    }

    hide_db_notification <- function() {
      for (id in db_notification_ids) {
        removeNotification(id, session = session)
      }
      db_notification_ids <<- character(0)
    }

    # Reload the database: reset all module-internal state (so every module
    # re-queries the updated DB) without removing panels or returning to the
    # landing screen, then navigate directly to the Database Browser. Uses
    # reload_data() rather than reset_session() - db_path must stay intact so
    # Database Browser (and everything else) can actually re-query it.
    observeEvent(input$reload_db, {
      hide_db_notification()
      log_event(
        "DB",
        "Database reloaded",
        {
          p <- LANDING_PAGE_vals$db_path()
          if (length(p) && !is.na(p)) p else "(none)"
        }
      )

      # Step 1 (this flush): bring the Database Browser to the front - both the
      # top-level tab and, inside it, the Database module's own Browse Entries
      # sub-panel. Both matter: the reload can be triggered from anywhere, and
      # the overlay in Step 2 covers an element that lives inside Browse
      # Entries. Triggered from Typing that panel happens to be frontmost
      # already; triggered from the Import panel it is not, and covering a
      # display:none tab-pane paints nothing.
      #
      # Both selections are input messages, so they leave R together at the end
      # of this flush - i.e. before the deferred Step 2 sends the overlay - and
      # the client applies them in that order.
      nav_select(id = "tabs", selected = "database_panel")
      show_browse(show_browse() + 1L)

      # The overlay reuses the Database Browser's own waiter style. main.R's
      # ns("database-browse_entries-module-container") resolves to the same DOM
      # id as that module's internal ns("module-container") (Shiny joins
      # namespaces with "-"), so this targets the right element.
      reload_waiter <- Waiter$new(
        id = "app-database-browse_entries-module-container",
        html = div(
          class = "spinner-custom",
          spin_flower(),
          tags$h5("Reloading Database ...")
        )
      )

      # Read the reactive here (Step 1 runs in a reactive context); the later()
      # callback below does not, so it must work off this captured plain value.
      db_path <- LANDING_PAGE_vals$db_path()

      # Step 2 (deferred to a later tick, so the nav switch + overlay actually
      # paint before the potentially blocking work runs behind it): backfill
      # allele hashes for anything typed this session, sync the metadata table,
      # then reload every module. Isolates typed this session add rows to
      # `sequences` but not to the per-allele `hashes` table (only
      # hash_database() maintains it), so backfilling here keeps hash-dependent
      # reads (profile export, db import/export) correct without a full
      # landing-page reload; hash_database() is a cheap no-op when nothing new
      # was typed. sync_metadata_table() is the same kind of belt-and-suspenders
      # backfill: typing and import already call it themselves at the point
      # they change the isolate pool, so this is normally a no-op too - it is
      # here so that "Reload Database" stays a complete, no-questions-asked
      # recovery action even if some future write path forgets to.
      #
      # withReactiveDomain() restores the session so waiter/reactiveVal writes
      # resolve; isolate() supplies the reactive context reload_data() needs to
      # read/bump data_reset(). finally = guarantees the overlay clears even if
      # hashing errors.
      later::later(function() {
        shiny::withReactiveDomain(session, {
          shiny::isolate({
            reload_waiter$show()

            Sys.sleep(2)

            tryCatch(
              {
                if (
                  length(db_path) &&
                    !is.na(db_path) &&
                    hashes_pending(db_path)
                ) {
                  hash_database(db_path)
                }
                if (length(db_path) && !is.na(db_path)) {
                  sync_metadata_table(db_path)
                }
                reload_data()
              },
              finally = reload_waiter$hide()
            )
          })
        })
      })
    })

    observeEvent(input$reset, {
      log_event("APP", "Session reset")
      hide_db_notification()
      nav_show(id = "tabs", target = "landing_page_panel", select = TRUE)
      nav_show(id = "tabs", target = "scheme_browser_panel")

      for (panel in c(
        "database_panel",
        "analysis_dashboard_panel",
        "visualization_panel",
        "typing_panel"
      )) {
        nav_remove(id = "tabs", target = panel)
      }

      remove_navbar_items()

      reset_session()
    })
  })
}
