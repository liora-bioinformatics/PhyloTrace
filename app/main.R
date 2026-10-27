box::use(
  shiny[
    stopApp,
    moduleServer,
    NS,
    observe,
    observeEvent,
    invalidateLater,
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
    selectInput,
    uiOutput,
    renderUI,
    HTML
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
    nav_hide,
    as_fill_carrier,
    toggle_dark_mode,
    tooltip
  ],
  shinyjs[runjs],
  htmltools[tagQuery],
  waiter[useWaiter, waiterShowOnLoad, Waiter, spin_flower],
  htmltools[attachDependencies, findDependencies],
)
box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / database_functions[migrate_species_name, sync_metadata_table],
  app / logic / db_compat[check_db_loadable],
  app / logic / db_events[bump_all, new_bus],
  app / logic / db_guard[db_failed, guard_db, report_db_failure],
  app / logic / db_store[new_store],
  app / logic / functions[render_info],
  app / logic / logging[log_event, start_session_log],
  app / logic / paths[stat_json, app_local_share_path],
  app / logic / pymlst[hash_database, hashes_pending],
  app /
    logic /
    session_restart[
      begin_session,
      end_session,
      restart_session,
      theme_restore_script
    ],
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

# The modal a restarted session opens with after a database failed to load.
load_failed_modal <- function(title, reason) {
  div(
    class = "start-modal",
    modalDialog(
      title = tagList(icon("triangle-exclamation"), " ", title),
      tags$p(reason),
      tags$p(
        class = "text-muted mb-0",
        "Select another database or create a new one to continue."
      ),
      easyClose = TRUE,
      footer = modalButton("OK")
    )
  )
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
    tags$script(HTML(theme_restore_script())),
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
      # Empty (renders nothing) whenever no typing run is active - see
      # output$typing_indicator. Always present in the navbar so it is
      # recognisable from any tab, not just Add Isolates.
      nav_item(uiOutput(ns("typing_indicator"), inline = TRUE)),
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

    # Closing the window stops the app; a restart (see return_to_start()) hands
    # over to the session the reloaded page opens instead.
    restart_notice <- begin_session()
    session$onSessionEnded(function() end_session())
    if (!is.null(restart_notice)) {
      showModal(restart_notice)
    }

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

    # Bumped by "Reload Database" (input$reload_db): the same database stays
    # open and its panels stay mounted, but every module drops its transient
    # state and re-queries. Passed to the modules as their `session_reset`
    # argument. Returning to the start screen does not use it - that restarts
    # the session (see return_to_start()).
    #
    # Deliberately NOT given to Landing Page/Scheme Browser - if it were,
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

    SCHEME_BROWSER_vals <- scheme_browser$server("scheme_browser")

    scheme_browser_db <- reactive({
      SCHEME_BROWSER_vals$load_db()
      isolate(SCHEME_BROWSER_vals$db_location())
    })

    LANDING_PAGE_vals <- landing_page$server(
      "landing_page",
      external_db = scheme_browser_db
    )

    # TRUE once the loaded database file has been found missing (see the watcher
    # further down). Cleared when a database loads successfully.
    db_gone <- reactiveVal(FALSE)

    # The database path every module actually reads - deliberately not Landing
    # Page's raw one.
    #
    # When the file disappears mid-session the session is restarted, but until
    # the page has reloaded the old modules are still live, and any read they
    # run would still carry the dead path: connect() then *recreates* it as an
    # empty file and the query dies with "no such table: mlst", inside a
    # reactive, which ends the session before the restart can take over.
    #
    # Gating here makes it deterministic instead: db_gone is a dependency of
    # this reactive, so the instant it flips, every reader in the same flush
    # sees NULL - the same state they all start the session in and already
    # handle - and no query is issued at all.
    db_path <- reactive({
      if (isTRUE(db_gone())) NULL else LANDING_PAGE_vals$db_path()
    })

    # The shared metadata cache (app/logic/db_store.R): one reactive read of
    # the `metadata` table, passed to every module that displays it, so the
    # question "does the whole app agree on the isolate table" has exactly one
    # answer rather than one per consumer.
    store <- new_store(db_path = db_path, db_rev = db_rev)

    TYPING_vals <- typing$server(
      "typing",
      db_path = db_path,
      session_reset = data_reset,
      db_rev = db_rev
    )

    # Navbar spinner: the one indicator of a typing run visible from every
    # tab, not just Add Isolates. Empty (renders nothing) the rest of the time.
    output$typing_indicator <- renderUI({
      if (isTRUE(TYPING_vals$typing_active())) {
        tags$span(
          icon("circle-notch", class = "fa-spin"),
          title = "Typing in progress",
          class = "navbar-typing-indicator"
        )
      }
    })

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
      db_path = db_path,
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
      db_path = db_path,
      session_reset = data_reset,
      db_rev = db_rev,
      store = store
    )
    # No data_reset here, the one module without it: "Reload Database" leaves
    # every panel in place, and tearing the plot tabs down behind nav markup
    # that stayed on screen left blank tabs that could not be used or closed.
    # Everything Visualization would re-read on a reload it re-reads through
    # db_rev, which reload_data() bumps.
    VISUALIZATION_vals <- visualization$server(
      "visualization",
      db_path = db_path,
      # Dashboard -> Visualization: "Add Plot" for an Analysis and "open a saved
      # plot" both route here and preselect / restore in the Save panel.
      launch_ctx = ANALYSIS_DASHBOARD_vals$request_add_plot,
      open_ctx = ANALYSIS_DASHBOARD_vals$request_open_plot,
      db_rev = db_rev,
      store = store
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
    # Called before inserting, because the two nav_insert() calls are the only
    # writers of the `app-reset` element id and nothing else keeps count of
    # them. Sweeping first makes the insertion
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

    # Builds the loading-overlay content for one stage of a database load,
    # keeping the database name under the title so it stays visible while the
    # title itself changes to narrate progress.
    loading_stage_html <- function(stage, db_path) {
      div(
        class = "spinner-custom",
        spin_flower(),
        div(
          tags$h5(stage),
          if (length(db_path) && !is.na(db_path)) {
            div(id = "db-load", basename(db_path))
          }
        )
      )
    }

    # Shared between the two observers below: created and shown by the first
    # (as soon as a load is requested, before db_path() has even changed) and
    # updated/hidden by the second (once the database and what to do with it
    # are known). A plain variable rather than a reactiveVal - both observers
    # already run inside a reactive context of their own, and this only needs
    # to survive between them, not itself trigger anything.
    db_load_waiter <- NULL

    # Fires the instant a load is requested (button click or external db),
    # strictly before LANDING_PAGE_vals$load_database() below - that reactive
    # only changes one tick later (see landing_page's fire_load()), once
    # db_path() has cascaded into every downstream module's eager
    # (suspendWhenHidden = FALSE) reads. Some of those are slow on a large
    # database, so showing the overlay off this earlier signal - rather than
    # from inside the observer below - means it is already on screen before
    # that cascade starts, instead of the UI sitting merely unresponsive
    # (cursor: wait, nothing painted) until the whole thing finishes.
    observeEvent(
      LANDING_PAGE_vals$loading_started(),
      {
        db_load_waiter <<- Waiter$new(
          id = NULL,
          html = loading_stage_html("Loading Database ...", NULL)
        )
        db_load_waiter$show()
      },
      ignoreInit = TRUE
    )

    observeEvent(LANDING_PAGE_vals$load_database(), {
      db_path <- LANDING_PAGE_vals$db_path()

      # Open a fresh session log for this database load. Its header names the
      # database, so every line written afterwards (DB writes, typing, ...) can
      # omit it. Must run first, so the DB writes triggered further down this
      # handler land in the new file.
      start_session_log(if (length(db_path)) db_path else NA_character_)

      # Guard against a database that vanished or was replaced between selection
      # and load (deleted or moved on disk, drive unmounted). Without this,
      # SQLite silently creates an empty file at the missing path and every
      # downstream read ("no such table: mlst" / "sequences") aborts the
      # session. Abort the load, tell the user, and stay on the start screen.
      db_check <- check_db_loadable(db_path)
      if (!isTRUE(db_check$ok)) {
        log_event("DB", "Load aborted", db_check$reason)
        # Restart rather than stay: the modules have already started reading
        # the path, and the next load should not open next to what they kept.
        return_to_start(load_failed_modal(
          "Database unavailable",
          db_check$reason
        ))
        return()
      }

      # The path is good, so whatever the last database did is history.
      db_gone(FALSE)
      db_gone_handled(FALSE)

      # The overlay is already showing (from the loading_started observer
      # above) - just point it at the actual file now that db_path is known.
      w <- db_load_waiter
      w$update(html = loading_stage_html("Loading Database ...", db_path))

      # Step 2 (deferred to a later tick): db_path() changing in this same
      # flush also wakes every other module's eager (suspendWhenHidden =
      # FALSE) reads - some slow on a large database - so running the work
      # below inline here would just add to that same block of synchronous
      # work. Deferring it keeps this observer's share of it off the current
      # flush; either way it now runs behind the overlay, already visible
      # from the loading_started observer above. withReactiveDomain()
      # restores the session so waiter/reactiveVal writes resolve; isolate()
      # supplies the reactive context bump_all()/ui_mounted() need.
      later::later(function() {
        shiny::withReactiveDomain(session, shiny::isolate({
          # Bring the scheme species up to the current spelling before
          # anything reads the database, then fill in any missing allele
          # hashes, then sync the metadata table - creating it and
          # backfilling a row for every isolate `mlst` already has (this is
          # only a no-op past the first load; a database opened before ever
          # having a metadata table gets one here).
          #
          # This is the one explicit, deterministic sync call every module's
          # read can rely on instead of triggering its own: see
          # `sync_metadata_table()`'s docs for why performing it lazily,
          # inside whichever reactive happened to read first, was the actual
          # bug behind modules disagreeing about which isolates existed.
          #
          # The overlay title is updated ahead of each step so a long wait
          # reads as visible progress instead of one static message.
          #
          # All three write, so a lock held by another writer (another
          # PhyloTrace session on the same file, or a typing run's pyMLST
          # subprocess) past BUSY_TIMEOUT_MS fails them. The load is then
          # abandoned and the session sent back to the start screen, where
          # loading can simply be retried.
          synced <- guard_db("Loading the database", {
            w$update(html = loading_stage_html(
              "Checking scheme compatibility ...",
              db_path
            ))
            migrate_species_name(db_path)

            w$update(html = loading_stage_html(
              "Computing allele hashes ...",
              db_path
            ))
            hash_database(db_path)

            w$update(html = loading_stage_html(
              "Synchronizing isolate metadata ...",
              db_path
            ))
            sync_metadata_table(db_path)
          })
          if (db_failed(synced)) {
            return_to_start(load_failed_modal(
              "Loading the database failed",
              synced$reason
            ))
            return()
          }

          w$update(html = loading_stage_html("Preparing workspace ...", db_path))

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

          # Both items below carry fixed element ids, so any copy left over from
          # an earlier load has to go before another is inserted beside it.
          remove_navbar_items()

          nav_insert(
            id = "tabs",
            nav = nav_item(
              tooltip(
                actionButton(
                  inputId = ns("reset"),
                  label = NULL,
                  icon = icon("arrow-rotate-left"),
                  `aria-label` = "Close database"
                ),
                "Close the database and return to the start screen",
                placement = "bottom"
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

          # The overlay has already been visible for the duration of the work
          # above, so hold it for one short extra buffer after the panels are
          # ready - long enough that "Preparing workspace ..." is legible
          # rather than swapped out the instant it appears. Hidden
          # asynchronously via later() so this and the panel insertion above
          # land in separate flushes and both are actually painted.
          later::later(
            function() shiny::withReactiveDomain(session, w$hide()),
            delay = 1
          )
        }))
      })
    })

    # Increment data_reset so every subscribed module observer fires and
    # tears down its own reactive state (files, results, processes), without
    # touching Landing Page's load_trigger / db_path.
    reload_data <- function() {
      data_reset(data_reset() + 1L)
      # "Reload Database" means every query is suspect, including the ones whose
      # module opted out of automatic refresh (the Browser's edit grid) - the
      # user asking for a reload is the explicit consent that opt-out waits for.
      bump_all(db_rev)
    }

    hide_db_notification <- function() {
      for (id in db_notification_ids) {
        removeNotification(id, session = session)
      }
      db_notification_ids <<- character(0)
    }

    # Watch the loaded database file for the whole session. If it disappears
    # (deleted, moved, renamed, drive unmounted) every subsequent query would
    # crash the session, so detect it within a few seconds, explain what
    # happened, and fall back to the start screen cleanly.
    #
    # Reads Landing Page's raw path rather than the gated db_path(): the gate is
    # what this observer closes, so watching through it would mean losing sight
    # of the file the moment it is reported missing. `db_gone_handled` keeps the
    # poll from re-firing the modal every tick; it clears when a database loads.
    #
    # ASSUMPTION: file.exists() returns promptly. It costs ~2 microseconds on a
    # local disk, which is why polling it every 5s is free. On a *hung* network
    # mount (NFS/SMB) the same call can block indefinitely, and R is
    # single-threaded, so it would take the whole UI with it.
    #
    # Deliberately not defended against here, for two reasons. First, this
    # observer would not be what breaks: every database read opens the same
    # file and does far more with it, so an unreachable mount freezes the
    # session at the next query regardless - fixing it properly means making
    # all ~60 connect() sites timeout-aware, not hardening one poll. Second,
    # PhyloTrace databases are not supposed to live there at all: SQLite needs
    # file locking that network filesystems do not implement reliably, and a
    # typing run has two writers (this session and the pyMLST process), so such
    # a database risks corruption long before anyone notices a stall. The
    # Scheme Browser's "Initiate New Database" card says so at the point the
    # location is chosen.
    db_gone_handled <- reactiveVal(FALSE)
    observe({
      loaded <- LANDING_PAGE_vals$db_path()
      if (is.null(loaded) || is.na(loaded) || !nzchar(loaded)) {
        return()
      }

      invalidateLater(5000)

      if (file.exists(loaded) || isTRUE(db_gone_handled())) {
        return()
      }
      db_gone_handled(TRUE)
      log_event("DB", "Database file vanished", loaded)

      # Close the gate before restarting: until the page has reloaded the
      # modules are still live, and with db_gone set any read they run sees
      # NULL instead of a path whose connect() would recreate it as an empty
      # file and then die on "no such table: mlst" inside a reactive.
      db_gone(TRUE)
      return_to_start(div(
        class = "start-modal",
        modalDialog(
          title = tagList(
            icon("triangle-exclamation"),
            " Database connection lost"
          ),
          tags$p("The loaded database file is no longer accessible:"),
          tags$pre(loaded),
          tags$p(
            "It may have been deleted, moved, or on a disconnected drive. ",
            "PhyloTrace has returned to the start screen to avoid errors — ",
            "re-select the database once it is available again."
          ),
          easyClose = FALSE,
          footer = modalButton("OK")
        )
      ))
    })

    # Reload the database: reset all module-internal state (so every module
    # re-queries the updated DB) without removing panels or returning to the
    # landing screen, then navigate directly to the Database Browser. Uses
    # reload_data() rather than return_to_start() - db_path must stay intact so
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
      # Through the gate, so a reload attempted after the file vanished carries
      # NULL rather than a path whose every use would recreate it empty.
      path <- db_path()

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
      # the backfill errors.
      #
      # error = matters as much: this runs from later(), outside any observer
      # Shiny would isolate the failure to, so an escaping error takes the
      # session down. The database can be gone by now (deleted between the click
      # and this tick, before the watcher's next poll), which makes connect()
      # refuse - a reload that reports it cannot re-read the database is a far
      # better outcome than a dead session.
      later::later(function() {
        shiny::withReactiveDomain(session, {
          shiny::isolate({
            reload_waiter$show()

            Sys.sleep(2)

            tryCatch(
              {
                if (
                  length(path) &&
                    !is.na(path) &&
                    hashes_pending(path)
                ) {
                  hash_database(path)
                }
                if (length(path) && !is.na(path)) {
                  sync_metadata_table(path)
                }
                reload_data()
              },
              error = function(e) {
                report_db_failure("Reloading the database", e)
              },
              finally = reload_waiter$hide()
            )
          })
        })
      })
    })

    # Return to the start screen by restarting the session: the page reloads
    # into a new session that has never seen this database, with every module,
    # output and cache in its initial state (app/logic/session_restart.R says
    # why a reset signal could not get there). `notice` is a modal for the new
    # session, explaining a return the user did not ask for.
    return_to_start <- function(notice = NULL) {
      restart_session(notice)
    }

    # What closing the database would discard, as one line per item - empty
    # when nothing unsaved would be lost.
    unsaved_work <- function() {
      n_plots <- VISUALIZATION_vals$unsaved_plots()
      c(
        if (isTRUE(DATABASE_vals$pending())) {
          "Unsaved edits in Browse Entries"
        },
        if (isTRUE(DATABASE_vals$custom_pending())) {
          "Unsaved edits in Custom Variables"
        },
        if (n_plots > 0L) {
          sprintf(
            "%d plot%s not saved to an Analysis",
            n_plots,
            if (n_plots == 1L) "" else "s"
          )
        },
        if (isTRUE(TYPING_vals$typing_active())) {
          "A typing run in progress, which will be cancelled"
        }
      )
    }

    # Ask before closing the database: everything not written to it is gone
    # once the session restarts, with no undo.
    observeEvent(input$reset, {
      lost <- unsaved_work()
      path <- LANDING_PAGE_vals$db_path()
      showModal(div(
        class = "start-modal",
        modalDialog(
          title = "Close database",
          tags$p(
            "Close ",
            tags$strong(if (length(path) && !is.na(path)) basename(path)),
            " and return to the start screen?"
          ),
          tags$p(
            class = "text-muted small",
            "PhyloTrace starts over: open plots, filters and selections are ",
            "discarded. Everything saved to the database is kept."
          ),
          if (length(lost)) {
            div(
              class = "alert alert-warning p-2 mb-0 small",
              icon("triangle-exclamation"),
              tags$strong(" This will be lost:"),
              tags$ul(class = "mb-0", lapply(lost, tags$li))
            )
          },
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("conf_reset"),
              "Close database",
              class = if (length(lost)) "btn-danger"
            )
          )
        )
      ))
    })

    observeEvent(input$conf_reset, {
      removeModal()
      log_event("APP", "Session reset")
      return_to_start()
    })
  })
}
