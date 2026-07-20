# app/view/visualization.R
#
# Visualization coordinator. Hosts a navset of plot tabs: a permanent
# "New plot" tab carrying the creator form, plus one tab per live plot. Each
# plot tab is an app/view/visualization_plot.R instance that owns its own setup
# sidebar and its own engine submodule server, so several plots — including
# several of the same type — run independently, with independent controls,
# selections and Save targets.
#
# What stays here is only what is genuinely shared: the cached per-isolate
# metadata read, the staged import sets, the Save picker's grouped choices, the
# page-level scripts that must be registered exactly once, and the tab registry
# itself (create, close, teardown, and the dashboard's Add Plot / open-saved-plot
# hand-offs).

box::use(
  shiny[
    NS,
    moduleServer,
    isolate,
    observeEvent,
    reactive,
    reactiveVal,
    req,
    div,
    icon,
    actionButton,
    showNotification,
    tags,
    tagList,
    span,
    showModal,
    modalDialog,
    modalButton,
    removeModal,
    uiOutput,
    renderUI,
    textInput,
    updateTextInput,
  ],
  bslib[
    navset_card_tab,
    nav_panel,
    nav_insert,
    nav_remove,
    nav_select,
    card_body,
    as_fill_carrier,
  ],
  shinyWidgets[radioGroupButtons],
)
box::use(
  app / logic / database_functions[make_metadata_table, append_classical_mlst],
  app / logic / db_staging[list_imported_sets],
  app / logic / analysis_store,
  app / view / visualization_plot,
  jsonlite[fromJSON],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Value of the permanent creator tab. Also the fallback insertion target, so
# the first plot tab lands directly after it.
.NEW_TAB <- "new_plot"

# The creator form. Emitted exactly once, in the permanent first tab — the
# whole point of keeping that tab rather than inserting a fresh copy of the
# form per "Add", which would put duplicate input ids in the page.
.creator_ui <- function(ns) {
  div(
    class = "viz-creator p-3",
    radioGroupButtons(
      ns("plot_type"),
      label = "Plot type",
      choices = visualization_plot$plot_types,
      selected = "MST",
      justified = TRUE
    ),
    textInput(
      ns("plot_name"),
      "Plot name",
      value = "Plot 1",
      width = "100%"
    ),
    uiOutput(ns("preset_info")),
    actionButton(
      ns("initiate"),
      "Initiate",
      icon = icon("plus"),
      class = "btn-primary"
    )
  )
}

# Page-level scripts. These register document-wide handlers and must be emitted
# once for the whole module, not once per plot tab — hence the one-shot window
# flags: app/main.R re-emits this UI on every database load, and the tabs
# themselves mount and unmount freely underneath it.
.global_scripts <- function() {
  tagList(
    # The coordinator drives shinyjs itself (the deferred save on tab close),
    # so it must not rely on a plot tab having brought it in.
    shinyjs::useShinyjs(),
    # MutationObserver: as soon as a .viz-nav-wrap mounts in the DOM (every
    # engine's control panel is inserted with its tab, long after page load),
    # copy each icon-only tab's text content to its title attribute so the
    # browser shows a native hover tooltip.
    tags$script(
      "(function(){
         if(window.__phylotraceTabLabelerRegistered) return;
         window.__phylotraceTabLabelerRegistered = true;
         function labelTabs(wrap){
           wrap.querySelectorAll('.nav-link').forEach(function(a){
             if(!a.title) a.title=a.textContent.trim();
           });
         }
         var mo=new MutationObserver(function(muts){
           muts.forEach(function(m){
             m.addedNodes.forEach(function(n){
               if(n.nodeType!==1)return;
               if(n.classList&&n.classList.contains('viz-nav-wrap')){labelTabs(n);}
               else if(n.querySelector){var w=n.querySelector('.viz-nav-wrap');if(w)labelTabs(w);}
             });
           });
         });
         mo.observe(document.body,{childList:true,subtree:true});
       })();"
    ),
    # html2canvas powers the Map (Leaflet) thumbnail capture below. Loaded from
    # a CDN — the app already reaches the network at runtime (Nominatim
    # geocoding), and if it is unavailable the capture handler falls back to a
    # placeholder. For a fully offline build, vendor html2canvas.min.js into
    # app/static/js and point this <script> at it instead.
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"
    ),
    # Plot-thumbnail capture for the dashboard "Save Analysis" feature. The
    # engine sends {selector, mode, inputId}: "canvas" reads a widget's own
    # <canvas> (MST/visNetwork) via toDataURL; "html2canvas" rasterises a DOM
    # subtree (the Leaflet map) if the html2canvas library is present. Either
    # way the PNG data URI (or "" on any failure — tainted canvas, missing lib)
    # is returned through Shiny.setInputValue(inputId), which the engine's
    # thumb_data() reactive observes to finish the save with a placeholder.
    # The selector and the input id are both namespaced by the sending engine,
    # so one handler serves every plot tab.
    tags$script(
      "(function(){
         if(window.__phylotraceCaptureRegistered) return;
         window.__phylotraceCaptureRegistered = true;
         function send(id,val){Shiny.setInputValue(id,val,{priority:'event'});}
         Shiny.addCustomMessageHandler('phylotrace_capture', function(msg){
           try{
             var root=document.querySelector(msg.selector);
             if(!root){send(msg.inputId,'');return;}
             if(msg.mode==='canvas'){
               var cv=(root.tagName==='CANVAS')?root:root.querySelector('canvas');
               if(!cv){send(msg.inputId,'');return;}
               var url=''; try{url=cv.toDataURL('image/png');}catch(e){url='';}
               send(msg.inputId,url);
             } else if(msg.mode==='html2canvas'){
               if(typeof html2canvas!=='function'){send(msg.inputId,'');return;}
               html2canvas(root,{useCORS:true,logging:false,backgroundColor:'#ffffff'})
                 .then(function(canvas){
                   var url=''; try{url=canvas.toDataURL('image/png');}catch(e){url='';}
                   send(msg.inputId,url);
                 }).catch(function(){send(msg.inputId,'');});
             } else {send(msg.inputId,'');}
           }catch(e){send(msg.inputId,'');}
         });
       })();"
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    .global_scripts(),
    as_fill_carrier(
      navset_card_tab(
        id = ns("plot_set"),
        height = "100%",
        wrapper = function(...) card_body(..., padding = 0, fillable = TRUE),
        nav_panel(
          title = div(
            icon("plus"),
            span("New", style = "margin-left: 0.5rem;")
          ),
          value = .NEW_TAB,
          .creator_ui(ns)
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L),
  # Dashboard integration: `launch_ctx` fires when "Add Plot" was clicked for an
  # Analysis (carries its id); `open_ctx` fires when a saved plot was clicked to
  # reopen it (carries its plot id). `plots_changed` is the shared tick both
  # this module and the dashboard bump/observe to stay in sync.
  launch_ctx = shiny::reactive(NULL),
  open_ctx = shiny::reactive(NULL),
  plots_changed = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------------------------------------------------- shared reads -----
    # Per-isolate metadata, computed once here and shared with every tab, so
    # the database is read at most once per invalidation no matter how many
    # plots are open. Local isolates only — a staged peer isolate has no place
    # on a map it was never geocoded for.
    viz_metadata <- reactive({
      req(db_path())
      # Surface the classical-MLST columns (ST + per-locus alleles) as ordinary
      # metadata so every engine can label, colour and map by them. Display
      # only — append_classical_mlst never writes them back to the database.
      append_classical_mlst(make_metadata_table(db_path()), db_path())
    })

    staged_sets <- reactive({
      req(db_path())
      list_imported_sets(db_path())
    })

    # Grouped Save-target choices: an ungrouped "None" entry (the plot isn't
    # part of any Analysis — the default) followed by one optgroup per
    # Analysis, each led by a "New plot" entry (value "analysis:<id>") and its
    # saved plots (value "plot:<id>"). One read here feeds every tab's picker.
    NONE_TARGET <- "none"
    picker_choices <- reactive({
      plots_changed()
      path <- db_path()
      none_entry <- list("None (not part of an Analysis)" = NONE_TARGET)
      req(path)
      df_a <- analysis_store$list_analyses(path)
      if (!nrow(df_a)) {
        return(none_entry)
      }
      groups <- lapply(seq_len(nrow(df_a)), function(i) {
        aid <- df_a$id[i]
        ch <- c("+ New plot" = paste0("analysis:", aid))
        pl <- analysis_store$list_plots(path, aid)
        if (nrow(pl)) {
          ch <- c(ch, stats::setNames(paste0("plot:", pl$id), pl$name))
        }
        ch
      })
      c(none_entry, stats::setNames(groups, df_a$name))
    })

    # ------------------------------------------------------- tab registry --
    # Ids are minted monotonically and never reused, not even after a close or
    # a session reset: a closed tab's module server stays resident (Shiny has
    # no way to unregister a namespace), so recycling an id would collide with
    # its leftover inputs and outputs.
    tab_seq <- reactiveVal(0L)
    # Named list, insertion-ordered: tid -> list(alive, handle, plot_type).
    tabs <- reactiveVal(list())
    # Applied to the next tab created from the creator form; set by the
    # dashboard's "Add Plot".
    pending_preset <- reactiveVal(NULL)
    # Tab awaiting the outcome of an unsaved-close confirmation.
    closing_tab <- reactiveVal(NULL)

    # A small type glyph, prepended to the tab label so the plot type is
    # readable at a glance in the nav strip. Keyed by the fixed plot-type value
    # (see visualization_plot$plot_types); an unknown type simply gets no icon.
    .type_icon <- function(plot_type) {
      nm <- switch(
        plot_type,
        MST = "circle-nodes",
        Tree = "sitemap",
        Map = "earth-europe",
        Epi = "chart-column",
        NULL
      )
      if (is.null(nm)) NULL else span(class = "viz-tab-type-icon", icon(nm))
    }

    # Tab label: a plot-type glyph, the plot's name, and a close affordance. The
    # "x" sits inside the nav link, so its click has to be stopped from also
    # selecting the tab.
    .tab_title <- function(tid, name, plot_type) {
      span(
        class = "viz-tab-label",
        .type_icon(plot_type),
        span(class = "viz-tab-name", name),
        tags$span(
          class = "viz-tab-close",
          title = "Close plot",
          # priority 'event' matters: closing tab_3, opening a new one and
          # closing it again would otherwise send an identical value and not
          # re-fire.
          onclick = sprintf(
            paste0(
              "event.stopPropagation();event.preventDefault();",
              "Shiny.setInputValue('%s','%s',{priority:'event'});"
            ),
            ns("close_tab"),
            tid
          ),
          icon("xmark")
        )
      )
    }

    create_tab <- function(plot_type, name, preset = NULL) {
      n <- isolate(tab_seq()) + 1L
      tab_seq(n)
      tid <- sprintf("tab_%d", n)

      reg <- isolate(tabs())
      # Insert after the last still-open tab so tabs stay in creation order.
      target <- if (length(reg)) utils::tail(names(reg), 1L) else .NEW_TAB

      nav_insert(
        id = "plot_set",
        nav = nav_panel(
          title = .tab_title(tid, name, plot_type),
          value = tid,
          # nav_insert bypasses the navset's `wrapper`, so the inserted pane
          # never gets the fillable card_body the creator tab does — mirror it
          # here (padding 0 for the tab's full-bleed layout) so the plot fills
          # the card. The pane itself is stretched via CSS (see main.scss).
          card_body(
            visualization_plot$ui(ns(tid), plot_type = plot_type),
            padding = 0,
            fillable = TRUE
          )
        ),
        target = target,
        position = "after",
        select = TRUE
      )

      alive <- reactiveVal(TRUE)
      handle <- visualization_plot$server(
        id = tid,
        plot_type = plot_type,
        name = name,
        db_path = db_path,
        session_reset = session_reset,
        viz_metadata = viz_metadata,
        staged_sets = staged_sets,
        picker_choices = picker_choices,
        plots_changed = plots_changed,
        is_active = reactive(identical(input$plot_set, tid)),
        alive = alive,
        preset = preset
      )

      reg[[tid]] <- list(alive = alive, handle = handle, plot_type = plot_type)
      tabs(reg)
      tid
    }

    # Tear a tab down as completely as Shiny allows. `remove_ui` is FALSE only
    # on session reset, where app/main.R has already removed the whole
    # Visualization panel and there is no nav left to remove from.
    close_tab <- function(tid, remove_ui = TRUE, collect = TRUE) {
      reg <- isolate(tabs())
      entry <- reg[[tid]]
      if (is.null(entry)) {
        return(invisible(NULL))
      }

      # Order matters. Flipping `alive` first invalidates every reactive the
      # tab feeds its engine, which both stops the engine's own observers
      # (they abort on req()) and drops their cached values; `destroy()` then
      # takes down the observers this codebase does own.
      entry$alive(FALSE)
      try(entry$handle$destroy(), silent = TRUE)

      reg[[tid]] <- NULL
      tabs(reg)

      if (remove_ui) {
        was_active <- identical(isolate(input$plot_set), tid)
        nav_remove(id = "plot_set", target = tid)
        # Removing the selected pane leaves the navset showing nothing — but
        # only then: closing a background tab must not pull the user off the
        # plot they are looking at.
        if (was_active) {
          nav_select(
            "plot_set",
            selected = if (length(reg)) {
              utils::tail(names(reg), 1L)
            } else {
              .NEW_TAB
            }
          )
        }
      }

      # Without this R keeps the freed pages and the reclaim is invisible.
      # `collect` is FALSE when closing tabs in a loop, so a session reset pays
      # for one collection rather than one per open plot.
      if (collect) {
        gc()
      }
      invisible(NULL)
    }

    find_tab_by_plot <- function(pid) {
      reg <- isolate(tabs())
      for (tid in names(reg)) {
        cur <- reg[[tid]]$handle$plot_id()
        if (!is.null(cur) && identical(as.integer(cur), as.integer(pid))) {
          return(tid)
        }
      }
      NULL
    }

    # ---------------------------------------------------- creator form -----
    # What the next created tab will be bound to, when the dashboard sent the
    # user here from an Analysis.
    output$preset_info <- renderUI({
      p <- pending_preset()
      if (is.null(p) || is.null(p$analysis_name)) {
        return(NULL)
      }
      div(
        class = "small text-info mb-2",
        icon("link"),
        sprintf(" Will be saved into '%s'", p$analysis_name)
      )
    })

    observeEvent(input$initiate, {
      pt <- input$plot_type
      req(pt)
      # Named after how many plots are open, not after the never-resetting tab
      # counter, so the suggestion stays "Plot 3" rather than drifting to
      # "Plot 47" over a long session of opening and closing tabs.
      next_name <- function(offset) {
        sprintf("Plot %d", length(isolate(tabs())) + offset)
      }
      name <- trimws(input$plot_name %||% "")
      if (!nzchar(name)) {
        name <- next_name(1L)
      }

      preset <- isolate(pending_preset())
      pending_preset(NULL)
      create_tab(pt, name, preset = preset)

      # Ready the form for the next plot.
      updateTextInput(session, "plot_name", value = next_name(1L))
    })

    # ------------------------------------------------------- closing -------
    observeEvent(input$close_tab, {
      tid <- input$close_tab
      entry <- isolate(tabs())[[tid]]
      req(entry)

      if (!isTRUE(entry$handle$dirty())) {
        return(close_tab(tid))
      }

      closing_tab(tid)
      showModal(modalDialog(
        title = "Unsaved plot",
        sprintf(
          paste(
            "'%s' has been generated but not saved to an Analysis.",
            "Closing it discards the plot."
          ),
          entry$handle$name()
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("close_discard"),
            "Close anyway",
            class = "btn-danger"
          ),
          actionButton(
            ns("close_save"),
            "Save and close",
            class = "btn-primary"
          )
        ),
        easyClose = FALSE
      ))
    })

    observeEvent(input$close_discard, {
      removeModal()
      tid <- isolate(closing_tab())
      closing_tab(NULL)
      req(tid)
      close_tab(tid)
    })

    observeEvent(input$close_save, {
      removeModal()
      tid <- isolate(closing_tab())
      closing_tab(NULL)
      req(tid)
      entry <- isolate(tabs())[[tid]]
      req(entry)
      # Close only once the save has actually completed: the MST and Map
      # thumbnails are rasterised in the browser, so removing the tab's DOM
      # first would store an empty preview.
      #
      # Deferred past the modal we just dismissed: saving over an existing plot
      # raises its own confirmation, and showing a modal while the previous one
      # is still animating out strands the backdrop.
      shinyjs::delay(300, {
        entry$handle$save_now(function(ok) {
          if (isTRUE(ok)) {
            close_tab(tid)
          }
        })
      })
    })

    # --------------------------------------------- dashboard hand-offs -----
    # "Add Plot" for an Analysis: land on the creator form with the Analysis
    # pre-bound, so the user picks a type and a name and the resulting tab is
    # already targeted at it.
    observeEvent(
      launch_ctx(),
      {
        ctx <- launch_ctx()
        req(ctx)
        row <- analysis_store$get_analysis(db_path(), ctx$analysis_id)
        pending_preset(list(
          save_target = paste0("analysis:", ctx$analysis_id),
          analysis_name = if (is.null(row)) NULL else row$name,
          # A restricting Analysis owns the isolate set, so the new plot starts
          # against it rather than against everything.
          selection = if (is.null(row)) {
            NULL
          } else {
            visualization_plot$parse_static(row$isolate_selection)
          }
        ))
        nav_select("plot_set", selected = .NEW_TAB)
      },
      ignoreInit = TRUE
    )

    # Reopen a saved plot: select the tab if that plot is already on screen,
    # otherwise create one restored from its snapshot.
    observeEvent(
      open_ctx(),
      {
        ctx <- open_ctx()
        req(ctx)
        row <- analysis_store$get_plot(db_path(), ctx$plot_id)
        if (is.null(row)) {
          return()
        }

        existing <- find_tab_by_plot(ctx$plot_id)
        if (!is.null(existing)) {
          nav_select("plot_set", selected = existing)
          return()
        }

        snap <- tryCatch(
          fromJSON(row$inputs_json, simplifyVector = TRUE),
          error = function(e) NULL
        )
        if (is.null(snap)) {
          return()
        }

        # A static Analysis selection overrides the plot's own snapshot, so
        # every plot in the Analysis stays on the same isolate set.
        arow <- analysis_store$get_analysis(db_path(), row$analysis_id)
        astatic <- if (is.null(arow)) {
          NULL
        } else {
          visualization_plot$parse_static(arow$isolate_selection)
        }
        effective <- astatic %||% snap$selection

        # The Analysis's selection may have been edited after this plot was
        # saved, in which case it is about to be regenerated against a
        # different isolate set than it was built from — say so, rather than
        # leaving the user to wonder why it no longer matches its thumbnail.
        if (analysis_store$selection_differs(snap$selection, effective)) {
          showNotification(
            paste(
              "This Analysis's isolate selection changed after this plot was",
              "saved. It is being rebuilt with the Analysis's current",
              "isolates, so it may differ from its saved preview."
            ),
            type = "warning",
            duration = 12
          )
        }
        snap$selection <- effective

        create_tab(
          snap$plot_type %||% "MST",
          row$name,
          preset = list(
            save_target = paste0("plot:", ctx$plot_id),
            plot_id = ctx$plot_id,
            snapshot = snap
          )
        )
      },
      ignoreInit = TRUE
    )

    # On session reset every plot belongs to the previous database. app/main.R
    # removes the whole Visualization panel and re-inserts a fresh one, so
    # there is no nav to remove from — but the tab servers are still resident
    # here and have to be taken down explicitly. `tab_seq` deliberately keeps
    # counting, so ids from the old session are never handed out again.
    observeEvent(
      session_reset(),
      {
        for (tid in names(isolate(tabs()))) {
          close_tab(tid, remove_ui = FALSE, collect = FALSE)
        }
        tabs(list())
        gc()
        pending_preset(NULL)
        closing_tab(NULL)
      },
      ignoreInit = TRUE
    )

    invisible(NULL)
  })
}
