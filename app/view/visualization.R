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
    HTML,
    span,
    showModal,
    modalDialog,
    modalButton,
    removeModal,
    uiOutput,
    renderUI,
    textInput,
    updateTextInput,
    h4,
    p,
  ],
  bslib[
    navset_card_tab,
    nav_panel,
    nav_insert,
    nav_remove,
    nav_select,
    card,
    card_header,
    card_body,
    layout_columns,
    as_fill_carrier,
  ],
  shinyWidgets[radioGroupButtons],
)
box::use(
  app / logic / analysis_store,
  app / logic / custom_fields[append_custom],
  app / logic / database_functions[append_amr, append_classical_mlst],
  app / logic / db_events,
  app / logic / db_staging[list_imported_sets],
  app / logic / db_store,
  app / logic / field_profile[field_profiles],
  app / logic / field_types[field_types],
  app / view / visualization_plot,
  jsonlite[fromJSON],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Value of the permanent creator tab. Also the fallback insertion target, so
# the first plot tab lands directly after it.
.NEW_TAB <- "new_plot"

# One plot type's tile, used as a radio button's label. The preview sits in its
# own layer rather than on the button itself: the caption then keeps a solid
# background and stays legible without a scrim washing out the image. Preview
# URLs come from visualization_plot$plot_type_meta and are resolved against the
# app root by the browser, the same way app/main.R's logos are.
.type_tile <- function(m) {
  div(
    class = "viz-type-tile",
    div(
      class = "viz-type-tile-preview",
      style = sprintf("background-image: url('%s');", m$image)
    ),
    div(
      class = "viz-type-tile-caption",
      span(class = "viz-type-tile-icon", icon(m$icon)),
      span(class = "viz-type-tile-name", m$key),
      span(class = "viz-type-tile-tagline", m$tagline)
    )
  )
}

# The creator form. Emitted exactly once, in the permanent first tab — the
# whole point of keeping that tab rather than inserting a fresh copy of the
# form per "Add", which would put duplicate input ids in the page.
.creator_ui <- function(ns) {
  meta <- visualization_plot$plot_type_meta
  div(
    # html-fill-container/-item: the creator sits in a fillable, height="100%"
    # card_body (see the caller below), but a plain div doesn't join bslib's
    # fill system on its own. Tagging it both roles lets it claim the card's
    # full height *and* hand that height down to its own children — see
    # viz-type-picker below, and .viz-creator-cols' layout_columns() which
    # already carries html-fill-item but had no fill-container parent to act on.
    class = "viz-creator html-fill-container html-fill-item",
    div(
      # html-fill-item: without this the picker takes only its natural content
      # height and every byte of the card's leftover height goes to the
      # sibling form card instead — see .viz-type-tile-preview's CSS for the
      # rest of the chain this feeds.
      class = "viz-type-picker html-fill-item",
      radioGroupButtons(
        ns("plot_type"),
        label = NULL,
        # choiceNames/choiceValues rather than `choices`: the label of each
        # button is a whole tile, while the input's value stays the bare plot
        # type the rest of the module keys on.
        choiceNames = unname(lapply(meta, .type_tile)),
        choiceValues = names(meta),
        selected = "MST",
        justified = TRUE
      )
    ),
    layout_columns(
      class = "viz-creator-cols",
      col_widths = c(7, 5),
      uiOutput(ns("type_info")),
      card(
        class = "viz-creator-form",
        card_header(uiOutput(ns("create_header"), inline = TRUE)),
        card_body(
          div(
            # Plot names are display text, not identifiers: opt out of the
            # global charset filter in app/js/index.js so spaces and
            # punctuation survive typing.
            class = "allow-free-text",
            textInput(
              ns("plot_name"),
              "Plot name",
              value = "Plot 1",
              width = "100%"
            )
          ),
          uiOutput(ns("preset_info")),
          actionButton(
            ns("initiate"),
            "Initiate plot",
            icon = icon("plus"),
            class = "btn-primary btn-lg w-100"
          )
        )
      )
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
    # HTML() is not optional on any of the inline scripts below. htmltools
    # escapes a script body like ordinary text, so a bare `&&` reaches the
    # browser as `&amp;&amp;` and the whole block dies on a SyntaxError —
    # silently, since a script that fails to parse simply never runs.
    tags$script(
      HTML("(function(){
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
       })();")
    ),
    # html2canvas rasterises the Leaflet map, for both its thumbnail and its
    # image export. Vendored into app/static/js rather than pulled from a CDN:
    # export has to work on a machine that never reaches the network, and the
    # bundled copy inside visNetwork is 0.5.0-alpha1, which has no `scale`
    # option and so cannot render above screen resolution.
    tags$script(src = "static/js/html2canvas.min.js"),
    # Widget capture, serving both the dashboard's "Save Analysis" thumbnails
    # and the sidebar's image export. The engine sends
    # {selector, mode, inputId, targetPx, scale, format, background}; the
    # resulting data URI (or "" on any failure — tainted canvas, missing
    # library, a widget that has not drawn yet) comes back through
    # Shiny.setInputValue(inputId). Selector and input id are both namespaced
    # by the sending engine, so one handler serves every plot tab.
    #
    # Both raster modes redraw the widget at a multiple of its on-screen size
    # instead of copying the pixels already on screen, so text and strokes are
    # rasterised afresh at the higher resolution rather than scaled up:
    #
    #   "visnetwork" — vis.js derives its canvas backing store from
    #     canvas._determinePixelRatio(). Overriding that and re-running
    #     setSize() makes it redraw the entire network into a larger buffer, so
    #     a 4x export of a 960px widget is a true 3840px render.
    #   "html2canvas" — rasterises a DOM subtree at a multiple. Leaflet's
    #     overlays (markers, labels, legend, choropleth polygons) are DOM/SVG
    #     and are redrawn crisply; the basemap tiles are 256px rasters and can
    #     only be upscaled, which is why the Map also offers interactive HTML.
    #   "canvas" — the original 1:1 read, still used for cheap thumbnails.
    #
    # `targetPx` (preferred) names a physical export width in pixels rather
    # than a bare multiplier: effectiveScale() measures the widget's current
    # on-screen width and divides, so a target survives being asked for from a
    # maximized window or a half-width one and produces the same output either
    # way — a raw `scale` is what the multiplier meant before this existed, and
    # still works as a fallback for anything that sends one directly.
    tags$script(
      HTML("(function(){
         if(window.__phylotraceCaptureRegistered) return;
         window.__phylotraceCaptureRegistered = true;
         function send(id,val){Shiny.setInputValue(id,val,{priority:'event'});}

         function isClear(bg){
           return !bg || bg==='transparent' ||
                  /rgba\\(\\s*0\\s*,\\s*0\\s*,\\s*0\\s*,\\s*0\\s*\\)/.test(bg);
         }

         // A widget canvas holds only what was drawn on it, so the sidebar's
         // background colour has to be laid down underneath here. JPEG has no
         // alpha channel and would otherwise come back black.
         function encode(src,format,bg){
           var type=(format==='jpeg')?'image/jpeg':'image/png';
           if(isClear(bg)&&type==='image/png') return src.toDataURL(type);
           var out=document.createElement('canvas');
           out.width=src.width; out.height=src.height;
           var ctx=out.getContext('2d');
           ctx.fillStyle=isClear(bg)?'#ffffff':bg;
           ctx.fillRect(0,0,out.width,out.height);
           ctx.drawImage(src,0,0);
           var url=out.toDataURL(type,type==='image/jpeg'?0.98:undefined);
           out.width=out.height=0;
           return url;
         }

         function visNet(root){
           var host=root.classList.contains('visNetwork')?root
                    :root.querySelector('.visNetwork');
           var init=host&&host.htmlwidget_data_init_result;
           return (init&&init.network)?init.network:null;
         }

         // The multiplier to redraw at, worked out from the widget's actual
         // current on-screen width rather than trusted blind — a `scale`
         // meant something different on a maximized window than on a small
         // one, and `targetPx` is what fixes that. Clamped well past the
         // 1-6x range this app tests at, as a guard against a corrupted or
         // absurd request rather than an expected ceiling.
         function effectiveScale(currentPx, msg){
           var s = msg.targetPx ? (msg.targetPx / currentPx) : (msg.scale || 1);
           if (!isFinite(s) || s <= 0) s = 1;
           return Math.min(Math.max(s, 0.25), 10);
         }

         function grabVis(root,msg){
           var net=visNet(root);
           if(!net||!net.canvas) return null;
           var cv=net.canvas, orig=cv._determinePixelRatio, url=null;
           // Falls back to the widget's own clientData default (see
           // visualization_mst.R's canvas_px()) if the canvas has not laid
           // out yet — a hidden tab, most likely.
           var current=(cv.frame&&cv.frame.canvas&&cv.frame.canvas.clientWidth)||960;
           var scale=effectiveScale(current,msg);
           try{
             cv._determinePixelRatio=function(){return scale;};
             cv.setSize(); net.redraw();
             url=encode(cv.frame.canvas,msg.format,msg.background);
           } finally {
             // Always put the widget back at screen resolution, or the tab is
             // left holding a buffer several times larger than it needs.
             cv._determinePixelRatio=orig;
             cv.setSize(); net.redraw();
           }
           return url;
         }

         Shiny.addCustomMessageHandler('phylotrace_capture', function(msg){
           msg.format=msg.format||'png';
           try{
             var root=document.querySelector(msg.selector);
             if(!root){send(msg.inputId,'');return;}
             if(msg.mode==='visnetwork'){
               var u=grabVis(root,msg);
               send(msg.inputId,u||'');
             } else if(msg.mode==='canvas'){
               var cv=(root.tagName==='CANVAS')?root:root.querySelector('canvas');
               if(!cv){send(msg.inputId,'');return;}
               var url=''; try{url=cv.toDataURL('image/png');}catch(e){url='';}
               send(msg.inputId,url);
             } else if(msg.mode==='html2canvas'){
               if(typeof html2canvas!=='function'){send(msg.inputId,'');return;}
               var current=root.clientWidth||
                 (root.getBoundingClientRect&&root.getBoundingClientRect().width)||960;
               var scale=effectiveScale(current,msg);
               html2canvas(root,{useCORS:true,logging:false,scale:scale,
                                 backgroundColor:isClear(msg.background)
                                   ?'#ffffff':msg.background})
                 .then(function(canvas){
                   var url='';
                   try{url=encode(canvas,msg.format,msg.background);}catch(e){url='';}
                   send(msg.inputId,url);
                 }).catch(function(){send(msg.inputId,'');});
             } else {send(msg.inputId,'');}
           }catch(e){send(msg.inputId,'');}
         });
       })();")
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
  # Dashboard integration: `launch_ctx` fires when "Add Plot" was clicked for an
  # Analysis (carries its id); `open_ctx` fires when a saved plot was clicked to
  # reopen it (carries its plot id). `db_rev`'s `analyses` revision is what both
  # this module and the dashboard bump/observe to stay in sync.
  launch_ctx = shiny::reactive(NULL),
  open_ctx = shiny::reactive(NULL),
  db_rev = db_events$new_bus(),
  # Wired to whatever db_path/db_rev this call actually received - see
  # database.R's server() for why the default can't just be
  # `db_store$new_store()`.
  store = db_store$new_store(db_path = db_path, db_rev = db_rev),
  typing_active = shiny::reactive(FALSE)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------------------------------------------------- shared reads -----
    # Per-isolate metadata, computed once here and shared with every tab, so
    # the database is read at most once per invalidation no matter how many
    # plots are open. Local isolates only — a staged peer isolate has no place
    # on a map it was never geocoded for.
    viz_metadata <- reactive({
      # `custom_fields` and `amr` because append_custom()/append_amr() below
      # read those tables directly, not through the store. `isolates` and
      # `metadata` need no explicit dependency here: reading store$metadata()
      # reactively (below) already carries that dependency transitively - the
      # store invalidates on those two domains, and anything that reads it
      # reactively invalidates right along with it.
      db_events$depend(db_rev, "custom_fields", "amr")
      req(db_path())
      # Surface the classical-MLST columns (ST + per-locus alleles), the
      # AMR-screening columns (the resistance profile plus one per drug class)
      # and the user-defined custom variables as ordinary metadata so every
      # engine can label, colour and map by them. Display only — no appender
      # writes anything back to the database. Each records what it added in its
      # own attribute ("mlst_cols" / "amr_cols" / "custom_cols"), which
      # grouped_field_choices() turns into the optgroups the engines' field
      # pickers show; engines that pass no explicit sets get the same grouping
      # from the columns' name prefixes.
      append_custom(
        append_amr(
          append_classical_mlst(store$metadata(), db_path()),
          db_path()
        ),
        db_path()
      )
    })

    # What each of those columns *is* — its declared type, its distinct-value
    # count, its coverage and which group it belongs to — as one frame every
    # engine reads instead of working half of it out again.
    #
    # Computed here for the same reason viz_metadata is: field_types() reads the
    # custom-variable table, so doing this per engine would multiply one small
    # query by the number of open plots on every invalidation. The counts come
    # off the full metadata table, not a tab's isolate selection, so a variable
    # describes itself the same way in every tab.
    viz_field_profiles <- reactive({
      meta <- viz_metadata()
      field_profiles(
        meta,
        types = field_types(db_path(), names(meta)),
        mlst_cols = attr(meta, "mlst_cols"),
        amr_cols = attr(meta, "amr_cols"),
        custom_cols = attr(meta, "custom_cols")
      )
    })

    staged_sets <- reactive({
      db_events$depend(db_rev, "staged")
      req(db_path())
      list_imported_sets(db_path())
    })

    # Grouped Save-target choices: an ungrouped "None" entry (the plot isn't
    # part of any Analysis — the default) followed by one optgroup per
    # Analysis, each led by a "New plot" entry (value "analysis:<id>") and its
    # saved plots (value "plot:<id>"). One read here feeds every tab's picker.
    NONE_TARGET <- "none"
    picker_choices <- reactive({
      db_events$depend(db_rev, "analyses")
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
        AMR = "shield-virus",
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
        viz_field_profiles = viz_field_profiles,
        staged_sets = staged_sets,
        picker_choices = picker_choices,
        db_rev = db_rev,
        typing_active = typing_active,
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
    # "Create <Plot Type>" — keeps the form header in sync with the picker
    # without the header needing its own copy of the type-name lookup.
    output$create_header <- renderUI({
      m <- visualization_plot$plot_type_meta[[input$plot_type %||% "MST"]]
      req(m)
      sprintf("Create %s", m$title)
    })

    # What the currently picked plot type draws, and what it needs to draw it.
    # Copy and preview images come from the engine registry
    # (visualization_plot$plot_type_meta), so this reacts to the picker without
    # knowing anything about the engines itself.
    output$type_info <- renderUI({
      m <- visualization_plot$plot_type_meta[[input$plot_type %||% "MST"]]
      req(m)
      # The two distance engines are the ones that can fold staged peer
      # isolates in and that consume the missing-value handling; the other
      # three read metadata straight off the local database.
      badges <- if (isTRUE(m$distance)) {
        list(
          c("diagram-project", "Pairwise allelic distances"),
          c("layer-group", "Staged peer isolates can be included")
        )
      } else {
        list(
          c("database", "Reads isolate metadata directly"),
          c("house", "Local isolates only")
        )
      }

      card(
        class = "viz-type-info",
        card_header(
          span(class = "viz-type-info-icon", icon(m$icon)),
          span(class = "viz-type-info-title", m$title)
        ),
        card_body(
          p(class = "viz-type-info-about", m$about),
          div(
            class = "viz-type-badges",
            lapply(badges, function(b) {
              span(class = "viz-type-badge", icon(b[[1]]), span(b[[2]]))
            })
          ),
          tags$ul(
            class = "viz-type-tech",
            lapply(m$technical, function(t) tags$li(t))
          )
        )
      )
    })

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
      showModal(div(
        class = "unsaved-plot-modal",
        modalDialog(
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
          easyClose = TRUE
        )
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
    #
    # `remove_ui = FALSE` makes this a contract, not a preference: whatever is
    # wired to `session_reset` MUST already have removed this module's panel.
    # Raise it without doing so and every tab becomes a destroyed server behind
    # nav markup that is still on screen — visibly a plot tab, but blank and
    # inert, with even its close button dead. app/main.R's `data_reset` is the
    # signal that looks right and is not: "Reload Database" raises it and leaves
    # the panels standing. A reload needs no teardown here at all; the plots
    # belong to the same database, and db_rev refreshes what they read.
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
