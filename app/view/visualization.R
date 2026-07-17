# app/view/visualization.R
#
# Visualization coordinator. Hosts the shared "setup" sidebar (plot type,
# Generate, options, reset) and swaps between the self-contained plot-engine
# submodules — app/view/visualization_mst.R, app/view/visualization_tree.R and
# app/view/visualization_map.R — inside a navset_hidden. Each engine owns its own control
# panel and plot area; this module only forwards the shared reactives (db_path,
# session_reset, the per-isolate metadata, na_handling, the Generate tick and
# the selected plot type) down to all three engines and toggles which panel is
# visible. The map engine geocodes the metadata's spatial fields on Generate.

box::use(
  shiny[
    NS,
    moduleServer,
    observeEvent,
    reactive,
    reactiveVal,
    req,
    div,
    icon,
    actionButton,
    selectInput,
    tags,
    tagList,
    span,
    showModal,
    modalDialog,
    modalButton,
    removeModal,
    uiOutput,
    renderUI,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    accordion,
    accordion_panel,
    navset_hidden,
    nav_panel,
    tooltip,
    input_switch,
    as_fill_carrier,
  ],
  shinyWidgets[
    radioGroupButtons,
    prettyRadioButtons,
    pickerInput,
    pickerOptions
  ],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows],
)
box::use(
  app / logic / database_functions[make_metadata_table, append_classical_mlst],
  app / logic / db_staging[list_imported_sets, imported_metadata_wide],
  app / view / visualization_mst,
  app / view / visualization_tree,
  app / view / visualization_map,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  # Left = plot setup; the engine submodules (right controls + plot area) live
  # in the hidden tabset swapped by `plot_type`.
  layout_sidebar(
    fillable = TRUE,
    border_radius = FALSE,
    class = "p-0",
    sidebar = sidebar(
      id = ns("sidebar"),
      title = NULL,
      width = 320,
      div(
        class = "plot-controls",
        radioGroupButtons(
          ns("plot_type"),
          label = "Plot type",
          choices = c("MST", "Tree", "Map"),
          selected = "MST",
          justified = TRUE
        ),
        actionButton(
          ns("generate"),
          "Generate Plot",
          icon = icon("play"),
          width = "100%"
        )
      ),
      accordion(
        id = ns("setup_accordion"),
        # Initial state matches the default plot type (MST): "Options" shows the
        # missing-value control so it's open, while "Info" is just the "Not
        # available" placeholder (no geocoding) so it starts collapsed. The
        # plot_type observer flips Options/Info as the engine changes — for Map
        # it's the opposite (Options has no controls, Info gets geocode status
        # after Generate).
        open = c("Selection", "Options"),
        # Isolate preselection (present for all three plot types, like Options
        # and Info). The button opens a modal with the metadata table where the
        # user picks which isolates feed the plots; the info line summarises the
        # current selection. See the selection_button / sel_* handlers in the
        # server for the modal, and viz_metadata_selected for how the choice is
        # threaded into every engine.
        accordion_panel(
          "Selection",
          icon = icon("list-check"),
          actionButton(
            ns("selection_button"),
            "Choose isolates",
            icon = icon("list-check")
          ),
          uiOutput(ns("selection_info"))
        ),
        accordion_panel(
          "Options",
          icon = icon("gear"),
          # Missing-value handling only makes sense for the pairwise-distance
          # computations behind MST/Tree; Map doesn't use it. Hidden for Map
          # via shinyjs (visible by default since MST is the initial
          # plot_type), same toggle pattern as algo_wrap below.
          div(
            id = ns("na_handling_wrap"),
            selectInput(
              ns("na_handling"),
              span(
                "Missing values ",
                span(
                  class = "tooltip-bttn",
                  actionButton(
                    ns("na_handling_info"),
                    label = NULL,
                    icon = icon("circle-info")
                  )
                )
              ),
              choices = c(
                "Ignore for pairwise comparison" = "ignore_na",
                "Omit loci with missing values" = "omit",
                "Treat missing as allele variant" = "category"
              )
            )
          ),
          # Typing results imported from a peer (Database > Import). They carry
          # allele identity but no sequences, so they can join a distance
          # computation but nothing else — hidden for Map, like na_handling.
          div(
            id = ns("imported_wrap"),
            uiOutput(ns("imported_picker_ui"))
          ),
          # Algorithm is a Tree-only *computation* input (feeds
          # compute_phylo_tree, applied on Generate — like the missing-value
          # handling above). Hidden for MST/Map via shinyjs; forwarded to the
          # Tree submodule as a reactive.
          shinyjs::hidden(
            div(
              id = ns("algo_wrap"),
              prettyRadioButtons(
                ns("algo"),
                "Algorithm",
                choices = c("Neighbour-Joining", "UPGMA")
              )
            )
          ),
          # Tree-only *display* option: switches the plot area between "fit"
          # (whole tree scaled to the panel) and "zoom" (full rendered size,
          # scrollable). Purely presentational — forwarded to the Tree submodule
          # as a reactive and applied there as a CSS class, no re-render. Hidden
          # for MST/Map like algo_wrap above.
          shinyjs::hidden(
            div(
              id = ns("zoom_view_wrap"),
              input_switch(ns("zoom_view"), "Zoom view", FALSE)
            )
          ),
          # Shown only for Map, where both option wraps above are hidden and the
          # panel body would otherwise be empty. Hidden by default (MST is the
          # initial plot_type); toggled alongside the wraps in the plot_type
          # observer below.
          shinyjs::hidden(
            div(
              id = ns("options_empty"),
              class = "text-muted small",
              "Not available"
            )
          )
        ),
        # Always present, same for all three plot types, so the sidebar
        # layout is identical regardless of which is selected) — currently
        # only ever gets content for Map: how many isolates' locations
        # Nominatim could resolve on the last Generate. MST/Tree keep the
        # "Info" panel with an empty body — see output$map_geocode_status,
        # which returns NULL whenever Map isn't the active engine.
        accordion_panel(
          "Info",
          icon = icon("circle-info"),
          uiOutput(ns("map_geocode_status"))
        )
      )
    ),
    shinyjs::useShinyjs(),
    # One panel per engine (values match the plot_type choices verbatim). Each
    # panel hosts a submodule's full inner layout_sidebar under its namespace;
    # the shared Generate button id is passed through so the engine's loading
    # overlay can hook it.
    navset_hidden(
      id = ns("engine"),
      as_fill_carrier(nav_panel(
        title = "MST",
        value = "MST",
        visualization_mst$ui(ns("mst"), generate_id = ns("generate"))
      )),
      as_fill_carrier(nav_panel(
        title = "Tree",
        value = "Tree",
        visualization_tree$ui(ns("tree"), generate_id = ns("generate"))
      )),
      as_fill_carrier(nav_panel(
        title = "Map",
        value = "Map",
        visualization_map$ui(ns("map"), generate_id = ns("generate"))
      ))
    ),
    # MutationObserver: as soon as a .viz-nav-wrap mounts in the DOM (the panels
    # are inserted with the visualization tab after the DB loads), copy each
    # icon-only tab's text content to its title attribute so the browser shows a
    # native hover tooltip. One observer covers both engines' control panels.
    tags$script(
      "(function(){
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
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  typing_status = shiny::reactive("idle"),
  db_updated = shiny::reactiveVal(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Per-isolate metadata (cached until the database changes); computed once
    # here and shared with both engines' labels, mappings, and select choices so
    # the database is read at most once per invalidation.
    # Local isolates only. This is what the Map and the Dashboard see — a staged
    # peer isolate has no place on a map it was never geocoded for.
    viz_metadata <- reactive({
      req(db_path())
      # Surface the classical-MLST columns (ST + per-locus alleles) as ordinary
      # metadata so every engine can label, colour and map by them. Display
      # only - append_classical_mlst never writes them back to the database.
      append_classical_mlst(make_metadata_table(db_path()), db_path())
    })

    staged_sets <- reactive({
      req(db_path())
      list_imported_sets(db_path())
    })

    output$imported_picker_ui <- renderUI({
      sets <- staged_sets()
      if (!nrow(sets)) {
        return(NULL)
      }
      pickerInput(
        ns("imported_sets"),
        span(
          "Imported typing results ",
          span(
            class = "tooltip-bttn",
            actionButton(
              ns("imported_info"),
              label = NULL,
              icon = icon("circle-info")
            )
          )
        ),
        choices = stats::setNames(sets$set_id, sets$name),
        selected = character(0),
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "None",
          selectedTextFormat = "count > 1",
          countSelectedText = "{0} set(s)"
        )
      )
    })

    observeEvent(input$imported_info, {
      showModal(modalDialog(
        title = "Imported typing results",
        tags$p(
          "Profile tables imported from a peer (Database › Import). They",
          " carry allele identity but no DNA sequences."
        ),
        tags$p(
          "Selecting a set folds its isolates into the distance matrix behind",
          " the Tree and the MST, so they can be compared against your own.",
          " They do not appear on the Map, which needs geocoded metadata."
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    imported_sets <- reactive({
      s <- input$imported_sets
      if (!length(s)) NULL else as.integer(s)
    })

    # Local + staged, with a `source` column naming the origin. Drives the
    # isolate selection modal and the Tree/MST engines.
    viz_metadata_all <- reactive({
      local <- viz_metadata()
      req(local)
      local$source <- "local"

      ext <- imported_metadata_wide(db_path(), imported_sets())
      if (is.null(ext) || !nrow(ext)) {
        return(local)
      }

      # Union of columns, so a peer field the local table lacks (and vice versa)
      # simply comes through as NA rather than dropping the rows.
      for (col in setdiff(names(local), names(ext))) ext[[col]] <- NA_character_
      for (col in setdiff(names(ext), names(local))) local[[col]] <- NA_character_

      rbind(local, ext[, names(local), drop = FALSE])
    })

    # Isolate preselection. NULL = no filter (all isolates); otherwise a
    # character vector of the isolate names the user confirmed in the selection
    # modal. Kept in a reactiveVal so it survives plot-type switches and
    # re-Generate; cleared only on a new database, app reset, or a new
    # confirmed selection.
    selected_isolates <- reactiveVal(NULL)

    # A new database means the previous isolate names may not exist any more, so
    # drop the selection back to "all". ignoreInit so the initial load starts in
    # the (already default) "all" state without an extra invalidation.
    observeEvent(viz_metadata(), selected_isolates(NULL), ignoreInit = TRUE)

    # Metadata forwarded to the engines, restricted to the current selection.
    # Two flavours: the Map plots straight from the metadata and must see only
    # local, geocodable isolates; the Tree and the MST also label and colour the
    # staged peer isolates they compute distances for.
    .subset_meta <- function(meta, sel) {
      if (is.null(meta) || is.null(sel)) {
        return(meta)
      }
      meta[meta$isolate %in% sel, , drop = FALSE]
    }

    viz_metadata_selected <- reactive({
      .subset_meta(viz_metadata(), selected_isolates())
    })

    viz_metadata_selected_all <- reactive({
      .subset_meta(viz_metadata_all(), selected_isolates())
    })

    # Selection modal: the full metadata table with per-column filters and
    # multi-row selection, pre-checked to the current selection. Confirm reads
    # the selected rows back as isolate names.
    observeEvent(input$selection_button, {
      meta <- viz_metadata_all()
      req(meta)
      showModal(div(
        class = "selection-modal",
        modalDialog(
          title = NULL,
          div(
            class = "selection-modal-toolbar",
            actionButton(
              ns("sel_all"),
              "Select all",
              icon = icon("check-double")
            ),
            actionButton(
              ns("sel_none"),
              "Select none",
              icon = icon("xmark")
            ),
            # Live count, pushed to the right of the toolbar row (see the
            # .selection-modal-count SCSS rule).
            uiOutput(ns("sel_count"), class = "selection-modal-count")
          ),
          DTOutput(ns("sel_table"), fill = FALSE),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("sel_confirm"),
              "Confirm selection",
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        )
      ))
    })

    output$sel_table <- renderDT(
      {
        meta <- viz_metadata_all()
        req(meta)
        datatable(
          meta,
          rownames = FALSE,
          filter = "top",
          # Drop the default "display" class's zebra striping (keep borders /
          # hover / sortable) so the cells aren't tinted per row.
          class = "row-border hover order-column",
          # Open with nothing selected ("0 of N"); confirming an empty
          # selection is treated as "all" (see the sel_confirm handler).
          selection = list(mode = "multiple", selected = NULL),
          # Bounded to the modal: paginate (dom "p") so the table can't grow
          # past a page, and scroll both axes within that page (scrollY caps
          # the body height, scrollX handles the wide metadata columns) the
          # same way the browse-entries table scrolls.
          options = list(
            # table / info / pagination (no page-length selector).
            dom = "tip",
            pageLength = 10,
            scrollX = TRUE,
            scrollY = "42vh",
            scrollCollapse = TRUE
          )
        )
      },
      server = FALSE
    )

    # Live selected-isolate count shown in the modal toolbar; reacts to the
    # DT selection input so it updates as rows are (de)selected, before Confirm.
    output$sel_count <- renderUI({
      meta <- viz_metadata_all()
      req(meta)
      n <- length(input$sel_table_rows_selected)
      span(sprintf("%d of %d selected", n, nrow(meta)))
    })

    # Select all / none act on the currently *filtered* rows (rows_all reflects
    # the active column filters), so "Select all" after filtering only checks
    # the visible subset.
    sel_proxy <- dataTableProxy("sel_table")
    observeEvent(
      input$sel_all,
      selectRows(sel_proxy, input$sel_table_rows_all)
    )
    observeEvent(input$sel_none, selectRows(sel_proxy, NULL))

    observeEvent(input$sel_confirm, {
      meta <- viz_metadata_all()
      req(meta)
      rows <- input$sel_table_rows_selected
      # An empty confirmation means "no filter" — fall back to all isolates
      # (NULL), so the plots use the full set rather than nothing.
      selected_isolates(if (length(rows)) meta$isolate[rows] else NULL)
      removeModal()
    })

    # Short summary shown under the button in the "Selection" accordion.
    output$selection_info <- renderUI({
      meta <- viz_metadata_all()
      if (is.null(meta)) {
        return(div(class = "text-muted small mt-2", "No database loaded"))
      }
      total <- nrow(meta)
      sel <- selected_isolates()
      if (is.null(sel)) {
        div(
          class = "small mt-2 text-muted",
          sprintf("All %d isolates selected", total)
        )
      } else {
        div(
          class = "small mt-2",
          sprintf("%d of %d isolates selected", length(sel), total)
        )
      }
    })

    # "Missing values" help: a modal rather than a tooltip since the
    # explanation is long-form (one paragraph per option). The "info-modal"
    # class is generic so any future large tooltip can reuse it.
    observeEvent(input$na_handling_info, {
      showModal(div(
        class = "info-modal",
        modalDialog(
          title = "Missing values",
          tags$dl(
            tags$dt("Ignore for pairwise comparison"),
            tags$dd(
              "Excludes a locus only from the specific pairs where it's",
              "missing, using all other loci for those comparisons."
            ),
            tags$dt("Omit loci with missing values"),
            tags$dd(
              "Drops any locus that's missing in at least one isolate from",
              "the whole analysis, so all isolates are compared over the",
              "same reduced set of loci."
            ),
            tags$dt("Treat missing as allele variant"),
            tags$dd(
              "Keeps every locus and treats a missing call as its own",
              "allele state, so it counts as a difference against any",
              "called allele."
            )
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      ))
    })

    # Shared reactives forwarded to both engine submodules. Each engine now
    # owns its own "Reset settings" button/logic locally (see mst_controls(),
    # tree_controls() and map_plot.R's map_controls()) rather than being
    # driven from a single shared button here.
    shared <- list(
      db_path = db_path,
      session_reset = session_reset,
      selected_isolates = selected_isolates,
      na_handling = reactive(input$na_handling),
      generate = reactive(input$generate),
      plot_type = reactive(input$plot_type)
    )

    # The distance engines additionally see the staged peer isolates; the Map
    # does not, because it plots straight from metadata it has geocoded.
    distance_shared <- c(
      shared,
      list(
        viz_metadata = viz_metadata_selected_all,
        imported_sets = imported_sets
      )
    )

    do.call(visualization_mst$server, c(list("mst"), distance_shared))
    do.call(
      visualization_tree$server,
      c(
        list("tree"),
        distance_shared,
        list(
          algo = reactive(input$algo),
          zoom_view = reactive(input$zoom_view)
        )
      )
    )
    map_ret <- do.call(
      visualization_map$server,
      c(list("map"), shared, list(viz_metadata = viz_metadata_selected))
    )

    # Swap the visible engine panel when the plot type changes, and show the
    # Tree-only algorithm picker / MST+Tree-only missing-value handling only
    # while relevant to the selected engine.
    observeEvent(input$plot_type, {
      bslib::nav_select("engine", selected = input$plot_type)
      shinyjs::toggle(
        "algo_wrap",
        condition = identical(input$plot_type, "Tree")
      )
      shinyjs::toggle(
        "zoom_view_wrap",
        condition = identical(input$plot_type, "Tree")
      )
      shinyjs::toggle(
        "na_handling_wrap",
        condition = !identical(input$plot_type, "Map")
      )
      # Staged peer isolates only contribute allele identity, so they are
      # offered to the distance engines and hidden for the Map.
      shinyjs::toggle(
        "imported_wrap",
        condition = !identical(input$plot_type, "Map")
      )
      shinyjs::toggle(
        "options_empty",
        condition = identical(input$plot_type, "Map")
      )
      # Open the panel that actually holds content for this engine and collapse
      # the one that only shows the "Not available" placeholder: MST/Tree have
      # Options controls but no Info, whereas Map has no Options but populates
      # Info with the geocode status after Generate.
      if (identical(input$plot_type, "Map")) {
        bslib::accordion_panel_open("setup_accordion", "Info")
        bslib::accordion_panel_close("setup_accordion", "Options")
      } else {
        bslib::accordion_panel_open("setup_accordion", "Options")
        bslib::accordion_panel_close("setup_accordion", "Info")
      }
    })

    # Geocoding feedback for the "Info" accordion: how many isolates the last
    # Generate was able to place on the map, with a warning + a few example
    # place names when some couldn't be resolved. NULL (nothing rendered,
    # leaving the "Info" panel body empty) unless Map is the active engine
    # and a Generate has produced a status — this keeps MST/Tree's "Info"
    # panel genuinely empty rather than showing stale Map feedback after
    # switching plot types.
    output$map_geocode_status <- renderUI({
      # Empty-state placeholder so the "Info" panel body is never blank: shown
      # for MST/Tree (no geocoding) and for Map until a Generate produces a
      # status.
      not_available <- div(class = "text-muted small", "Not available")
      if (!identical(input$plot_type, "Map")) {
        return(not_available)
      }
      s <- map_ret$geocode_status()
      if (is.null(s) || s$n_isolates == 0) {
        return(not_available)
      }
      # Distinct places resolved (each backs one or more of the mapped isolates)
      # — shown under the isolate summary so it's clear how many geocode lookups
      # the mapped isolates collapsed to, plus how many of those came from the
      # persistent cross-session cache vs. were freshly fetched this Generate
      # (see build_map_coords()'s status / geocode_places_cached()). The cache
      # breakdown is only appended when there's actually something cached to
      # report, so a first-ever run stays a plain count.
      cache_note <- if (s$n_cached > 0 && s$n_new > 0) {
        sprintf(" (%d cached, %d newly fetched)", s$n_cached, s$n_new)
      } else if (s$n_cached > 0) {
        " (all from cache)"
      } else {
        ""
      }
      locations_line <- div(
        class = "small text-muted",
        sprintf(
          "%d location%s geocoded%s",
          s$n_locations,
          if (s$n_locations == 1) "" else "s",
          cache_note
        )
      )
      if (s$n_failed_places == 0) {
        tagList(
          div(
            class = "mt-2 small text-success",
            icon("circle-check"),
            sprintf(" All %d isolates mapped", s$n_mapped)
          ),
          locations_line
        )
      } else {
        tagList(
          tooltip(
            div(
              class = "mt-2 small text-warning",
              icon("triangle-exclamation"),
              sprintf(" %d of %d isolates mapped", s$n_mapped, s$n_isolates)
            ),
            paste("Could not geocode:", s$failed_preview)
          ),
          locations_line
        )
      }
    })

    # Collapse the setup sidebar to give the freshly generated plot full width.
    # `toggle_sidebar` sends an input message the module session namespaces
    # itself, so pass the bare (un-namespaced) id here.
    # Temporarily disabled — keep the sidebar open while testing Generate.
    # observeEvent(input$generate, {
    #   bslib::toggle_sidebar(id = "sidebar", open = FALSE, session = session)
    # })

    # On session reset, return the engine selector to the default and clear the
    # isolate preselection back to "all".
    observeEvent(
      session_reset(),
      {
        bslib::nav_select("engine", selected = "MST")
        selected_isolates(NULL)
      },
      ignoreInit = TRUE
    )
  })
}
