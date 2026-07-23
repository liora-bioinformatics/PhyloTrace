# app/view/visualization_plot.R
#
# One plot tab. Everything that used to be the visualization module's single
# shared workspace — the setup sidebar (Generate, isolate Selection, the
# distance Options, Save Analysis) plus one plot engine — now lives here, once
# per tab, so several plots (including several of the same type) run side by
# side with independent controls and independent servers.
#
# The plot type is fixed for the tab's lifetime: it is chosen on the
# coordinator's "New plot" form and decides both which controls the sidebar
# emits and which of the four engine submodules is instantiated. That is why it
# is a plain character scalar rather than a reactive — nothing downstream may
# assume it can change, and the Map engine in particular used to infer "my panel
# just became visible" from a plot-type switch (see the `visible` argument).
#
# The coordinator owns everything shared: the cached metadata read, the staged
# import sets, the Save picker's choices, and the global capture/labelling
# scripts. This module receives them as reactives and never touches the database
# for them itself.

box::use(
  shiny[
    NS,
    moduleServer,
    isolate,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    req,
    div,
    icon,
    actionButton,
    selectInput,
    updateSelectInput,
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
    outputOptions,
  ],
  bslib[
    sidebar,
    layout_sidebar,
    accordion,
    accordion_panel,
    accordion_panel_open,
    input_switch,
    as_fill_carrier,
  ],
  shinyWidgets[
    prettyRadioButtons,
    updatePrettyRadioButtons,
    pickerInput,
    updatePickerInput,
    pickerOptions,
  ],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows],
)
box::use(
  app / logic / db_staging[imported_metadata_wide],
  app / logic / analysis_store,
  app / view / visualization_mst,
  app / view / visualization_tree,
  app / view / visualization_map,
  app / view / visualization_epi,
  app / view / visualization_amr,
  jsonlite[toJSON, fromJSON],
  base64enc[base64encode],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# The engines, keyed by the plot-type values the creator form offers.
# `distance` marks the two engines built on a pairwise-distance computation:
# they are the ones that consume the missing-value handling and can fold staged
# peer isolates into their input, so they get the wider metadata bundle and the
# Options panel. The Map geocodes metadata, the Epi curve bins collection dates
# and the AMR views read the screening tables, so none of the three has any use
# for either.
.ENGINES <- list(
  MST = list(mod = visualization_mst, distance = TRUE),
  Tree = list(mod = visualization_tree, distance = TRUE),
  Map = list(mod = visualization_map, distance = FALSE),
  Epi = list(mod = visualization_epi, distance = FALSE),
  AMR = list(mod = visualization_amr, distance = FALSE)
)

#' @export
plot_types <- names(.ENGINES)

# What the creator's plot-type picker shows for each engine: the preview image
# that backs its tile, a one-line tagline, the prose description and the
# technical notes shown once a type is selected. It lives here, beside
# .ENGINES, so the copy cannot drift away from the engines it describes —
# `distance` is read from .ENGINES below rather than restated. Preview images
# are resolved by the browser against the app root (Rhino serves app/static at
# /static), exactly like the logos in app/main.R.
.TYPE_INFO <- list(
  MST = list(
    title = "Minimum-Spanning Tree",
    icon = "circle-nodes",
    tagline = "Allelic distances as an interactive network",
    image = "static/images/tiles/tile_mst.png",
    about = paste(
      "The minimum-spanning tree over the pairwise allelic distance matrix:",
      "every isolate is a node, every edge is labelled with the number of",
      "differing loci. Nodes can be dragged, zoomed and clicked to select",
      "isolates, and single-linkage clusters below a chosen threshold are",
      "shaded behind the network."
    ),
    technical = c(
      "visNetwork (vis.js) over a distance matrix built in app/logic/phylo.R",
      "Nodes can be drawn as pie charts over any categorical metadata field",
      "Missing loci follow the missing-value handling set in the Options panel",
      "Export as a self-contained interactive HTML file or as a canvas PNG"
    )
  ),
  Tree = list(
    title = "Phylogenetic Tree",
    icon = "sitemap",
    tagline = "Neighbour-Joining or UPGMA dendrogram",
    image = "static/images/tiles/tile_tree.png",
    about = paste(
      "A dendrogram inferred from the same allelic distance matrix, using",
      "either Neighbour-Joining or UPGMA. Tips carry metadata as coloured",
      "symbols and rings, so host, country and collection date can be read",
      "off the tree alongside its topology."
    ),
    technical = c(
      "ggtree/ape; algorithm chosen per plot in the engine's control panel",
      "Rectangular, slanted and circular layouts with adjustable tip labels",
      "Metadata mapped onto tip shape, tip colour and surrounding heat rings",
      "Export via ggsave: PNG, JPEG, PDF or SVG at a chosen size and DPI"
    )
  ),
  Map = list(
    title = "Geographic Map",
    icon = "earth-europe",
    tagline = "Isolates placed on an interactive world map",
    image = "static/images/tiles/tile_map.png",
    about = paste(
      "Plots isolates at the places their metadata names. City, state and",
      "country fields are geocoded once per distinct location when you press",
      "Generate, then drawn in one of four modes: markers, a country",
      "choropleth, a density heatmap, or a mini-chart per location."
    ),
    technical = c(
      "leaflet; coordinates from OSM/Nominatim, cached per distinct place",
      "Reads geo_loc_name_city, _state_province and _country from the metadata",
      "Choropleth shading uses Natural Earth country polygons",
      "Export as an interactive HTML map; timeline playback over collection date"
    )
  ),
  Epi = list(
    title = "Epidemiological Curve",
    icon = "chart-column",
    tagline = "Case counts binned over collection date",
    image = "static/images/tiles/tile_epi.png",
    about = paste(
      "The classic epi curve: collection dates binned into equal intervals,",
      "one bar per interval, optionally stacked or faceted by a metadata",
      "field. A moving average can be laid over the bars, and playback walks",
      "the curve forward one interval at a time."
    ),
    technical = c(
      "ggplot2; day/week/month/year bins with integer-only count axes",
      "Bars are exactly one interval wide, so gaps are real gaps in the data",
      "Optional moving average, cumulative view and per-group faceting",
      "Export via ggsave: PNG, JPEG, PDF or SVG"
    )
  ),
  AMR = list(
    title = "Resistance Profile",
    icon = "shield-virus",
    tagline = "Screening results across isolates and genes",
    image = "static/images/tiles/tile_amr.png",
    about = paste(
      "Views over the antimicrobial-resistance screening stored with your",
      "isolates: a presence/absence heatmap of isolates against genes, a",
      "coarser isolates-against-drug-classes grid keeping abritamr's",
      "confident/partial distinction, or a ranked prevalence bar chart."
    ),
    technical = c(
      "ggplot2 plots built in app/logic/amr_plot.R from the amr_results tables",
      "Screening comes from abritamr/NCBI AMRFinderPlus, run alongside typing",
      "Genes group by element type or drug class, or cluster hierarchically",
      "Only isolates screened in this database appear; export via ggsave"
    )
  )
)

#' Plot-type presentation metadata for the creator form, keyed by plot type and
#' carrying the engine's own `distance` flag so the picker can state what each
#' view needs without a second copy of that fact.
#' @export
plot_type_meta <- stats::setNames(
  lapply(plot_types, function(k) {
    c(.TYPE_INFO[[k]], list(key = k, distance = .ENGINES[[k]]$distance))
  }),
  plot_types
)

# Restrict a metadata table to the confirmed isolate preselection; NULL
# selection means "no filter" and passes the table through untouched.
.subset_meta <- function(meta, sel) {
  if (is.null(meta) || is.null(sel)) {
    return(meta)
  }
  meta[meta$isolate %in% sel, , drop = FALSE]
}

#' Parse an Analysis's stored isolate_selection JSON into a character vector, or
#' NULL when the Analysis doesn't restrict isolates. Exported because the
#' coordinator resolves the same field when it opens a saved plot.
#' @export
parse_static <- function(raw) {
  if (is.null(raw) || length(raw) != 1 || is.na(raw)) {
    return(NULL)
  }
  as.character(fromJSON(raw))
}

# Thumbnail size (px). Small on purpose: it only backs the dashboard box
# miniature, and stays inside the database as base64 text.
.THUMB_W <- 480L
.THUMB_H <- 320L

#' The tab's full inner layout: setup sidebar on the left, the engine's own
#' control panel and plot area (its whole `layout_sidebar`) on the right.
#' @export
ui <- function(id, plot_type) {
  ns <- NS(id)
  spec <- .ENGINES[[plot_type]]
  stopifnot(!is.null(spec))

  layout_sidebar(
    fillable = TRUE,
    border_radius = FALSE,
    padding = 0,
    sidebar = sidebar(
      id = ns("sidebar"),
      title = NULL,
      width = 320,
      actionButton(
        ns("generate"),
        "Generate",
        icon = icon("play"),
        width = "100%"
      ),
      accordion(
        id = ns("setup_accordion"),
        open = if (spec$distance) c("Selection", "Options") else "Selection",
        # Isolate preselection. The button opens a modal with the metadata
        # table where the user picks which isolates feed this plot; the info
        # line summarises the current selection. See the selection_button /
        # sel_* handlers below, and how selected_isolates threads into the
        # engine through viz_metadata_selected*.
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
        # Only the distance engines have anything to put here. For Map and Epi
        # the panel is not emitted at all — it used to be rendered empty with a
        # "Not available" placeholder and hidden with shinyjs, which is
        # unnecessary now that the tab knows its type up front.
        if (spec$distance) {
          accordion_panel(
            "Options",
            icon = icon("gear"),
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
            ),
            # Typing results imported from a peer (Database > Import). They
            # carry allele identity but no sequences, so they can join a
            # distance computation but nothing else.
            uiOutput(ns("imported_picker_ui")),
            # Tree-only. `algo` is a computation input (feeds compute_phylo_tree
            # on Generate, like the missing-value handling above); `zoom_view`
            # is purely presentational — the engine applies it as a CSS class
            # with no re-render.
            if (identical(plot_type, "Tree")) {
              tagList(
                prettyRadioButtons(
                  ns("algo"),
                  "Algorithm",
                  choices = c("Neighbour-Joining", "UPGMA")
                ),
                input_switch(ns("zoom_view"), "Zoom view", FALSE)
              )
            }
          )
        },
        # Save the currently displayed plot into an Analysis on the dashboard.
        # The picker's grouped choices are the Analyses (each with a "New plot"
        # entry) and the plots already saved in them; picking a plot overwrites
        # it, picking "New plot" adds one.
        accordion_panel(
          "Save Analysis",
          icon = icon("floppy-disk"),
          pickerInput(
            ns("save_target"),
            "Save into",
            choices = list(),
            options = pickerOptions(
              title = "No analysis",
              size = 10,
              container = "body"
            )
          ),
          actionButton(
            ns("save_plot"),
            "Save",
            icon = icon("floppy-disk"),
            class = "btn-primary w-100"
          ),
          uiOutput(ns("save_status"))
        )
      )
    ),
    shinyjs::useShinyjs(),
    # The engine's whole inner layout. `as_fill_carrier` used to come from the
    # coordinator wrapping each navset panel; the tab has to supply it now.
    as_fill_carrier(
      spec$mod$ui(ns("engine"), generate_id = ns("generate"))
    )
  )
}

#' @export
server <- function(
  id,
  # Fixed for the tab's lifetime — a scalar, not a reactive. See the file header.
  plot_type,
  # The plot's name, as typed on the creator form or carried over from the
  # saved plot this tab was opened from. It labels the tab and is the name the
  # plot is stored under, so it must not be re-derived from the type here.
  name = NULL,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  # Local per-isolate metadata, read and cached once by the coordinator.
  viz_metadata = shiny::reactive(NULL),
  # Staged peer typing-result sets available for import into a distance plot.
  staged_sets = shiny::reactive(NULL),
  # Grouped Save-target choices, kept current by the coordinator.
  picker_choices = shiny::reactive(list()),
  plots_changed = shiny::reactiveVal(0L),
  # TRUE while this tab is the selected nav panel. Only the Map needs it (to
  # nudge Leaflet into recomputing its size), but it costs nothing to thread.
  is_active = shiny::reactive(TRUE),
  # Flipped to FALSE by the coordinator immediately before it tears this tab
  # down. Everything handed to the engine is gated on it, which both stops the
  # engine's own observers and drops their cached values. See `destroy()`.
  alive = shiny::reactive(TRUE),
  # Optional list(save_target=, plot_id=, name=, snapshot=, selection=) applied
  # once at startup, used when the dashboard opens or pre-binds a plot.
  preset = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    spec <- .ENGINES[[plot_type]]
    needs_distance <- isTRUE(spec$distance)

    # ------------------------------------------------------ lifecycle ------
    # Every observer this module creates is registered here so `destroy()` can
    # take them down again on close. Shiny has no module-level teardown, so
    # this is the only way to stop them; render* observers are not returned to
    # us and are suspended by id instead (see destroy()).
    handles <- list()
    reg <- function(h) {
      handles[[length(handles) + 1L]] <<- h
      invisible(h)
    }

    # Wrapper for every reactive handed to the engine. When the coordinator
    # flips `alive` to FALSE each wrapper invalidates, which (a) makes the
    # engine's own observers abort on req() instead of doing work, and (b)
    # drops the cached value of every engine reactive downstream of it —
    # distance matrices, merged metadata, built plot objects. That is what
    # actually returns the memory; the engine's module environment itself
    # stays resident until the session ends.
    gate <- function(r) {
      reactive({
        req(isTRUE(alive()))
        r()
      })
    }

    # --------------------------------------------------------- metadata ----
    output$imported_picker_ui <- renderUI({
      sets <- staged_sets()
      if (is.null(sets) || !nrow(sets)) {
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
          countSelectedText = "{0} set(s)",
          container = "body"
        )
      )
    })

    reg(observeEvent(input$imported_info, {
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
    }))

    imported_sets <- reactive({
      s <- input$imported_sets
      if (!length(s)) NULL else as.integer(s)
    })

    # Local + staged, with a `source` column naming the origin. Drives the
    # isolate selection modal and the distance engines. It lives here rather
    # than in the coordinator because it depends on this tab's own
    # `imported_sets` picker.
    viz_metadata_all <- reactive({
      local <- viz_metadata()
      req(local)
      local$source <- "local"

      ext <- imported_metadata_wide(db_path(), imported_sets())
      if (is.null(ext) || !nrow(ext)) {
        return(local)
      }

      # Union of columns, so a peer field the local table lacks (and vice
      # versa) simply comes through as NA rather than dropping the rows.
      for (col in setdiff(names(local), names(ext))) {
        ext[[col]] <- NA_character_
      }
      for (col in setdiff(names(ext), names(local))) {
        local[[col]] <- NA_character_
      }

      rbind(local, ext[, names(local), drop = FALSE])
    })

    # --------------------------------------------------------- selection ---
    # NULL = no filter (all isolates); otherwise the isolate names confirmed in
    # the selection modal. Survives re-Generate; cleared only on a new
    # database, a session reset, or a new confirmed selection.
    selected_isolates <- reactiveVal(NULL)

    # A new database means the previous isolate names may not exist any more.
    reg(observeEvent(
      viz_metadata(),
      selected_isolates(NULL),
      ignoreInit = TRUE
    ))

    # Two flavours: the Map and the Epi curve plot straight from the metadata
    # and must see only local isolates; the distance engines also label and
    # colour the staged peer isolates they compute distances for.
    viz_metadata_selected <- reactive({
      .subset_meta(viz_metadata(), selected_isolates())
    })

    viz_metadata_selected_all <- reactive({
      .subset_meta(viz_metadata_all(), selected_isolates())
    })

    reg(observeEvent(input$selection_button, {
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
            uiOutput(ns("sel_count"), class = "selection-modal-count")
          ),
          div(
            class = "isolate-selection-table",
            DTOutput(ns("sel_table"), fill = FALSE)
          ),
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
    }))

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
          options = list(
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

    output$sel_count <- renderUI({
      meta <- viz_metadata_all()
      req(meta)
      n <- length(input$sel_table_rows_selected)
      span(sprintf("%d of %d selected", n, nrow(meta)))
    })

    # Select all / none act on the currently *filtered* rows, so "Select all"
    # after filtering only checks the visible subset.
    sel_proxy <- dataTableProxy("sel_table")
    reg(observeEvent(
      input$sel_all,
      selectRows(sel_proxy, input$sel_table_rows_all)
    ))
    reg(observeEvent(input$sel_none, selectRows(sel_proxy, NULL)))

    reg(observeEvent(input$sel_confirm, {
      meta <- viz_metadata_all()
      req(meta)
      rows <- input$sel_table_rows_selected
      # An empty confirmation means "no filter" — fall back to all isolates
      # (NULL), so the plot uses the full set rather than nothing.
      selected_isolates(if (length(rows)) meta$isolate[rows] else NULL)
      removeModal()
    }))

    output$selection_info <- renderUI({
      meta <- viz_metadata_all()
      if (is.null(meta)) {
        return(div(class = "text-muted small mt-2", "No database loaded"))
      }
      total <- nrow(meta)
      sel <- selected_isolates()
      base <- if (is.null(sel)) {
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
      if (!is.null(active_analysis_restriction())) {
        tagList(
          base,
          div(
            class = "small text-info",
            icon("lock"),
            " Set by this Analysis"
          )
        )
      } else {
        base
      }
    })

    # "Missing values" help: a modal rather than a tooltip since the
    # explanation is long-form (one paragraph per option).
    reg(observeEvent(input$na_handling_info, {
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
    }))

    # ------------------------------------------------------------ engine ---
    # One engine per tab, chosen by the fixed plot type. Every engine server
    # takes named, defaulted reactives, so a single do.call covers the
    # differences between their signatures.
    engine_args <- c(
      list("engine"),
      list(
        db_path = gate(db_path),
        session_reset = gate(session_reset),
        selected_isolates = gate(selected_isolates),
        na_handling = gate(reactive(input$na_handling %||% "ignore_na")),
        generate = gate(reactive(input$generate)),
        plot_type = gate(reactive(plot_type))
      )
    )
    engine_args <- c(
      engine_args,
      if (needs_distance) {
        list(
          viz_metadata = gate(viz_metadata_selected_all),
          imported_sets = gate(imported_sets)
        )
      } else {
        list(viz_metadata = gate(viz_metadata_selected))
      }
    )
    if (identical(plot_type, "Tree")) {
      engine_args <- c(
        engine_args,
        list(
          algo = gate(reactive(input$algo)),
          zoom_view = gate(reactive(input$zoom_view))
        )
      )
    }
    if (identical(plot_type, "Map")) {
      # Leaflet comes up at zero size in a hidden tab and never recomputes on
      # its own; the engine turns this into an invalidateSize() nudge.
      engine_args <- c(engine_args, list(visible = gate(is_active)))
    }
    eng <- do.call(spec$mod$server, engine_args)

    # -------------------------------------------------------------- save ---
    # Identity of this tab. `plot_id` is NULL until the plot has been saved
    # into an Analysis at least once; the coordinator reads it to avoid
    # opening a second tab for a plot that is already on screen.
    plot_id <- reactiveVal(NULL)
    tab_name <- reactiveVal(name %||% paste(plot_type, "plot"))

    # Dirty tracking: a plot is unsaved once it has been generated more
    # recently than it was last saved. Both are plain counters; the save path
    # records which generation it captured, because the client-side thumbnail
    # capture for MST/Map completes asynchronously and a Generate can land in
    # between (in which case the tab correctly stays dirty).
    generated_tick <- reactiveVal(0L)
    saved_tick <- reactiveVal(0L)
    # Set while a restored snapshot's synthetic Generate is in flight, so the
    # generation it produces is booked as already-saved (it is, by definition,
    # what is stored) rather than leaving a freshly reopened plot dirty.
    restore_pending <- reactiveVal(FALSE)
    reg(observeEvent(
      input$generate,
      {
        n <- isolate(generated_tick()) + 1L
        generated_tick(n)
        if (isTRUE(isolate(restore_pending()))) {
          restore_pending(FALSE)
          saved_tick(n)
        }
      },
      ignoreInit = TRUE
    ))

    NONE_TARGET <- "none"

    # A target to adopt as soon as the choices contain it. Saving into an
    # Analysis's "+ New plot" writes a row whose id doesn't exist in the
    # picker yet, so the retarget can't be done by writing input$save_target
    # directly — it has to wait for the refreshed choices below.
    preferred_target <- reactiveVal(NULL)

    # Keep the picker in sync with the coordinator's choices, preserving the
    # current selection when still valid and defaulting to "None" (the
    # picker's resting state, which must always resolve to a real choice).
    reg(observe({
      ch <- picker_choices()
      valid <- unlist(ch, use.names = FALSE)
      pref <- isolate(preferred_target())
      cur <- if (!is.null(pref) && pref %in% valid) {
        preferred_target(NULL)
        pref
      } else {
        isolate(input$save_target)
      }
      sel <- if (!is.null(cur) && cur %in% valid) cur else NONE_TARGET
      updatePickerInput(session, "save_target", choices = ch, selected = sel)
    }))

    # The Analysis currently targeted by the picker (a "plot:" target resolves
    # to its owning Analysis).
    active_analysis_id <- reactive({
      t <- input$save_target
      if (is.null(t) || !nzchar(t)) {
        return(NULL)
      }
      parts <- strsplit(t, ":", fixed = TRUE)[[1]]
      if (identical(parts[1], "analysis")) {
        return(as.integer(parts[2]))
      }
      if (identical(parts[1], "plot")) {
        row <- analysis_store$get_plot(db_path(), as.integer(parts[2]))
        if (!is.null(row)) {
          return(as.integer(row$analysis_id))
        }
      }
      NULL
    })

    # The targeted Analysis's fixed isolate set, or NULL when either no
    # Analysis is targeted or the targeted Analysis doesn't restrict isolates.
    # Some Analyses fix a selection and some don't — this, not merely "is an
    # Analysis targeted", is what gates the per-plot isolate controls.
    active_analysis_restriction <- reactive({
      plots_changed()
      aid <- active_analysis_id()
      if (is.null(aid)) {
        return(NULL)
      }
      row <- analysis_store$get_analysis(db_path(), aid)
      if (is.null(row)) {
        return(NULL)
      }
      parse_static(row$isolate_selection)
    })

    # A restricting Analysis owns the isolate set (defined in the dashboard's
    # setup wizard): apply it and lock the per-plot controls.
    reg(observe({
      sel <- active_analysis_restriction()
      restricted <- !is.null(sel)
      shinyjs::toggleState("selection_button", condition = !restricted)
      shinyjs::toggleClass(
        id = "selection_info",
        class = "viz-disabled-note",
        condition = restricted
      )
      if (restricted) {
        isolate(selected_isolates(sel))
      }
    }))

    # Holds the snapshot while an async client thumbnail is captured; NULL when
    # idle, which also guards against overlapping saves.
    pending <- reactiveVal(NULL)

    finalize_save <- function(b64) {
      p <- pending()
      if (is.null(p)) {
        return()
      }
      pending(NULL)

      thumb <- if (is.null(b64) || length(b64) != 1 || is.na(b64)) {
        NA_character_
      } else {
        # Client widgets return a full data URI; store just the base64 payload.
        sub("^data:image/[^;]+;base64,", "", b64)
      }

      new_id <- tryCatch(
        analysis_store$upsert_plot(
          db_path(),
          p$analysis_id,
          p$plot_id,
          p$name,
          p$plot_type,
          p$inputs_json,
          thumb
        ),
        error = function(e) NULL
      )
      ok <- !is.null(new_id)

      if (ok) {
        plot_id(as.integer(new_id))
        # The tab keeps its own label; overwriting a stored plot preserves that
        # plot's name (see perform_save). Renaming the tab to match would leave
        # the visible nav label stale, since it is built once at insert time.
        # Mark the generation that was actually captured, not "now": a Generate
        # that landed while the browser was rasterising leaves the tab dirty.
        saved_tick(p$gen_at)
        # Point the picker at the row just written. Without this a tab saved
        # into an Analysis's "+ New plot" stays aimed at "+ New plot", so every
        # further Save appends another copy instead of updating this one —
        # easy to trip now that a tab sticks around to be saved repeatedly.
        preferred_target(paste0("plot:", new_id))
        plots_changed(isolate(plots_changed()) + 1L)
        showNotification(
          "Plot saved to the Analysis dashboard.",
          type = "message"
        )
      } else {
        showNotification("Could not save the plot.", type = "error")
      }

      if (is.function(p$on_done)) {
        p$on_done(ok)
      }
    }

    # Runs the actual save (snapshot + thumbnail dispatch) for a resolved
    # `save_target`. Split out so an overwrite can be confirmed first.
    perform_save <- function(target, on_done = NULL) {
      parts <- strsplit(target, ":", fixed = TRUE)[[1]]
      if (identical(parts[1], "plot")) {
        pid <- as.integer(parts[2])
        existing <- analysis_store$get_plot(db_path(), pid)
        analysis_id <- if (is.null(existing)) {
          NA_integer_
        } else {
          existing$analysis_id
        }
        target_plot_id <- pid
        name <- if (is.null(existing)) {
          isolate(tab_name())
        } else {
          existing$name
        }
      } else {
        analysis_id <- as.integer(parts[2])
        target_plot_id <- NULL
        name <- isolate(tab_name())
      }

      # If the target Analysis fixes an isolate selection, the plot is saved
      # with it (not whatever the sidebar currently shows), so every plot in
      # the Analysis stays consistent.
      static_sel <- if (!is.na(analysis_id)) {
        active_analysis_restriction()
      } else {
        NULL
      }
      effective_selection <- static_sel %||% selected_isolates()

      # `selection` is the *restriction* (NULL = "all isolates"), which on its
      # own can't distinguish "all" at save time from "all" later — so a plot
      # built before isolates were added would silently look unchanged. Record
      # the concrete set actually used alongside it, so drift (a changed
      # Analysis selection *or* new isolates) is detectable on reopen.
      resolved_selection <- effective_selection
      if (is.null(resolved_selection)) {
        m <- viz_metadata()
        resolved_selection <- if (is.null(m)) character(0) else m$isolate
      }

      snap <- list(
        plot_type = plot_type,
        selection = effective_selection,
        selection_resolved = resolved_selection,
        na_handling = input$na_handling,
        imported_sets = input$imported_sets,
        algo = input$algo,
        zoom_view = isTRUE(input$zoom_view),
        engine = tryCatch(eng$snapshot(), error = function(e) list())
      )
      inputs_json <- toJSON(
        snap,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      )

      pending(list(
        inputs_json = as.character(inputs_json),
        plot_type = plot_type,
        analysis_id = analysis_id,
        plot_id = target_plot_id,
        name = name,
        gen_at = isolate(generated_tick()),
        on_done = on_done
      ))

      # Server-rendered ggplot engines write the PNG here and now; client
      # widget engines trigger an async browser capture that returns via
      # thumb_data(). The tab must therefore stay mounted until that lands —
      # see save_now()'s contract with the coordinator.
      if (!is.null(eng$save_thumb)) {
        f <- tempfile(fileext = ".png")
        b64 <- tryCatch(
          {
            eng$save_thumb(f, .THUMB_W, .THUMB_H)
            if (file.exists(f)) base64encode(f) else NA_character_
          },
          error = function(e) NA_character_
        )
        finalize_save(b64)
      } else if (!is.null(eng$request_thumb)) {
        eng$request_thumb()
      } else {
        finalize_save(NA_character_)
      }
    }

    # Target (and completion callback) stashed while an overwrite confirmation
    # modal is open.
    pending_save_target <- reactiveVal(NULL)

    # Shared entry point for both the Save button and the coordinator's
    # "Save and close". Validates, confirms a destructive overwrite, then saves.
    request_save <- function(target, on_done = NULL) {
      done <- function(ok) if (is.function(on_done)) on_done(ok)

      if (
        is.null(target) || !nzchar(target) || identical(target, NONE_TARGET)
      ) {
        showNotification("Pick an Analysis to save into.", type = "warning")
        return(done(FALSE))
      }
      if (!is.null(isolate(pending()))) {
        showNotification("A save is already in progress…", type = "message")
        return(done(FALSE))
      }

      parts <- strsplit(target, ":", fixed = TRUE)[[1]]
      if (identical(parts[1], "plot")) {
        # Overwriting an existing saved plot destroys its prior snapshot and
        # thumbnail irreversibly — confirm before proceeding.
        existing <- analysis_store$get_plot(db_path(), as.integer(parts[2]))
        pending_save_target(list(target = target, on_done = on_done))
        showModal(modalDialog(
          title = "Overwrite saved plot?",
          paste0(
            "This replaces the saved plot '",
            if (is.null(existing)) "this plot" else existing$name,
            "' with the plot currently shown. The previous version cannot",
            " be recovered."
          ),
          footer = tagList(
            actionButton(ns("cancel_overwrite_save"), "Cancel"),
            actionButton(
              ns("confirm_overwrite_save"),
              "Overwrite",
              class = "btn-danger"
            )
          ),
          easyClose = FALSE
        ))
      } else {
        # A new plot in an Analysis is purely additive — nothing to confirm.
        perform_save(target, on_done)
      }
    }

    reg(observeEvent(input$save_plot, request_save(input$save_target)))

    reg(observeEvent(input$confirm_overwrite_save, {
      removeModal()
      t <- pending_save_target()
      pending_save_target(NULL)
      req(t)
      perform_save(t$target, t$on_done)
    }))

    # Cancelling has to report back too, or a "Save and close" that the user
    # backs out of would leave the coordinator waiting forever.
    reg(observeEvent(input$cancel_overwrite_save, {
      removeModal()
      t <- pending_save_target()
      pending_save_target(NULL)
      if (!is.null(t) && is.function(t$on_done)) {
        t$on_done(FALSE)
      }
    }))

    # Completion of an async (client-captured) thumbnail for MST / Map.
    if (!is.null(eng$thumb_data)) {
      reg(observeEvent(
        eng$thumb_data(),
        {
          if (!is.null(pending())) {
            finalize_save(eng$thumb_data())
          }
        },
        ignoreInit = TRUE
      ))
    }

    output$save_status <- renderUI({
      if (!is.null(pending())) {
        div(class = "small text-muted mt-2", "Saving…")
      } else {
        NULL
      }
    })

    # ----------------------------------------------------------- restore ---
    # Apply a saved snapshot to this tab's controls, then let the input
    # updates round-trip to the browser before clicking Generate, so the
    # engine recomputes the identical plot.
    restore_snapshot <- function(snap) {
      if (is.null(snap)) {
        return(invisible(NULL))
      }
      if (!is.null(snap$na_handling)) {
        updateSelectInput(session, "na_handling", selected = snap$na_handling)
      }
      updatePickerInput(
        session,
        "imported_sets",
        selected = snap$imported_sets %||% character(0)
      )
      if (!is.null(snap$algo)) {
        updatePrettyRadioButtons(session, "algo", selected = snap$algo)
      }
      if (identical(plot_type, "Tree")) {
        session$sendInputMessage(
          "zoom_view",
          list(value = isTRUE(snap$zoom_view))
        )
      }
      selected_isolates(snap$selection)

      if (!is.null(eng$restore)) {
        try(eng$restore(snap$engine), silent = TRUE)
      }

      # Booked as already-saved when the click below lands; see restore_pending.
      restore_pending(TRUE)
      shinyjs::delay(
        500,
        shinyjs::runjs(sprintf(
          "var b=document.getElementById('%s'); if(b){b.click();}",
          ns("generate")
        ))
      )
      invisible(NULL)
    }

    # Startup: apply whatever the coordinator pre-bound this tab to. A tab
    # opened from a saved plot restores and regenerates; one opened from
    # "Add Plot" only inherits the Analysis target and its fixed selection.
    #
    # Deferred by a beat: this server is constructed in the same flush that
    # nav_insert's this tab's UI, and every call below addresses a control that
    # has to exist in the DOM first (the picker, the accordion, the switch).
    if (!is.null(preset)) {
      # Non-UI state can be set straight away.
      if (!is.null(preset$plot_id)) {
        plot_id(as.integer(preset$plot_id))
      }
      if (!is.null(preset$selection)) {
        selected_isolates(preset$selection)
      }
      shinyjs::delay(150, {
        if (!is.null(preset$save_target)) {
          accordion_panel_open("setup_accordion", "Save Analysis")
          updatePickerInput(
            session,
            "save_target",
            choices = isolate(picker_choices()),
            selected = preset$save_target
          )
        }
        if (!is.null(preset$snapshot)) {
          restore_snapshot(preset$snapshot)
        }
      })
    }

    # ---------------------------------------------------------- teardown ---
    destroy <- function() {
      for (h in handles) {
        try(h$destroy(), silent = TRUE)
      }
      handles <<- list()
      # render* observers have no handle we can reach; suspending them by id
      # is the equivalent.
      for (o in c(
        "imported_picker_ui",
        "selection_info",
        "sel_table",
        "sel_count",
        "save_status"
      )) {
        try(outputOptions(output, o, suspend = TRUE), silent = TRUE)
      }
      selected_isolates(NULL)
      pending(NULL)
      pending_save_target(NULL)
      invisible(NULL)
    }

    list(
      plot_type = plot_type,
      plot_id = plot_id,
      name = tab_name,
      dirty = reactive(generated_tick() > saved_tick()),
      # Save on the coordinator's behalf and report the outcome. The callback
      # is what makes "Save and close" safe: MST and Map rasterise in the
      # browser, so the tab must stay mounted until the PNG comes back.
      save_now = function(on_done = NULL) {
        request_save(isolate(input$save_target), on_done)
      },
      destroy = destroy
    )
  })
}
