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
    downloadHandler,
    showNotification,
    tags,
    tagList,
    span,
    showModal,
    modalDialog,
    modalButton,
    removeModal,
    dateRangeInput,
    updateDateRangeInput,
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
    updateRadioGroupButtons,
  ],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows, JS],
)
box::use(
  app / logic / db_events,
  app / logic / db_staging[imported_metadata_wide],
  app / logic / field_labels[field_labels_for],
  app / logic / field_types[as_date_safe, date_fields],
  app /
    logic /
    viz_export[
      export_filename,
      export_hint,
      export_modal,
      export_panel,
      export_preset,
      export_presets,
      write_data_uri
    ],
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
# `export_kind` picks the export route, which is a property of where the plot is
# drawn rather than of what it shows: "ggplot" engines hold a plot object on the
# server and can be re-rendered at any size, while "widget" engines are drawn by
# the browser and have to be captured from it. See app/logic/viz_export.R.
.ENGINES <- list(
  MST = list(mod = visualization_mst, distance = TRUE, export_kind = "widget"),
  Tree = list(
    mod = visualization_tree,
    distance = TRUE,
    export_kind = "ggplot"
  ),
  Map = list(mod = visualization_map, distance = FALSE, export_kind = "widget"),
  Epi = list(mod = visualization_epi, distance = FALSE, export_kind = "ggplot"),
  AMR = list(mod = visualization_amr, distance = FALSE, export_kind = "ggplot")
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
      "differing loci. Positions are computed, not dragged; pan and zoom the",
      "canvas and hover a node to see its members, and single-linkage",
      "clusters below a chosen threshold are shaded behind the network."
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

  as_fill_carrier(
    div(
      id = "plot-setup",
      layout_sidebar(
        fillable = TRUE,
        border_radius = FALSE,
        padding = 0,
        sidebar = sidebar(
          id = ns("sidebar"),
          class = "plot-setup",
          title = NULL,
          width = 340,
          actionButton(
            ns("generate"),
            "Generate",
            icon = icon("chalkboard"),
            class = "btn-primary",
            width = "100%"
          ),
          accordion(
            id = ns("setup_accordion"),
            open = c("Selection", "Export Plot"),
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
              "Export Plot",
              icon = icon("arrow-up-from-bracket"),
              export_panel(ns, "export")
            ),
            # Save the currently displayed plot into an Analysis on the dashboard.
            # The picker's grouped choices are the Analyses (each with a "New plot"
            # entry) and the plots already saved in them; picking a plot overwrites
            # it, picking "New plot" adds one.
            accordion_panel(
              "Save Analysis",
              icon = icon("floppy-disk"),
              pickerInput(
                ns("save_target"),
                "Save in analysis group",
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
                width = "100%"
              ),
              uiOutput(ns("save_status"))
            )
          )
        ),
        shinyjs::useShinyjs(),
        # The engine's whole inner layout. `as_fill_carrier` used to come from the
        # coordinator wrapping each navset panel; the tab has to supply it now.
        as_fill_carrier(
          spec$mod$ui(
            ns("engine"),
            generate_id = ns("generate"),
            # The distance-computation controls, rendered in the engine's own
            # right-hand sidebar rather than here. They stay namespaced to this
            # module, so every observer behind them is unchanged — what moves is
            # only where they appear, and with it what they mean: the left
            # sidebar is the "press Generate to apply" side, the right sidebar
            # takes effect live. Only the distance engines have any.
            options_ui = if (spec$distance) {
              tagList(
                pickerInput(
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
                if (identical(plot_type, "Tree")) {
                  tagList(
                    prettyRadioButtons(
                      ns("algo"),
                      "Algorithm",
                      choices = c("Neighbour-Joining", "UPGMA")
                    )
                  )
                }
              )
            }
          )
        )
      )
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
  # One row per metadata column: its declared type, distinct-value count,
  # coverage and group. Built once by the coordinator and threaded to every
  # engine, which is what lets their field pickers agree on what a variable is
  # without each recomputing it. See app/logic/field_profile.R.
  viz_field_profiles = shiny::reactive(NULL),
  # Staged peer typing-result sets available for import into a distance plot.
  staged_sets = shiny::reactive(NULL),
  # Grouped Save-target choices, kept current by the coordinator.
  picker_choices = shiny::reactive(list()),
  db_rev = db_events$new_bus(),
  # TRUE while a typing run is writing the database - see the Generate gate.
  typing_active = shiny::reactive(FALSE),
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

    # The selection the plot on screen was actually drawn from, as opposed to the
    # one the user has picked but not yet applied. Everything the engine sees
    # goes through this, which is what makes the left sidebar a form you submit
    # rather than a set of live controls: confirming a different set of isolates
    # changed the metadata under the tree immediately, so the plot redrew with
    # new labels against a tree still computed from the old set.
    #
    # NULL before the first Generate; thereafter whatever Generate captured.
    applied_selection <- reactiveVal(NULL)

    # The isolate pool moved underneath this tab, so the sidebar's selection has
    # to be re-checked against it.
    #
    # This used to clear it outright, which was right while viz_metadata() only
    # ever changed when a different database was loaded. Now that it also
    # advances on an isolate removal, a typing run or a metadata edit, clearing
    # would throw away a selection the user built by hand every time somebody
    # touched an unrelated cell. So: keep what still exists, drop what does not,
    # and only fall back to NULL ("all isolates") when nothing survives.
    #
    # Only `selected_isolates`, the sidebar form. `applied_selection` is the
    # record of what the plot on screen was drawn from and is deliberately left
    # alone: reconciling it would erase the very difference that tells the user
    # their plot is out of date.
    reg(observeEvent(
      viz_metadata(),
      {
        live <- db_events$reconcile_names(
          isolate(selected_isolates()),
          viz_metadata()$isolate
        )
        if (live$changed) {
          selected_isolates(if (length(live$kept)) live$kept else NULL)
        }
      },
      ignoreInit = TRUE
    ))

    # Say it once, when it happens, rather than leaving the user to notice that
    # Generate has lit up. Fires only for a plot already on screen - before the
    # first Generate there is nothing to be out of date.
    reg(observeEvent(plot_missing(), {
      n <- length(plot_missing())
      if (!n) {
        return()
      }
      showNotification(
        tagList(
          tags$strong(sprintf(
            "%d isolate(s) in this plot were removed from the database.",
            n
          )),
          tags$br(),
          "The plot still shows them."
        ),
        type = "warning",
        duration = 12
      )
    }))

    # Two flavours: the Map and the Epi curve plot straight from the metadata
    # and must see only local isolates; the distance engines also label and
    # colour the staged peer isolates they compute distances for.
    #
    # Both read the *applied* selection: they feed the engine, and the engine
    # draws whatever it is given the moment it is given it.
    # The metadata the plot on screen was actually drawn from, captured at
    # Generate alongside `applied_selection`.
    #
    # This has to be a snapshot, not a live read. The distance engines keep the
    # computed topology in a reactiveVal that only Generate replaces, but they
    # rebuild the *drawing* whenever their metadata changes - so a live table
    # let the two halves drift apart. Removing isolates from the database left
    # the tree with the branches and tips it was computed from and a metadata
    # table that no longer described them, and it redrew as a tree whose tips
    # had silently lost their labels.
    #
    # NULL until the first Generate, where the reactives below fall through to
    # the live table: the engines fill their variable pickers from it as soon as
    # a database is loaded, which must keep working before anything is drawn.
    applied_metadata <- reactiveVal(NULL)
    applied_metadata_all <- reactiveVal(NULL)

    viz_metadata_selected <- reactive({
      applied_metadata() %||%
        .subset_meta(viz_metadata(), applied_selection())
    })

    viz_metadata_selected_all <- reactive({
      applied_metadata_all() %||%
        .subset_meta(viz_metadata_all(), applied_selection())
    })

    # The isolate set handed to the engines: always concrete, never NULL.
    #
    # Read only by the two distance engines, which pass it straight to
    # compute_phylo_tree() / compute_mst(). It must not be NULL there. NULL
    # means "all isolates" everywhere in this sidebar, but load_allele_profile()
    # resolves NULL with a live `SELECT ... FROM mlst` of its own - a different
    # question, answered at a different moment, against a different table. Mid
    # typing run that query sees isolates pyMLST has only half-written, which
    # the sidebar count, the selection modal and the tip labels all know nothing
    # about, so the tree came back with more tips than there were labels for.
    #
    # Deriving the list from the very frame that supplies those labels is what
    # makes topology and labels agree by construction rather than by two
    # separate reads happening to land on the same answer.
    engine_isolates <- reactive({
      meta <- viz_metadata_selected_all()
      if (is.null(meta)) NULL else meta$isolate
    })

    # How the drawn plot differs from the database as it stands now.
    #
    # `plot_missing` is what makes the plot wrong rather than merely dated: those
    # isolates are still drawn, but nothing in the database backs them any more.
    # `plot_new` only counts for a plot drawn from the whole database - with an
    # explicit selection a newly typed isolate simply is not part of it, so it is
    # no reason to call the plot out of date.
    plot_missing <- reactive({
      snap <- applied_metadata()
      live <- viz_metadata()
      if (is.null(snap) || is.null(live)) {
        return(character(0))
      }
      setdiff(snap$isolate, live$isolate)
    })

    plot_new <- reactive({
      snap <- applied_metadata()
      live <- viz_metadata()
      if (is.null(snap) || is.null(live) || !is.null(applied_selection())) {
        return(character(0))
      }
      setdiff(live$isolate, snap$isolate)
    })

    # Reported, not acted on. This drives the note under the isolate count and
    # nothing else: a database change does not arm Generate, because the plot on
    # screen is still a valid answer for the isolates it was built from and a
    # redraw is the user's call. They make it by choosing isolates.
    plot_stale <- reactive({
      length(plot_missing()) > 0L || length(plot_new()) > 0L
    })

    # TRUE once this tab has drawn a plot at least once.
    generated_once <- reactiveVal(FALSE)

    # Whether the left sidebar holds anything not yet in the plot. Before the
    # first Generate everything is pending, so the button is live from the start.
    # Set when the user confirms the isolate modal, cleared at Generate.
    #
    # Needed because confirming collapses "all of them" back to NULL (see
    # do_sel_confirm). Without it a plot drawn from the whole database could
    # never be rebuilt after new isolates arrived: the user would tick them,
    # confirm, and the selection would collapse to the same NULL the plot was
    # generated from, comparing equal and leaving Generate disabled over a plot
    # that no longer covers the database.
    selection_touched <- reactiveVal(FALSE)

    # Deliberately does not include plot_stale(). A database change on its own
    # is reported (see selection_info) but does not arm Generate: the plot on
    # screen is still a valid answer for the isolates it was built from, and
    # nothing should invite a redraw the user did not ask for. Choosing isolates
    # is what arms it — including re-confirming "all", which after a typing run
    # means a different set than it did at the last Generate.
    pending_changes <- reactive({
      !isTRUE(generated_once()) ||
        !identical(selected_isolates(), applied_selection()) ||
        isTRUE(selection_touched())
    })

    # Generate is the only way to apply them, so it is only offered when there is
    # something to apply — and says so, rather than looking pressable and doing
    # nothing.
    #
    # Also withheld outright while a typing run is under way. The distance
    # engines are the one place that reads allele profiles straight from `mlst`
    # rather than through the store, and during a run that table is being
    # written by pyMLST from another process: a tree built from it would be
    # computed from isolates whose profiles are still only partly written, so
    # their distances would be wrong rather than merely incomplete. Declining
    # for the minute or two a run lasts is the honest answer.
    reg(observe({
      busy <- isTRUE(typing_active())
      pending <- isTRUE(pending_changes()) && !busy
      shinyjs::toggleState("generate", condition = pending)
      shinyjs::toggleClass("generate", "is-pending", condition = pending)
      shinyjs::toggleClass("generate", "btn-primary", condition = pending)
    }))

    # ------------------------------------------------------ time filter ---
    # The date-typed columns the modal's time filter can work along: the fixed
    # schema's collection date, the app-stamped add time, and every user-defined
    # `date` variable. None of them is named here — they come from the shared
    # type map (app/logic/field_types.R), which reads `phylotrace_custom_fields`
    # for the custom ones, so a date variable the user defines in Database >
    # Custom fields becomes a filterable time axis with no change here.
    sel_date_choices <- reactive({
      meta <- viz_metadata_all()
      if (is.null(meta)) {
        return(character(0))
      }
      cols <- date_fields(db_path(), names(meta))
      stats::setNames(cols, field_labels_for(cols))
    })

    # The chosen column parsed to Date, NA where the cell is empty or (for the
    # free-text collection date) unreadable. NULL when no column is chosen.
    sel_dates <- reactive({
      meta <- viz_metadata_all()
      col <- input$sel_date_field
      if (is.null(meta) || is.null(col) || !nzchar(col)) {
        return(NULL)
      }
      if (!col %in% names(meta)) {
        return(NULL)
      }
      as_date_safe(meta[[col]])
    })

    # The rows the table shows. Without a chosen column, or before the range
    # input has reported a complete window, that is every isolate. Rows whose
    # date is missing or unreadable drop out while a filter is active — a row
    # with no date cannot be shown to fall inside a window, and silently keeping
    # them would make the window mean nothing.
    sel_filtered <- reactive({
      meta <- viz_metadata_all()
      req(meta)
      d <- sel_dates()
      rng <- input$sel_date_range
      if (is.null(d) || is.null(rng) || length(rng) != 2 || anyNA(rng)) {
        return(meta)
      }
      keep <- !is.na(d) & d >= as.Date(rng[1]) & d <= as.Date(rng[2])
      meta[keep, , drop = FALSE]
    })

    # Isolates ticked in the table, held by name rather than by row index so
    # that changing the window — which re-renders the table and renumbers its
    # rows — does not silently discard them. Only names still in the window
    # survive: the window defines the pool the selection is drawn from.
    sel_checked <- reactiveVal(character(0))

    reg(observeEvent(
      input$sel_table_rows_selected,
      {
        rows <- input$sel_table_rows_selected
        tbl <- sel_filtered()
        sel_checked(if (length(rows)) tbl$isolate[rows] else character(0))
      },
      ignoreNULL = FALSE
    ))

    # Switching the time axis re-bases the window on the new column's own span,
    # since a window that made sense for the collection date is meaningless on,
    # say, the date an isolate entered the database.
    reg(observeEvent(input$sel_date_field, {
      d <- sel_dates()
      shinyjs::toggleState("sel_date_range", condition = !is.null(d))
      if (is.null(d) || all(is.na(d))) {
        return()
      }
      updateDateRangeInput(
        session,
        "sel_date_range",
        start = min(d, na.rm = TRUE),
        end = max(d, na.rm = TRUE),
        min = min(d, na.rm = TRUE),
        max = max(d, na.rm = TRUE)
      )
    }))

    reg(observeEvent(input$selection_button, {
      meta <- viz_metadata_all()
      req(meta)
      # Fresh table, but a sticky time filter: the controls are recreated on
      # every open carrying the values they had when the modal was last closed,
      # so what the table shows always matches what the footer says. Re-creating
      # them with defaults instead would leave the inputs' retained values
      # briefly disagreeing with the visible controls.
      dates <- sel_date_choices()
      field <- isolate(input$sel_date_field) %||% ""
      rng <- isolate(input$sel_date_range)
      # Seeds the tick state from the last confirmed selection, so reopening
      # the modal shows what's actually applied instead of starting blank
      # every time (NULL means "everything", which ticks nothing).
      seed <- isolate(selected_isolates()) %||% character(0)
      sel_checked(seed)
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
            # Shares the footer row with the buttons (pushed to the left of
            # them by .selection-modal-filter). Omitted entirely when the
            # database has no date-typed column to filter along.
            if (length(dates)) {
              div(
                class = "selection-modal-filter",
                pickerInput(
                  ns("sel_date_field"),
                  label = NULL,
                  choices = c("No time filter" = "", dates),
                  selected = if (field %in% dates) field else "",
                  width = "fit"
                ),
                div(
                  id = ns("sel_date_range_wrap"),
                  dateRangeInput(
                    ns("sel_date_range"),
                    label = NULL,
                    start = rng[1],
                    end = rng[2],
                    separator = "–",
                    width = "17rem"
                  )
                )
              )
            },
            modalButton("Cancel"),
            actionButton(
              ns("sel_confirm"),
              "Confirm",
              class = "btn-primary"
            ),
            actionButton(
              ns("sel_confirm_generate"),
              "Confirm & Generate",
              icon = icon("bolt"),
              class = "btn-primary"
            )
          ),
          easyClose = TRUE
        )
      ))
      # The range input is created enabled; disable it right away when the
      # modal opens with no column chosen (the observer above owns it from
      # then on).
      shinyjs::toggleState(
        "sel_date_range",
        condition = nzchar(field) && field %in% dates
      )
      # Baking `seed` into this render's own `selection=` argument isn't
      # reliable: reopening rebinds the DT widget, and its own initial
      # "nothing selected yet" report can reach the server after this render
      # already ran, wiping out `sel_checked` before the ticks ever show.
      # Applying it out-of-band, once the widget has had time to settle,
      # sidesteps that race.
      if (length(seed)) {
        shinyjs::delay(150, {
          tbl <- isolate(sel_filtered())
          rows <- match(seed, tbl$isolate)
          selectRows(sel_proxy, rows[!is.na(rows)])
        })
      }
    }))

    output$sel_table <- renderDT(
      {
        tbl <- sel_filtered()
        req(tbl)
        # Row indices of the isolates that were ticked before the window
        # changed, so the re-render restores rather than clears them. Read
        # non-reactively: the checked set is an *output* of this table, and
        # depending on it would re-render on every click.
        keep <- match(isolate(sel_checked()), tbl$isolate)

        # Pinned (FixedColumns) columns: `isolate` always, plus whatever date
        # column the time filter is currently set to — moved to sit right
        # after it, so both stay in view together while the rest of the wide
        # metadata table scrolls underneath. Isolate alone when there is no
        # time filter (or it names a column this frame doesn't have, e.g. the
        # instant the picker's own choices are still catching up to a new
        # database).
        date_col <- input$sel_date_field
        pin_cols <- if (
          !is.null(date_col) &&
            nzchar(date_col) &&
            !identical(date_col, "isolate") &&
            date_col %in% names(tbl)
        ) {
          c("isolate", date_col)
        } else {
          "isolate"
        }
        tbl <- tbl[, c(pin_cols, setdiff(names(tbl), pin_cols)), drop = FALSE]

        datatable(
          tbl,
          rownames = FALSE,
          filter = "top",
          # Drop the default "display" class's zebra striping (keep borders /
          # hover / sortable) so the cells aren't tinted per row.
          class = "row-border hover order-column",
          # Opens with nothing selected ("0 of N"); confirming an empty
          # selection is treated as "all rows in the window" (see the
          # sel_confirm handler).
          selection = list(
            mode = "multiple",
            selected = keep[!is.na(keep)]
          ),
          extensions = "FixedColumns",
          options = list(
            dom = "tip",
            pageLength = 20,
            scrollX = TRUE,
            # `scrollY` only has to be a non-empty value: it is what tells
            # DataTables to build the header/body scroll structure at all —
            # main.scss then takes over sizing entirely (single scroller, the
            # header pinned inside it with position: sticky; see the
            # `.edit-table, .isolate-selection-table, .db-page_body` rules and
            # `.isolate-selection-table .dataTables_scroll`'s max-height), the
            # same convention `database_browser.R`'s metadata_table uses.
            scrollY = "1px",
            scrollCollapse = TRUE,
            fixedColumns = list(leftColumns = length(pin_cols)),
            # FixedColumns pins each pinned column's own header cell (adding
            # `dtfc-fixed-left` + a computed `left` offset it works out from
            # the actual rendered column widths) but has no notion of the
            # filter = "top" row underneath — that is a plain <td> row DT
            # bolts onto <thead> outside the header API, so FixedColumns never
            # touches it and it scrolls out from under its own column instead
            # of staying put. `database_browser.R`'s metadata_table hits the
            # same gap, but only ever pins one column, so a CSS rule hardcoded
            # to `left: 0` (see the `.edit-table` rules in main.scss) is
            # enough there; the second pinned column here is a date field
            # whose width isn't knowable in advance, so its offset has to be
            # read from the header cell FixedColumns already positioned,
            # rather than guessed at in CSS.
            initComplete = JS(sprintf(
              "function(settings) {
                 var rows = $(this.api().table().header()).find('tr');
                 var headerCells = rows.eq(0).find('th');
                 var filterCells = rows.eq(1).find('td');
                 for (var i = 0; i < %d; i++) {
                   filterCells.eq(i).addClass('dtfc-fixed-left').css({
                     position: 'sticky',
                     left: headerCells.eq(i).css('left') || '0px',
                     zIndex: 3
                   });
                 }
               }",
              length(pin_cols)
            ))
          )
        )
      },
      server = FALSE
    )

    output$sel_count <- renderUI({
      meta <- viz_metadata_all()
      req(meta)
      shown <- nrow(sel_filtered())
      n <- length(input$sel_table_rows_selected)
      if (shown == nrow(meta)) {
        return(span(sprintf("%d of %d selected", n, shown)))
      }
      # A row leaves the table for one of two different reasons — its date
      # falls outside the chosen window, or it has no usable date at all (blank
      # cell, or free text that didn't parse) — and lumping both into one
      # "outside window" figure would read as "the window excludes N" when some
      # of those N were never judgeable against the window in the first place.
      # Reporting them apart is what keeps a database with sparse date entry
      # from looking like a window drawn too narrow.
      d <- sel_dates()
      missing <- if (is.null(d)) 0L else sum(is.na(d))
      outside <- nrow(meta) - shown - missing
      detail <- if (missing > 0 && outside > 0) {
        sprintf(" · %d outside window, %d missing date", outside, missing)
      } else if (missing > 0) {
        sprintf(" · %d missing date", missing)
      } else {
        sprintf(" · %d outside window", outside)
      }
      span(
        sprintf("%d of %d selected", n, shown),
        span(class = "selection-modal-count-total", detail)
      )
    })

    # Unlike plain Confirm, an empty tick set has no "everything in the
    # window" fallback here — it would generate immediately from a selection
    # the user never actually made.
    reg(observe({
      shinyjs::toggleState(
        "sel_confirm_generate",
        condition = length(input$sel_table_rows_selected) > 0
      )
    }))

    # Select all / none act on the currently *filtered* rows, so "Select all"
    # after filtering only checks the visible subset.
    sel_proxy <- dataTableProxy("sel_table")
    reg(observeEvent(
      input$sel_all,
      selectRows(sel_proxy, input$sel_table_rows_all)
    ))
    reg(observeEvent(input$sel_none, selectRows(sel_proxy, NULL)))

    # Shared by the plain confirm and the "confirm & generate" button: applies
    # the ticked (or, if none ticked, filtered) rows as the selection. Returns
    # FALSE, leaving the modal open, when there is nothing to select.
    do_sel_confirm <- function() {
      meta <- viz_metadata_all()
      tbl <- sel_filtered()
      req(meta, tbl)
      rows <- input$sel_table_rows_selected
      # An empty confirmation means "everything the window leaves" — with no
      # window that is every isolate, which is how confirming without ticking
      # anything has always behaved.
      picked <- if (length(rows)) tbl$isolate[rows] else tbl$isolate
      if (!length(picked)) {
        showNotification(
          "No isolates left to select — widen the time filter.",
          type = "warning"
        )
        return(FALSE)
      }
      # Collapse "all of them" back to NULL (no filter), so a window that
      # happens to cover the whole database costs the engines nothing.
      selected_isolates(if (setequal(picked, meta$isolate)) NULL else picked)
      # The user has chosen a set. That, not a database change, is what arms
      # Generate — and it has to be recorded separately because the collapse
      # above can leave the value identical to the applied one.
      selection_touched(TRUE)
      removeModal()
      TRUE
    }

    reg(observeEvent(input$sel_confirm, do_sel_confirm()))

    # Applies the selection exactly like Confirm, then synthesizes a click on
    # Generate so the plot redraws without a second click. The Generate button
    # is disabled whenever nothing is pending; the toggleState observer that
    # would flip it back on only fires on the next reactive flush, after this
    # message is already queued, so the click's own JS also clears `disabled`
    # first rather than trusting the button's client-side state to have caught
    # up yet.
    reg(observeEvent(input$sel_confirm_generate, {
      if (isTRUE(do_sel_confirm())) {
        shinyjs::runjs(sprintf(
          "var b=document.getElementById('%s'); if(b){b.disabled=false; b.click();}",
          ns("generate")
        ))
      }
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
      # A toast fades; this does not. It is the standing answer to "why is
      # Generate lit up on a plot I have not touched?", and it names the two
      # cases separately because they mean different things: the plot is *wrong*
      # when it still draws isolates the database no longer has, and merely
      # *behind* when new isolates would join it.
      missing_n <- length(plot_missing())
      new_n <- length(plot_new())
      staleness <- if (missing_n || new_n) {
        div(
          class = "small mt-1 text-warning",
          icon("triangle-exclamation"),
          " ",
          if (missing_n) {
            sprintf("%d isolate(s) in this plot were removed.", missing_n)
          },
          if (missing_n && new_n) " ",
          if (new_n) {
            sprintf("%d new isolate(s) are not included.", new_n)
          }
        )
      }

      restriction <- if (!is.null(active_analysis_restriction())) {
        div(
          class = "small text-info",
          icon("lock"),
          " Set by this Analysis"
        )
      }

      # Says why Generate is greyed out, so a run in another tab does not read
      # as the button being broken.
      busy <- if (isTRUE(typing_active())) {
        div(
          class = "small mt-1 text-info",
          icon("hourglass-half"),
          " Typing in progress — Generate is available once the run finishes."
        )
      }

      parts <- list(base, restriction, staleness, busy)
      parts <- parts[!vapply(parts, is.null, logical(1))]
      if (length(parts) == 1L) parts[[1]] else do.call(tagList, parts)
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
        # Not gated: it is a bus, not a reactive, and the engines that read the
        # database directly (AMR hits, the tree's AMR matrix, the MST's scheme
        # overview) need it to know when those reads went stale.
        db_rev = db_rev,
        session_reset = gate(session_reset),
        # Resolved, never NULL - see engine_isolates. Declared by every engine
        # but read only by the two that compute a distance matrix.
        selected_isolates = gate(engine_isolates),
        na_handling = gate(reactive(input$na_handling %||% "ignore_na")),
        generate = gate(reactive(input$generate)),
        plot_type = gate(reactive(plot_type)),
        field_profiles = gate(viz_field_profiles)
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
        list(algo = gate(reactive(input$algo)))
      )
    }
    if (identical(plot_type, "Map")) {
      # Leaflet comes up at zero size in a hidden tab and never recomputes on
      # its own; the engine turns this into an invalidateSize() nudge.
      engine_args <- c(engine_args, list(visible = gate(is_active)))
    }
    eng <- do.call(spec$mod$server, engine_args)

    # ------------------------------------------------------------ export ---
    # The engine declares what it can produce and how to write it (its `export`
    # contract); this owns the sidebar panel, the settings, and the single
    # download the user actually clicks. Keeping the panel here rather than in
    # each engine is what lets one plot type's export look and behave like
    # another's.
    exporter <- eng$export
    export_widget <- identical(exporter$kind, "widget")

    # A plain string for most engines; the AMR views make it a reactive, since
    # their three modes are different plots and the file name has to say which.
    export_label <- function() {
      lbl <- exporter$label
      if (is.function(lbl)) lbl() else lbl
    }

    export_format <- reactive(input$export_filetype %||% "png")

    # No raw width/DPI/target-width controls: the preset alone decides them.
    # Falls back to the first preset if the picker hasn't reported a selection
    # yet (its own `selected=` default), so this never has to represent "no
    # settings chosen."
    export_opts <- reactive({
      preset <- export_preset(input$export_preset) %||% export_presets[[1]]
      vals <- preset[[spec$export_kind]]
      list(
        width_cm = vals$width_cm,
        dpi = suppressWarnings(as.numeric(vals$dpi)),
        target_px = vals$target_px
      )
    })

    # What the modal's controls hold, mirrored continuously so reopening it
    # restores the last choice however it was dismissed — Cancel, Export, or a
    # click outside it, only the first two of which are observable events.
    export_settings <- reactiveVal(list())

    reg(observe({
      export_settings(Filter(
        Negate(is.null),
        list(
          filetype = input$export_filetype,
          preset = input$export_preset
        )
      ))
    }))

    # The settings the confirmed export is running with, captured the moment
    # Export is pressed. The controls live in the modal and are gone by the time
    # the file is written, so nothing downstream may read them live.
    export_request <- reactiveVal(NULL)

    reg(observeEvent(input$export_open, {
      # Refuse here rather than opening a settings form for a plot that does not
      # exist yet.
      if (!isTRUE(exporter$ready())) {
        showNotification(
          "Generate a plot before saving it.",
          type = "warning"
        )
        return()
      }
      showModal(export_modal(
        ns,
        "export",
        spec$export_kind,
        isolate(export_settings())
      ))
    }))

    reg(observeEvent(input$export_cancel, removeModal()))

    # HTML has no width/DPI or target pixel size of its own — it is the
    # self-contained widget as-is, not a re-render at a chosen size — so a
    # widget engine's presets do nothing while it is the selected format.
    # Disabled rather than hidden, so the layout doesn't jump when switching
    # format back and forth.
    if (export_widget) {
      reg(observe({
        updateRadioGroupButtons(
          session,
          "export_preset",
          disabled = identical(export_format(), "html")
        )
      }))
    }

    output$export_hint <- renderUI({
      aspect <- if (is.null(exporter$aspect)) {
        0.62
      } else {
        tryCatch(exporter$aspect(), error = function(e) 0.62)
      }
      opts <- export_opts()
      span(export_hint(
        exporter$kind,
        export_format(),
        opts$width_cm,
        if (export_widget) opts$target_px else opts$dpi,
        aspect
      ))
    })

    # The finished file, waiting to be handed to the browser. Written before the
    # download link is clicked, never inside the handler — see `stage()`.
    export_ready <- reactiveVal(NULL)

    # Write the file now and only then trigger the download.
    #
    # Doing it the other way round — letting the download handler render — makes
    # every failure unreportable: an engine with nothing to draw fails its own
    # `req()`, and Shiny answers the HTTP request with its 500 page, which the
    # browser dutifully saves under the .png name the user asked for. `ready()`
    # cannot catch that on its own, since it only says Generate was pressed, not
    # that the result had any data in it.
    stage <- function(fmt, opts) {
      previous <- isolate(export_ready())
      if (!is.null(previous)) {
        unlink(previous)
      }
      export_ready(NULL)
      f <- tempfile(fileext = paste0(".", fmt))
      ok <- tryCatch(
        {
          exporter$save(f, fmt, opts)
          file.exists(f) && file.size(f) > 0
        },
        error = function(e) FALSE
      )
      if (!ok) {
        unlink(f)
        showNotification(
          "Nothing to export — this plot has no data to draw.",
          type = "error"
        )
        return(invisible(FALSE))
      }
      export_ready(f)
      shinyjs::click("export_file")
      invisible(TRUE)
    }

    # The modal's Export button. Settings are read before the modal closes and
    # carried from here on, so nothing downstream depends on controls that no
    # longer exist.
    reg(observeEvent(input$export_download, {
      fmt <- export_format()
      opts <- export_opts()
      removeModal()
      if (!isTRUE(exporter$ready())) {
        showNotification(
          "Generate a plot before saving it.",
          type = "warning"
        )
        return()
      }
      export_request(list(format = fmt, label = export_label()))
      # A widget's raster only exists once the browser has drawn it, so it
      # arrives asynchronously through capture_data() below and is staged there.
      if (export_widget && !identical(fmt, "html")) {
        exporter$capture(fmt, opts)
      } else {
        stage(fmt, opts)
      }
    }))

    # Arrival of a browser capture: stage the decoded image, then download it.
    if (!is.null(exporter$capture_data)) {
      reg(observeEvent(
        exporter$capture_data(),
        {
          fmt <- isolate(export_request())$format %||% "png"
          previous <- isolate(export_ready())
          if (!is.null(previous)) {
            unlink(previous)
          }
          export_ready(NULL)
          f <- tempfile(fileext = paste0(".", fmt))
          if (!write_data_uri(exporter$capture_data(), f)) {
            unlink(f)
            showNotification(
              "The browser could not capture this plot. Try a smaller target width.",
              type = "error"
            )
            return()
          }
          export_ready(f)
          shinyjs::click("export_file")
        },
        ignoreInit = TRUE
      ))
    }

    # Purely a handover: whatever `stage()` (or the capture observer) already
    # wrote is what gets sent. The staged file is deliberately not deleted here
    # — a browser is free to request a download URL more than once, and clearing
    # it on the first request would answer the second with an empty file.
    output$export_file <- downloadHandler(
      filename = function() {
        req <- isolate(export_request())
        export_filename(req$label %||% plot_type, req$format %||% "png")
      },
      content = function(file) {
        staged <- isolate(export_ready())
        req(staged)
        file.copy(staged, file, overwrite = TRUE)
        invisible(NULL)
      }
    )

    # Load-bearing, and the reason the Save button did nothing at all before.
    # Shiny hands a download link its URL as an *output value*, and outputs
    # default to being suspended while hidden — which this one always is, since
    # it sits in a `d-none` wrapper so the visible control can be an
    # actionButton that validates first. Suspended, it keeps `href=""` and the
    # `disabled` class for the whole session, so clicking it navigates nowhere
    # and the content function never runs.
    outputOptions(output, "export_file", suspendWhenHidden = FALSE)

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

    # Apply what the left sidebar is holding. `priority` is load-bearing: the
    # engine's own Generate observer was created before this one, so at equal
    # priority it would run first and compute from the *previous* selection.
    # Higher priority runs first, so the engine sees the applied selection in the
    # same flush that told it to generate.
    reg(observeEvent(
      input$generate,
      {
        sel <- isolate(selected_isolates())
        applied_selection(sel)
        # Pin the metadata to the same instant the engine computes from, so the
        # topology and the table describing it can never disagree afterwards.
        applied_metadata(.subset_meta(isolate(viz_metadata()), sel))
        applied_metadata_all(.subset_meta(isolate(viz_metadata_all()), sel))
        generated_once(TRUE)
        selection_touched(FALSE)
      },
      ignoreInit = TRUE,
      priority = 100
    ))

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
    # An Analysis stores its fixed set as isolate *names*, so unlike the
    # in-memory selections above it can still be naming isolates that were
    # removed from the database long after it was defined. Reconcile it here,
    # at the one point it is read, rather than rewriting the stored Analysis:
    # the set the user chose is a record of intent, and an isolate that comes
    # back (re-typed, or restored from a backup) should fall back under it.
    #
    # Depends on `isolates` as well as `analyses` for that reason - the stored
    # value has not changed, but what it resolves to has.
    active_analysis_restriction <- reactive({
      db_events$depend(db_rev, "analyses", "isolates")
      aid <- active_analysis_id()
      if (is.null(aid)) {
        return(NULL)
      }
      row <- analysis_store$get_analysis(db_path(), aid)
      if (is.null(row)) {
        return(NULL)
      }
      stored <- parse_static(row$isolate_selection)
      if (is.null(stored)) {
        return(NULL)
      }
      # An Analysis whose isolates are all gone stops restricting rather than
      # restricting to nothing, which would render an empty plot with the
      # controls locked and no way for the user to act on it.
      live <- db_events$reconcile_names(stored, viz_metadata()$isolate)
      if (length(live$kept)) live$kept else NULL
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
        isolate(applied_selection(sel))
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
        db_events$bump(db_rev, "analyses")
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
      # The *applied* selection: a save records the plot that is displayed, not
      # a selection the user has picked and not yet generated.
      effective_selection <- static_sel %||% applied_selection()

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
          easyClose = TRUE
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
        updatePickerInput(session, "na_handling", selected = snap$na_handling)
      }
      updatePickerInput(
        session,
        "imported_sets",
        selected = snap$imported_sets %||% character(0)
      )
      if (!is.null(snap$algo)) {
        updatePrettyRadioButtons(session, "algo", selected = snap$algo)
      }
      # Both, because a restored plot is drawn from the snapshot rather than
      # waiting for the user to press Generate — the synthetic Generate below
      # would otherwise find nothing pending and be disabled.
      selected_isolates(snap$selection)
      applied_selection(snap$selection)
      generated_once(TRUE)

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
        applied_selection(preset$selection)
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
        "save_status",
        "export_hint"
      )) {
        try(outputOptions(output, o, suspend = TRUE), silent = TRUE)
      }
      selected_isolates(NULL)
      applied_selection(NULL)
      applied_metadata(NULL)
      applied_metadata_all(NULL)
      selection_touched(FALSE)
      generated_once(FALSE)
      pending(NULL)
      pending_save_target(NULL)
      # A capture that never reached its download would otherwise leave its
      # temp file behind for the rest of the session.
      leftover <- export_ready()
      if (!is.null(leftover)) {
        unlink(leftover)
      }
      export_ready(NULL)
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
