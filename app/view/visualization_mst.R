# app/view/visualization_mst.R
#
# Minimum Spanning Tree (visNetwork) visualization submodule. Owns its own
# control panel, the visNetwork render, and all MST-specific reactive state.
# Mounted by app/view/visualization.R inside a navset_hidden panel; the shared
# Generate button, plot type, na_handling and per-isolate metadata are forwarded
# in as reactives.
#
# What is drawn — geometry, colour, clustering, legend — is app/logic/mst_plot.R.
# This module is the control panel and the reactive wiring, and it holds two
# arrangements worth knowing about before editing it:
#
# * The widget is rebuilt only when its *shell* changes (see `shell` below).
#   Everything that lives in the node and edge tables — colours, sizes, fonts,
#   labels, positions — is pushed into the running network through a
#   visNetworkProxy instead, which skips tearing down and re-creating the canvas.
#
# * Controls the data can decide are decided by the data (`refit_layout`), the
#   way the Tree fits its aspect ratio and label size. Their values are read from
#   mirrors rather than from `input$`, for the reason set out at `fitted`.

box::use(
  bslib[
    accordion,
    accordion_panel,
    as_fill_carrier,
    card,
    card_body,
    input_switch,
    layout_sidebar,
    nav_panel,
    navset_tab,
    sidebar,
    tooltip,
  ],
  igraph[V, as_data_frame, vcount],
  shiny,
  shinyWidgets[pickerInput, updatePickerInput, updateVirtualSelect],
  stats[setNames],
  visNetwork[
    renderVisNetwork,
    visNetworkOutput,
    visNetworkProxy,
    visUpdateEdges,
    visUpdateNodes,
  ],
)
box::use(
  app / logic / database_functions[load_db_scheme_overview],
  app / logic / date_bins[granularity_label],
  app / logic / db_events,
  app /
    logic /
    field_profile[
      field_profiles_of = field_profiles,
      profile_for,
      scale_categories_for,
    ],
  app / logic / functions[render_info],
  app /
    logic /
    mapping_engine[
      aesthetic_block_reason,
      aesthetic_labels,
      assign_mapping_layer,
      granularity_profile,
      is_date_profile,
      max_layers,
      normalize_layer_records,
      rebalance_layers,
      set_layer_granularity,
    ],
  app /
    logic /
    mst_plot[
      MST_FIT_DEFAULTS,
      MST_LENGTH_MODES,
      MST_MAX_EDGE_MULT,
      build_mst_visnetwork,
      mst_auto_layout,
      mst_frames,
      mst_threshold_default,
      save_mst_html,
    ],
  app / logic / phylo[compute_mst],
  app /
    logic /
    viz_helpers[
      apply_input_snapshot,
      collect_input_snapshot,
      field_select,
      granularity_select,
      layer_action_btn,
      reset_viz_colors,
      scale_select,
      suitable_scale_categories,
      update_field_select,
      viz_color,
    ],
)

# --- Variable mapping layers -------------------------------------------------

# The medium this engine maps onto, in app/logic/mapping_engine.R's terms: node
# fill alone.
MEDIUM <- "mst"

# Canonical shape of one mapping layer. The engine fills these in; the
# snapshot/restore path has to rebuild exactly this shape.
LAYER_DEFAULTS <- list(
  id = NA_character_,
  field = NA_character_,
  title = NA_character_,
  aesthetic = "node_fill",
  palette = "viridis",
  family = "Gradient",
  n_levels = 1L,
  continuous = FALSE,
  transform = NULL,
  granularity = NULL,
  auto = TRUE
)

.normalize_layers <- function(x) {
  out <- normalize_layer_records(x, LAYER_DEFAULTS)
  if (is.null(out)) {
    return(NULL)
  }
  channels <- names(aesthetic_labels(MEDIUM))
  out <- lapply(out, function(l) {
    # An Analysis saved while node border and label colour were still channels
    # names one of them here. Hand it back to the engine as an automatic layer
    # rather than dropping the mapping or drawing it as something it did not
    # ask for.
    if (!isTRUE(l$aesthetic %in% channels)) {
      l$aesthetic <- LAYER_DEFAULTS$aesthetic
      l$auto <- TRUE
    }
    l
  })
  # A layer whose field is gone (the database changed under a saved Analysis)
  # cannot be drawn and must not reach the builder.
  Filter(function(l) !is.na(l$field %||% NA), out)
}

# --- Controls fitted to the data ---------------------------------------------

# Which fitted control takes which field of an mst_auto_layout() fit.
FITTED_FIELDS <- c(
  mst_show_label = "show_label",
  mst_show_edge_label = "show_edge_label",
  mst_length_mode = "length_mode",
  mst_node_size = "node_size",
  mst_node_label_fontsize = "node_font_size",
  mst_edge_font_size = "edge_font_size"
)

# The values those controls hold before any data is loaded. Taken from the logic
# module rather than written out again, because they are also the fit's
# calibration anchor — a slider declared with anything else would quietly move
# it.
FITTED_DEFAULTS <- list(
  mst_show_label = MST_FIT_DEFAULTS$show_label,
  mst_show_edge_label = MST_FIT_DEFAULTS$show_edge_label,
  mst_length_mode = MST_FIT_DEFAULTS$length_mode,
  mst_node_size = MST_FIT_DEFAULTS$node_size,
  mst_node_label_fontsize = MST_FIT_DEFAULTS$node_font_size,
  mst_edge_font_size = MST_FIT_DEFAULTS$edge_font_size
)

# The placeholder threshold, used only until a database says otherwise
# (mst_plot$mst_threshold_default reads the scheme's own complex-type distance).
THRESHOLD_PLACEHOLDER <- 10L

# Everything the render reads through a mirror: the fitted controls above, the
# spread (which the fit leaves alone but which the geometry is built from), the
# label source, and the cluster threshold the scheme resolves. See `fitted`.
MIRRORED_IDS <- c(
  names(FITTED_DEFAULTS),
  "mst_edge_length_scale",
  "mst_node_label",
  "mst_cluster_threshold"
)

# --- MST control tabs --------------------------------------------------------

# `options_ui`: the tab's distance-computation controls, namespaced to the tab
# and rendered here rather than in the left sidebar. See visualization_tree.R.
mst_controls <- function(ns, options_ui = NULL) {
  shiny$tagList(
    navset_tab(
      if (!is.null(options_ui)) {
        nav_panel(
          "Options",
          icon = shiny$icon("gear"),
          options_ui,
          accordion(
            open = "clustering",
            accordion_panel(
              "Clustering",
              icon = shiny$icon("tag"),
              input_switch(ns("mst_show_clusters"), "Show clusters", TRUE),
              shiny$numericInput(
                ns("mst_cluster_threshold"),
                "Threshold (allelic distance)",
                value = THRESHOLD_PLACEHOLDER,
                min = 1,
                max = 10000
              ),
              shiny$div(
                id = ns("mst_threshold_note"),
                class = "text-muted small mb-2"
              ),
              scale_select(ns, "mst_cluster_col_scale", selected = "turbo"),
            ),
            accordion_panel(
              "Cluster Options",
              icon = shiny$icon("sliders"),
              shiny$sliderInput(
                ns("mst_cluster_width"),
                "Width",
                0,
                60,
                20,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("mst_cluster_opacity"),
                "Opacity (%)",
                5,
                100,
                66,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("mst_cluster_label_size"),
                "Label size (0 = off)",
                0,
                48,
                18,
                ticks = FALSE
              ),
              input_switch(
                ns("mst_cluster_label_tint"),
                "Label Colors",
                FALSE
              )
            ),
            accordion_panel(
              "Collapsing",
              icon = shiny$icon("arrows-to-dot"),
              shiny$numericInput(
                ns("mst_collapse_threshold"),
                "Collapse branches ≤ (allelic distance)",
                value = 0,
                min = 0,
                max = 10000
              ),
              shiny$div(
                id = ns("mst_collapse_note"),
                class = "text-muted small mb-2"
              )
            )
          )
        )
      },
      # Nodes -----------------------------------------------------------------
      nav_panel(
        "Nodes",
        icon = shiny$icon("circle"),
        accordion(
          open = "Labels",
          accordion_panel(
            "Labels",
            icon = shiny$icon("tag"),
            tooltip(
              input_switch(
                ns("mst_show_label"),
                "Show node labels",
                FITTED_DEFAULTS$mst_show_label
              ),
              paste(
                "A node holding several identical isolates lists the first few",
                "and counts the rest. Hover any node for all of them."
              )
            ),
            field_select(
              ns,
              "mst_node_label",
              "Label source",
              extra = c(Isolate = "isolate"),
              selected = "isolate"
            ),
            shiny$sliderInput(
              ns("mst_node_label_fontsize"),
              "Font size",
              6,
              30,
              FITTED_DEFAULTS$mst_node_label_fontsize,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Size",
            icon = shiny$icon("arrows-up-down"),
            tooltip(
              input_switch(ns("mst_scale_nodes"), "Scale by duplicates", TRUE),
              "Node area is proportional to the number of isolates it holds."
            ),
            shiny$uiOutput(ns("mst_node_size_ui"))
          ),
          accordion_panel(
            "Shadow",
            icon = shiny$icon("circle-dot"),
            input_switch(ns("mst_shadow"), "Show shadow", TRUE)
          )
        )
      ),
      # Edges -----------------------------------------------------------------
      nav_panel(
        "Edges",
        icon = shiny$icon("grip-lines-vertical"),
        accordion(
          open = c("Length"),
          accordion_panel(
            "Length",
            icon = shiny$icon("grip-lines"),
            # Edge length is a named transform, not a multiplier on an unnamed
            # one. See mst_plot$MST_LENGTH_MODES for why these three.
            pickerInput(
              ns("mst_length_mode"),
              "Length",
              choices = MST_LENGTH_MODES,
              selected = FITTED_DEFAULTS$mst_length_mode
            ),
            shiny$sliderInput(
              ns("mst_edge_length_scale"),
              "Spread",
              3,
              60,
              MST_FIT_DEFAULTS$spread,
              ticks = FALSE
            ),
            tooltip(
              shiny$div(
                id = ns("mst_shorten_wrap"),
                input_switch(
                  ns("mst_shorten_long"),
                  "Shorten long branches",
                  TRUE
                )
              ),
              paste(
                "Applies only while branch length carries the allelic distance.",
                "With equal lengths there is nothing to shorten."
              )
            ),
            tooltip(
              shiny$div(
                id = ns("mst_cap_mult_wrap"),
                class = "viz-slider-row",
                shiny$sliderInput(
                  ns("mst_cap_mult"),
                  "Cap at (x median)",
                  2,
                  20,
                  MST_MAX_EDGE_MULT,
                  step = 1,
                  ticks = FALSE
                )
              ),
              paste(
                "A branch longer than this multiple of the median is capped",
                "and dashed rather than drawn to scale; its distance stays on",
                "the label."
              )
            )
          ),
          accordion_panel(
            "Labels",
            icon = shiny$icon("code-branch"),
            input_switch(
              ns("mst_show_edge_label"),
              "Show allelic distances",
              FITTED_DEFAULTS$mst_show_edge_label
            ),
            shiny$sliderInput(
              ns("mst_edge_font_size"),
              "Font size",
              6,
              30,
              FITTED_DEFAULTS$mst_edge_font_size,
              ticks = FALSE
            )
          )
        )
      ),
      # Variable mapping -------------------------------------------------------
      nav_panel(
        "Mapping",
        icon = shiny$icon("palette"),
        # One picker, the same arrangement as the Tree, though the MST has only
        # one channel to give a variable: node fill. Picking a variable adds a
        # layer and app/logic/mapping_engine.R decides its palette, from the
        # variable's own profile.
        tooltip(
          field_select(ns, "mst_layer_add", "Map a variable"),
          paste(
            "A node can stand for several identical isolates; fill shows",
            "every value they carry, as a pie."
          )
        ),
        shiny$uiOutput(ns("mst_layers_ui"))
      ),
      # colors ----------------------------------------------------------------
      nav_panel(
        "Colors",
        icon = shiny$icon("fill-drip"),
        shiny$div(
          class = "viz-color-grid",
          viz_color(ns, "mst_text_color", "Text", "#000000"),
          viz_color(ns, "mst_color_node", "Nodes", "#B2FACA"),
          # No separate node-border swatch: the outline is the branch colour, so
          # the graph's linework is one decision instead of two that had to be
          # kept in agreement by hand.
          viz_color(ns, "mst_color_edge", "Edges & node borders", "#000000"),
          viz_color(ns, "mst_edge_font_color", "Edge Font", "#000000"),
          viz_color(ns, "mst_background_color", "Background", "#ffffff"),
          input_switch(
            ns("mst_background_transparent"),
            "Transparent background",
            TRUE
          )
        )
      ),
      # Layout -----------------------------------------------------------------
      nav_panel(
        "Layout",
        icon = shiny$icon("sliders"),
        open = c("Orientation", "Legend", "Caption"),
        accordion(
          accordion_panel(
            "Orientation",
            icon = shiny$icon("compass"),
            shiny$sliderInput(
              ns("mst_rotation"),
              "Rotation",
              -180,
              180,
              0,
              step = 5,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Legend",
            icon = shiny$icon("list"),
            input_switch(ns("mst_show_legend"), "Show legend", TRUE),
            pickerInput(
              ns("mst_legend_ori"),
              "Position",
              c(Left = "left", Right = "right")
            )
          ),
          accordion_panel(
            "Caption",
            icon = shiny$icon("closed-captioning"),
            input_switch(
              ns("mst_show_scale_caption"),
              "Show caption",
              TRUE
            )
          )
        )
      )
    ),
    shiny$div(
      class = "reset-buttons",
      shiny$actionButton(
        ns("reset_settings"),
        "Reset settings",
        icon = shiny$icon("rotate-left"),
        width = "100%"
      )
    )
  )
}

#' @export
ui <- function(id, generate_id, options_ui = NULL) {
  ns <- shiny$NS(id)

  layout_sidebar(
    # `padding`, not a CSS id: layout_sidebar has no `id` formal, so an `id`
    # would be spliced onto the inner .main div — and with one engine instance
    # per plot tab that id is no longer unique anyway.
    padding = 0,
    border = FALSE,
    sidebar = sidebar(
      id = ns("controls_sidebar"),
      class = "viz-controls-sidebar",
      position = "right",
      width = 380,
      open = TRUE,
      fillable = TRUE,
      as_fill_carrier(
        shiny$div(
          id = ns("controls_wrap"),
          class = "viz-nav-wrap",
          mst_controls(ns, options_ui)
        )
      )
    ),
    shinyjs::useShinyjs(),
    # Loads waiter.js so the flower spinner used in the loading overlay is styled.
    waiter::useWaiter(),
    # Loading overlay: shown the moment Generate (parent namespace) is clicked,
    # scoped to this engine's own stage id. Cleared by the network's first
    # completed draw (mst_plot$MST_DRAWN_JS) — there is no stabilization event to
    # wait for any more, because there is no physics. The timeout is a safety net.
    shiny$tags$script(
      shiny$HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var out='",
          ns("mst_plot"),
          "';var stage='",
          ns("plot_stage"),
          "';var timer;",
          "function set(on){var s=document.getElementById(stage);if(!s)return;",
          # Ignore Generate clicks while this engine's panel is hidden (the
          # sibling engine is active) — offsetParent is null when display:none.
          "if(on&&s.offsetParent===null)return;",
          "s.classList.toggle('is-loading',on);",
          "if(on){var p=s.querySelector('.viz-plot-prompt');if(p)p.style.display='none';",
          "clearTimeout(timer);timer=setTimeout(function(){set(false);},45000);}",
          "else{clearTimeout(timer);}}",
          "$(document).on('click','#'+gen.replace(/([:.])/g,'\\\\$1'),",
          "function(){set(true);});",
          "$(document).on('shiny:error',",
          "function(e){if(e.target.id===out)set(false);});",
          "})();"
        )
      )
    ),
    card(
      full_screen = TRUE,
      class = "plot-card",
      card_body(shiny$uiOutput(ns("plot_area")))
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  db_rev = db_events$new_bus(),
  session_reset = shiny$reactive(0L),
  viz_metadata = shiny$reactive(NULL),
  # Per-column profile of the metadata: declared type, distinct-value count,
  # coverage and group, built once by the coordinator
  # (app/logic/field_profile.R). Field pickers read it so every engine
  # describes a variable the same way.
  field_profiles = shiny$reactive(NULL),
  selected_isolates = shiny$reactive(NULL),
  na_handling = shiny$reactive("ignore_na"),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("MST"),
  # Staged peer typing results (Database > Import) folded into the distance
  # matrix; NULL means local isolates only.
  imported_sets = shiny$reactive(NULL)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Mirrors of the fitted controls, and the only thing the render reads for
    # them — never input$mst_node_size and friends directly.
    #
    # updateSliderInput does not set an input; it sends a message to the browser,
    # which applies it and echoes the new value back as an input change a flush
    # later. A widget built from the inputs is therefore built once with the
    # stale values and again with the fitted ones — once more per control, since
    # each echoes separately. Writing the fit into these mirrors *before* the
    # graph is published makes the first build the correct one; the echo that
    # follows then assigns a mirror the value it already holds, which shiny does
    # not treat as a change. No delays and no ordering assumptions, so there is
    # nothing to race.
    fitted <- do.call(
      shiny$reactiveValues,
      c(
        FITTED_DEFAULTS,
        list(
          mst_edge_length_scale = MST_FIT_DEFAULTS$spread,
          mst_node_label = "isolate",
          mst_cluster_threshold = THRESHOLD_PLACEHOLDER
        )
      )
    )

    # all.equal, not identical: the browser can echo 0.6 back as 0.6000000000001
    # and a bit-exact test would read that as a fresh user edit.
    set_fitted <- function(id, value) {
      if (!isTRUE(all.equal(shiny$isolate(fitted[[id]]), value))) {
        fitted[[id]] <- value
      }
    }

    # A user drag lands in the same mirror the fit writes to, so the two are
    # indistinguishable downstream and whichever happened last simply wins.
    lapply(MIRRORED_IDS, function(id) {
      shiny$observeEvent(input[[id]], set_fitted(id, input[[id]]))
    })

    # Whether a plot has been generated for this engine. Retained across
    # plot-type switches (only session reset clears it).
    generated <- shiny$reactiveVal(FALSE)

    # The mapping layers, in draw order. Written only by explicit user action
    # (add, delete, edit) and by a restore, which is why — unlike the fitted
    # controls — they need no mirror: there is no server-sent value for the
    # browser to echo back a flush later.
    mst_layers <- shiny$reactiveVal(list())
    mst_layer_seq <- shiny$reactiveVal(0L)

    # Every column's profile, for the variable pickers and the mapping engine.
    profiles <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(NULL)
      }
      field_profiles() %||% field_profiles_of(meta)
    })

    # A profile frame fit to label the node label source itself picker.
    #
    # `field_profiles()` marks a column `groupable` only while it partitions
    # isolates into more than one bucket, which `isolate` never does — every
    # row is its own value. That is the right rule for a *mapping* (fill has
    # nothing to show for a variable that cannot group), and the wrong one
    # for a *label*: any column can caption a node, grouping or not, and
    # Isolate is the one column that has to stay pickable. update_field_select()
    # disables whatever `!groupable` names, so a copy with every row forced
    # groupable is what keeps the label picker from greying out its own
    # default.
    label_profile <- function() {
      prof <- profiles()
      if (is.null(prof) || !nrow(prof)) {
        return(NULL)
      }
      prof$groupable <- TRUE
      prof
    }

    # mst_node_label's and mst_layer_add's *choices* are swapped out for the
    # loaded database's actual metadata columns — the UI-declared choices are
    # placeholders shown before any data is loaded. shinyjs::reset() only knows
    # how to restore the selected *value* it captured at page load (back when
    # those placeholders were current); it never restores `choices`, so after
    # Generate has swapped them out that captured value usually is not among the
    # select's current options any more, leaving the control visibly blank.
    # force_default = TRUE (Reset settings) always jumps to Isolate;
    # force_default = FALSE (Generate, a fresh database) keeps a still-valid
    # selection, so re-Generating does not clobber a deliberate choice.
    populate_metadata_selects <- function(force_default = FALSE) {
      prof <- profiles()
      label_prof <- label_profile()
      if (is.null(prof) || !nrow(prof)) {
        return(invisible(NULL))
      }

      label_choice <- if (
        !force_default && isTRUE(input$mst_node_label %in% prof$field)
      ) {
        input$mst_node_label
      } else {
        "isolate"
      }
      update_field_select(
        session,
        "mst_node_label",
        label_prof,
        selected = label_choice
      )
      set_fitted("mst_node_label", label_choice)

      # `isolate` names every node uniquely; it cannot group them, so it is
      # left out of the *mapping* picker specifically.
      mappable <- prof[prof$field != "isolate", , drop = FALSE]
      if (nrow(mappable)) {
        update_field_select(session, "mst_layer_add", mappable)
      }
    }

    # Fill the pickers as soon as a database is loaded rather than waiting for a
    # Generate: until then every variable picker lists the placeholder names
    # viz_helpers declares them with — "Isolation Date", "Host", "Country" —
    # which are not columns of any real database.
    shiny$observeEvent(
      profiles(),
      populate_metadata_selects(force_default = FALSE),
      ignoreNULL = TRUE
    )

    # --- Cluster threshold from the scheme ---------------------------------

    # The scheme's published complex-type distance, read once per database.
    scheme_threshold <- shiny$reactive({
      db_events$depend(db_rev, "schema")
      path <- db_path()
      if (is.null(path) || !nzchar(path) || !file.exists(path)) {
        return(NULL)
      }
      overview <- tryCatch(
        load_db_scheme_overview(path),
        error = function(e) NULL
      )
      if (is.null(overview) || !nrow(overview)) {
        return(NULL)
      }
      value <- mst_threshold_default(overview, fallback = NA_integer_)
      if (is.na(value)) NULL else value
    })

    # Default the threshold to the scheme's own number, and say where it came
    # from — a threshold of 12 the user did not choose has to be attributable,
    # or it reads as an arbitrary default.
    apply_scheme_threshold <- function(force_default = FALSE) {
      value <- scheme_threshold()
      shinyjs::html(
        id = "mst_threshold_note",
        html = if (is.null(value)) {
          paste(
            "This scheme publishes no complex-type distance;",
            THRESHOLD_PLACEHOLDER,
            "is a placeholder. Set the distance your cluster definition uses."
          )
        } else {
          sprintf(
            "%d is this scheme's own complex-type distance (cgMLST.org).",
            value
          )
        }
      )
      if (is.null(value)) {
        return(invisible(NULL))
      }
      # Only while the user has not moved it: an explicit threshold is the whole
      # point of the control.
      untouched <- force_default ||
        isTRUE(all.equal(
          shiny$isolate(fitted$mst_cluster_threshold),
          THRESHOLD_PLACEHOLDER
        ))
      if (untouched) {
        shiny$updateNumericInput(
          session,
          "mst_cluster_threshold",
          value = value
        )
        set_fitted("mst_cluster_threshold", value)
      }
      invisible(NULL)
    }

    shiny$observeEvent(
      scheme_threshold(),
      apply_scheme_threshold(),
      ignoreNULL = FALSE
    )

    # --- Node size: one slider, one or two handles -------------------------

    output$mst_node_size_ui <- shiny$renderUI({
      scaled <- isTRUE(input$mst_scale_nodes)
      value <- shiny$isolate(fitted$mst_node_size)
      shiny$sliderInput(
        ns("mst_node_size"),
        if (scaled) "Size (smallest – largest)" else "Size",
        4,
        60,
        value = if (scaled) {
          if (length(value) >= 2L) {
            value
          } else {
            c(MST_FIT_DEFAULTS$node_size_min, value[[1]])
          }
        } else {
          value[[length(value)]]
        },
        ticks = FALSE
      )
    })

    # The cap only matters under "Proportional": "Equal length" has no distance
    # in the branch to shorten, and "Logarithmic" already compresses the range
    # so the cap would not fire (see mst_plot$mst_length_mode). Disabled rather
    # than hidden, so the tooltip on its wrapper can say why. The cap multiplier
    # is the same question one level down: it means nothing while the cap
    # itself is off, whether that is because of the length mode or because the
    # user switched it off directly.
    apply_capping_state <- function() {
      capping <- identical(fitted$mst_length_mode, "real")
      shinyjs::toggleState(id = "mst_shorten_long", condition = capping)
      shinyjs::toggleClass(
        id = "mst_shorten_wrap",
        class = "is-disabled",
        condition = !capping
      )
      capping <- capping && isTRUE(input$mst_shorten_long)
      shinyjs::toggleState(id = "mst_cap_mult", condition = capping)
      shinyjs::toggleClass(
        id = "mst_cap_mult_wrap",
        class = "is-disabled",
        condition = !capping
      )
    }
    shiny$observe(apply_capping_state())
    # The observer's very first run fires before this tab's freshly-inserted
    # markup has reached the browser, so the toggleState message it sends has
    # nothing to attach to and is silently dropped — same race as
    # populate_metadata_selects() below. One isolated, deferred re-application
    # after mount is enough; every later change is a live reactive update with
    # the DOM already in place.
    shinyjs::delay(400, shiny$isolate(apply_capping_state()))

    # Grey out the node colour swatch while a mapping layer owns the fill: a
    # fixed colour loses to the per-node one, so the swatch is genuinely dead.
    shiny$observe({
      owned <- any(vapply(
        mst_layers(),
        function(l) identical(l$aesthetic, "node_fill"),
        logical(1)
      ))
      shinyjs::toggleClass(
        id = "mst_color_node_row",
        class = "is-disabled",
        condition = owned
      )
    })

    # --- Reset --------------------------------------------------------------

    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path below, so both routes return the controls
    # identically.
    #
    # shinyjs::reset() can't reach colorPickr (every "colors" tab swatch) — see
    # reset_viz_colors() in viz_helpers.R for why — so those are patched up
    # explicitly right after the blanket reset.
    #
    # populate_metadata_selects() has to be deferred with shinyjs::delay()
    # rather than called right after shinyjs::reset(): mst_node_label *is* a
    # plain <select> that shinyjs::reset() recognises and restores — but only
    # asynchronously (it round-trips through the browser to read back each
    # resettable element's page-load value before calling the matching
    # update*Input() on the server). A same-tick call would run first and
    # shinyjs's own stale restoration would land after it and overwrite it: the
    # control resets fine, then silently reverts to blank a moment later.
    reset_mst_settings <- function() {
      shinyjs::reset(id = "controls_wrap")

      reset_viz_colors(
        session,
        mst_text_color = "#000000",
        mst_color_node = "#B2FACA",
        mst_color_edge = "#000000",
        mst_edge_font_color = "#000000",
        mst_background_color = "#ffffff"
      )
      mst_layers(list())
      mst_layer_seq(0L)
      for (id in names(FITTED_DEFAULTS)) {
        set_fitted(id, FITTED_DEFAULTS[[id]])
      }
      set_fitted("mst_edge_length_scale", MST_FIT_DEFAULTS$spread)
      shinyjs::delay(400, {
        populate_metadata_selects(force_default = TRUE)
        apply_scheme_threshold(force_default = TRUE)
      })
    }

    shiny$observeEvent(input$reset_settings, reset_mst_settings())

    # --- Variable mapping layers -------------------------------------------

    next_layer_id <- function() {
      n <- mst_layer_seq() + 1L
      mst_layer_seq(n)
      paste0("L", n)
    }

    shiny$observeEvent(input$mst_layer_add, {
      field <- input$mst_layer_add
      shiny$req(nzchar(field %||% ""))
      # Clear the picker straight away so the same variable can be re-picked
      # after a delete, and so the selection cannot re-fire on a later flush.
      updateVirtualSelect(
        inputId = "mst_layer_add",
        session = session,
        selected = character(0)
      )

      layers <- mst_layers()
      if (
        any(vapply(layers, function(l) identical(l$field, field), logical(1)))
      ) {
        return()
      }
      if (length(layers) >= max_layers(MEDIUM)) {
        shiny$showNotification(
          paste(
            "An MST node can show only one mapped variable at a time. Remove",
            "it first."
          ),
          type = "warning"
        )
        return()
      }
      prof <- profile_for(profiles(), field)
      layer <- assign_mapping_layer(
        prof,
        layers,
        next_layer_id(),
        MEDIUM,
        viz_metadata()[[field]]
      )
      if (is.null(layer)) {
        shiny$showNotification(
          aesthetic_block_reason(prof, NULL, MEDIUM) %||%
            "That variable cannot be mapped.",
          type = "warning"
        )
        return()
      }
      mst_layers(c(layers, list(layer)))
    })

    # One delegated handler per action rather than one observer per row: an
    # observeEvent created inside renderUI is re-registered on every render, so
    # the ids push their own value into a single input instead.
    shiny$observeEvent(input$mst_layer_delete, {
      keep <- Filter(
        function(l) !identical(l$id, input$mst_layer_delete),
        mst_layers()
      )
      mst_layers(rebalance_layers(keep, profiles(), MEDIUM, viz_metadata()))
    })

    output$mst_layers_ui <- shiny$renderUI({
      layers <- mst_layers()
      if (!length(layers)) {
        return(shiny$div(
          class = "text-muted fst-italic mb-2 tree-layer-empty",
          "No mappings yet."
        ))
      }
      labels <- aesthetic_labels(MEDIUM)
      shiny$div(
        class = "tree-layer-list",
        lapply(layers, function(l) {
          shiny$div(
            class = "tree-layer-card",
            shiny$div(
              class = "tree-layer_body",
              shiny$div(class = "tree-layer_title", title = l$title, l$title),
              shiny$div(
                class = "tree-layer_meta",
                paste(
                  labels[[l$aesthetic]] %||% l$aesthetic,
                  "·",
                  sprintf("%d values", l$n_levels),
                  if (!is.null(granularity_label(l$granularity))) {
                    paste("· by", tolower(granularity_label(l$granularity)))
                  },
                  if (!is.null(l$palette)) paste("·", l$palette)
                )
              )
            ),
            layer_action_btn(ns, "mst_layer_edit", l$id, "pen", "Edit mapping"),
            layer_action_btn(
              ns,
              "mst_layer_delete",
              l$id,
              "xmark",
              "Remove mapping"
            )
          )
        })
      )
    })

    editing <- shiny$reactiveVal(NULL)

    shiny$observeEvent(input$mst_layer_edit, {
      layers <- mst_layers()
      hit <- Filter(function(l) identical(l$id, input$mst_layer_edit), layers)
      shiny$req(length(hit))
      l <- hit[[1]]
      prof <- profile_for(profiles(), l$field)
      shiny$req(!is.null(prof))
      editing(l$id)

      values <- viz_metadata()[[l$field]]
      # The palette has to suit the variable as the chosen granularity leaves
      # it: binned to months it is a category, not a continuum.
      binned <- granularity_profile(prof, values, l$granularity)
      cats <- scale_categories_for(
        if (isTRUE(binned$continuous)) values else as.character(values),
        suitable_scale_categories(
          if (isTRUE(binned$continuous)) "Numeric" else "Factor",
          values
        )
      )

      # No "Show as" picker: node fill is the medium's only channel, so what
      # is left to decide is the palette and, for a date, its grouping.
      shiny$showModal(shiny$modalDialog(
        title = paste("Mapping:", l$title),
        size = "s",
        easyClose = TRUE,
        if (is_date_profile(prof)) {
          granularity_select(ns, "mst_layer_granularity", l$granularity)
        },
        scale_select(
          ns,
          "mst_layer_palette",
          categories = cats,
          selected = l$palette
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("mst_layer_apply"), "Apply")
        )
      ))
    })

    shiny$observeEvent(input$mst_layer_apply, {
      id <- editing()
      shiny$req(!is.null(id))
      layers <- lapply(mst_layers(), function(l) {
        if (!identical(l$id, id)) {
          return(l)
        }
        l$palette <- input$mst_layer_palette %||% l$palette
        l <- set_layer_granularity(
          l,
          input$mst_layer_granularity,
          viz_metadata()[[l$field]]
        )
        # Pinned: rebalance_layers() rebuilds automatic layers from scratch and
        # would discard the palette just chosen.
        l$auto <- FALSE
        l
      })
      mst_layers(rebalance_layers(layers, profiles(), MEDIUM, viz_metadata()))
      editing(NULL)
      shiny$removeModal()
    })

    # --- The graph ---------------------------------------------------------

    # The computed MST. Held in a reactiveVal (not an eventReactive) so a
    # Generate for the *other* engine — which also ticks the shared `generate()`
    # — leaves this engine's last result untouched. It is (re)computed only by
    # the guarded Generate observer below when MST is the active engine.
    mst_obj <- shiny$reactiveVal(NULL)

    # Fit the controls the data can decide: the length transform (a scheme whose
    # distances span three orders of magnitude cannot be drawn proportionally),
    # the node and font sizes (which follow from the shortest edge), and whether
    # labels can be drawn at all.
    # How long the labels will be, for the half of the fit that is about whether
    # they fit at all. The label source is a metadata column; `isolate` is the
    # node's own newline-joined membership list, whose longest *line* is what
    # matters rather than its total length.
    label_chars <- function(graph) {
      field <- shiny$isolate(fitted$mst_node_label)
      meta <- shiny$isolate(viz_metadata())
      vals <- if (
        !identical(field, "isolate") &&
          !is.null(meta) &&
          isTRUE(field %in% names(meta))
      ) {
        meta[[field]]
      } else {
        unlist(strsplit(V(graph)$name, "\n", fixed = TRUE))
      }
      suppressWarnings(max(nchar(as.character(vals)), 1L))
    }

    refit_layout <- function(graph, notify = FALSE) {
      if (is.null(graph)) {
        return(invisible(NULL))
      }
      edges <- as_data_frame(graph, what = "edges")
      fit <- mst_auto_layout(
        vcount(graph),
        edges$weight,
        spread = shiny$isolate(fitted$mst_edge_length_scale),
        label_chars = label_chars(graph)
      )

      for (id in names(FITTED_FIELDS)) {
        value <- fit[[FITTED_FIELDS[[id]]]]
        if (identical(id, "mst_node_size")) {
          # One slider, one or two handles: the fitted value has to match the
          # shape the slider currently has (see output$mst_node_size_ui).
          value <- if (isTRUE(shiny$isolate(input$mst_scale_nodes))) {
            c(fit$node_size_min, fit$node_size_max)
          } else {
            fit$node_size
          }
        }
        set_fitted(id, value)
        if (id %in% c("mst_show_label", "mst_show_edge_label")) {
          bslib::update_switch(id, value = isTRUE(value), session = session)
        } else if (identical(id, "mst_length_mode")) {
          updatePickerInput(session, id, selected = value)
        } else {
          shiny$updateSliderInput(session, id, value = value)
        }
      }

      if (notify && !isTRUE(fit$labels_legible)) {
        shiny$showNotification(
          "Node labels disabled: too many nodes to display legibly. Hover a node to view it.",
          type = "warning",
          duration = 8
        )
      }
      invisible(fit)
    }

    # There is deliberately no refit when the label source changes any more. It
    # used to exist because the source decides whether the labels fit —
    # label_chars() above still reflects a source change correctly — but a
    # refit also touched "Show node labels" every time, and a fit that
    # concluded the *new* source's labels were illegible switched the user's
    # own choice back off without being asked. The fit now runs on Generate
    # alone; changing the source after that changes what the labels say, never
    # whether they are shown.

    # Fires on Generate, and on the computation options that now live in this
    # engine's own sidebar rather than the tab's left one — those are on the live
    # side of the split. The isolate selection is not among them: the tab only
    # publishes it once Generate has applied it (see applied_selection in
    # visualization_plot.R). See visualization_tree.R for the same shape.
    shiny$observeEvent(
      list(generate(), na_handling(), imported_sets()),
      {
        if (!identical(plot_type(), "MST")) {
          return()
        }
        if (!isTRUE(generated()) && !isTRUE((generate() %||% 0L) > 0L)) {
          return()
        }

        populate_metadata_selects()

        graph <- tryCatch(
          compute_mst(
            db_path(),
            na_handling(),
            selected_isolates(),
            imported_sets()
          ),
          error = function(e) {
            shiny$showNotification(
              paste("MST computation failed:", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )
        if (is.null(graph)) {
          shiny$showNotification(
            "Could not build an MST: need at least 2 isolates in the database.",
            type = "warning"
          )
          # No network will render, so the draw event that normally clears the
          # loading overlay never fires — hide it now instead of leaving the
          # spinner up until the 45s client-side safety timeout.
          shinyjs::removeClass(id = "plot_stage", class = "is-loading")
        } else {
          refit_layout(graph, notify = TRUE)
        }
        mst_obj(graph)

        generated(TRUE)
      }
    )

    # --- Options, frames, widget -------------------------------------------

    # The canvas the legend is arranged against.
    #
    # isolate(), and it has to be explicit: session$clientData is a
    # reactiveValues, so merely reading it inside a reactive takes a dependency
    # on it. Without this every browser report of a new size — the first layout
    # pass, a font finishing loading, a scrollbar appearing, a window drag —
    # invalidates mst_opts(), which rebuilds every frame and pushes it into the
    # network again. The comment here used to claim there was no dependency;
    # there was, and it is a large part of what put three "Rendering mst_plot"
    # lines in the log for a single Generate.
    canvas_px <- function() {
      shiny$isolate({
        w <- session$clientData[[paste0("output_", ns("mst_plot"), "_width")]]
        h <- session$clientData[[paste0("output_", ns("mst_plot"), "_height")]]
        c(
          if (is.null(w) || w < 200) 900 else w,
          if (is.null(h) || h < 200) 620 else h
        )
      })
    }

    # Resolved control values, gathered once so the live render, the incremental
    # update and the HTML export share an identical configuration. Fitted
    # controls are read from `fitted`, never from `input`.
    mst_opts <- shiny$reactive(
      list(
        # Labels.
        show_label = isTRUE(fitted$mst_show_label),
        field = fitted$mst_node_label,
        label_lines = MST_FIT_DEFAULTS$label_lines,
        show_edge_label = isTRUE(fitted$mst_show_edge_label),
        node_font_size = fitted$mst_node_label_fontsize,
        edge_font_size = fitted$mst_edge_font_size,
        # Colours. The node outline is the edge colour, and the node label the
        # text colour; neither is a control of its own any more.
        node_font_color = input$mst_text_color,
        node_color = input$mst_color_node,
        edge_color = input$mst_color_edge,
        edge_font_color = input$mst_edge_font_color,
        background = input$mst_background_color,
        transparent = input$mst_background_transparent,
        # Sizing.
        scale_nodes = input$mst_scale_nodes,
        node_size = fitted$mst_node_size,
        shadow = input$mst_shadow,
        length_mode = fitted$mst_length_mode,
        spread = fitted$mst_edge_length_scale,
        shorten_long = input$mst_shorten_long,
        cap_mult = input$mst_cap_mult,
        rotation = input$mst_rotation,
        collapse_threshold = input$mst_collapse_threshold,
        # Mapping.
        layers = mst_layers(),
        # Clustering.
        show_clusters = input$mst_show_clusters,
        cluster_threshold = fitted$mst_cluster_threshold,
        cluster_col_scale = input$mst_cluster_col_scale,
        cluster_width = input$mst_cluster_width,
        cluster_opacity = (input$mst_cluster_opacity %||% 35) / 100,
        cluster_label_size = input$mst_cluster_label_size,
        cluster_label_tint = input$mst_cluster_label_tint,
        # Legend.
        show_legend = input$mst_show_legend,
        legend_ori = input$mst_legend_ori,
        show_caption = input$mst_show_scale_caption,
        canvas_px = canvas_px()
      )
    )

    # Node and edge tables. Everything in here can be pushed into a running
    # network; see `shell` for what cannot.
    frames <- shiny$reactive({
      shiny$req(mst_obj())
      mst_frames(mst_obj(), viz_metadata(), mst_opts())
    })

    # How many branches the threshold folded, and into how few nodes — stated
    # plainly because that count is not otherwise visible anywhere on the
    # canvas, and "0 collapsed" says just as clearly that the control is
    # currently a no-op.
    shiny$observe({
      fr <- frames()
      shinyjs::html(
        id = "mst_collapse_note",
        html = if (isTRUE(fr$collapsed > 0L)) {
          sprintf(
            "%d branch%s collapsed, leaving %d node%s.",
            fr$collapsed,
            if (fr$collapsed == 1L) "" else "es",
            nrow(fr$nodes),
            if (nrow(fr$nodes) == 1L) "" else "s"
          )
        } else {
          ""
        }
      )
    })

    # What the widget's *shell* is built from: the parts that live in the
    # widget's options rather than in its data, and so cannot be updated in
    # place — the canvas background, the legend, the custom node renderer, and
    # the beforeDrawing hook that paints the cluster regions. A change to any of
    # these rebuilds the network; a change to anything else is a data push.
    shell <- shiny$reactive(
      list(
        graph = mst_obj(),
        # Collapsing changes which nodes exist, not just what is drawn on
        # them — the incremental path only updates and adds rows by id, it
        # never removes one, so a node the threshold has just folded away
        # would be stranded on screen forever without a full rebuild.
        collapse_threshold = input$mst_collapse_threshold,
        rotation = input$mst_rotation,
        transparent = input$mst_background_transparent,
        background = input$mst_background_color,
        shadow = input$mst_shadow,
        legend = input$mst_show_legend,
        legend_ori = input$mst_legend_ori,
        show_caption = input$mst_show_scale_caption,
        # Both the legend's type and an untinted cluster caption are painted by
        # hooks, so the text colour is a shell input as well as node data.
        text_color = input$mst_text_color,
        # Any mapping at all installs the pie/border renderer, and the
        # legend's keys come from the layers.
        layers = mst_layers(),
        # The cluster regions are painted by a hook, and their geometry follows
        # the coordinates and the node radii. Everything the hook is built from
        # belongs here, right down to its opacity — a beforeDrawing handler is
        # baked into the widget and cannot be pushed through the proxy.
        clusters = input$mst_show_clusters,
        cluster_threshold = fitted$mst_cluster_threshold,
        cluster_col_scale = input$mst_cluster_col_scale,
        cluster_width = input$mst_cluster_width,
        cluster_opacity = input$mst_cluster_opacity,
        cluster_label_size = input$mst_cluster_label_size,
        cluster_label_tint = input$mst_cluster_label_tint,
        scale_nodes = input$mst_scale_nodes,
        node_size = fitted$mst_node_size,
        length_mode = fitted$mst_length_mode,
        spread = fitted$mst_edge_length_scale,
        shorten_long = input$mst_shorten_long,
        cap_mult = input$mst_cap_mult,
        # Rotation moves every coordinate, and the cluster regions are baked
        # from those coordinates at build time — so it belongs here for the
        # same reason spread and length_mode do.
        rotation = input$mst_rotation
      )
    )

    # The frames currently on the canvas, so an incremental update can tell
    # whether it has anything to do.
    drawn <- shiny$reactiveVal(NULL)

    # The shell as last *published*, republished only when it differs from what
    # it held before.
    #
    # Depending on shell() directly made the rebuild invalidation-driven: shiny
    # never compares a reactive's value, so any control that merely reports a
    # change tore the network down and built it again, whether or not the change
    # meant anything. A Generate touches a great many of them — refit_layout
    # writes six mirrors and then sends six update*Input() messages, each of
    # which the browser echoes back in its own flush — and every echo was a
    # rebuilt canvas. Comparing the resolved value makes it value-driven
    # instead, which no echo can defeat.
    #
    # priority: the gate has to settle before outputs are recalculated in the
    # same flush, or the widget is built once from the stale value and again
    # from the fresh one — the very thing this exists to stop. This is the
    # arrangement visualization_tree.R uses for the same reason; see
    # `plot_inputs` there.
    shell_built <- shiny$reactiveVal(NULL)
    shiny$observe(
      {
        shiny$req(mst_obj())
        current <- shell()
        if (!identical(shiny$isolate(shell_built()), current)) {
          shell_built(current)
        }
      },
      priority = 100
    )

    output$mst_plot <- renderVisNetwork({
      render_info("visualization_mst mst_plot")
      shiny$req(shell_built())
      fr <- shiny$isolate(frames())
      shiny$isolate(drawn(fr))
      build_mst_visnetwork(
        shiny$isolate(mst_obj()),
        shiny$isolate(viz_metadata()),
        shiny$isolate(mst_opts()),
        fr
      )
    })

    # The incremental path: push the new node and edge tables into the network
    # already on screen. This is what a colour, a font size or a label change
    # costs now — no teardown, no re-created canvas, and (since the layout is
    # computed in R) nothing to stabilise.
    shiny$observe({
      fr <- frames()
      previous <- shiny$isolate(drawn())
      # Nothing on screen yet: the render above will draw it.
      if (is.null(previous) || identical(previous, fr)) {
        return()
      }
      shiny$isolate(drawn(fr))
      proxy <- visNetworkProxy(ns("mst_plot"))
      visUpdateNodes(proxy, fr$nodes)
      visUpdateEdges(proxy, fr$edges)
    })

    # Top-level app-reset: clear the computed graph so the stale visNetwork is
    # torn down (not just hidden behind the re-shown prompt), and restore the
    # sidebar controls to their defaults — mirroring the local "Reset settings"
    # button.
    shiny$observeEvent(
      session_reset(),
      {
        mst_obj(NULL)
        drawn(NULL)
        shell_built(NULL)
        generated(FALSE)
        reset_mst_settings()
      },
      ignoreInit = TRUE
    )

    # The plot output element is kept mounted so that each Generate re-renders
    # the *same* output. The "press Generate" prompt is an overlay toggled
    # separately.
    output$plot_area <- shiny$renderUI({
      render_info("visualization_mst plot_area")
      prompt <- shiny$div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny$isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        shiny$icon("circle-nodes", class = "viz-plot-icon"),
        shiny$p(
          "Configure the MST options, then press ",
          shiny$tags$strong("Generate Plot"),
          "."
        )
      )

      # Loading overlay, shown/hidden client-side via the `.is-loading` class.
      loading <- shiny$div(
        class = "viz-loading",
        shiny$div(
          class = "spinner-custom viz-spinner-dark",
          waiter::spin_flower(),
          shiny$tags$h5("Generating plot …", class = "viz-loading_text")
        )
      )

      # The canvas is sized by CSS (.mst-canvas-wrap's --mst-aspect), not by
      # clientData and not by a user control.
      #
      # It used to be read right here from session$clientData, which made this
      # output re-render for no user action at all: the width was computed from
      # the height the *browser* reported for mst_plot, so every report
      # re-rendered plot_area, which re-created the visNetworkOutput element,
      # which made the browser report a size again. That loop is the duplicated
      # "Rendering plot_area" / "Rendering mst_plot" pairs in the log, and it
      # re-created the whole widget each time round.
      #
      # There used to be a user-facing aspect-ratio slider too. It is gone: the
      # ratio is fixed at 16:9 (see .mst-canvas-wrap in main.scss) — the
      # standard shape for a figure that is going to be screenshotted,
      # presented or dropped into a document, which the downloaded HTML always
      # will be. Nothing reactive is left in here, so this renders once per
      # Generate.
      shiny$div(
        class = "viz-plot-stage mst-stage",
        id = ns("plot_stage"),
        prompt,
        loading,
        shiny$div(
          id = ns("mst_canvas_wrap"),
          class = "mst-canvas-wrap",
          visNetworkOutput(ns("mst_plot"), height = "100%", width = "100%")
        )
      )
    })

    # Hide the prompt overlay once a plot has been generated.
    shiny$observeEvent(
      generated(),
      {
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    # The background the export paints behind the network. The canvas itself
    # carries only what was drawn on it, so both the HTML wrapper and the raster
    # capture have to be told this separately.
    export_bg <- shiny$reactive({
      if (isTRUE(input$mst_background_transparent)) {
        "rgba(0,0,0,0)"
      } else {
        input$mst_background_color
      }
    })

    # ---- Export contract ----------------------------------------------------
    # A browser-drawn engine: the tab's sidebar asks for a raster and the client
    # answers through `capture_data`. HTML is the one format the server can
    # write on its own, and it is built fresh rather than taken from the
    # on-screen widget so an export always carries every incremental update
    # pushed since it was drawn.
    export <- list(
      kind = "widget",
      label = "MST",
      ready = shiny$reactive(isTRUE(generated())),
      save = function(file, format, opts) {
        save_mst_html(
          build_mst_visnetwork(
            mst_obj(),
            viz_metadata(),
            mst_opts(),
            frames()
          ),
          file,
          export_bg()
        )
      },
      # "visnetwork" mode makes vis.js redraw the whole network into a larger
      # buffer, rather than copying the pixels already on screen. `targetPx`
      # rather than a raw scale is what makes the result reproducible: the
      # handler measures the widget's actual on-screen width itself and works
      # out the multiplier from that, so the same target produces the same
      # output whether the browser window is maximized or half that size. See
      # effectiveScale() in app/view/visualization.R.
      capture = function(format, opts) {
        session$sendCustomMessage(
          "phylotrace_capture",
          list(
            selector = paste0("#", ns("mst_plot")),
            mode = "visnetwork",
            inputId = session$ns("export_capture"),
            targetPx = opts$target_px,
            format = format,
            background = export_bg()
          )
        )
      },
      capture_data = shiny$reactive(input$export_capture)
    )

    # Keep the outputs reactive while hidden: the panel is nav_remove'd on
    # session reset AND the inactive engine's panel is display:none-hidden by
    # navset_hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "mst_plot", suspendWhenHidden = FALSE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    # Snapshot every mst_* control for reproduction, plus the mapping layers,
    # which are reactiveVal state rather than an input.
    snapshot <- shiny$reactive(
      c(
        collect_input_snapshot(input, "mst_"),
        list(.layers = mst_layers())
      )
    )

    # Rebuild a mapping layer from a pre-rewrite snapshot's flat keys. Each
    # saved MST carried one pie-chart mapping in three of them, and rebuilding a
    # layer from those is what stops every saved MST silently losing its mapping
    # on first reopen.
    migrate_legacy_mapping <- function(vals) {
      if (!isTRUE(vals$mst_color_var) || is.null(vals$mst_col_var)) {
        return(NULL)
      }
      prof <- profile_for(profiles(), vals$mst_col_var)
      if (is.null(prof)) {
        return(NULL)
      }
      layer <- assign_mapping_layer(prof, list(), "L1", MEDIUM)
      if (is.null(layer)) {
        return(NULL)
      }
      # The old control offered "Viridis"/"Rainbow"; only the first has a
      # counterpart in the shared palette list, and viridis is the engine's
      # default anyway, so "Rainbow" simply keeps whatever the engine chose.
      if (identical(vals$mst_col_scale, "Viridis")) {
        layer$palette <- "viridis"
      }
      list(layer)
    }

    # Restore controls from a snapshot. Metadata-backed selects get their
    # choices set alongside the value so the saved field sticks even before
    # Generate repopulates them.
    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "mst_show_label",
          "mst_show_edge_label",
          "mst_background_transparent",
          "mst_scale_nodes",
          "mst_shorten_long",
          "mst_shadow",
          "mst_show_clusters",
          "mst_cluster_label_tint",
          "mst_show_legend",
          "mst_show_scale_caption"
        ),
        selects = c(
          "mst_length_mode",
          "mst_cluster_col_scale",
          "mst_legend_ori"
        ),
        sliders = c(
          "mst_node_size",
          "mst_edge_length_scale",
          "mst_edge_font_size",
          "mst_node_label_fontsize",
          "mst_rotation",
          "mst_cap_mult",
          "mst_cluster_width",
          "mst_cluster_opacity",
          "mst_cluster_label_size"
        ),
        numerics = c("mst_cluster_threshold", "mst_collapse_threshold"),
        colors = c(
          "mst_text_color",
          "mst_color_node",
          "mst_color_edge",
          "mst_edge_font_color",
          "mst_background_color"
        )
      )
      # The mirrors are what the render reads, and a restored input reaches them
      # only via the browser's echo — which never arrives for a control whose
      # value did not change. Writing them here makes a restore take effect on
      # the same flush.
      for (id in MIRRORED_IDS) {
        if (!is.null(vals[[id]])) {
          set_fitted(id, vals[[id]])
        }
      }

      layers <- .normalize_layers(vals$.layers)
      if (is.null(layers)) {
        layers <- migrate_legacy_mapping(vals)
      }
      if (!is.null(layers)) {
        mst_layers(layers)
        mst_layer_seq(length(layers))
      }
      # Likewise for the edge length model: "Scale allelic distance" switched
      # off used to mean every edge the same length, which is now a named
      # transform.
      if (isFALSE(vals$mst_scale_edges)) {
        updatePickerInput(session, "mst_length_mode", selected = "uniform")
        set_fitted("mst_length_mode", "uniform")
      }

      # Fills the pickers' choices and defaults the label source to Isolate;
      # overridden right after by the saved field, if the snapshot has one and
      # the current database still carries it.
      populate_metadata_selects(force_default = TRUE)
      prof <- profiles()
      label_prof <- label_profile()
      if (
        !is.null(prof) &&
          nrow(prof) &&
          isTRUE(vals$mst_node_label %in% prof$field)
      ) {
        update_field_select(
          session,
          "mst_node_label",
          label_prof,
          selected = vals$mst_node_label
        )
        set_fitted("mst_node_label", vals$mst_node_label)
      }
    }

    # Thumbnail: capture the vis-network <canvas> in the browser and return the
    # PNG data URI through input$thumb_data.
    request_thumb <- function() {
      session$sendCustomMessage(
        "phylotrace_capture",
        list(
          selector = paste0("#", ns("mst_plot"), " canvas"),
          mode = "canvas",
          inputId = session$ns("thumb_data")
        )
      )
    }

    list(
      snapshot = snapshot,
      restore = restore,
      save_thumb = NULL,
      request_thumb = request_thumb,
      thumb_data = shiny$reactive(input$thumb_data),
      export = export
    )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
