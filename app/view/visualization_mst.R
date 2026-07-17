# app/view/visualization_mst.R
#
# Minimum Spanning Tree (visNetwork) visualization submodule. Owns its own
# control panel, the visNetwork render, and all MST-specific reactive state.
# Mounted by app/view/visualization.R inside a navset_hidden panel; the shared
# Generate button, plot type, na_handling and per-isolate metadata are forwarded
# in as reactives.

box::use(
  shiny,
  bslib[
    sidebar,
    layout_sidebar,
    card,
    card_body,
    accordion,
    accordion_panel,
    navset_tab,
    nav_panel,
    input_switch,
    as_fill_carrier,
  ],
  visNetwork[visNetworkOutput, renderVisNetwork],
)
box::use(
  app / logic / functions[render_info],
  app / logic / phylo[compute_mst, build_mst_visnetwork, save_mst_html],
  app / logic / field_labels[grouped_field_choices],
  app /
    logic /
    viz_helpers[meta_vars, viz_color, export_panel, reset_viz_colors],
)

# --- MST control tabs --------------------------------------------------------

mst_controls <- function(ns) {
  shiny$tagList(
    navset_tab(
      # Labels -----------------------------------------------------------------
      nav_panel(
        "Labels",
        icon = shiny$icon("tag"),
        input_switch(ns("mst_show_label"), "Show node labels", TRUE),
        shiny$selectInput(
          ns("mst_node_label"),
          "Label source",
          c("Assembly Name", "Isolation Date", "Host", "Country", "City")
        )
      ),
      # Variable mapping -------------------------------------------------------
      nav_panel(
        "Mapping",
        icon = shiny$icon("palette"),
        input_switch(ns("mst_color_var"), "Map variable to node color", FALSE),
        shiny$selectInput(ns("mst_col_var"), "Variable", meta_vars),
        shiny$selectInput(
          ns("mst_col_scale"),
          "color scale",
          c("Viridis", "Rainbow")
        )
      ),
      # colors ----------------------------------------------------------------
      nav_panel(
        "colors",
        icon = shiny$icon("fill-drip"),
        shiny$div(
          class = "viz-color-grid",
          viz_color(ns, "mst_text_color", "Text", "#000000"),
          viz_color(ns, "mst_color_node", "Nodes", "#B2FACA"),
          viz_color(ns, "mst_color_edge", "Edges", "#000000"),
          viz_color(ns, "mst_edge_font_color", "Edge Font", "#000000"),
          viz_color(ns, "mst_background_color", "Background", "#ffffff"),
          input_switch(
            ns("mst_background_transparent"),
            "Transparent background",
            TRUE
          )
        )
      ),
      # Sizing -----------------------------------------------------------------
      nav_panel(
        "Sizing",
        icon = shiny$icon("up-down"),
        accordion(
          open = "Nodes",
          accordion_panel(
            "Nodes",
            icon = shiny$icon("circle"),
            input_switch(ns("mst_scale_nodes"), "Scale by duplicates", TRUE),
            shiny$sliderInput(
              ns("mst_node_size"),
              "Size",
              1,
              100,
              30,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Edges",
            icon = shiny$icon("grip-lines"),
            input_switch(ns("mst_scale_edges"), "Scale allelic distance", TRUE),
            shiny$sliderInput(
              ns("mst_edge_length_scale"),
              "Multiplier",
              1,
              40,
              15,
              ticks = FALSE
            ),
            shiny$sliderInput(
              ns("mst_edge_font_size"),
              "Font size",
              8,
              30,
              18,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Labels",
            icon = shiny$icon("font"),
            shiny$sliderInput(
              ns("mst_node_label_fontsize"),
              "Font size",
              8,
              30,
              14,
              ticks = FALSE
            )
          )
        )
      ),
      # Layout -----------------------------------------------------------------
      nav_panel(
        "Layout",
        icon = shiny$icon("sliders"),
        accordion(
          open = "Dimensions",
          accordion_panel(
            "Dimensions",
            icon = shiny$icon("up-right-and-down-left-from-center"),
            shiny$sliderInput(
              ns("mst_aspect_ratio"),
              "Aspect ratio",
              0.5,
              2,
              0.6,
              step = 0.1,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Node Shapes",
            icon = shiny$icon("shapes"),
            input_switch(ns("mst_shadow"), "Show shadow", TRUE),
            shiny$selectInput(
              ns("mst_node_shape"),
              "Shape",
              list(
                `Label inside` = c(
                  Circle = "circle",
                  Box = "box",
                  Text = "text"
                ),
                `Label outside` = c(
                  Diamond = "diamond",
                  Hexagon = "hexagon",
                  Dot = "dot",
                  Square = "square"
                )
              ),
              selected = "dot"
            )
          ),
          accordion_panel(
            "Clustering",
            icon = shiny$icon("circle-nodes"),
            input_switch(ns("mst_show_clusters"), "Show clusters", FALSE),
            shiny$numericInput(
              ns("mst_cluster_threshold"),
              "Threshold",
              value = 10,
              min = 1,
              max = 99
            ),
            shiny$selectInput(
              ns("mst_cluster_col_scale"),
              "color scale",
              c("Viridis", "Rainbow")
            ),
            shiny$selectInput(
              ns("mst_cluster_type"),
              "Type",
              c("Area", "Skeleton")
            ),
            shiny$sliderInput(
              ns("mst_cluster_width"),
              "Skeleton width",
              1,
              50,
              24,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Legend",
            icon = shiny$icon("list"),
            shiny$selectInput(
              ns("mst_legend_ori"),
              "Orientation",
              c(Left = "left", Right = "right")
            ),
            shiny$sliderInput(
              ns("mst_font_size"),
              "Font size",
              15,
              30,
              18,
              ticks = FALSE
            ),
            shiny$sliderInput(
              ns("mst_symbol_size"),
              "Key size",
              10,
              30,
              20,
              ticks = FALSE
            )
          )
        )
      ),
      export_panel(ns, "mst", c("html", "png", "jpeg", "bmp"))
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
ui <- function(id, generate_id) {
  ns <- shiny$NS(id)

  layout_sidebar(
    id = "plot-sidebar",
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
          mst_controls(ns)
        )
      )
    ),
    shinyjs::useShinyjs(),
    # Loads waiter.js so the flower spinner used in the loading overlay is styled.
    waiter::useWaiter(),
    # Loading overlay: shown the moment Generate (parent namespace) is clicked,
    # scoped to this engine's own stage id. Unlike the Tree, the visNetwork keeps
    # running a client-side physics layout after its value arrives, so it is
    # cleared by the network's stabilization event (see build_mst_visnetwork,
    # which removes `.is-loading`). The timeout here is a safety net.
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
  session_reset = shiny$reactive(0L),
  viz_metadata = shiny$reactive(NULL),
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

    # Whether a plot has been generated for this engine. Retained across
    # plot-type switches (only session reset clears it).
    generated <- shiny$reactiveVal(FALSE)

    # mst_node_label and mst_col_var's *choices* are swapped out at Generate
    # time for the loaded database's actual metadata columns (see the
    # generate() observer below) — the UI-declared choices are just
    # placeholders shown before any data is loaded. shinyjs::reset() only
    # knows how to restore the selected *value* it captured at page load
    # (back when those placeholder choices were still current); it never
    # restores `choices`, so after Generate has swapped them out, that
    # captured value usually isn't even among the select's current options
    # any more, leaving the control visibly blank instead of at any real
    # value. force_default = TRUE (Reset settings) always jumps to the same
    # default Generate would use for a metadata set it's never seen a
    # selection for; force_default = FALSE (Generate) keeps the current
    # selection when it's still valid, so re-Generating doesn't clobber a
    # deliberate user choice.
    populate_metadata_selects <- function(force_default = FALSE) {
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(invisible(NULL))
      }
      fields <- names(meta)
      # Categorised, human-readable labels (a "Classical MLST" group holding the
      # derived ST + locus columns); the selected value stays the raw column
      # name the plot builder keys on, so the `%in% fields` checks below still
      # hold.
      field_choices <- grouped_field_choices(fields)

      shiny$updateSelectInput(
        session,
        "mst_node_label",
        choices = field_choices,
        selected = if (
          !force_default && isTRUE(input$mst_node_label %in% fields)
        ) {
          input$mst_node_label
        } else {
          "isolate"
        }
      )
      # Default the color variable to the first non-isolate field, where one
      # exists, so a freshly enabled mapping is meaningful.
      non_isolate <- setdiff(fields, "isolate")
      shiny$updateSelectInput(
        session,
        "mst_col_var",
        choices = field_choices,
        selected = if (
          !force_default && isTRUE(input$mst_col_var %in% fields)
        ) {
          input$mst_col_var
        } else if (length(non_isolate)) {
          non_isolate[1]
        } else {
          fields[1]
        }
      )
    }

    # Reset settings: restore every control in this engine's own sidebar to
    # its coded default. Local to this module (see the "Reset settings"
    # button in mst_controls()) — no confirmation modal, mirroring the
    # directness of the "Reset view" button in Map.
    #
    # shinyjs::reset() can't reach colorPickr (every "colors" tab swatch) —
    # see reset_viz_colors() in viz_helpers.R for why — so it's patched up
    # explicitly right after the blanket reset.
    #
    # populate_metadata_selects(), unlike the color patch-up, has to be
    # deferred with shinyjs::delay() rather than just called right after
    # shinyjs::reset(): mst_node_label/mst_col_var *are* plain <select>s that
    # shinyjs::reset() recognizes and restores — but only asynchronously (it
    # round-trips through the browser to read back each resettable element's
    # page-load value before calling shiny::updateSelectInput() on the
    # server). That means a same-tick call right after shinyjs::reset() runs
    # and sends its corrected choices/selection *first*, and shinyjs's own
    # (stale, pre-Generate) restoration lands *after* it and overwrites it —
    # the control resets fine, then silently reverts to blank a moment later.
    # Delaying past that round-trip (typically well under 100ms locally)
    # guarantees this runs last and wins.
    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path below, so both routes return the controls
    # identically.
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
      shinyjs::delay(400, populate_metadata_selects(force_default = TRUE))
    }

    shiny$observeEvent(input$reset_settings, reset_mst_settings())

    # The computed MST graph. Held in a reactiveVal (not an eventReactive) so a
    # Generate for the *other* engine — which also ticks the shared `generate()`
    # — leaves this engine's last result untouched. It is (re)computed only by
    # the guarded Generate observer below when MST is the active engine.
    mst_obj <- shiny$reactiveVal(NULL)

    # Resolved MST control values, gathered once so the live render and the
    # HTML export share an identical configuration.
    mst_opts <- shiny$reactive(
      list(
        show_label = input$mst_show_label,
        field = input$mst_node_label,
        node_font_color = input$mst_text_color,
        node_font_size = input$mst_node_label_fontsize,
        node_color = input$mst_color_node,
        node_size = input$mst_node_size,
        scale_nodes = input$mst_scale_nodes,
        shape = input$mst_node_shape,
        shadow = input$mst_shadow,
        edge_color = input$mst_color_edge,
        edge_font_color = input$mst_edge_font_color,
        edge_font_size = input$mst_edge_font_size,
        scale_edges = input$mst_scale_edges,
        edge_length_scale = input$mst_edge_length_scale,
        background = input$mst_background_color,
        transparent = input$mst_background_transparent,
        # Variable pie-chart coloring + legend.
        color_var = input$mst_color_var,
        col_var = input$mst_col_var,
        col_scale = input$mst_col_scale,
        legend_ori = input$mst_legend_ori,
        legend_font_size = input$mst_font_size,
        legend_symbol_size = input$mst_symbol_size,
        # Clustering.
        show_clusters = input$mst_show_clusters,
        cluster_threshold = input$mst_cluster_threshold,
        cluster_col_scale = input$mst_cluster_col_scale,
        cluster_type = input$mst_cluster_type,
        cluster_width = input$mst_cluster_width
      )
    )

    # The visNetwork widget: rebuilt live as controls change, but never
    # recomputes the (expensive) MST itself.
    mst_widget <- shiny$reactive({
      shiny$req(mst_obj())
      build_mst_visnetwork(mst_obj(), viz_metadata(), mst_opts())
    })

    # Top-level app-reset: clear the computed graph so the stale visNetwork is
    # torn down (not just hidden behind the re-shown prompt), and restore the
    # sidebar controls to their defaults — mirroring the local "Reset
    # settings" button.
    shiny$observeEvent(
      session_reset(),
      {
        mst_obj(NULL)
        generated(FALSE)
        reset_mst_settings()
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(generate(), {
      if (!identical(plot_type(), "MST")) {
        return()
      }

      # Populate the metadata-backed selects (no heavy compute here; the MST is
      # computed lazily by its output so the waiter can cover it).
      populate_metadata_selects(force_default = FALSE)

      # Compute the MST (heavy work is covered by the client-side loading
      # overlay, which the visNetwork stabilization event clears).
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
        # No network will render, so the stabilization event that normally
        # clears the loading overlay never fires — hide it now instead of
        # leaving the spinner up until the 45s client-side safety timeout.
        shinyjs::removeClass(id = "plot_stage", class = "is-loading")
      }
      mst_obj(graph)

      generated(TRUE)
    })

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

      # Canvas width derives from the panel height and the aspect-ratio control;
      # the height is only known after a first render, so fall back until the
      # browser reports it.
      aspect <- if (is.null(input$mst_aspect_ratio)) {
        0.6
      } else {
        input$mst_aspect_ratio
      }
      h <- session$clientData[[paste0("output_", ns("mst_plot"), "_height")]]
      width <- if (!is.null(h)) {
        as.integer(h * (1 / aspect))
      } else {
        as.integer(500 * aspect)
      }
      shiny$div(
        class = "viz-plot-stage",
        id = ns("plot_stage"),
        prompt,
        loading,
        visNetworkOutput(
          ns("mst_plot"),
          height = "100%",
          width = paste0(width, "px")
        ),
        # Hidden target the export action button clicks to start the download.
        shiny$div(
          style = "display:none;",
          shiny$downloadButton(ns("mst_html"), "Download HTML")
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

    output$mst_plot <- renderVisNetwork({
      render_info("visualization_mst mst_plot")
      mst_widget()
    })

    # Serialise the current MST as a self-contained HTML file.
    output$mst_html <- shiny$downloadHandler(
      filename = function() paste0(Sys.Date(), "_MST.html"),
      content = function(file) {
        bg <- if (isTRUE(input$mst_background_transparent)) {
          "rgba(0,0,0,0)"
        } else {
          input$mst_background_color
        }
        save_mst_html(mst_widget(), file, bg)
      }
    )

    # The export tab uses an action button; route it to the hidden download
    # link. Only HTML export is wired for now.
    shiny$observeEvent(input$mst_download, {
      if (identical(input$mst_filetype, "html")) {
        shinyjs::click("mst_html")
      } else {
        shiny$showNotification(
          "Only HTML export is available currently.",
          type = "message"
        )
      }
    })

    # Keep the outputs reactive while hidden: the panel is nav_remove'd on
    # session reset AND the inactive engine's panel is display:none-hidden by
    # navset_hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "mst_plot", suspendWhenHidden = FALSE)
  })
}
