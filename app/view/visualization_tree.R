# app/view/visualization_tree.R
#
# Tree (Neighbour-Joining / UPGMA, ggtree) visualization submodule. Owns its own
# control panel (including the algorithm picker), the ggtree render, and all
# tree-specific reactive state. Mounted by app/view/visualization.R inside a
# navset_hidden panel; the shared Generate button, plot type, na_handling and
# per-isolate metadata are forwarded in as reactives.

box::use(
  shiny,
  bslib[
    sidebar,
    layout_sidebar,
    card,
    card_body,
    layout_columns,
    accordion,
    accordion_panel,
    navset_tab,
    nav_panel,
    input_switch,
    as_fill_carrier,
  ],
  shinyWidgets[
    radioGroupButtons,
    pickerInput,
    pickerOptions
  ],
)
box::use(
  app / logic / functions[render_info],
  app / logic / tree_plot[build_tree_ggtree, save_tree_plot],
  app / logic / phylo[compute_phylo_tree],
  app /
    logic /
    viz_helpers[
      meta_vars,
      label_vars,
      branch_vars,
      fontfaces,
      point_shapes,
      viz_color,
      scale_select,
      color_scales,
      suitable_scale_categories,
      export_panel,
      reset_viz_colors,
      reset_viz_radio_buttons,
    ],
)

# --- Tree (NJ / UPGMA) control tabs ------------------------------------------

tree_controls <- function(ns) {
  shiny$tagList(
    navset_tab(
      # Labels -----------------------------------------------------------------
      nav_panel(
        "Labels",
        icon = shiny$icon("tag"),
        accordion(
          open = "Isolate Labels",
          accordion_panel(
            "Isolate Labels",
            icon = shiny$icon("tag"),
            input_switch(ns("nj_tiplab_show"), "Show isolate labels", TRUE),
            shiny$selectInput(ns("nj_tiplab"), "Label source", label_vars),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_tiplab_size"),
                "Size",
                1,
                10,
                4,
                step = 0.1,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_tiplab_alpha"),
                "Opacity",
                0.1,
                1,
                1,
                step = 0.05,
                ticks = FALSE
              )
            ),
            layout_columns(
              col_widths = c(6, 6),
              shiny$selectInput(ns("nj_tiplab_fontface"), "Font", fontfaces),
              shiny$sliderInput(
                ns("nj_tiplab_angle"),
                "Angle",
                -90,
                90,
                0,
                ticks = FALSE
              )
            ),
            input_switch(ns("nj_align"), "Align labels", TRUE)
          ),
          accordion_panel(
            "Branch Labels",
            icon = shiny$icon("code-branch"),
            input_switch(
              ns("nj_show_branch_label"),
              "Show branch labels",
              FALSE
            ),
            shiny$selectInput(ns("nj_branch_label"), "Label source", branch_vars),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_branch_size"),
                "Size",
                2,
                10,
                4,
                step = 0.5,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_branchlabel_cutoff"),
                "Cutoff",
                0,
                100,
                10,
                ticks = FALSE
              )
            )
          ),
          accordion_panel(
            "Title & Subtitle",
            icon = shiny$icon("heading"),
            shiny$textInput(ns("nj_title"), "Title", placeholder = "Plot title"),
            shiny$sliderInput(
              ns("nj_title_size"),
              "Title size",
              15,
              40,
              30,
              ticks = FALSE
            ),
            shiny$textInput(
              ns("nj_subtitle"),
              "Subtitle",
              placeholder = "Plot subtitle"
            ),
            shiny$sliderInput(
              ns("nj_subtitle_size"),
              "Subtitle size",
              15,
              40,
              30,
              ticks = FALSE
            )
          )
        )
      ),
      # Variable mapping -------------------------------------------------------
      nav_panel(
        "Mapping",
        icon = shiny$icon("palette"),
        accordion(
          open = "Tip Label color",
          accordion_panel(
            "Tip Label color",
            icon = shiny$icon("font"),
            input_switch(ns("nj_mapping_show"), "Map variable to color", FALSE),
            shiny$selectInput(ns("nj_color_mapping"), "Variable", meta_vars),
            scale_select(ns, "nj_tiplab_scale")
          ),
          accordion_panel(
            "Tip Point color",
            icon = shiny$icon("circle"),
            input_switch(
              ns("nj_tipcolor_mapping_show"),
              "Map variable to color",
              FALSE
            ),
            shiny$selectInput(ns("nj_tipcolor_mapping"), "Variable", meta_vars),
            scale_select(ns, "nj_tippoint_scale")
          ),
          accordion_panel(
            "Tip Point Shape",
            icon = shiny$icon("shapes"),
            input_switch(
              ns("nj_tipshape_mapping_show"),
              "Map variable to shape",
              FALSE
            ),
            shiny$selectInput(ns("nj_tipshape_mapping"), "Variable", meta_vars)
          ),
          accordion_panel(
            "Tiles",
            icon = shiny$icon("table-cells"),
            shiny$selectInput(ns("nj_tile_num"), "Tile", as.character(1:5)),
            input_switch(ns("nj_tiles_show"), "Show tile", FALSE),
            shiny$selectInput(ns("nj_fruit_variable"), "Variable", meta_vars),
            scale_select(ns, "nj_tiles_scale")
          ),
          accordion_panel(
            "Heatmap",
            icon = shiny$icon("border-all"),
            input_switch(ns("nj_heatmap_show"), "Show heatmap", FALSE),
            shiny$actionButton(
              ns("nj_heatmap_button"),
              "Select variables",
              icon = shiny$icon("list-check")
            ),
            scale_select(ns, "nj_heatmap_scale")
          )
        )
      ),
      # colors ----------------------------------------------------------------
      nav_panel(
        "colors",
        icon = shiny$icon("fill-drip"),
        shiny$div(
          class = "viz-color-grid",
          viz_color(ns, "nj_color", "Lines / Text", "#000000"),
          viz_color(ns, "nj_bg", "Background", "#ffffff"),
          viz_color(ns, "nj_title_color", "Title", "#000000"),
          viz_color(ns, "nj_tiplab_color", "Tip Label", "#000000"),
          viz_color(ns, "nj_tiplab_fill", "Label Panel", "#84D9A0"),
          viz_color(ns, "nj_branch_color", "Branch Label", "#000000"),
          viz_color(ns, "nj_branch_label_color", "Branch Panel", "#FFB7B7"),
          viz_color(ns, "nj_tippoint_color", "Tip Point", "#3A4657"),
          viz_color(ns, "nj_nodepoint_color", "Node Point", "#3A4657")
        )
      ),
      # Elements ---------------------------------------------------------------
      nav_panel(
        "Elements",
        icon = shiny$icon("shapes"),
        accordion(
          open = "Tip Points",
          accordion_panel(
            "Tip Points",
            icon = shiny$icon("circle"),
            input_switch(ns("nj_tippoint_show"), "Show tip points", FALSE),
            shiny$selectInput(ns("nj_tippoint_shape"), "Shape", point_shapes),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_tippoint_alpha"),
                "Opacity",
                0.1,
                1,
                0.5,
                step = 0.05,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_tippoint_size"),
                "Size",
                1,
                20,
                4,
                step = 0.5,
                ticks = FALSE
              )
            )
          ),
          accordion_panel(
            "Node Points",
            icon = shiny$icon("circle-dot"),
            input_switch(ns("nj_nodepoint_show"), "Show node points", FALSE),
            shiny$selectInput(ns("nj_nodepoint_shape"), "Shape", point_shapes),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_nodepoint_alpha"),
                "Opacity",
                0.1,
                1,
                1,
                step = 0.05,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_nodepoint_size"),
                "Size",
                1,
                20,
                2.5,
                step = 0.5,
                ticks = FALSE
              )
            )
          ),
          accordion_panel(
            "Tiles",
            icon = shiny$icon("table-cells"),
            shiny$selectInput(ns("nj_tile_number"), "Tile", as.character(1:5)),
            shiny$sliderInput(
              ns("nj_fruit_alpha"),
              "Opacity",
              0.1,
              1,
              1,
              step = 0.05,
              ticks = FALSE
            ),
            shiny$sliderInput(
              ns("nj_fruit_width"),
              "Width",
              0.1,
              10,
              2,
              step = 0.1,
              ticks = FALSE
            ),
            shiny$sliderInput(
              ns("nj_fruit_offset"),
              "Position",
              -0.6,
              0.6,
              0.05,
              step = 0.01,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Clade Highlight",
            icon = shiny$icon("highlighter"),
            input_switch(ns("nj_nodelabel_show"), "Toggle node view", FALSE),
            pickerInput(
              ns("nj_parentnode"),
              "Nodes",
              choices = character(0),
              multiple = TRUE,
              options = list(
                liveSearch = TRUE,
                size = 10,
                liveSearchPlaceholder = "Search nodes ..."
              )
            ),
            viz_color(ns, "nj_clade_scale", "Highlight color", "#D0F221"),
            shiny$selectInput(
              ns("nj_clade_type"),
              "Form",
              c(Rounded = "roundrect", Rectangular = "rect")
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
              ns("nj_aspect_ratio"),
              "Aspect ratio",
              0.5,
              2,
              0.6,
              step = 0.1,
              ticks = FALSE
            ),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_v"),
                "Vertical",
                -0.5,
                0.5,
                0,
                step = 0.01,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_h"),
                "Horizontal",
                -0.5,
                0.5,
                -0.05,
                step = 0.01,
                ticks = FALSE
              )
            ),
            shiny$sliderInput(
              ns("nj_zoom"),
              "Zoom",
              0.5,
              1.5,
              0.95,
              step = 0.05,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Tree Rooting",
            icon = shiny$icon("seedling"),
            shiny$selectInput(ns("nj_root_isolate"), "Outgroup", c("Automatic"))
          ),
          accordion_panel(
            "Layout",
            icon = shiny$icon("project-diagram"),
            shiny$selectInput(
              ns("nj_layout"),
              "Layout",
              list(
                Linear = c(
                  Rectangular = "rectangular",
                  Roundrect = "roundrect",
                  Slanted = "slanted",
                  Ellipse = "ellipse"
                ),
                Circular = c(Circular = "circular", Inward = "inward")
              )
            ),
            input_switch(ns("nj_ladder"), "Ladderize", TRUE),
            input_switch(ns("nj_rootedge_show"), "Root edge", TRUE),
            input_switch(ns("nj_treescale_show"), "Tree scale", TRUE)
          ),
          accordion_panel(
            "Legend",
            icon = shiny$icon("list"),
            radioGroupButtons(
              ns("nj_legend_orientation"),
              "Orientation",
              c(Vertical = "vertical", Horizontal = "horizontal"),
              justified = TRUE
            ),
            shiny$sliderInput(ns("nj_legend_size"), "Size", 5, 25, 10, ticks = FALSE),
            layout_columns(
              col_widths = c(6, 6),
              shiny$sliderInput(
                ns("nj_legend_x"),
                "Horizontal",
                -0.9,
                1.9,
                0.9,
                step = 0.1,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("nj_legend_y"),
                "Vertical",
                -1.5,
                1.5,
                0.2,
                step = 0.1,
                ticks = FALSE
              )
            )
          )
        )
      ),
      export_panel(ns, "nj", c("png", "jpeg", "bmp", "svg"))
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
        shiny$div(id = ns("controls_wrap"), class = "viz-nav-wrap", tree_controls(ns))
      )
    ),
    shinyjs::useShinyjs(),
    # Loads waiter.js so the flower spinner used in the loading overlay is styled.
    waiter::useWaiter(),
    # Loading overlay: shown the moment Generate (in the parent namespace) is
    # clicked, hidden once the ggtree render fires its value event. Scoped to
    # this engine's own stage id so it never touches the sibling MST panel. A
    # timeout is a safety net.
    shiny$tags$script(
      shiny$HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var out='",
          ns("tree_plot"),
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
          "$(document).on('shiny:value shiny:recalculated',",
          "function(e){if(e.target.id===out)set(false);});",
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
  # Staged peer typing results (Database > Import) folded into the distance
  # matrix; NULL means local isolates only.
  imported_sets = shiny$reactive(NULL),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("Tree"),
  algo = shiny$reactive("Neighbour-Joining"),
  # Display mode driven from the parent module's Options accordion (left
  # sidebar): FALSE = "fit" (default), TRUE = "zoom". Applied as the .is-zoom
  # class on the stage below — purely presentational, never re-renders.
  zoom_view = shiny$reactive(FALSE)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether a plot has been generated for this engine. Drives the preview
    # between its prompt and the rendered plot. Retained across plot-type
    # switches (only session reset clears it).
    generated <- shiny$reactiveVal(FALSE)

    # nj_tiplab/nj_color_mapping/nj_tipcolor_mapping/nj_tipshape_mapping/
    # nj_fruit_variable/nj_branch_label/nj_root_isolate/nj_parentnode's
    # *choices* are all swapped out at Generate time for the loaded
    # database's actual metadata columns / isolate names (see the generate()
    # observer below) — the UI-declared choices are just placeholders shown
    # before any data is loaded. shinyjs::reset() only knows how to restore
    # the selected *value* it captured at page load (back when those
    # placeholder choices were still current); it never restores `choices`,
    # so after Generate has swapped them out, that captured value usually
    # isn't even among the select's current options any more, leaving the
    # control visibly blank instead of at any real value. force_default =
    # TRUE (Reset settings) always jumps to the same default Generate would
    # use for a metadata set it's never seen a selection for; force_default =
    # FALSE (Generate) keeps the current selection when it's still valid, so
    # re-Generating doesn't clobber a deliberate user choice.
    populate_metadata_selects <- function(force_default = FALSE) {
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(invisible(NULL))
      }
      fields <- names(meta)

      keep <- function(id, choices, default) {
        shiny$updateSelectInput(
          session,
          id,
          choices = choices,
          selected = if (!force_default && isTRUE(input[[id]] %in% choices)) {
            input[[id]]
          } else {
            default
          }
        )
      }
      keep("nj_tiplab", fields, "isolate")
      keep("nj_color_mapping", fields, fields[1])
      keep("nj_tipcolor_mapping", fields, fields[1])
      keep("nj_tipshape_mapping", fields, fields[1])
      keep("nj_fruit_variable", fields, fields[1])
      keep(
        "nj_branch_label",
        c("Allelic Distance", fields),
        "Allelic Distance"
      )

      # Outgroup + clade node choices are derived from the isolate set without
      # computing the tree (tips = isolates; internal node count follows from
      # the algorithm), keeping this cheap enough to call from Reset too.
      tips <- meta$isolate
      n_tip <- length(tips)
      shiny$updateSelectInput(
        session,
        "nj_root_isolate",
        choices = c("Automatic", tips),
        selected = if (
          !force_default && isTRUE(input$nj_root_isolate %in% tips)
        ) {
          input$nj_root_isolate
        } else {
          "Automatic"
        }
      )
      if (n_tip >= 3) {
        n_node <- if (identical(algo(), "UPGMA")) {
          n_tip - 1L
        } else {
          n_tip - 2L
        }
        nodes <- as.character(seq.int(n_tip + 1L, n_tip + n_node))
        shinyWidgets::updatePickerInput(
          session,
          "nj_parentnode",
          choices = nodes,
          selected = if (force_default) {
            character(0)
          } else {
            intersect(input$nj_parentnode, nodes)
          }
        )
      }
    }

    # Reset settings: restore every control in this engine's own sidebar to
    # its coded default. Local to this module (see the "Reset settings"
    # button in tree_controls()) — no confirmation modal, mirroring the
    # directness of the "Reset view" button in Map.
    #
    # shinyjs::reset() alone can't reach colorPickr (every "colors" tab
    # swatch) or the "Legend" tab's radioGroupButtons orientation picker —
    # see reset_viz_colors()/reset_viz_radio_buttons() in viz_helpers.R for
    # why — so both are patched up explicitly right after the blanket reset.
    #
    # populate_metadata_selects(), unlike those two, has to be deferred with
    # shinyjs::delay() rather than just called right after shinyjs::reset():
    # nj_tiplab/.../nj_root_isolate/nj_parentnode *are* plain <select>s (or a
    # pickerInput, itself a <select> underneath) that shinyjs::reset()
    # recognizes and restores — but only asynchronously (it round-trips
    # through the browser to read back each resettable element's page-load
    # value before calling the matching update*Input() on the server). That
    # means a same-tick call right after shinyjs::reset() runs and sends its
    # corrected choices/selection *first*, and shinyjs's own (stale,
    # pre-Generate) restoration lands *after* it and overwrites it — the
    # controls reset fine, then silently revert to blank a moment later.
    # Delaying past that round-trip (typically well under 100ms locally)
    # guarantees this runs last and wins.
    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path below, so both routes return the controls
    # identically.
    reset_tree_settings <- function() {
      shinyjs::reset(id = "controls_wrap")

      reset_viz_colors(
        session,
        nj_color = "#000000",
        nj_bg = "#ffffff",
        nj_title_color = "#000000",
        nj_tiplab_color = "#000000",
        nj_tiplab_fill = "#84D9A0",
        nj_branch_color = "#000000",
        nj_branch_label_color = "#FFB7B7",
        nj_tippoint_color = "#3A4657",
        nj_nodepoint_color = "#3A4657",
        nj_clade_scale = "#D0F221"
      )
      reset_viz_radio_buttons(
        session,
        nj_legend_orientation = "vertical"
      )
      shinyjs::delay(400, populate_metadata_selects(force_default = TRUE))
    }

    shiny$observeEvent(input$reset_settings, reset_tree_settings())

    # The computed phylo tree. Held in a reactiveVal (not an eventReactive) so a
    # Generate for the *other* engine — which also ticks the shared `generate()`
    # — leaves this engine's last result untouched. It is (re)computed only by
    # the guarded Generate observer below when Tree is the active engine.
    tree_obj <- shiny$reactiveVal(NULL)

    # Settings for the five metadata tile strips. The Mapping and Elements tabs
    # each edit one strip (selected by nj_tile_num / nj_tile_number).
    nj_tiles <- shiny$reactiveVal(
      replicate(
        5,
        list(
          show = FALSE,
          variable = NULL,
          scale = "viridis",
          alpha = 1,
          width = 2,
          offset = 0.05
        ),
        simplify = FALSE
      )
    )

    # Metadata columns selected for the heatmap annotation (via its modal).
    nj_heatmap_select <- shiny$reactiveVal(character(0))

    # Resolved Tree control values, shared by the live render and the export.
    tree_opts <- shiny$reactive(
      list(
        # Layout / rooting.
        root = input$nj_root_isolate,
        layout = input$nj_layout,
        ladderize = input$nj_ladder,
        line_color = input$nj_color,
        bg = input$nj_bg,
        # Tip labels.
        tiplab_show = input$nj_tiplab_show,
        tiplab = input$nj_tiplab,
        tiplab_size = input$nj_tiplab_size,
        tiplab_alpha = input$nj_tiplab_alpha,
        tiplab_fontface = input$nj_tiplab_fontface,
        tiplab_position = input$nj_tiplab_position %||% 0,
        tiplab_angle = input$nj_tiplab_angle,
        align = input$nj_align,
        label_panel = input$nj_geom %||% FALSE,
        tiplab_color = input$nj_tiplab_color,
        tiplab_fill = input$nj_tiplab_fill,
        # Tip-label color mapping.
        mapping_show = input$nj_mapping_show,
        color_mapping = input$nj_color_mapping,
        tiplab_scale = input$nj_tiplab_scale,
        # Branch labels.
        branch_show = input$nj_show_branch_label,
        branch_label = input$nj_branch_label,
        branch_size = input$nj_branch_size,
        branch_cutoff = input$nj_branchlabel_cutoff,
        branch_color = input$nj_branch_color,
        branch_label_color = input$nj_branch_label_color,
        # Tip / node points.
        tippoint_show = input$nj_tippoint_show,
        tippoint_alpha = input$nj_tippoint_alpha,
        tippoint_size = input$nj_tippoint_size,
        tippoint_color = input$nj_tippoint_color,
        tippoint_shape = input$nj_tippoint_shape,
        tipcolor_mapping_show = input$nj_tipcolor_mapping_show,
        tipcolor_mapping = input$nj_tipcolor_mapping,
        tippoint_scale = input$nj_tippoint_scale,
        tipshape_mapping_show = input$nj_tipshape_mapping_show,
        tipshape_mapping = input$nj_tipshape_mapping,
        nodepoint_show = input$nj_nodepoint_show,
        nodepoint_alpha = input$nj_nodepoint_alpha,
        nodepoint_color = input$nj_nodepoint_color,
        nodepoint_shape = input$nj_nodepoint_shape,
        nodepoint_size = input$nj_nodepoint_size,
        # Clade highlights.
        nodelabel_show = input$nj_nodelabel_show,
        parentnodes = input$nj_parentnode,
        clade_color = input$nj_clade_scale,
        clade_type = input$nj_clade_type,
        # Tiles / heatmap.
        tiles = nj_tiles(),
        heatmap_show = input$nj_heatmap_show,
        heatmap_select = nj_heatmap_select(),
        # Elements toggles.
        rootedge_show = input$nj_rootedge_show,
        treescale_show = input$nj_treescale_show,
        # Titles.
        title = input$nj_title,
        subtitle = input$nj_subtitle,
        title_color = input$nj_title_color,
        title_size = input$nj_title_size,
        subtitle_size = input$nj_subtitle_size,
        # Dimensions / legend.
        zoom = input$nj_zoom,
        h = input$nj_h,
        v = input$nj_v,
        legend_orientation = input$nj_legend_orientation,
        legend_x = input$nj_legend_x,
        legend_y = input$nj_legend_y,
        legend_size = input$nj_legend_size
      )
    )

    # The ggtree plot: rebuilt live as controls change; the phylo itself is
    # never recomputed here.
    tree_plot_built <- shiny$reactive({
      shiny$req(tree_obj())
      build_tree_ggtree(tree_obj(), viz_metadata(), tree_opts())
    })

    # Top-level app-reset: clear the computed tree so the stale plot image is
    # torn down (not just hidden behind the re-shown prompt), and restore the
    # sidebar controls to their defaults — mirroring the local "Reset
    # settings" button.
    shiny$observeEvent(
      session_reset(),
      {
        tree_obj(NULL)
        generated(FALSE)
        reset_tree_settings()
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(generate(), {
      if (!identical(plot_type(), "Tree")) {
        return()
      }

      # Populate the metadata-backed selects (no heavy compute here; the tree is
      # computed lazily by its output so the waiter can cover it).
      populate_metadata_selects(force_default = FALSE)

      # Compute the tree (heavy work is covered by the client-side loading
      # overlay, which stays up until this engine's plot fires its value event).
      tree <- tryCatch(
        compute_phylo_tree(
          db_path(),
          na_handling(),
          algo(),
          selected_isolates(),
          imported_sets()
        ),
        error = function(e) {
          shiny$showNotification(
            paste("Tree computation failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(tree)) {
        shiny$showNotification(
          "Could not build a tree: need at least 3 isolates in the database.",
          type = "warning"
        )
        # No plot will render, so the value event that normally clears the
        # loading overlay never fires — hide it now instead of leaving the
        # spinner up until the 45s client-side safety timeout.
        shinyjs::removeClass(id = "plot_stage", class = "is-loading")
      }
      tree_obj(tree)

      generated(TRUE)
    })

    # Restrict a variable-mapping color-scale picker's choices to whichever
    # color_scales categories suit the type of the variable currently mapped
    # to it (mirrors visualization_map.R's map_col_scale filtering). Tree
    # variables have no separate Numeric/Bin/Quantile type selector of their
    # own — app/logic/tree_plot.R's tree_scale() always resolves numeric vs.
    # discrete purely from is.numeric(), so that's what decides suitability
    # here too, keeping the picker and the renderer in agreement.
    filter_scale_choices <- function(input_id, vals) {
      cats <- if (!length(vals)) {
        names(color_scales)
      } else {
        suitable_scale_categories(
          if (is.numeric(vals)) "Numeric" else "Factor",
          vals
        )
      }
      sel <- if (
        isTRUE(
          input[[input_id]] %in% unlist(color_scales[cats], use.names = FALSE)
        )
      ) {
        input[[input_id]]
      } else {
        color_scales[[cats[1]]][1]
      }
      shiny$updateSelectInput(
        session,
        input_id,
        choices = color_scales[cats],
        selected = sel
      )
    }

    shiny$observeEvent(
      list(input$nj_mapping_show, input$nj_color_mapping, viz_metadata()),
      {
        meta <- viz_metadata()
        vals <- if (
          isTRUE(input$nj_mapping_show) &&
            isTRUE(input$nj_color_mapping %in% names(meta))
        ) {
          meta[[input$nj_color_mapping]]
        } else {
          character(0)
        }
        filter_scale_choices("nj_tiplab_scale", vals)
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      list(
        input$nj_tipcolor_mapping_show,
        input$nj_tipcolor_mapping,
        viz_metadata()
      ),
      {
        meta <- viz_metadata()
        vals <- if (
          isTRUE(input$nj_tipcolor_mapping_show) &&
            isTRUE(input$nj_tipcolor_mapping %in% names(meta))
        ) {
          meta[[input$nj_tipcolor_mapping]]
        } else {
          character(0)
        }
        filter_scale_choices("nj_tippoint_scale", vals)
      },
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      list(input$nj_tiles_show, input$nj_fruit_variable, viz_metadata()),
      {
        meta <- viz_metadata()
        vals <- if (
          isTRUE(input$nj_tiles_show) &&
            isTRUE(input$nj_fruit_variable %in% names(meta))
        ) {
          meta[[input$nj_fruit_variable]]
        } else {
          character(0)
        }
        filter_scale_choices("nj_tiles_scale", vals)
      },
      ignoreInit = TRUE
    )

    # The plot output element is kept mounted so that each Generate re-renders
    # the *same* output — that is what fires the recalculating event the waiter
    # hooks. The "press Generate" prompt is an overlay toggled separately.
    output$plot_area <- shiny$renderUI({
      render_info("visualization_tree plot_area")
      prompt <- shiny$div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny$isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        shiny$icon("sitemap", class = "viz-plot-icon"),
        shiny$p(
          "Configure the Tree options, then press ",
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

      shiny$div(
        # `tree-stage` scopes the display-mode CSS to this engine (MST/map share
        # `viz-plot-stage`). The `is-zoom` modifier is re-applied here on every
        # (re)mount from the current switch value via isolate() — read without a
        # reactive dependency so toggling the switch never re-renders the plot;
        # live toggles are handled by the observer below.
        class = paste(
          "viz-plot-stage tree-stage",
          if (isTRUE(shiny$isolate(zoom_view()))) "is-zoom"
        ),
        id = ns("plot_stage"),
        prompt,
        loading,
        # height="auto" lets the container shrink-wrap the image, whose pixel
        # height renderPlot() derives from the panel width and the aspect-ratio
        # control below. The plotOutput default (height="400px") would instead
        # pin the box at 400px and clip the aspect-sized plot.
        shiny$plotOutput(ns("tree_plot"), height = "auto"),
        # Hidden target the export action button clicks to start the download.
        shiny$div(
          style = "display:none;",
          shiny$downloadButton(ns("download_nj"), "Download plot")
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

    # The ggtree plot, sized from the panel width and the aspect-ratio control
    # (circular/inward layouts are square), rendered at print resolution.
    output$tree_plot <- shiny$renderPlot(
      {
        render_info("visualization_tree tree_plot")
        tree_plot_built()
      },
      height = function() {
        w <- session$clientData[[paste0("output_", ns("tree_plot"), "_width")]]
        aspect <- if (
          identical(input$nj_layout, "circular") ||
            identical(input$nj_layout, "inward")
        ) {
          1
        } else {
          input$nj_aspect_ratio %||% 0.6
        }
        if (!is.null(w)) as.integer(w * aspect) else 600L
      },
      res = 192
    )

    # Fit ⇄ Zoom display mode, driven by the parent's `zoom_view` switch (left
    # sidebar → Options). Purely toggles the .is-zoom class on the mounted stage
    # — the image itself is not re-rendered (see the tree-stage CSS in
    # app/styles/main.scss). ignoreInit: the initial state is already stamped on
    # the stage div by renderUI's isolate() read, so the first (FALSE) value
    # needs no toggle; this only fires on user changes.
    shiny$observeEvent(
      zoom_view(),
      {
        shinyjs::toggleClass(
          id = "plot_stage",
          class = "is-zoom",
          condition = isTRUE(zoom_view())
        )
      },
      ignoreInit = TRUE
    )

    # --- Tiles, heatmap, and export ------------------------------------------

    # Persist edits to the currently selected tile (Mapping tab → nj_tile_num).
    shiny$observeEvent(
      list(input$nj_tiles_show, input$nj_fruit_variable, input$nj_tiles_scale),
      {
        i <- as.integer(input$nj_tile_num)
        tiles <- nj_tiles()
        tiles[[i]]$show <- input$nj_tiles_show
        tiles[[i]]$variable <- input$nj_fruit_variable
        tiles[[i]]$scale <- input$nj_tiles_scale
        nj_tiles(tiles)
      },
      ignoreInit = TRUE
    )
    # Persist edits to the currently selected tile (Elements tab → nj_tile_number).
    shiny$observeEvent(
      list(input$nj_fruit_alpha, input$nj_fruit_width, input$nj_fruit_offset),
      {
        i <- as.integer(input$nj_tile_number)
        tiles <- nj_tiles()
        tiles[[i]]$alpha <- input$nj_fruit_alpha
        tiles[[i]]$width <- input$nj_fruit_width
        tiles[[i]]$offset <- input$nj_fruit_offset
        nj_tiles(tiles)
      },
      ignoreInit = TRUE
    )
    # Restore the stored settings into the controls when the tile selector moves.
    shiny$observeEvent(input$nj_tile_num, {
      tile <- nj_tiles()[[as.integer(input$nj_tile_num)]]
      bslib::update_switch("nj_tiles_show", value = tile$show)
      shiny$updateSelectInput(session, "nj_fruit_variable", selected = tile$variable)
      shiny$updateSelectInput(session, "nj_tiles_scale", selected = tile$scale)
    })
    shiny$observeEvent(input$nj_tile_number, {
      tile <- nj_tiles()[[as.integer(input$nj_tile_number)]]
      shiny$updateSliderInput(session, "nj_fruit_alpha", value = tile$alpha)
      shiny$updateSliderInput(session, "nj_fruit_width", value = tile$width)
      shiny$updateSliderInput(session, "nj_fruit_offset", value = tile$offset)
    })

    # Heatmap column picker modal.
    shiny$observeEvent(input$nj_heatmap_button, {
      fields <- setdiff(names(viz_metadata()), "isolate")
      shiny$showModal(shiny$modalDialog(
        title = "Heatmap variables",
        shiny$checkboxGroupInput(
          ns("nj_heatmap_cols"),
          NULL,
          choices = fields,
          selected = nj_heatmap_select()
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("nj_heatmap_apply"), "Apply")
        ),
        easyClose = TRUE
      ))
    })
    shiny$observeEvent(input$nj_heatmap_apply, {
      nj_heatmap_select(input$nj_heatmap_cols %||% character(0))
      shiny$removeModal()
    })

    # Render the current tree to a file at the configured aspect ratio.
    output$download_nj <- shiny$downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_tree.", input$nj_filetype)
      },
      content = function(file) {
        aspect <- if (
          identical(input$nj_layout, "circular") ||
            identical(input$nj_layout, "inward")
        ) {
          1
        } else {
          input$nj_aspect_ratio %||% 0.6
        }
        save_tree_plot(tree_plot_built(), file, input$nj_filetype, aspect)
      }
    )
    shiny$observeEvent(input$nj_download, {
      shinyjs::click("download_nj")
    })

    # Keep the outputs reactive while hidden: the panel is nav_remove'd on
    # session reset AND the inactive engine's panel is display:none-hidden by
    # navset_hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    shiny$outputOptions(output, "tree_plot", suspendWhenHidden = FALSE)
  })
}
