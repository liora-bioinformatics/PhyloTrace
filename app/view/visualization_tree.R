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
    pickerOptions,
    updatePickerInput
  ],
  stats[setNames],
)
box::use(
  app / logic / functions[render_info],
  app /
    logic /
    tree_plot[
      build_tree_ggtree,
      save_tree_plot,
      tree_auto_layout,
      TREE_FIT_DEFAULTS
    ],
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
      reset_viz_colors,
      reset_viz_radio_buttons,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
)

# --- Metadata tile strips ----------------------------------------------------

# Canonical shape of one tile strip's settings. `nj_tiles` holds N_TILES of
# these in a plain list of lists; the snapshot/restore path must rebuild exactly
# that shape (see .normalize_tiles).
N_TILES <- 5L
TILE_DEFAULTS <- list(
  show = FALSE,
  variable = NULL,
  scale = "viridis",
  alpha = 1,
  width = 2,
  offset = 0.05
)

# jsonlite reads a JSON array of same-shaped objects back as a *data.frame*, so
# a saved snapshot's `.tiles` does NOT come back as the list-of-lists `nj_tiles`
# requires. Assigning that straight into the reactiveVal corrupts it, and the
# tile observers then fail with "replacement has 6 rows, data has 5" (they do
# `tiles[[i]]$show <- ...`, which on a data.frame column coerces the LHS to a
# list and grows it). Rebuild the canonical shape from whatever JSON handed us,
# filling anything absent from the defaults so older/partial snapshots restore
# cleanly too.
.normalize_tiles <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  rows <- if (is.data.frame(x)) {
    lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
  } else if (is.list(x)) {
    x
  } else {
    return(NULL)
  }

  lapply(seq_len(N_TILES), function(i) {
    tile <- TILE_DEFAULTS
    row <- if (i <= length(rows)) rows[[i]] else NULL
    if (is.list(row)) {
      for (f in names(TILE_DEFAULTS)) {
        v <- row[[f]]
        # A JSON null arrives as NULL or NA — keep the default for those.
        if (!is.null(v) && !is.list(v) && length(v) == 1L && !is.na(v)) {
          tile[[f]] <- unname(v[[1]])
        }
      }
    }
    tile
  })
}

# `character(0)` serialises to `[]`, which jsonlite reads back as an empty
# *list* rather than a character vector — coerce it back so the heatmap column
# selection stays the type the plot builder expects.
.normalize_heatmap_select <- function(x) {
  v <- unlist(x, use.names = FALSE)
  if (!length(v)) character(0) else as.character(v)
}

# --- Plot device geometry ----------------------------------------------------

# The canvas the tree is drawn on: 1056 x (1056 * aspect) pixels at 192 dpi,
# i.e. 5.5 inches wide. tree_auto_layout works in inches because ggplot2's text
# sizes do.
#
# Fixed, and deliberately *not* the panel's own width. renderPlot re-executes
# whenever the output's pixel size changes, so sizing the canvas from
# session$clientData makes every width the browser reports — the first layout
# pass, a font finishing loading, a scrollbar appearing, bslib settling a fill
# container, any window drag — redraw a plot that takes a second to build for a
# few hundred isolates. That is what put three "Rendering tree_plot" lines in
# the log for a single Generate. Nothing in the render path reads clientData
# now (renderPlot's own `width = "auto"` default is replaced too, since that is
# a clientData dependency in disguise), so no report from the browser can
# trigger a draw: the only things that can are a new tree and a changed
# control.
#
# What the panel width is worth here is presentation, and CSS already does that
# — the stage scales the finished image to fit, or shows it full size under
# Zoom view (the .tree-stage rules in app/styles/main.scss). A fixed canvas
# also makes the export match the preview exactly, and makes a saved Analysis
# render identically on a different screen.
PLOT_RES <- 192
PLOT_WIDTH_PX <- 1056
# A few hundred tips at aspect 8 is already ~8400px; this is the ceiling.
PLOT_MAX_PX <- 12000

# --- Controls fitted to the data ---------------------------------------------

# Which field of a tree_auto_layout() fit feeds which control.
FITTED_FIELDS <- c(
  nj_aspect_ratio = "aspect",
  nj_tiplab_size = "tiplab_size",
  nj_branch_size = "branch_size",
  nj_tippoint_size = "tippoint_size",
  nj_nodepoint_size = "nodepoint_size",
  nj_zoom = "zoom",
  nj_h = "h"
)

# The values those controls hold before any data is loaded. Taken from the logic
# module rather than written out again here, because they are also the fit's
# calibration anchor (tree_auto_layout returns exactly these at ~15 tips) — a
# slider declared with anything else would quietly move the anchor. Used both to
# build the sliders and to seed the mirrors below.
FITTED_DEFAULTS <- setNames(
  TREE_FIT_DEFAULTS[FITTED_FIELDS],
  names(FITTED_FIELDS)
)

# The other half of what the render reads through a mirror: the selects the
# server resolves rather than the user, from the loaded metadata
# (populate_metadata_selects) or from the mapped variable's type
# (filter_scale_choices). They need mirroring for exactly the reason the fitted
# sliders do — updatePickerInput reaches input$ only once the browser has
# echoed it back, a flush after the tree it belongs to was drawn, so the plot
# is drawn once with the stale value and again with the resolved one. The
# console named "tiplab" as one of those second draws.
MIRRORED_SELECTS <- c(
  "nj_tiplab",
  "nj_root_isolate",
  "nj_branch_label",
  "nj_color_mapping",
  "nj_tipcolor_mapping",
  "nj_tipshape_mapping",
  "nj_parentnode",
  "nj_tiplab_scale",
  "nj_tippoint_scale"
)

# Everything mirrored, in one list: the fitted sliders, the tip-label switch the
# fit can turn off, and those selects.
MIRRORED_IDS <- c(names(FITTED_DEFAULTS), "nj_tiplab_show", MIRRORED_SELECTS)

# TEMPORARY (2026-07-28) — names what actually changed ahead of each rebuild,
# so the console says *why* a Generate drew the tree rather than only that it
# did. Silent unless something really changed, so a well-behaved Generate logs
# one line. Remove once the count is confirmed settled.
.log_rebuild <- function(previous, current) {
  changed <- if (is.null(previous)) {
    "first plot"
  } else {
    fields <- union(names(previous$opts), names(current$opts))
    paste(
      c(
        if (!identical(previous$tree, current$tree)) "tree",
        if (!identical(previous$metadata, current$metadata)) "metadata",
        fields[
          !vapply(
            fields,
            function(f) identical(previous$opts[[f]], current$opts[[f]]),
            logical(1)
          )
        ]
      ),
      collapse = ", "
    )
  }
  message(
    format(Sys.time(), digits = 3L),
    " | ----- tree rebuild: ",
    changed
  )
}

# Longest label the tips will carry, for the width half of the layout fit. The
# tree's own tip labels are the isolate names (including any folded in from an
# imported set, which the metadata table does not carry); every other label
# source is a metadata column.
.label_chars <- function(tree, meta, field) {
  vals <- if (
    !identical(field, "isolate") &&
      !is.null(meta) &&
      isTRUE(field %in% names(meta))
  ) {
    meta[[field]]
  } else {
    tree$tip.label
  }
  suppressWarnings(max(nchar(as.character(vals)), 1L))
}

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
            shiny$pickerInput(ns("nj_tiplab"), "Label source", label_vars),
            layout_columns(
              col_widths = c(6, 6),
              # Floor below the 1 this used to stop at: a few hundred tips are
              # legible only at ~2mm, and the fit (tree_auto_layout) needs room
              # underneath that for the trees that are larger still.
              shiny$sliderInput(
                ns("nj_tiplab_size"),
                "Size",
                0.5,
                10,
                FITTED_DEFAULTS$nj_tiplab_size,
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
              shiny$pickerInput(ns("nj_tiplab_fontface"), "Font", fontfaces),
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
            shiny$pickerInput(
              ns("nj_branch_label"),
              "Label source",
              branch_vars
            ),
            layout_columns(
              col_widths = c(6, 6),
              # Same floor and step as the tip labels: branch labels are fitted
              # to the same row pitch, so they need the same room to move in.
              shiny$sliderInput(
                ns("nj_branch_size"),
                "Size",
                0.5,
                10,
                FITTED_DEFAULTS$nj_branch_size,
                step = 0.1,
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
            # allow-free-text: title/subtitle are display text, so they take
            # spaces and punctuation — they opt out of the identifier charset
            # restriction in app/js/index.js.
            shiny$div(
              class = "allow-free-text",
              shiny$textInput(
                ns("nj_title"),
                "Title",
                placeholder = "Plot title"
              )
            ),
            shiny$sliderInput(
              ns("nj_title_size"),
              "Title size",
              15,
              40,
              30,
              ticks = FALSE
            ),
            shiny$div(
              class = "allow-free-text",
              shiny$textInput(
                ns("nj_subtitle"),
                "Subtitle",
                placeholder = "Plot subtitle"
              )
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
            shiny$pickerInput(ns("nj_color_mapping"), "Variable", meta_vars),
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
            shiny$pickerInput(ns("nj_tipcolor_mapping"), "Variable", meta_vars),
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
            shiny$pickerInput(ns("nj_tipshape_mapping"), "Variable", meta_vars)
          ),
          accordion_panel(
            "Tiles",
            icon = shiny$icon("table-cells"),
            shiny$pickerInput(ns("nj_tile_num"), "Tile", as.character(1:5)),
            input_switch(ns("nj_tiles_show"), "Show tile", FALSE),
            shiny$pickerInput(ns("nj_fruit_variable"), "Variable", meta_vars),
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
            shiny$pickerInput(ns("nj_tippoint_shape"), "Shape", point_shapes),
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
                0.5,
                20,
                FITTED_DEFAULTS$nj_tippoint_size,
                step = 0.1,
                ticks = FALSE
              )
            )
          ),
          accordion_panel(
            "Node Points",
            icon = shiny$icon("circle-dot"),
            input_switch(ns("nj_nodepoint_show"), "Show node points", FALSE),
            shiny$pickerInput(ns("nj_nodepoint_shape"), "Shape", point_shapes),
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
                0.5,
                20,
                FITTED_DEFAULTS$nj_nodepoint_size,
                step = 0.1,
                ticks = FALSE
              )
            )
          ),
          accordion_panel(
            "Tiles",
            icon = shiny$icon("table-cells"),
            shiny$pickerInput(ns("nj_tile_number"), "Tile", as.character(1:5)),
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
                liveSearchPlaceholder = "Search nodes ...",
                container = "body"
              )
            ),
            viz_color(ns, "nj_clade_scale", "Highlight color", "#D0F221"),
            shiny$pickerInput(
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
            # Ceiling well above the 2 this used to stop at: height per tip is
            # what makes a tree readable, so a few hundred isolates need a tall,
            # scrollable plot (Options > Zoom) that the old range could not
            # express at all. Generate fits this to the data — see
            # tree_auto_layout.
            shiny$sliderInput(
              ns("nj_aspect_ratio"),
              "Aspect ratio",
              0.3,
              8,
              FITTED_DEFAULTS$nj_aspect_ratio,
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
                FITTED_DEFAULTS$nj_h,
                step = 0.01,
                ticks = FALSE
              )
            ),
            shiny$sliderInput(
              ns("nj_zoom"),
              "Zoom",
              0.5,
              1.5,
              FITTED_DEFAULTS$nj_zoom,
              step = 0.05,
              ticks = FALSE
            )
          ),
          accordion_panel(
            "Tree Rooting",
            icon = shiny$icon("seedling"),
            shiny$pickerInput(ns("nj_root_isolate"), "Outgroup", c("Automatic"))
          ),
          accordion_panel(
            "Layout",
            icon = shiny$icon("project-diagram"),
            shiny$pickerInput(
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
            shiny$sliderInput(
              ns("nj_legend_size"),
              "Size",
              5,
              25,
              10,
              ticks = FALSE
            ),
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
ui <- function(id, generate_id) {
  ns <- shiny$NS(id)

  layout_sidebar(
    # See visualization_mst.R: `padding` replaces the old non-unique
    # `id = "plot-sidebar"`, which layout_sidebar has no formal for.
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
          tree_controls(ns)
        )
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

    # Width of the canvas, in inches. Everything that has to reason about how
    # much room the plot has — the layout fit at Generate, the tip-label
    # reserve, the export canvas — goes through this one constant, which is why
    # all three agree and why none of them can be invalidated by the browser
    # (see PLOT_WIDTH_PX).
    plot_width_in <- function() PLOT_WIDTH_PX / PLOT_RES

    # Mirrors of the fitted controls, and the only thing the render reads for
    # them — never input$nj_aspect_ratio and friends directly.
    #
    # updateSliderInput does not set an input; it sends a message to the browser,
    # which applies it and echoes the new value back as an input change a flush
    # later. A plot built from the inputs is therefore drawn once with the stale
    # values and then again with the fitted ones — and once more per control,
    # since each slider echoes separately, which is the three renders one
    # Generate used to log. Writing the fit into these mirrors *before* the tree
    # is published makes the first draw the correct one; the echo that follows
    # then assigns a mirror the value it already holds, and shiny does not treat
    # that as a change, so nothing redraws. No delays and no ordering
    # assumptions are involved, so there is nothing to race.
    #
    # nj_tiplab_show is mirrored for the same reason though it is not a slider:
    # the fit switches it off when no legible label size exists, and that update
    # echoes back exactly as a slider's does.
    fitted <- do.call(
      shiny$reactiveValues,
      c(FITTED_DEFAULTS, list(nj_tiplab_show = TRUE))
    )

    # all.equal, not identical: the browser can echo 0.6 back as 0.6000000000001
    # and a bit-exact test would read that as a fresh user edit.
    set_fitted <- function(id, value) {
      if (!isTRUE(all.equal(shiny$isolate(fitted[[id]]), value))) {
        fitted[[id]] <- value
      }
    }

    # A user drag lands in the same mirror the fit writes to, so the two are
    # indistinguishable downstream and the last one to happen simply wins.
    lapply(MIRRORED_IDS, function(id) {
      shiny$observeEvent(input[[id]], set_fitted(id, input[[id]]))
    })

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

      # The resolved value goes to the mirror as well as to the browser, and
      # the mirror is what the plot reads. Sending it only to the browser means
      # it comes back an echo later, after the tree has already been drawn from
      # the stale value — a second draw for a label source that was decided
      # before the tree was even computed.
      keep <- function(id, choices, default) {
        value <- if (!force_default && isTRUE(input[[id]] %in% choices)) {
          input[[id]]
        } else {
          default
        }
        updatePickerInput(
          session,
          id,
          choices = choices,
          selected = value
        )
        set_fitted(id, value)
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
      root <- if (!force_default && isTRUE(input$nj_root_isolate %in% tips)) {
        input$nj_root_isolate
      } else {
        "Automatic"
      }
      updatePickerInput(
        session,
        "nj_root_isolate",
        choices = c("Automatic", tips),
        selected = root
      )
      set_fitted("nj_root_isolate", root)
      if (n_tip >= 3) {
        n_node <- if (identical(algo(), "UPGMA")) {
          n_tip - 1L
        } else {
          n_tip - 2L
        }
        nodes <- as.character(seq.int(n_tip + 1L, n_tip + n_node))
        picked <- if (force_default) {
          character(0)
        } else {
          intersect(input$nj_parentnode, nodes)
        }
        shinyWidgets::updatePickerInput(
          session,
          "nj_parentnode",
          choices = nodes,
          selected = picked
        )
        set_fitted("nj_parentnode", picked)
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

    # Settings for the metadata tile strips. The Mapping and Elements tabs each
    # edit one strip (selected by nj_tile_num / nj_tile_number). Shape is
    # TILE_DEFAULTS x N_TILES — see .normalize_tiles(), which the restore path
    # uses to rebuild exactly this from a snapshot.
    nj_tiles <- shiny$reactiveVal(
      replicate(N_TILES, TILE_DEFAULTS, simplify = FALSE)
    )

    # Metadata columns selected for the heatmap annotation (via its modal).
    nj_heatmap_select <- shiny$reactiveVal(character(0))

    # Resolved Tree control values, shared by the live render and the export.
    tree_opts <- shiny$reactive(
      list(
        # Layout / rooting.
        root = fitted$nj_root_isolate,
        layout = input$nj_layout,
        ladderize = input$nj_ladder,
        line_color = input$nj_color,
        bg = input$nj_bg,
        # Tip labels.
        tiplab_show = fitted$nj_tiplab_show,
        tiplab = fitted$nj_tiplab,
        tiplab_size = fitted$nj_tiplab_size,
        tiplab_alpha = input$nj_tiplab_alpha,
        tiplab_fontface = input$nj_tiplab_fontface,
        tiplab_position = input$nj_tiplab_position %||% 0,
        tiplab_angle = input$nj_tiplab_angle,
        align = input$nj_align,
        label_panel = input$nj_geom %||% FALSE,
        tiplab_color = input$nj_tiplab_color,
        tiplab_fill = input$nj_tiplab_fill,
        # Tip-label color mapping. A mapping's variable and palette are read
        # only when its switch is on, so that switched off they hold a constant
        # rather than whatever the pickers happen to carry. Generate rewrites
        # those pickers (populate_metadata_selects, filter_scale_choices) and
        # every rewrite echoes back; without this the plot would count an echo
        # about something it is not drawing as a reason to draw again.
        mapping_show = input$nj_mapping_show,
        color_mapping = if (isTRUE(input$nj_mapping_show)) {
          fitted$nj_color_mapping
        },
        tiplab_scale = if (isTRUE(input$nj_mapping_show)) {
          fitted$nj_tiplab_scale
        },
        # Branch labels.
        branch_show = input$nj_show_branch_label,
        branch_label = if (isTRUE(input$nj_show_branch_label)) {
          fitted$nj_branch_label
        },
        branch_size = fitted$nj_branch_size,
        branch_cutoff = input$nj_branchlabel_cutoff,
        branch_color = input$nj_branch_color,
        branch_label_color = input$nj_branch_label_color,
        # Tip / node points.
        tippoint_show = input$nj_tippoint_show,
        tippoint_alpha = input$nj_tippoint_alpha,
        tippoint_size = fitted$nj_tippoint_size,
        tippoint_color = input$nj_tippoint_color,
        tippoint_shape = input$nj_tippoint_shape,
        tipcolor_mapping_show = input$nj_tipcolor_mapping_show,
        tipcolor_mapping = if (isTRUE(input$nj_tipcolor_mapping_show)) {
          fitted$nj_tipcolor_mapping
        },
        tippoint_scale = if (isTRUE(input$nj_tipcolor_mapping_show)) {
          fitted$nj_tippoint_scale
        },
        tipshape_mapping_show = input$nj_tipshape_mapping_show,
        tipshape_mapping = if (isTRUE(input$nj_tipshape_mapping_show)) {
          fitted$nj_tipshape_mapping
        },
        nodepoint_show = input$nj_nodepoint_show,
        nodepoint_alpha = input$nj_nodepoint_alpha,
        nodepoint_color = input$nj_nodepoint_color,
        nodepoint_shape = input$nj_nodepoint_shape,
        nodepoint_size = fitted$nj_nodepoint_size,
        # Clade highlights.
        nodelabel_show = input$nj_nodelabel_show,
        parentnodes = fitted$nj_parentnode %||% character(0),
        clade_color = input$nj_clade_scale,
        clade_type = input$nj_clade_type,
        # Tiles / heatmap. Only the strips actually being drawn: Generate
        # repopulates the tile variable picker whether or not a strip is shown,
        # and the observer behind it rewrites nj_tiles() in response — which the
        # console named ("tiles") as a second draw for five hidden strips. The
        # builder discards hidden strips anyway, so this only moves that filter
        # to where it can also stop a redraw.
        tiles = Filter(function(t) isTRUE(t$show), nj_tiles()),
        heatmap_show = input$nj_heatmap_show,
        heatmap_select = nj_heatmap_select(),
        # Elements toggles.
        rootedge_show = input$nj_rootedge_show,
        treescale_show = input$nj_treescale_show,
        # Panel width, for the tip-label reserve (see .tiplab_frac).
        width_in = plot_width_in(),
        # Titles.
        title = input$nj_title,
        subtitle = input$nj_subtitle,
        title_color = input$nj_title_color,
        title_size = input$nj_title_size,
        subtitle_size = input$nj_subtitle_size,
        # Dimensions / legend.
        zoom = fitted$nj_zoom,
        h = fitted$nj_h,
        v = input$nj_v,
        legend_orientation = input$nj_legend_orientation,
        legend_x = input$nj_legend_x,
        legend_y = input$nj_legend_y,
        legend_size = input$nj_legend_size
      )
    )

    # Everything the ggtree build consumes, republished only when it actually
    # differs from what was published last.
    #
    # Building straight from tree_opts() makes the redraw *invalidation*-driven:
    # any control that reports a change redraws the plot, whether or not the
    # change means anything. A Generate touches a great many of them without
    # changing what they resolve to — populate_metadata_selects alone
    # repopulates seven metadata-backed selects and a picker, each echoing back
    # from the browser in its own flush — and at a few hundred tips every one of
    # those echoes costs a second of drawing. Comparing the resolved value and
    # republishing only on a real difference makes it *value*-driven instead,
    # which no echo can defeat, whichever control it came from.
    #
    # priority: the barrier has to settle before outputs are recalculated in the
    # same flush, or the plot draws once from the stale value and again from the
    # fresh one — the very thing this exists to stop. Higher priority than the
    # default 0 that output observers carry guarantees the order.
    plot_inputs <- shiny$reactiveVal(NULL)
    shiny$observe(
      {
        shiny$req(tree_obj())
        current <- list(
          tree = tree_obj(),
          metadata = viz_metadata(),
          opts = tree_opts()
        )
        previous <- shiny$isolate(plot_inputs())
        if (!identical(previous, current)) {
          .log_rebuild(previous, current)
          plot_inputs(current)
        }
      },
      priority = 100
    )

    # The ggtree plot: rebuilt when — and only when — one of those inputs
    # differs; the phylo itself is never recomputed here.
    tree_plot_built <- shiny$reactive({
      p <- plot_inputs()
      shiny$req(p)
      build_tree_ggtree(p$tree, p$metadata, p$opts)
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
      # Fit the layout controls to the data before publishing the tree, the way
      # the Map fits its "Max intensity" slider to the busiest place. The coded
      # defaults (aspect 0.6, tip label size 4) suit roughly fifteen tips and
      # nothing else — five isolates got a strip with oversized, overlapping
      # labels and three hundred got an unreadable smear that no hand-tuning
      # could fix, since the aspect ratio needed is several times what the
      # slider used to offer. Re-fitted on every Generate, so a manual tweak
      # lives until the next one.
      #
      # Each fitted value goes to two places: the mirror the render reads, so
      # this Generate draws once and draws right, and the slider, so the sidebar
      # shows what was chosen and stays the place to adjust it. The slider is
      # only touched when the value really changed — its echo is harmless (see
      # `fitted`) but pointless.
      if (!is.null(tree)) {
        fit <- tree_auto_layout(
          length(tree$tip.label),
          plot_width_in(),
          input$nj_layout,
          .label_chars(tree, viz_metadata(), input$nj_tiplab)
        )
        for (id in names(FITTED_DEFAULTS)) {
          value <- fit[[FITTED_FIELDS[[id]]]]
          if (!isTRUE(all.equal(shiny$isolate(input[[id]]), value))) {
            shiny$updateSliderInput(session, id, value = value)
          }
          set_fitted(id, value)
        }
        # Past a certain tip count the labels are a grey smudge at any size that
        # fits, so the fit draws the tree without them — and says so, rather
        # than silently moving a toggle the user may have set deliberately.
        if (
          !fit$labels_legible && isTRUE(shiny$isolate(fitted$nj_tiplab_show))
        ) {
          set_fitted("nj_tiplab_show", FALSE)
          bslib::update_switch("nj_tiplab_show", value = FALSE)
          shiny$showNotification(
            paste0(
              length(tree$tip.label),
              " isolates leave no room for readable tip labels — they have ",
              "been switched off. Narrow the isolate selection to bring them ",
              "back, or re-enable them under Labels > Isolate Labels."
            ),
            type = "warning",
            duration = 12
          )
        }
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
      updatePickerInput(
        session,
        input_id,
        choices = color_scales[cats],
        selected = sel
      )
      # Mirrored for the same reason as the metadata selects above — the plot
      # reads the palette from the mirror, not from the echo.
      set_fitted(input_id, sel)
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

    # The ggtree plot, drawn on the fixed canvas at the aspect ratio the fit
    # chose (circular/inward layouts are square), at print resolution.
    #
    # `width` is given explicitly rather than left at renderPlot's "auto",
    # which resolves to the panel's clientData width and would re-execute this
    # — a second of work for a few hundred tips — on every width the browser
    # reports. Between them these two functions are the plot's whole dependency
    # on its own size, and neither can be invalidated from the client.
    output$tree_plot <- shiny$renderPlot(
      {
        render_info("visualization_tree tree_plot")
        tree_plot_built()
      },
      width = function() PLOT_WIDTH_PX,
      height = function() {
        aspect <- if (
          identical(input$nj_layout, "circular") ||
            identical(input$nj_layout, "inward")
        ) {
          1
        } else {
          fitted$nj_aspect_ratio
        }
        min(PLOT_MAX_PX, as.integer(PLOT_WIDTH_PX * aspect))
      },
      res = PLOT_RES
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
      updatePickerInput(
        session,
        "nj_fruit_variable",
        selected = tile$variable
      )
      updatePickerInput(session, "nj_tiles_scale", selected = tile$scale)
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
          fitted$nj_aspect_ratio
        }
        # Exported on the same canvas width as the preview, so the tip labels
        # keep the proportion they were tuned to (see save_tree_plot).
        save_tree_plot(
          tree_plot_built(),
          file,
          input$nj_filetype,
          aspect,
          width = plot_width_in()
        )
      }
    )
    shiny$observeEvent(input$nj_download, {
      shinyjs::click("download_nj")
    })

    # `plot_area` is a cheap renderUI gating the "press Generate" prompt, and
    # the plot output has to bind through it, so it stays live while hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    # The tree itself is a server-side ggplot with no client state to lose, so
    # let Shiny suspend it while its plot tab is in the background. With
    # several tabs open that is the difference between one metadata change
    # re-rendering every tree in the session and re-rendering only the visible
    # one; it re-executes on the way back in.
    shiny$outputOptions(output, "tree_plot", suspendWhenHidden = TRUE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    # Snapshot the nj_* controls plus the two pieces of state held in
    # reactiveVals rather than inputs (per-tile config and the heatmap columns).
    snapshot <- shiny$reactive(c(
      collect_input_snapshot(input, "nj_"),
      list(.tiles = nj_tiles(), .heatmap_select = nj_heatmap_select())
    ))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "nj_tiplab_show",
          "nj_align",
          "nj_show_branch_label",
          "nj_mapping_show",
          "nj_tipcolor_mapping_show",
          "nj_tipshape_mapping_show",
          "nj_tiles_show",
          "nj_heatmap_show",
          "nj_tippoint_show",
          "nj_nodepoint_show",
          "nj_nodelabel_show",
          "nj_ladder",
          "nj_rootedge_show",
          "nj_treescale_show"
        ),
        selects = c(
          "nj_tiplab_fontface",
          "nj_tiplab_scale",
          "nj_tippoint_scale",
          "nj_tiles_scale",
          "nj_heatmap_scale",
          "nj_tippoint_shape",
          "nj_nodepoint_shape",
          "nj_tile_num",
          "nj_tile_number",
          "nj_clade_type",
          "nj_layout"
        ),
        sliders = c(
          "nj_tiplab_size",
          "nj_tiplab_alpha",
          "nj_tiplab_angle",
          "nj_branch_size",
          "nj_branchlabel_cutoff",
          "nj_title_size",
          "nj_subtitle_size",
          "nj_tippoint_alpha",
          "nj_tippoint_size",
          "nj_nodepoint_alpha",
          "nj_nodepoint_size",
          "nj_fruit_alpha",
          "nj_fruit_width",
          "nj_fruit_offset",
          "nj_aspect_ratio",
          "nj_v",
          "nj_h",
          "nj_zoom",
          "nj_legend_size",
          "nj_legend_x",
          "nj_legend_y"
        ),
        texts = c("nj_title", "nj_subtitle"),
        colors = c(
          "nj_color",
          "nj_bg",
          "nj_title_color",
          "nj_tiplab_color",
          "nj_tiplab_fill",
          "nj_branch_color",
          "nj_branch_label_color",
          "nj_tippoint_color",
          "nj_nodepoint_color",
          "nj_clade_scale"
        ),
        radio_groups = "nj_legend_orientation"
      )

      # Put the fitted controls' saved values straight into the mirrors the
      # render reads, so restoring an Analysis redraws the tree once rather than
      # once more for each of these echoing back from the browser (see `fitted`).
      for (id in MIRRORED_IDS) {
        if (!is.null(vals[[id]])) {
          set_fitted(id, vals[[id]])
        }
      }

      # Metadata-/isolate-backed selects: set choices alongside the value so the
      # saved field/isolate/node sticks (mirrors populate_metadata_selects()).
      meta <- viz_metadata()
      if (!is.null(meta) && length(names(meta))) {
        fields <- names(meta)
        setsel <- function(id, choices) {
          if (!is.null(vals[[id]])) {
            updatePickerInput(
              session,
              id,
              choices = choices,
              selected = vals[[id]]
            )
          }
        }
        setsel("nj_tiplab", fields)
        setsel("nj_color_mapping", fields)
        setsel("nj_tipcolor_mapping", fields)
        setsel("nj_tipshape_mapping", fields)
        setsel("nj_fruit_variable", fields)
        setsel("nj_branch_label", c("Allelic Distance", fields))

        tips <- meta$isolate
        if (!is.null(vals$nj_root_isolate)) {
          # An Analysis's isolate selection can change after a plot was saved,
          # so the stored outgroup may no longer be among the current isolates.
          # Fall back to "Automatic" rather than leaving the select pinned to a
          # value it can no longer offer (which renders it blank).
          root <- vals$nj_root_isolate
          if (!isTRUE(root %in% c("Automatic", tips))) {
            root <- "Automatic"
          }
          updatePickerInput(
            session,
            "nj_root_isolate",
            choices = c("Automatic", tips),
            selected = root
          )
        }
        n_tip <- length(tips)
        if (n_tip >= 3 && !is.null(vals$nj_parentnode)) {
          n_node <- if (identical(algo(), "UPGMA")) n_tip - 1L else n_tip - 2L
          nodes <- as.character(seq.int(n_tip + 1L, n_tip + n_node))
          shinyWidgets::updatePickerInput(
            session,
            "nj_parentnode",
            choices = nodes,
            selected = intersect(vals$nj_parentnode, nodes)
          )
        }
      }

      # Both come back from JSON in the wrong container type — normalise before
      # storing, or the tile observers crash on the next control change.
      if (!is.null(vals$.tiles)) {
        tiles <- .normalize_tiles(vals$.tiles)
        if (!is.null(tiles)) {
          nj_tiles(tiles)
        }
      }
      if (!is.null(vals$.heatmap_select)) {
        nj_heatmap_select(.normalize_heatmap_select(vals$.heatmap_select))
      }
    }

    # Thumbnail: server-render the ggtree to a small PNG on the preview's own
    # canvas (so it looks like the plot it stands for), with dpi carrying the
    # requested pixel width.
    save_thumb <- function(file, w, h) {
      aspect <- if (
        identical(input$nj_layout, "circular") ||
          identical(input$nj_layout, "inward")
      ) {
        1
      } else {
        fitted$nj_aspect_ratio
      }
      width_in <- plot_width_in()
      save_tree_plot(
        tree_plot_built(),
        file,
        "png",
        aspect,
        width = width_in,
        dpi = max(24, round(w / width_in))
      )
    }

    list(
      snapshot = snapshot,
      restore = restore,
      save_thumb = save_thumb,
      request_thumb = NULL,
      thumb_data = NULL
    )
  })
}
