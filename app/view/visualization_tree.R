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
    updatePickerInput,
    updateVirtualSelect
  ],
  stats[setNames],
)
box::use(
  app / logic / field_labels[field_labels_for, grouped_field_choices],
  app /
    logic /
    field_profile[
      field_levels,
      mapping_fields,
      MAX_SHAPE_LEVELS,
      profile_for,
      scale_categories_for
    ],
  app /
    logic /
    mapping_engine[
      aesthetic_block_reason,
      AESTHETIC_LABELS,
      assign_mapping_layer,
      COLOR_AESTHETICS,
      eligible_aesthetics,
      MAX_LAYERS,
      rebalance_layers
    ],
  app / logic / functions[render_info],
  app /
    logic /
    tree_plot[
      build_tree_ggtree,
      save_tree_plot,
      tree_auto_layout,
      tree_branch_cutoff,
      TREE_FIT_DEFAULTS
    ],
  app / logic / phylo[compute_phylo_tree],
  app /
    logic /
    viz_helpers[
      meta_vars,
      label_vars,
      point_shapes,
      viz_color,
      field_select,
      update_field_select,
      scale_select,
      color_scales,
      suitable_scale_categories,
      reset_viz_colors,
      reset_viz_radio_buttons,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
)

# --- Variable mapping layers -------------------------------------------------

# Canonical shape of one mapping layer. The engine fills these in
# (app/logic/mapping_engine.R); the snapshot/restore path must rebuild exactly
# this shape (see .normalize_layers).
LAYER_DEFAULTS <- list(
  id = NA_character_,
  field = NA_character_,
  title = NA_character_,
  aesthetic = "tiplab_color",
  palette = "viridis",
  family = "Gradient",
  n_levels = 1L,
  continuous = FALSE,
  transform = NULL,
  auto = TRUE
)

# The heatmap panels, in draw order. Only the appender-built families make a
# coherent matrix: their columns are the same measurement repeated, which is
# what one shared fill scale is for. Sample metadata is a bag of unrelated
# fields, so it is structurally absent here rather than filtered out later.
HEATMAP_KINDS <- list(
  amr = list(
    attr = "amr_cols",
    title = "AMR screening",
    palette = "Reds",
    empty = "No AMR screening results in this database."
  ),
  custom = list(
    attr = "custom_cols",
    title = "Custom variables",
    palette = "Blues",
    empty = "No custom variables defined for this database."
  )
)

# jsonlite reads a JSON array of same-shaped objects back as a *data.frame*, so
# a saved snapshot's `.layers` does NOT come back as the list-of-lists the
# reactiveVal holds. Assigning that straight in corrupts it. Rebuild the
# canonical shape from whatever JSON handed us, filling anything absent from
# the defaults so older or partial snapshots restore cleanly too.
.normalize_records <- function(x, defaults) {
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

  lapply(rows, function(row) {
    rec <- defaults
    if (is.list(row)) {
      for (f in names(row)) {
        v <- row[[f]]
        # A JSON null arrives as NULL or NA — keep the default for those.
        if (is.null(v) || (!is.list(v) && length(v) == 1L && is.na(v))) {
          next
        }
        rec[[f]] <- if (is.list(v) && length(v) == 1L) {
          unname(v[[1]])
        } else {
          unname(v)
        }
      }
    }
    rec
  })
}

.normalize_layers <- function(x) {
  out <- .normalize_records(x, LAYER_DEFAULTS)
  if (is.null(out)) {
    return(NULL)
  }
  # A layer whose field is gone (the database changed under a saved Analysis)
  # cannot be drawn and must not reach the builder.
  Filter(function(l) !is.na(l$field %||% NA), out)
}

# A row-action button that reports which record it belongs to. The id travels
# in the value rather than in the button's own input id, so one observer serves
# every row however many times the list re-renders.
.layer_btn <- function(ns, input_id, record_id, icon, title) {
  shiny$tags$button(
    type = "button",
    class = "btn btn-sm tree-layer_btn",
    title = title,
    `aria-label` = title,
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      ns(input_id),
      record_id
    ),
    shiny$icon(icon)
  )
}

.normalize_heatmaps <- function(x) {
  out <- .normalize_records(
    x,
    list(kind = NA_character_, cols = character(0), palette = "Reds",
         title = NA_character_)
  )
  if (is.null(out)) {
    return(NULL)
  }
  lapply(out, function(h) {
    # `character(0)` serialises to `[]`, which jsonlite reads back as an empty
    # *list* rather than a character vector.
    v <- unlist(h$cols, use.names = FALSE)
    h$cols <- if (!length(v)) character(0) else as.character(v)
    h
  })
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
  nj_zoom = "zoom",
  nj_h = "h",
  nj_branchlabel_cutoff = "branch_cutoff"
)

# The values those controls hold before any data is loaded. Taken from the logic
# module rather than written out again here, because they are also the fit's
# calibration anchor (tree_auto_layout returns exactly these at ~15 tips) — a
# slider declared with anything else would quietly move the anchor.
#
# nj_branchlabel_cutoff is fitted but not part of the geometry anchor, so it is
# not in TREE_FIT_DEFAULTS and has no seed value here.
FITTED_SEED_IDS <- setdiff(names(FITTED_FIELDS), "nj_branchlabel_cutoff")
FITTED_DEFAULTS <- setNames(
  TREE_FIT_DEFAULTS[FITTED_FIELDS[FITTED_SEED_IDS]],
  FITTED_SEED_IDS
)

# Not every id here still renders a slider — nj_branch_size is fitted from the
# tip count and the branch-label section is a bare switch now. It keeps its
# entry regardless, because this list seeds the *mirrors* the render reads
# (MIRRORED_IDS below), and dropping a control must never drop its mirror.

# The other half of what the render reads through a mirror: the selects the
# server resolves rather than the user, from the loaded metadata
# (populate_metadata_selects). They need mirroring for exactly the reason the
# fitted sliders do — updatePickerInput reaches input$ only once the browser
# has echoed it back, a flush after the tree it belongs to was drawn, so the
# plot is drawn once with the stale value and again with the resolved one. The
# console named "tiplab" as one of those second draws.
#
# The variable mappings used to need this too, and no longer do: a layer is
# written only when the user adds or edits one, so there is no server-sent
# value for the browser to echo back a flush later.
MIRRORED_SELECTS <- c(
  "nj_tiplab",
  "nj_root_isolate",
  "nj_parentnode"
)

# Everything mirrored, in one list: the fitted sliders, the tip-label switch the
# fit can turn off, and those selects.
MIRRORED_IDS <- c(
  names(FITTED_DEFAULTS),
  "nj_tiplab_show",
  "nj_tippoint_show",
  "nj_branchlabel_cutoff",
  MIRRORED_SELECTS
)

# Diagnostic for "why did the tree just redraw?", kept commented rather than
# deleted because it is the thing that answered that question and the answer was
# not guessable.
#
# render_info() reports *that* the plot drew, which is no help when one Generate
# draws three times: two of those were controls the server had resolved
# (populate_metadata_selects settling the label source, the tile observer
# rewriting five hidden strips) reaching input$ an echo later, after the tree had
# already been drawn from the stale value. Naming the changed fields is what
# identified them, where reasoning about the reactive graph twice did not.
#
# To use it: uncomment this and its call in the plot_inputs barrier below. It
# only prints when something really changed, so a healthy Generate logs one
# line — "first plot" — and anything more names the field responsible.
#
# .log_rebuild <- function(previous, current) {
#   changed <- if (is.null(previous)) {
#     "first plot"
#   } else {
#     fields <- union(names(previous$opts), names(current$opts))
#     paste(
#       c(
#         if (!identical(previous$tree, current$tree)) "tree",
#         if (!identical(previous$metadata, current$metadata)) "metadata",
#         fields[
#           !vapply(
#             fields,
#             function(f) identical(previous$opts[[f]], current$opts[[f]]),
#             logical(1)
#           )
#         ]
#       ),
#       collapse = ", "
#     )
#   }
#   message(
#     format(Sys.time(), digits = 3L),
#     " | ----- tree rebuild: ",
#     changed
#   )
# }

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
            pickerInput(ns("nj_tiplab"), "Label source", label_vars),
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
            input_switch(ns("nj_align"), "Align labels", TRUE)
          ),
          accordion_panel(
            "Branch Labels",
            icon = shiny$icon("code-branch"),
            # One switch, and nothing else. Allelic distance is the only value
            # worth writing on a branch, and both of the numbers that used to be
            # exposed here — the text size and the percentile cutoff above which
            # a branch earns a label — are fitted from the tree itself
            # (tree_auto_layout, tree_branch_cutoff). They stay as mirrors the
            # render reads; they are just no longer anyone's decision to make.
            input_switch(
              ns("nj_show_branch_label"),
              "Show branch labels",
              FALSE
            ),
            shiny$div(
              class = "text-muted small",
              "Allelic distance, on the longest branches. Size and density are",
              "fitted to the number of isolates."
            )
          )
        )
      ),
      # Variable mapping -------------------------------------------------------
      nav_panel(
        "Mapping",
        icon = shiny$icon("palette"),
        # One picker over the *variables*, not one panel per aesthetic. Picking
        # a variable adds a layer, and app/logic/mapping_engine.R decides which
        # aesthetic and palette it gets from the variable's own profile and
        # what the other layers already hold. Every variable is listed — the
        # ones that cannot group say so in their sub-text rather than being
        # silently withheld, which is what left the old shape picker missing
        # entries with no explanation.
        field_select(ns, "nj_layer_add", "Map a variable"),
        shiny$uiOutput(ns("nj_layers_ui")),
        shiny$hr(),
        accordion(
          open = FALSE,
          accordion_panel(
            "Heatmaps",
            icon = shiny$icon("border-all"),
            shiny$uiOutput(ns("nj_heatmaps_ui"))
          )
        )
      ),
      # Colors -----------------------------------------------------------------
      nav_panel(
        "Colors",
        icon = shiny$icon("fill-drip"),
        shiny$div(
          class = "viz-color-grid",
          viz_color(ns, "nj_color", "Lines / Text", "#000000"),
          viz_color(ns, "nj_bg", "Background", "#ffffff"),
          viz_color(ns, "nj_tiplab_color", "Tip Label", "#000000"),
          viz_color(ns, "nj_branch_color", "Branch Label", "#000000"),
          viz_color(ns, "nj_tippoint_color", "Tip Point", "#3A4657"),
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
            pickerInput(ns("nj_tippoint_shape"), "Shape", point_shapes),
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
            viz_color(ns, "nj_clade_scale", "Highlight color", "#D0F221")
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
            pickerInput(ns("nj_root_isolate"), "Outgroup", c("Automatic"))
          ),
          accordion_panel(
            "Layout",
            icon = shiny$icon("project-diagram"),
            pickerInput(
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
            # No position sliders. The legend gets a reserved column beside the
            # tree (below it, for circular layouts) that the layout engine
            # sizes to the widest key — placing it by hand is what let it land
            # on top of the tips and run off the canvas.
            shiny$div(
              class = "text-muted small",
              "Placed beside the tree automatically, clear of the labels."
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
  # Per-column profile of the metadata: declared type, distinct-value count,
  # coverage and group, built once by the coordinator
  # (app/logic/field_profile.R). Field pickers read it so every engine
  # describes a variable the same way.
  field_profiles = shiny$reactive(NULL),
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
    # nj_fruit_variable/nj_root_isolate/nj_parentnode's
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

      # Grouped and labelled exactly as the Database > Browse Entries column
      # picker is, and from the same place — the loaded database's own columns.
      # This engine used to build its pickers from the hardcoded placeholder
      # names in viz_helpers ("Isolation Date", "Host", "Country"), which name
      # nothing in any real database; the sibling engines all went through
      # grouped_field_choices() already.
      grouped <- function(cols) {
        if (!length(cols)) {
          return(character(0))
        }
        grouped_field_choices(
          cols,
          attr(meta, "mlst_cols"),
          attr(meta, "amr_cols"),
          attr(meta, "custom_cols")
        )
      }

      # The resolved value goes to the mirror as well as to the browser, and
      # the mirror is what the plot reads. Sending it only to the browser means
      # it comes back an echo later, after the tree has already been drawn from
      # the stale value — a second draw for a label source that was decided
      # before the tree was even computed.
      keep <- function(id, choices, default, valid = choices) {
        value <- if (!force_default && isTRUE(input[[id]] %in% valid)) {
          input[[id]]
        } else {
          default
        }
        # updatePickerInput, not updateSelectInput: these are bootstrap-select
        # widgets, and they ignore a plain select's update message entirely —
        # the choices stayed on the placeholder names the UI declares and the
        # control read "Nothing selected".
        updatePickerInput(session, id, choices = choices, selected = value)
        set_fitted(id, value)
      }

      # Every column can name a tip; the isolate name is the one that always can.
      # The variable mappings are not here: they are layers the user adds, not
      # pickers holding a resolved default, and their choices come from
      # field_profiles() rather than from this function.
      keep("nj_tiplab", grouped(fields), "isolate", valid = fields)

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

    # A mapping onto the tip points draws nothing while the tip points
    # themselves are switched off, and that switch lives in a different tab
    # (Elements) from the mapping that needs it — so adding such a mapping
    # appeared to do nothing at all. Turn the points on with it.
    shiny$observeEvent(
      nj_layers(),
      {
        wants_points <- any(vapply(
          nj_layers(),
          function(l) l$aesthetic %in% c("tippoint_color", "tippoint_shape"),
          logical(1)
        ))
        if (wants_points && !isTRUE(shiny$isolate(fitted$nj_tippoint_show))) {
          set_fitted("nj_tippoint_show", TRUE)
          bslib::update_switch("nj_tippoint_show", value = TRUE)
        }
      },
      ignoreInit = TRUE
    )

    # Grey out a colour swatch whose element is not being drawn, or whose
    # aesthetic a mapping layer has taken over.
    #
    # The `!.layer_on(...)` clauses are the same predicate the renderer already
    # uses to decide whether to pass a fixed `color` at all (tree_tiplab_layer,
    # tree_tippoint_layer): a fixed colour parameter *overrides* the mapped
    # aesthetic in ggplot2 rather than losing to it, so while a colour layer
    # owns an aesthetic its swatch is genuinely dead. Sharing the predicate is
    # what keeps the control panel and the plot agreeing by construction.
    shiny$observe({
      ls <- nj_layers()
      layer_on <- function(aesthetic) {
        any(vapply(ls, function(l) identical(l$aesthetic, aesthetic), logical(1)))
      }
      active <- list(
        nj_color = TRUE,
        nj_bg = TRUE,
        nj_tiplab_color = isTRUE(fitted$nj_tiplab_show) &&
          !layer_on("tiplab_color"),
        nj_branch_color = isTRUE(input$nj_show_branch_label),
        nj_tippoint_color = isTRUE(fitted$nj_tippoint_show) &&
          !layer_on("tippoint_color"),
        nj_clade_scale = length(fitted$nj_parentnode %||% character(0)) > 0L
      )
      for (id in names(active)) {
        shinyjs::toggleClass(
          id = paste0(id, "_row"),
          class = "is-disabled",
          condition = !isTRUE(active[[id]])
        )
      }
    })

    # Fill the pickers as soon as a database is loaded, rather than waiting for
    # a Generate. Until this, every variable picker in this sidebar listed the
    # placeholder names viz_helpers declares them with — "Isolation Date",
    # "Host", "Country" — which are not columns of any real database, so the
    # whole Mapping tab read as nonsense to anyone who opened it before pressing
    # Generate. Keeps a selection that is still valid, so it cannot disturb a
    # configured plot; Generate still calls it too, for the isolate-derived
    # choices (outgroup, clade nodes) that depend on the selection rather than
    # on the database.
    shiny$observeEvent(
      viz_metadata(),
      populate_metadata_selects(force_default = FALSE),
      ignoreNULL = TRUE
    )

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
        nj_tiplab_color = "#000000",
        nj_branch_color = "#000000",
        nj_tippoint_color = "#3A4657",
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

    # The variable mappings, in draw order. Written only by explicit user
    # actions — never echoed back from the browser the way a picker's value is
    # — which is why they need none of the `fitted` mirroring the rest of the
    # resolved controls do.
    nj_layers <- shiny$reactiveVal(list())
    # Ids are minted monotonically and never reused, so a delete cannot hand a
    # stale button's id to the layer that replaced it.
    nj_layer_seq <- shiny$reactiveVal(0L)

    # The heatmap panels, keyed by HEATMAP_KINDS name.
    nj_heatmaps <- shiny$reactiveVal(list())

    next_layer_id <- function() {
      n <- nj_layer_seq() + 1L
      nj_layer_seq(n)
      paste0("L", n)
    }

    profiles <- shiny$reactive({
      p <- field_profiles()
      if (is.null(p) || !nrow(p)) NULL else p
    })

    # Resolved Tree control values, shared by the live render and the export.
    tree_opts <- shiny$reactive(
      list(
        # Layout / rooting.
        root = fitted$nj_root_isolate,
        layout = input$nj_layout,
        line_color = input$nj_color,
        bg = input$nj_bg,
        # Tip labels.
        tiplab_show = fitted$nj_tiplab_show,
        tiplab = fitted$nj_tiplab,
        tiplab_size = fitted$nj_tiplab_size,
        align = input$nj_align,
        tiplab_color = input$nj_tiplab_color,
        # Every variable mapping, in draw order. One key replaces the eight the
        # five per-aesthetic panels used to contribute, and it needs none of
        # their "only read this while its switch is on" guarding: a layer exists
        # only because the user added it, so there is no picker sitting on a
        # stale value for the plot to mistake for a change.
        layers = nj_layers(),
        # Branch labels. Allelic distance is the only thing they ever carry, so
        # there is no source to resolve — the picker that used to choose one was
        # removed along with the rest of this section's controls.
        branch_show = input$nj_show_branch_label,
        branch_size = fitted$nj_branch_size,
        branch_cutoff = fitted$nj_branchlabel_cutoff,
        branch_color = input$nj_branch_color,
        # Tip points.
        tippoint_show = fitted$nj_tippoint_show,
        tippoint_alpha = input$nj_tippoint_alpha,
        tippoint_size = fitted$nj_tippoint_size,
        tippoint_color = input$nj_tippoint_color,
        tippoint_shape = input$nj_tippoint_shape,
        # Clade highlights.
        nodelabel_show = input$nj_nodelabel_show,
        parentnodes = fitted$nj_parentnode %||% character(0),
        clade_color = input$nj_clade_scale,
        # Heatmap panels, in draw order.
        heatmaps = nj_heatmaps(),
        # Elements toggles.
        rootedge_show = input$nj_rootedge_show,
        treescale_show = input$nj_treescale_show,
        # Panel width, for the tip-label reserve (see .tiplab_frac).
        width_in = plot_width_in(),
        # Dimensions / legend.
        zoom = fitted$nj_zoom,
        h = fitted$nj_h,
        v = input$nj_v,
        legend_orientation = input$nj_legend_orientation,
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
          # Uncomment, with the helper it belongs to, to have the console name
          # which field caused each redraw rather than only report that one
          # happened. Both are already comparing the same two values, so it
          # costs nothing to switch on. See .log_rebuild above.
          # .log_rebuild(previous, current)
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
        # The branch-label cutoff is fitted from the branches themselves rather
        # than from the tip count, so it goes alongside the geometry rather than
        # inside it.
        fit$branch_cutoff <- tree_branch_cutoff(length(tree$edge.length))

        for (id in c(names(FITTED_DEFAULTS), "nj_branchlabel_cutoff")) {
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
        # Ordered by how well the family suits *this* column, so the first entry
        # — which is what an unset picker lands on — is the one to use. The
        # count matters as much as the type: ColorBrewer's qualitative palettes
        # are tabulated, and asking Set1 for the 46 countries in this database
        # returns nine colours and draws the other 37 grey. See
        # scale_categories_for().
        scale_categories_for(
          vals,
          suitable_scale_categories(
            if (is.numeric(vals)) "Numeric" else "Factor",
            vals
          )
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

    # --- Variable mapping layers -------------------------------------------

    # Every variable in the database, each carrying its own value count and
    # type as the option's second line. Rendered once in the UI and refilled
    # here, because updateVirtualSelect() has no `...` and so cannot re-set
    # hasOptionDescription.
    #
    # Through the shared helper rather than calling shinyWidgets directly: an
    # error raised in here is logged and swallowed by Shiny, so a broken call
    # leaves the picker silently empty with nothing failing. The helper is
    # covered by tests that call it outside an observer, where an error is an
    # error. Columns that cannot group stay listed but disabled, with the
    # reason in their sub-text — the old shape picker dropped them entirely,
    # which is what left users hunting for a variable that was never there.
    shiny$observe({
      prof <- profiles()
      shiny$req(prof)
      # `isolate` names every tip uniquely; it is a label, never a mapping.
      prof <- prof[prof$field != "isolate", , drop = FALSE]
      shiny$req(nrow(prof))
      update_field_select(session, "nj_layer_add", prof)
    })

    shiny$observeEvent(input$nj_layer_add, {
      field <- input$nj_layer_add
      shiny$req(nzchar(field %||% ""))
      # Clear the picker straight away so the same variable can be re-picked
      # after a delete, and so the selection cannot re-fire on a later flush.
      updateVirtualSelect(
        inputId = "nj_layer_add",
        session = session,
        selected = character(0)
      )

      layers <- nj_layers()
      if (any(vapply(layers, function(l) identical(l$field, field), logical(1)))) {
        return()
      }
      if (length(layers) >= MAX_LAYERS) {
        shiny$showNotification(
          sprintf(
            "%d mappings is the most the tree can show at once. Remove one first.",
            MAX_LAYERS
          ),
          type = "warning"
        )
        return()
      }
      prof <- profile_for(profiles(), field)
      layer <- assign_mapping_layer(prof, layers, id = next_layer_id())
      if (is.null(layer)) {
        shiny$showNotification(
          aesthetic_block_reason(prof, NULL) %||%
            "That variable cannot be mapped.",
          type = "warning"
        )
        return()
      }
      nj_layers(c(layers, list(layer)))
    })

    # One delegated handler per action rather than one observer per row: an
    # observeEvent created inside renderUI is re-registered on every render,
    # so the ids push their own value into a single input instead.
    shiny$observeEvent(input$nj_layer_delete, {
      keep <- Filter(
        function(l) !identical(l$id, input$nj_layer_delete),
        nj_layers()
      )
      nj_layers(rebalance_layers(keep, profiles()))
    })

    output$nj_layers_ui <- shiny$renderUI({
      layers <- nj_layers()
      if (!length(layers)) {
        return(shiny$div(
          class = "text-muted fst-italic mb-2 tree-layer-empty",
          "No mappings yet."
        ))
      }
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
                  AESTHETIC_LABELS[[l$aesthetic]],
                  "·",
                  sprintf("%d values", l$n_levels),
                  if (!is.null(l$palette)) paste("·", l$palette)
                )
              )
            ),
            .layer_btn(ns, "nj_layer_edit", l$id, "pen", "Edit mapping"),
            .layer_btn(ns, "nj_layer_delete", l$id, "xmark", "Remove mapping")
          )
        })
      )
    })

    # --- Editing one layer --------------------------------------------------

    editing <- shiny$reactiveVal(NULL)

    shiny$observeEvent(input$nj_layer_edit, {
      layers <- nj_layers()
      hit <- Filter(function(l) identical(l$id, input$nj_layer_edit), layers)
      shiny$req(length(hit))
      l <- hit[[1]]
      prof <- profile_for(profiles(), l$field)
      shiny$req(!is.null(prof))
      editing(l$id)

      taken <- vapply(
        Filter(function(x) !identical(x$id, l$id), layers),
        function(x) x$aesthetic,
        character(1)
      )
      free <- union(l$aesthetic, eligible_aesthetics(prof, taken))
      blocked <- setdiff(names(AESTHETIC_LABELS), free)
      reasons <- Filter(
        Negate(is.null),
        lapply(blocked, function(a) aesthetic_block_reason(prof, a))
      )

      cats <- scale_categories_for(
        viz_metadata()[[l$field]],
        suitable_scale_categories(
          if (isTRUE(prof$continuous)) "Numeric" else "Factor",
          viz_metadata()[[l$field]]
        )
      )

      shiny$showModal(shiny$modalDialog(
        title = paste("Mapping:", l$title),
        size = "s",
        easyClose = TRUE,
        pickerInput(
          ns("nj_layer_aesthetic"),
          "Show as",
          choices = setNames(free, unname(AESTHETIC_LABELS[free])),
          selected = l$aesthetic
        ),
        if (length(reasons)) {
          shiny$div(
            class = "small text-muted mb-2",
            # Saying why an option is missing is the whole point: the old
            # picker just left it out.
            lapply(reasons, shiny$tags$div)
          )
        },
        if (l$aesthetic %in% COLOR_AESTHETICS) {
          shiny$div(
            class = "viz-scale-select",
            pickerInput(
              ns("nj_layer_palette"),
              "Color scale",
              choices = color_scales[cats],
              selected = l$palette
            )
          )
        },
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("nj_layer_apply"), "Apply")
        )
      ))
    })

    shiny$observeEvent(input$nj_layer_apply, {
      id <- editing()
      shiny$req(!is.null(id))
      layers <- lapply(nj_layers(), function(l) {
        if (!identical(l$id, id)) {
          return(l)
        }
        l$aesthetic <- input$nj_layer_aesthetic %||% l$aesthetic
        if (l$aesthetic %in% COLOR_AESTHETICS) {
          l$palette <- input$nj_layer_palette %||% l$palette
        } else {
          l$palette <- NULL
        }
        # Pinned: rebalance_layers() must not undo a deliberate choice.
        l$auto <- FALSE
        l
      })
      # A pinned layer may now hold an aesthetic an automatic one had, so the
      # automatic ones move out of its way.
      nj_layers(rebalance_layers(layers, profiles()))
      editing(NULL)
      shiny$removeModal()
    })

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

    # --- Heatmap panels ------------------------------------------------------

    # Columns available to one heatmap kind: exactly the set its appender
    # added, recorded on viz_metadata() as an attribute. Sample metadata is in
    # none of them, so "not selectable for a heatmap" is a property of the data
    # model rather than a filter someone has to remember to apply.
    heatmap_cols <- function(kind) {
      meta <- viz_metadata()
      if (is.null(meta)) {
        return(character(0))
      }
      intersect(names(meta), attr(meta, HEATMAP_KINDS[[kind]]$attr) %||% character(0))
    }

    heatmap_of <- function(kind) {
      hit <- Filter(function(h) identical(h$kind, kind), nj_heatmaps())
      if (length(hit)) hit[[1]] else NULL
    }

    output$nj_heatmaps_ui <- shiny$renderUI({
      active <- nj_heatmaps()
      shiny$div(
        class = "tree-heatmap-list",
        lapply(names(HEATMAP_KINDS), function(kind) {
          spec <- HEATMAP_KINDS[[kind]]
          cols <- heatmap_cols(kind)
          on <- !is.null(heatmap_of(kind))
          if (!length(cols)) {
            return(shiny$div(
              class = "tree-heatmap-card is-empty",
              shiny$div(class = "tree-layer_title", spec$title),
              shiny$div(class = "text-muted small", spec$empty)
            ))
          }
          n_sel <- length(heatmap_of(kind)$cols %||% character(0))
          shiny$div(
            class = "tree-heatmap-card",
            input_switch(ns(paste0("nj_heatmap_", kind)), spec$title, on),
            shiny$div(
              class = "tree-layer_meta",
              if (on) {
                sprintf("%d of %d columns", n_sel, length(cols))
              } else {
                sprintf("%d columns available", length(cols))
              }
            ),
            shiny$actionButton(
              ns(paste0("nj_heatcols_", kind)),
              "Choose columns",
              icon = shiny$icon("list-check"),
              class = "btn-sm"
            )
          )
        })
      )
    })

    # One observer per kind, created once at startup — the kinds are a fixed
    # list, so this is not the renderUI re-registration trap the layer buttons
    # avoid.
    for (kind in names(HEATMAP_KINDS)) {
      local({
        k <- kind
        spec <- HEATMAP_KINDS[[k]]

        shiny$observeEvent(input[[paste0("nj_heatmap_", k)]], {
          on <- isTRUE(input[[paste0("nj_heatmap_", k)]])
          others <- Filter(function(h) !identical(h$kind, k), nj_heatmaps())
          if (!on) {
            nj_heatmaps(others)
            return()
          }
          existing <- heatmap_of(k)
          cols <- existing$cols %||% heatmap_cols(k)
          if (!length(cols)) {
            return()
          }
          # Order follows HEATMAP_KINDS so the panels always draw left to right
          # in the same sequence, whichever was switched on first.
          fresh <- c(others, list(list(
            kind = k, cols = cols, palette = spec$palette, title = spec$title
          )))
          nj_heatmaps(fresh[order(match(
            vapply(fresh, function(h) h$kind, character(1)),
            names(HEATMAP_KINDS)
          ))])
        }, ignoreInit = TRUE)

        shiny$observeEvent(input[[paste0("nj_heatcols_", k)]], {
          cols <- heatmap_cols(k)
          shiny$req(length(cols))
          meta <- viz_metadata()
          shiny$showModal(shiny$modalDialog(
            title = paste(spec$title, "columns"),
            size = "m",
            easyClose = TRUE,
            shiny$p(
              class = "text-muted small",
              "These columns share one colour scale, which is what makes the",
              "matrix readable as a block."
            ),
            shiny$checkboxGroupInput(
              ns(paste0("nj_heatcolsel_", k)),
              NULL,
              choices = setNames(
                cols,
                sprintf(
                  "%s (%d)",
                  field_labels_for(cols),
                  vapply(cols, function(f) field_levels(meta[[f]]), integer(1))
                )
              ),
              selected = heatmap_of(k)$cols %||% cols
            ),
            footer = shiny$tagList(
              shiny$modalButton("Cancel"),
              shiny$actionButton(ns(paste0("nj_heatapply_", k)), "Apply")
            )
          ))
        })

        shiny$observeEvent(input[[paste0("nj_heatapply_", k)]], {
          chosen <- input[[paste0("nj_heatcolsel_", k)]] %||% character(0)
          others <- Filter(function(h) !identical(h$kind, k), nj_heatmaps())
          if (!length(chosen)) {
            # An empty panel draws nothing, so switching it off is the honest
            # reading of "apply no columns".
            nj_heatmaps(others)
            bslib::update_switch(paste0("nj_heatmap_", k), value = FALSE)
          } else {
            fresh <- c(others, list(list(
              kind = k, cols = chosen, palette = spec$palette,
              title = spec$title
            )))
            nj_heatmaps(fresh[order(match(
              vapply(fresh, function(h) h$kind, character(1)),
              names(HEATMAP_KINDS)
            ))])
            bslib::update_switch(paste0("nj_heatmap_", k), value = TRUE)
          }
          shiny$removeModal()
        })
      })
    }


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
    # reactiveVals rather than inputs (the mapping layers and the heatmaps).
    snapshot <- shiny$reactive(c(
      collect_input_snapshot(input, "nj_"),
      list(.layers = nj_layers(), .heatmaps = nj_heatmaps())
    ))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "nj_tiplab_show",
          "nj_align",
          "nj_show_branch_label",
          "nj_tippoint_show",
          "nj_nodelabel_show",
          "nj_rootedge_show",
          "nj_treescale_show",
          paste0("nj_heatmap_", names(HEATMAP_KINDS))
        ),
        selects = c(
          "nj_tippoint_shape",
          "nj_layout"
        ),
        sliders = c(
          "nj_tiplab_size",
          "nj_branch_size",
          "nj_branchlabel_cutoff",
          "nj_tippoint_alpha",
          "nj_tippoint_size",
          "nj_aspect_ratio",
          "nj_v",
          "nj_h",
          "nj_zoom",
          "nj_legend_size"
        ),
        colors = c(
          "nj_color",
          "nj_bg",
          "nj_tiplab_color",
          "nj_branch_color",
          "nj_tippoint_color",
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

      # Both come back from JSON as data.frames rather than lists of lists —
      # normalise before storing.
      layers <- .normalize_layers(vals$.layers)
      if (is.null(layers)) {
        # An Analysis saved before the layer rewrite carries the old
        # per-aesthetic keys instead. Rebuilding layers from them is what stops
        # every saved tree silently losing its mappings on first reopen.
        layers <- .migrate_legacy_mapping(vals)
      }
      if (!is.null(layers)) {
        nj_layers(layers)
        nj_layer_seq(length(layers))
      }
      heatmaps <- .normalize_heatmaps(vals$.heatmaps)
      if (!is.null(heatmaps)) {
        nj_heatmaps(heatmaps)
      }
    }

    # Rebuild mapping layers from a pre-rewrite snapshot's flat keys. Each of
    # the three old switch/variable/scale triples becomes one layer on the
    # aesthetic it used to drive, and the tile strips become tile layers, so a
    # reopened Analysis draws what it drew when it was saved.
    .migrate_legacy_mapping <- function(vals) {
      legacy <- list(
        list(show = "nj_mapping_show", field = "nj_color_mapping",
             palette = "nj_tiplab_scale", aesthetic = "tiplab_color"),
        list(show = "nj_tipcolor_mapping_show", field = "nj_tipcolor_mapping",
             palette = "nj_tippoint_scale", aesthetic = "tippoint_color"),
        list(show = "nj_tipshape_mapping_show", field = "nj_tipshape_mapping",
             palette = NULL, aesthetic = "tippoint_shape")
      )
      out <- list()
      prof <- profiles()
      add <- function(field, aesthetic, palette) {
        row <- profile_for(prof, field)
        if (is.null(row) || !isTRUE(row$groupable)) {
          return()
        }
        layer <- assign_mapping_layer(row, out, id = paste0("L", length(out) + 1L))
        if (is.null(layer)) {
          return()
        }
        # Keep what the saved plot actually drew, not what the engine would
        # pick today — restoring an Analysis must reproduce it, not improve it.
        layer$aesthetic <- aesthetic
        layer$palette <- if (aesthetic %in% COLOR_AESTHETICS) palette else NULL
        layer$auto <- FALSE
        out[[length(out) + 1L]] <<- layer
      }

      for (spec in legacy) {
        if (!isTRUE(vals[[spec$show]])) {
          next
        }
        add(
          vals[[spec$field]],
          spec$aesthetic,
          if (is.null(spec$palette)) NULL else vals[[spec$palette]]
        )
      }
      for (tile in .normalize_records(vals$.tiles, list(
        show = FALSE, variable = NA_character_, scale = "viridis"
      )) %||% list()) {
        if (isTRUE(tile$show) && !is.na(tile$variable %||% NA)) {
          add(tile$variable, "tile", tile$scale)
        }
      }
      if (!length(out)) NULL else out
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
