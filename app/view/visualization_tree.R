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
    actionGroupButtons,
    radioGroupButtons,
    pickerInput,
    pickerOptions,
    prepare_choices,
    updatePickerInput,
    updateVirtualSelect,
    virtualSelectInput
  ],
  stats[setNames],
  rlang[`%||%`],
)
box::use(
  app / logic / amr_plot,
  app / logic / date_bins[bin_date_values],
  app / logic / db_events,
  app /
    logic /
    field_labels[
      amr_field_map,
      grouped_field_choices,
    ],
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
      crowded_tips,
      eligible_aesthetics,
      granularity_profile,
      is_date_profile,
      MAX_LAYERS,
      normalize_layer_records,
      rebalance_layers,
      set_layer_granularity,
      TIP_MAPPING_MAX
    ],
  app / logic / functions[render_info],
  app /
    logic /
    tree_plot[
      AMR_CONFIDENCE_COLORS,
      build_tree_ggtree,
      CLASS_STRIP_SCALE,
      DEND_DEPTH_DEFAULT,
      ELEMENT_POS_DEFAULT,
      HEAT_SCALE_DEFAULT,
      LEGEND_MAX_KEYS,
      MIN_PRINT_PT,
      scale_tree_opts,
      save_tree_plot,
      tree_auto_layout,
      tree_open_angle,
      tree_legend_width_in,
      tree_min_type_pt,
      tree_panel_width_in,
      TREE_FIT_DEFAULTS
    ],
  app / logic / phylo[compute_phylo_tree],
  app / logic / viz_export[CM_PER_IN, save_plot_export],
  app /
    logic /
    viz_helpers[
      meta_vars,
      label_vars,
      point_shapes,
      viz_color,
      field_select,
      granularity_select,
      layer_action_btn,
      update_field_select,
      update_scale_select,
      scale_select,
      color_scales,
      suitable_scale_categories,
      reset_viz_colors,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
  app / logic / viz_layers[layer_cards, layer_defaults, normalize_layers],
)

# --- Variable mapping layers -------------------------------------------------

# Canonical shape of one mapping layer, taken from the shared record so every
# engine's snapshot/restore path rebuilds the same thing. The tree opens on the
# tip-label colour rather than on its medium's first channel: shape is the
# scarcer aesthetic and the engine assigns it deliberately, never as a fallback.
LAYER_DEFAULTS <- layer_defaults("tree", aesthetic = "tiplab_color")

# The heatmap is the AMR screening result and nothing else.
#
# A heatmap is a matrix under one shared fill scale, which only says something
# when its columns are the same measurement repeated. AMR screening is exactly
# that — every column is "was this found in this isolate" — while a set of
# user-defined custom variables is a bag of unrelated fields, and drawing them
# under one scale produced a legend that explained none of them.
#
# One column per gene, coloured by the AMRFinderPlus method tier of the call
# (Absent .. Perfect), the same tiers the AMR-plot engine's gene heatmap uses.
#
# One panel per element type (Resistance / Virulence / Stress), each carrying
# its own gene set, its own four confidence colours and its own column order —
# the Heatmap tab's two modals, genes and style. The gene axis is the only one
# a panel can order: its rows are the tree's tips.

# Does any mapping draw on the tip points? Three answers hang off this one
# question — whether the points come on with the mapping, whether their switch
# is the user's to touch, and whether the plot draws them at all — and they only
# agree with each other because they ask it the same way.
.layers_want_tippoints <- function(layers) {
  any(vapply(
    layers,
    function(l) l$aesthetic %in% c("tippoint_color", "tippoint_shape"),
    logical(1)
  ))
}

# How a heatmap panel is drawn, as opposed to what it draws. Split out from the
# rest of the record because these are exactly the fields the edit modal owns,
# and because a snapshot saved before they existed has to restore with them —
# .normalize_heatmaps() fills them in from here.
#
# The four colours are the AMR-plot engine's own gene-heatmap defaults, taken
# from tree_plot rather than written out again, so a panel left alone still
# reads at the same colour as the same gene on the AMR tab. The cluster
# distance and linkage are amr_plot's defaults for the same reason, and are
# inert until clustering is switched on.
HEATMAP_STYLE_DEFAULTS <- list(
  cluster = FALSE,
  cluster_distance = amr_plot$AMR_CLUSTER_DISTANCE_DEFAULT,
  cluster_method = amr_plot$AMR_CLUSTER_METHOD_DEFAULT,
  dend_depth = DEND_DEPTH_DEFAULT,
  color_absent = AMR_CONFIDENCE_COLORS[["absent"]],
  color_partial = AMR_CONFIDENCE_COLORS[["partial"]],
  color_strong = AMR_CONFIDENCE_COLORS[["strong"]],
  color_present = AMR_CONFIDENCE_COLORS[["present"]],
  # Which class vocabulary files this panel's genes: abritamr's curated rollup
  # or AMRFinderPlus's own. Per panel rather than per plot, because a virulence
  # panel and a resistance panel are not filed by the same authority anyway.
  vocabulary = amr_plot$AMR_CLASS_VOCABULARY_DEFAULT,
  show_gene_names = TRUE,
  show_class_names = TRUE,
  show_class_strip = TRUE,
  # Whether the panel names its element type on the figure, and at which end.
  # Two matrices side by side are otherwise told apart only by their guide
  # titles, which sit off at the edge of the plot.
  show_element_type = FALSE,
  element_pos = ELEMENT_POS_DEFAULT,
  # Which of the two colour controls the panel is drawn from: the four tier
  # pickers ("tiers") or one sequential ramp across the same tiers ("scale").
  # The modal's segmented control sets it; tree_plot's `.heatmap_fill()` reads
  # it. Both values are kept, so switching back and forth loses neither.
  color_mode = "tiers",
  heat_scale = HEAT_SCALE_DEFAULT,
  # Palette the drug-class strip is keyed by. Defaulted to the shared
  # CLASS_STRIP_SCALE so a class reads the same colour as on the AMR tab until
  # the reader picks another in the colour modal.
  strip_scale = CLASS_STRIP_SCALE
)

# The fields the sidebar's Labels and Clustering accordions own, as opposed to
# the ones the per-panel colour modal owns.
#
# Every panel on a tree carries the same value for these: they describe how the
# heatmap block as a whole is arranged and labelled, and two matrices side by
# side with one clustered and one not — or one naming its classes and one not —
# read as a mistake rather than as a choice. Colour is the opposite case, which
# is why it stayed per panel.
HEATMAP_SHARED_FIELDS <- c(
  "show_gene_names",
  "show_class_names",
  "show_element_type",
  "element_pos",
  "vocabulary",
  "cluster",
  "cluster_distance",
  "cluster_method",
  "show_class_strip",
  "dend_depth"
)

# The sidebar's list of heatmap panels — one card per AMR element type the user
# has added (Resistance / Virulence / Stress), each with a genes, an edit and a
# remove button. Deliberately the same card as viz_layers$layer_cards() draws
# for a mapping layer (same CSS classes, same layer_action_btn), because both
# are "a list of removable things configured in a sidebar" — a heatmap panel
# just is not shaped like a mapping layer (no aesthetic/palette/granularity),
# so it gets its own summary line rather than being forced through
# .layer_meta().
#
# Two buttons rather than one, because a panel is configured along two
# unrelated axes: *which* genes it carries, and how they are drawn. Folding the
# gene picker into the style modal put a searchable list of a hundred symbols
# above four colour swatches, and the swatches were below the fold.
#
# The genes button opens the picker itself rather than a dialog around it — the
# picker is already a full-screen popup with its own search, Confirm and Cancel
# (showDropboxAsPopup, plus app/js/virtual-select-popup-confirm.js), so the
# modal it used to sit in was a second frame around a frame. Each card carries
# its own select, parked off-screen; the button calls the widget's own open().
.heatmap_gene_input_id <- function(record_id) {
  paste0("nj_heatmap_cols_", record_id)
}

# The button that opens one card's gene picker. Not layer_action_btn: this one
# reaches straight into the widget instead of setting an input, so nothing has
# to round-trip through the server before the list appears.
#
# Deferred a tick, which is the whole reason a plain `e.open()` here did
# nothing: virtual-select closes an open dropbox from a click handler on
# `document`, so the very click that opened it went on bubbling and closed it
# again in the same event. Opening from a timeout puts it after that handler
# has run, rather than relying on stopPropagation to outrun a listener whose
# phase this code does not control.
.heatmap_genes_btn <- function(ns, record_id) {
  target <- ns(.heatmap_gene_input_id(record_id))
  shiny$tags$button(
    type = "button",
    class = "btn btn-sm tree-layer_btn",
    title = "Choose genes",
    `aria-label` = "Choose genes",
    onclick = sprintf(
      paste0(
        "var e=document.getElementById('%s');",
        "if(e&&e.open){setTimeout(function(){e.open();},0);}"
      ),
      target
    ),
    shiny$icon("dna")
  )
}

.heatmap_layer_cards <- function(ns, layers, catalog_for) {
  if (!length(layers)) {
    return(shiny$div(
      class = "text-muted fst-italic mb-2 tree-layer-empty",
      "No heatmaps yet."
    ))
  }
  shiny$div(
    class = "tree-layer-list",
    lapply(layers, function(h) {
      cat <- catalog_for(h)
      total <- nrow(cat)
      n_sel <- length(h$cols)
      meta <- if (!total) {
        "No genes available"
      } else if (n_sel >= total) {
        sprintf("%d gene%s", total, if (total == 1L) "" else "s")
      } else {
        sprintf("%d of %d genes", n_sel, total)
      }
      if (isTRUE(h$cluster)) {
        meta <- paste(meta, "· clustered")
      }
      shiny$div(
        class = "tree-layer-card",
        shiny$div(
          class = "tree-layer_body",
          shiny$div(class = "tree-layer_title", title = h$title, h$title),
          shiny$div(class = "tree-layer_meta", meta)
        ),
        .heatmap_genes_btn(ns, h$id),
        # Colours only. How the panel is *arranged* — its labels and its
        # clustering — is shared across every panel and lives in the accordions
        # under the card list, so the one thing left that is this panel's alone
        # is what it is coloured with.
        layer_action_btn(
          ns,
          "nj_heatmap_colors",
          h$id,
          "palette",
          "Heatmap colours"
        ),
        layer_action_btn(
          ns,
          "nj_heatmap_delete",
          h$id,
          "xmark",
          "Remove heatmap"
        ),
        # Parked off-screen, opened by the button above. It has to be in the
        # document for virtual-select to have initialised it — and in the card,
        # so it is torn down with the card it belongs to.
        shiny$div(
          class = "tree-layer_genes",
          .heatmap_gene_select(ns, h, cat)
        )
      )
    })
  )
}

# One card's gene picker. Multi-select popups in this app stage their edits and
# commit on Confirm (virtual-select-popup-confirm.js), so `updateOn = "close"`
# reports once, with what the reader confirmed — the Apply button the old modal
# needed is the popup's own.
.heatmap_gene_select <- function(ns, h, cat) {
  virtualSelectInput(
    ns(.heatmap_gene_input_id(h$id)),
    label = NULL,
    choices = if (nrow(cat)) {
      prepare_choices(
        cat,
        label = label,
        value = col,
        group_by = group,
        description = description
      )
    } else {
      character(0)
    },
    selected = h$cols,
    multiple = TRUE,
    search = TRUE,
    # Without this, the header checkbox selects every gene, not just the ones
    # the search term currently matches.
    selectAllOnlyVisible = TRUE,
    searchPlaceholderText = "Search genes ...",
    placeholder = "All genes",
    hasOptionDescription = TRUE,
    optionsCount = 5,
    noOfDisplayValues = 2,
    dropboxWrapper = "body",
    showDropboxAsPopup = TRUE,
    popupDropboxBreakpoint = "10000px",
    updateOn = "close",
    width = "100%"
  )
}

.normalize_heatmaps <- function(x) {
  out <- normalize_layer_records(
    x,
    c(
      list(
        id = NA_character_,
        kind = NA_character_,
        level = "gene",
        element = NA_character_,
        cols = character(0),
        labels = character(0),
        classes = character(0),
        palette = "Reds",
        title = NA_character_
      ),
      HEATMAP_STYLE_DEFAULTS
    )
  )
  if (is.null(out)) {
    return(NULL)
  }
  # The drug-class panel no longer has a control, so a snapshot saved while it
  # existed restores without it rather than as a panel nothing can switch off.
  out <- Filter(function(h) !identical(h$level, "class"), out)
  lapply(out, function(h) {
    # `character(0)` serialises to `[]`, which jsonlite reads back as an empty
    # *list* rather than a character vector.
    v <- unlist(h$cols, use.names = FALSE)
    h$cols <- if (!length(v)) character(0) else as.character(v)
    l <- unlist(h$labels, use.names = FALSE)
    h$labels <- if (!length(l)) NULL else as.character(l)
    g <- unlist(h$classes, use.names = FALSE)
    h$classes <- if (!length(g)) NULL else as.character(g)
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

# Inches the tree and its tip labels always get, whatever else is on the plot.
# This is the budget tree_auto_layout and the tip-label reserve reason about,
# and it does not move — which is the point.
#
# Everything else the plot has to show (annotation strips, heatmap panels, the
# legend) is added to the *canvas* rather than taken out of this. Taking it out
# was the reported failure: .tiplab_xlim makes room for an annotation by
# widening the x axis, which on a fixed canvas means the same tree drawn into a
# narrower strip — so switching a heatmap on turned 346 isolates into an
# unreadable hairline, and a legend of long category names did it again.
TREE_PANEL_IN <- 5.5

# How far the canvas may grow past the tree's own budget before the annotations
# are simply given less. Without a ceiling, four wide legends and three heatmap
# panels ask for a canvas no screen can show and no export can rasterise.
CANVAS_MAX_FACTOR <- 2.6

# A few hundred tips at aspect 8 is already ~8400px; this is the ceiling.
PLOT_MAX_PX <- 12000

# The legend is not the user's to set. Its column is reserved beside the tree
# (below it, for circular layouts) and sized to the widest key by the layout
# engine, so orientation and text size are fixed here at the values that
# reserve agrees with — a horizontal box or a larger type size would claim a
# width the reserve was never computed for and land the keys on the tips.
LEGEND_ORIENTATION <- "vertical"
LEGEND_SIZE <- 10

# --- Controls fitted to the data ---------------------------------------------

# Which field of a tree_auto_layout() fit feeds which control.
FITTED_FIELDS <- c(
  nj_aspect_ratio = "aspect",
  nj_tiplab_size = "tiplab_size",
  nj_branch_size = "branch_size",
  nj_tippoint_size = "tippoint_size",
  nj_open_angle = "open_angle"
)

# The values those controls hold before any data is loaded. Taken from the logic
# module rather than written out again here, because they are also the fit's
# calibration anchor (tree_auto_layout returns exactly these at ~15 tips) — a
# slider declared with anything else would quietly move the anchor.
FITTED_DEFAULTS <- setNames(
  TREE_FIT_DEFAULTS[FITTED_FIELDS],
  names(FITTED_FIELDS)
)

# Not every id here still renders a slider — nj_branch_size is fitted from the
# tip count and the branch-label section is a bare switch now. It keeps its
# entry regardless, because this list seeds the *mirrors* the render reads
# (MIRRORED_IDS below), and dropping a control must never drop its mirror.

# The other half of what the render reads through a mirror: the selects the
# server resolves rather than the user, from the loaded metadata
# (populate_metadata_selects). They need mirroring for exactly the reason the
# fitted sliders do — updateVirtualSelect reaches input$ only once the browser
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

# `options_ui` is the tab's own distance-computation controls (missing-value
# handling, imported sets, algorithm), built in visualization_plot.R and
# rendered here rather than in the left sidebar. It arrives already namespaced
# to the *tab*, not to this engine, so every observer behind it keeps working
# where it is — this moves where the controls appear, not who owns them.
#
# The zoom-view switch below, by contrast, is local to this engine (Tree-only)
# and namespaced here, so it is read straight off this module's own `input`.
#
# They belong on this side because they are live: the left sidebar is the
# "press Generate to apply" side, and these take effect as soon as they change.
tree_controls <- function(ns, options_ui = NULL) {
  shiny$tagList(
    navset_tab(
      # Options ----------------------------------------------------------------
      # Rendered whether or not the tab supplied its own controls, because the
      # circle-opening slider below is this engine's and has to have somewhere
      # to live.
      nav_panel(
        "Options",
        icon = shiny$icon("gear"),
        options_ui,
        # Height per tip, which is the one thing about the shape that is a
        # judgement rather than a fit: Generate solves it from the tip count so
        # the rows come out legible, but how tall a figure is worth having is
        # the reader's call, not the engine's.
        shiny$sliderInput(
          ns("nj_aspect_ratio"),
          "Aspect ratio",
          0.3,
          8,
          FITTED_DEFAULTS$nj_aspect_ratio,
          step = 0.1,
          ticks = FALSE
        ),
        virtualSelectInput(
          ns("nj_layout"),
          "Layout",
          choices = list(
            Linear = c(
              Rectangular = "rectangular",
              Roundrect = "roundrect",
              Slanted = "slanted",
              Ellipse = "ellipse"
            ),
            Circular = c(Circular = "circular", Inward = "inward")
          ),
          selected = "rectangular",
          # A fixed six-item list, so no search — but the same body-appended
          # popup the other right-sidebar selects use, so the dropdown clears
          # the sidebar rather than being clipped inside it.
          search = FALSE,
          dropboxWrapper = "body",
          showDropboxAsPopup = TRUE,
          popupDropboxBreakpoint = "10000px",
          width = "100%"
        ),
        # How far a radial tree opens between its last tip and its first.
        # Generate solves it from what the ring headers need
        # (tree_open_angle) and this is where that answer can be argued with
        # — the one piece of geometry the engine cannot settle alone, because
        # it trades the tree's sweep against the room its headers get. Hidden
        # for the layouts that have no circle to open.
        shiny$div(
          id = ns("nj_open_angle_wrap"),
          class = "d-none",
          shiny$sliderInput(
            ns("nj_open_angle"),
            "Circle opening",
            0,
            90,
            0,
            step = 1,
            post = "\u00b0",
            ticks = FALSE
          )
        ),
        virtualSelectInput(
          ns("nj_root_isolate"),
          "Outgroup",
          choices = c("Automatic"),
          selected = "Automatic",
          search = TRUE,
          searchPlaceholderText = "Search isolates ...",
          placeholder = "Automatic",
          # The isolate names only arrive at Generate; left on its default
          # this would hand the selection to whichever name lands first,
          # silently rooting the tree on an arbitrary tip.
          autoSelectFirstOption = FALSE,
          optionsCount = 8,
          dropboxWrapper = "body",
          showDropboxAsPopup = TRUE,
          popupDropboxBreakpoint = "10000px",
          width = "100%"
        )
      ),
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
            virtualSelectInput(
              ns("nj_tiplab"),
              "Label source",
              choices = label_vars,
              selected = label_vars[[1]],
              search = TRUE,
              searchPlaceholderText = "Search variables ...",
              placeholder = "Pick a variable ...",
              # Same reason as the outgroup: the database's own columns replace
              # these placeholders at Generate.
              autoSelectFirstOption = FALSE,
              optionsCount = 8,
              dropboxWrapper = "body",
              showDropboxAsPopup = TRUE,
              popupDropboxBreakpoint = "10000px",
              width = "100%"
            ),
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
            )
          ),
          accordion_panel(
            "Allelic Distance",
            icon = shiny$icon("code-branch"),
            # One switch, and nothing else. Allelic distance is the only value
            # worth writing on a branch, and nothing about how it is written is
            # a decision: the text size is fitted from the tip count
            # (tree_auto_layout), and *which* branches get a label is solved
            # from the drawn geometry rather than chosen (tree_branch_keep) —
            # a branch is labelled when it is wide enough to hold the number
            # and no other label shares its row, which is the only rule under
            # which the numbers stay readable on a tree whose branch lengths
            # differ by three orders of magnitude.
            input_switch(ns("nj_axis_show"), "Distance axis", TRUE),
            input_switch(
              ns("nj_show_branch_label"),
              "Show on branches",
              FALSE
            ),
            input_switch(ns("nj_treescale_show"), "Scale bar", FALSE)
          )
        )
      ),
      # Variable mapping -------------------------------------------------------
      nav_panel(
        "Mapping",
        icon = shiny$icon("map-pin"),
        # One picker over the *variables*, not one panel per aesthetic. Picking
        # a variable adds a layer, and app/logic/mapping_engine.R decides which
        # aesthetic and palette it gets from the variable's own profile and
        # what the other layers already hold. Every variable is listed — the
        # ones that cannot group say so in their sub-text rather than being
        # silently withheld, which is what left the old shape picker missing
        # entries with no explanation.
        field_select(ns, "nj_layer_add", "Map a variable"),
        shiny$uiOutput(ns("nj_layers_ui"))
      ),
      # AMR heatmap ------------------------------------------------------------
      nav_panel(
        "Heatmap",
        icon = shiny$icon("border-all"),
        # Same shape as Mapping: a picker adds a panel with default settings —
        # every gene of that element type — and the panel appears below as a
        # card, editable and removable exactly like a mapping layer. Editing
        # opens a modal to include/exclude genes, rather than an always-visible
        # picker per panel: with three possible panels a static control per one
        # spent sidebar space on the two the reader had not turned on yet.
        shiny$uiOutput(ns("nj_heatmap_none")),
        virtualSelectInput(
          ns("nj_heatmap_add"),
          "Add a heatmap",
          choices = character(0),
          selected = character(0),
          multiple = FALSE,
          search = FALSE,
          placeholder = "Add a heatmap ...",
          autoSelectFirstOption = FALSE,
          dropboxWrapper = "body",
          showDropboxAsPopup = TRUE,
          popupDropboxBreakpoint = "10000px",
          width = "100%"
        ),
        shiny$uiOutput(ns("nj_heatmap_layers_ui")),
        # Shared across every panel, so they sit under the card list rather
        # than inside any one card's dialog. Static rather than rendered: an
        # input that is torn down and rebuilt loses the value the browser holds
        # for it, and these have to survive a panel being added or removed.
        # Hidden wholesale while there is no panel to apply them to
        # (`sync_heatmap_shared_state()`).
        shiny$div(
          id = ns("nj_heatmap_shared"),
          accordion(
            open = FALSE,
            accordion_panel(
              "Labels",
              icon = shiny$icon("tag"),
              input_switch(
                ns("nj_heatmap_gene_names"),
                "Gene names",
                HEATMAP_STYLE_DEFAULTS$show_gene_names
              ),
              # Clustered columns are not in class order to bracket and name,
              # so this is disabled while clustering is on and the drug-class
              # strip says the same thing instead.
              input_switch(
                ns("nj_heatmap_class_names"),
                "Class names",
                HEATMAP_STYLE_DEFAULTS$show_class_names
              ),
              input_switch(
                ns("nj_heatmap_element"),
                "Element type",
                HEATMAP_STYLE_DEFAULTS$show_element_type
              ),
              # Which end of the matrix that label goes to. Shown only once it
              # has something to place — an orphan Top/Bottom control above a
              # switch that is off reads as a setting with no subject.
              shiny$conditionalPanel(
                condition = "input.nj_heatmap_element",
                ns = ns,
                radioGroupButtons(
                  ns("nj_heatmap_element_pos"),
                  NULL,
                  choiceNames = c("Top", "Bottom"),
                  choiceValues = c("top", "bottom"),
                  selected = HEATMAP_STYLE_DEFAULTS$element_pos,
                  justified = TRUE,
                  size = "sm",
                  width = "100%"
                )
              ),
              # Which authority files the genes. It re-groups every card's
              # picker and re-orders every panel's columns, so it is a decision
              # about the whole heatmap block rather than about one matrix.
              pickerInput(
                ns("nj_heatmap_vocabulary"),
                "Classification",
                choices = amr_plot$AMR_CLASS_VOCABULARIES,
                selected = HEATMAP_STYLE_DEFAULTS$vocabulary
              )
            ),
            accordion_panel(
              "Clustering",
              icon = shiny$icon("sitemap"),
              input_switch(
                ns("nj_heatmap_cluster"),
                "Cluster genes",
                HEATMAP_STYLE_DEFAULTS$cluster
              ),
              # Always on screen, disabled until "Cluster genes" is on
              # (`sync_heatmap_shared_state()`). They describe an ordering only
              # computed while clustering, but a reader deciding whether to turn
              # it on can see what it will offer.
              #
              # Plain selectInputs, not pickerInputs: shinyjs greys a native
              # <select> outright, where a disabled pickerInput keeps its own
              # button drawn as live.
              shiny$selectInput(
                ns("nj_heatmap_distance"),
                "Distance",
                choices = amr_plot$AMR_CLUSTER_DISTANCES,
                selected = HEATMAP_STYLE_DEFAULTS$cluster_distance
              ),
              shiny$selectInput(
                ns("nj_heatmap_method"),
                "Linkage",
                choices = amr_plot$AMR_CLUSTER_METHODS,
                selected = HEATMAP_STYLE_DEFAULTS$cluster_method
              ),
              input_switch(
                ns("nj_heatmap_strip"),
                "Drug class strip",
                HEATMAP_STYLE_DEFAULTS$show_class_strip
              ),
              # Zero (the default) draws no dendrogram while keeping the
              # clustered order.
              shiny$sliderInput(
                ns("nj_heatmap_dend"),
                "Dendrogram depth",
                min = 0,
                max = 20,
                value = HEATMAP_STYLE_DEFAULTS$dend_depth,
                step = 1,
                post = "%",
                ticks = FALSE
              )
            )
          )
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
            # The switch is locked on while a mapping is drawn on the points
            # (see the sidebar-state observer): the hint is what says so,
            # because a disabled control with no reason given reads as a bug.
            input_switch(ns("nj_tippoint_show"), "Show tip points", FALSE),
            shiny$div(
              id = ns("nj_tippoint_show_hint"),
              class = "text-muted fst-italic small mb-2 d-none",
              "A mapped variable is drawn on the tip points. Remove the ",
              "mapping to hide them."
            ),
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
            virtualSelectInput(
              ns("nj_parentnode"),
              "Nodes",
              choices = character(0),
              selected = character(0),
              multiple = TRUE,
              search = TRUE,
              # Without this, the header checkbox highlights every internal
              # node, not just the ones the search term currently matches.
              selectAllOnlyVisible = TRUE,
              searchPlaceholderText = "Search nodes ...",
              placeholder = "No clade highlighted",
              optionsCount = 10,
              noOfDisplayValues = 3,
              # Every pick redraws the tree, so a selection of several clades
              # is batched to the dropdown's close rather than costing one
              # draw per node.
              updateOn = "close",
              dropboxWrapper = "body",
              showDropboxAsPopup = TRUE,
              popupDropboxBreakpoint = "10000px",
              width = "100%"
            ),
            viz_color(ns, "nj_clade_scale", "Highlight color", "#D0F221")
          ),
          accordion_panel(
            "Other Elements",
            icon = shiny$icon("code-branch"),
            input_switch(ns("nj_rootedge_show"), "Root edge", FALSE)
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
          viz_color(ns, "nj_branch_color", "Allelic Distance", "#000000"),
          viz_color(ns, "nj_tippoint_color", "Tip Point", "#3A4657"),
        )
      )
    ),
    shiny$div(
      class = "reset-buttons",
      radioGroupButtons(
        ns("zoom_view"),
        NULL,
        choiceNames = c("Full", "Zoomed"),
        choiceValues = c(FALSE, TRUE),
        width = "100%"
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
          tree_controls(ns, options_ui)
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
  # Staged peer typing results (Database > Import) folded into the distance
  # matrix; NULL means local isolates only.
  imported_sets = shiny$reactive(NULL),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("Tree"),
  algo = shiny$reactive("Neighbour-Joining")
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Inches the tree and its labels get. Everything that reasons about how much
    # room the *tree* has — the layout fit, the tip-label reserve, the export —
    # goes through this, which is why they agree, and none of them can be
    # invalidated by the browser (see TREE_PANEL_IN).
    plot_width_in <- function() TREE_PANEL_IN

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
    # before any data is loaded. Nothing else restores them: they are
    # virtual-select widgets, which shinyjs::reset() does not recognize at all,
    # and even for a widget it does recognize it only ever restores the
    # selected *value* captured at page load — never `choices`, which after
    # Generate no longer contains that value anyway. force_default =
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
          attr(meta, "custom_cols"),
          amr_field_map(meta)
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
        # updateVirtualSelect, not updateSelectInput/updatePickerInput: these
        # are virtual-select widgets and ignore either of those messages
        # entirely — the choices stayed on the placeholder names the UI
        # declares and the control read "Nothing selected". Note the argument
        # order, which is the reverse of updatePickerInput's.
        updateVirtualSelect(
          inputId = id,
          session = session,
          choices = choices,
          selected = value
        )
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
      updateVirtualSelect(
        inputId = "nj_root_isolate",
        session = session,
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
        updateVirtualSelect(
          inputId = "nj_parentnode",
          session = session,
          choices = nodes,
          selected = picked
        )
        set_fitted("nj_parentnode", picked)
      }
    }

    # Whether the tip points are on because a mapping needed them rather than
    # because the user asked for them — which is what makes the switch below
    # reversible without ever undoing a deliberate choice. Cleared as soon as
    # the switch is seen off (see the observer below): points the user turns
    # back on from there are theirs, and stay on when the mapping goes.
    tippoint_auto_on <- shiny$reactiveVal(FALSE)

    shiny$observeEvent(input$nj_tippoint_show, {
      if (!isTRUE(input$nj_tippoint_show)) {
        tippoint_auto_on(FALSE)
      }
    })

    # A mapping onto the tip points draws nothing while the tip points
    # themselves are switched off, and that switch lives in a different tab
    # (Elements) from the mapping that needs it — so adding such a mapping
    # appeared to do nothing at all. Turn the points on with it.
    #
    # And off again with it: the points are an artefact of the mapping, so
    # deleting the mapping (or editing it onto another aesthetic) left the tree
    # wearing dots nobody asked for, in a tab the user had never opened. Only
    # the points this observer switched on are switched back off.
    shiny$observeEvent(
      nj_layers(),
      {
        wants_points <- .layers_want_tippoints(nj_layers())
        shown <- isTRUE(shiny$isolate(fitted$nj_tippoint_show))
        if (wants_points && !shown) {
          tippoint_auto_on(TRUE)
          set_fitted("nj_tippoint_show", TRUE)
          bslib::update_switch("nj_tippoint_show", value = TRUE)
        } else if (
          !wants_points && shown && shiny$isolate(tippoint_auto_on())
        ) {
          tippoint_auto_on(FALSE)
          set_fitted("nj_tippoint_show", FALSE)
          bslib::update_switch("nj_tippoint_show", value = FALSE)
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
        any(vapply(
          ls,
          function(l) identical(l$aesthetic, aesthetic),
          logical(1)
        ))
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

      # The same rule one step further: while a mapping is drawn on the tip
      # points, "Show tip points" is not the user's to turn off. Switching it
      # off hid the points and left the mapping in place — a legend for marks
      # that were not on the tree, and no way to tell from the Elements tab
      # that anything was wrong. Removing the mapping is how the points go.
      owned <- .layers_want_tippoints(ls)
      shinyjs::toggleState("nj_tippoint_show", condition = !owned)
      shinyjs::toggleClass(
        id = "nj_tippoint_show_hint",
        class = "d-none",
        condition = !owned
      )
    })

    # Fit the layout controls to the data, the way the Map fits its "Max
    # intensity" slider to the busiest place. The coded defaults (aspect 0.6,
    # tip label size 4) suit roughly fifteen tips and nothing else — five
    # isolates got oversized overlapping labels and three hundred an unreadable
    # smear that no hand-tuning could fix, since the aspect ratio needed is
    # several times what the slider used to offer.
    #
    # Each fitted value goes to two places: the mirror the render reads, so the
    # tree draws once and draws right, and the slider, so the sidebar shows what
    # was chosen and stays the place to adjust it. The slider is only touched
    # when the value really changed — its echo is harmless (see `fitted`) but
    # pointless.
    refit_layout <- function(tree, notify = FALSE) {
      if (is.null(tree)) {
        return(invisible(NULL))
      }
      fit <- tree_auto_layout(
        length(tree$tip.label),
        plot_width_in(),
        shiny$isolate(input$nj_layout),
        .label_chars(
          tree,
          shiny$isolate(viz_metadata()),
          shiny$isolate(fitted$nj_tiplab)
        )
      )
      for (id in names(FITTED_DEFAULTS)) {
        value <- fit[[FITTED_FIELDS[[id]]]]
        # The layout fit sees only the tree, so the one value that depends on
        # what is drawn *beside* it is solved separately and applied over it.
        if (identical(id, "nj_open_angle")) {
          value <- tree_open_angle(
            shiny$isolate(tree_opts()),
            shiny$isolate(viz_metadata()) %||% data.frame()
          )
        }
        if (!isTRUE(all.equal(shiny$isolate(input[[id]]), value))) {
          shiny$updateSliderInput(session, id, value = value)
        }
        set_fitted(id, value)
      }

      # Past a certain tip count the labels are a grey smudge at any size that
      # fits, so the fit draws the tree without them. Only on a Generate: a
      # re-fit triggered by adding a mapping must not countermand a toggle the
      # user has just set by hand.
      # Two reasons to give up on them, and they are the same reason: room per
      # tip. `labels_legible` is the fitted type size falling under the floor;
      # TIP_MAPPING_MAX is the count past which a per-tip mark stops carrying
      # anything at all, which is the same ceiling the mapping engine uses to
      # stop reaching for the per-tip channels.
      crowded <- crowded_tips(length(tree$tip.label))
      if (
        notify &&
          (crowded || !fit$labels_legible) &&
          isTRUE(shiny$isolate(fitted$nj_tiplab_show))
      ) {
        set_fitted("nj_tiplab_show", FALSE)
        bslib::update_switch("nj_tiplab_show", value = FALSE)
      }
      invisible(fit)
    }

    # Keep the layout fitted as the plot's contents change, not only at
    # Generate. Switching layout between linear and circular changes the row
    # pitch outright, and the label source changes how much width the labels
    # need — both were previously fitted once and then left stale until the next
    # Generate, which is why a circular tree opened at a linear aspect.
    #
    # Deliberately *not* dependent on the mappings: the annotations they add are
    # paid for by a wider canvas (see plot_canvas), not by re-fitting the tree,
    # so a re-fit here would be a redraw that changed nothing. Nor on anything
    # this writes, which would loop.
    # The circle-opening slider belongs to the radial layouts and nowhere else.
    shiny$observe({
      shinyjs::toggleClass(
        id = "nj_open_angle_wrap",
        class = "d-none",
        condition = !identical(input$nj_layout, "circular")
      )
    })

    # Which layout the fit last ran for, so a switch can be told from the other
    # things that trigger one.
    fitted_layout <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      list(input$nj_layout, fitted$nj_tiplab, fitted$nj_tiplab_show),
      {
        shiny$req(tree_obj())
        # A layout switch is re-fitted with the legibility warning armed. Room
        # per tip is a row's height on a linear tree and an *arc* on a circular
        # one, so a count whose labels read perfectly well down a tall page can
        # be a grey smudge around a disc — the switch is exactly the moment the
        # user needs telling. The other triggers here leave it disarmed, since
        # neither of them may countermand a toggle just set by hand.
        switched <- !identical(fitted_layout(), input$nj_layout)
        fitted_layout(input$nj_layout)
        refit_layout(tree_obj(), notify = switched)
      },
      ignoreInit = TRUE
    )

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
    # swatch) — see reset_viz_colors() in viz_helpers.R for why — so those are
    # patched up explicitly right after the blanket reset.
    #
    # populate_metadata_selects() is what returns nj_tiplab / nj_root_isolate /
    # nj_parentnode to their defaults — shinyjs::reset() cannot: virtual-select
    # is a custom binding and is not among the widget types it knows how to
    # restore. It stays deferred with shinyjs::delay() regardless, because
    # shinyjs::reset() restores the rest of the panel asynchronously (it
    # round-trips through the browser to read back each resettable element's
    # page-load value before calling the matching update*Input() on the
    # server), and letting that pass finish first keeps the fitted sliders'
    # echoes from landing on top of the mirrors this call writes.
    # Restore every sidebar control to its coded default. Shared by this
    # engine's own "Reset settings" button and the top-level app-reset
    # (session_reset) path below, so both routes return the controls
    # identically.
    reset_tree_settings <- function() {
      shinyjs::reset(id = "controls_wrap")

      # virtual-select is a custom binding shinyjs::reset() does not restore —
      # the layout picker has to be put back to its default by hand, the same
      # as the metadata-backed selects below.
      updateVirtualSelect(
        inputId = "nj_layout",
        session = session,
        selected = "rectangular"
      )

      reset_viz_colors(
        session,
        nj_color = "#000000",
        nj_bg = "#ffffff",
        nj_tiplab_color = "#000000",
        nj_branch_color = "#000000",
        nj_tippoint_color = "#3A4657",
        nj_clade_scale = "#D0F221"
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

    # The heatmap panels, one per AMR element type the user has added, in the
    # order they were added. Same id discipline as the mapping layers, kept in
    # its own sequence so a heatmap card and a mapping card never share an id.
    nj_heatmaps <- shiny$reactiveVal(list())
    nj_heatmap_layer_seq <- shiny$reactiveVal(0L)

    next_layer_id <- function() {
      n <- nj_layer_seq() + 1L
      nj_layer_seq(n)
      paste0("L", n)
    }

    next_heatmap_layer_id <- function() {
      n <- nj_heatmap_layer_seq() + 1L
      nj_heatmap_layer_seq(n)
      paste0("H", n)
    }

    profiles <- shiny$reactive({
      p <- field_profiles()
      if (is.null(p) || !nrow(p)) NULL else p
    })

    # Tips this tree will draw, which the mapping engine needs to decide whether
    # the per-tip channels are still readable (mapping_engine$TIP_MAPPING_MAX).
    # The profile frame cannot answer it: its own `n` counts every isolate in
    # the database so a variable reads the same in every tab, while a tree draws
    # only the tab's selection — one row of `viz_metadata` per tip.
    n_tips <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta)) NULL else nrow(meta)
    })

    # Channels this plot is not drawing, so the engine neither picks them nor
    # leaves a mapping stranded on one. Tip labels are the only switchable one:
    # the tip points come *on* with a mapping (see .layers_want_tippoints), so
    # a point channel is never off while something needs it.
    off_channels <- shiny$reactive({
      c(
        if (isTRUE(fitted$nj_tiplab_show)) NULL else "tiplab_color",
        # An inward tree has no room past its tips for a strip — that space is
        # the middle of the disc (see tree_plot$tree_annotations_drawn), so a
        # variable is drawn onto the tips there instead.
        if (identical(input$nj_layout, "inward")) "tile" else NULL
      ) %||%
        character(0)
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
        tiplab_color = input$nj_tiplab_color,
        # Every variable mapping, in draw order. One key replaces the eight the
        # five per-aesthetic panels used to contribute, and it needs none of
        # their "only read this while its switch is on" guarding: a layer exists
        # only because the user added it, so there is no picker sitting on a
        # stale value for the plot to mistake for a change.
        layers = nj_layers(),
        # Branch labels. Allelic distance is the only thing they ever carry, so
        # there is no source to resolve — the picker that used to choose one was
        # removed along with the rest of this section's controls. Which branches
        # get one is not passed either: the renderer solves it from the axis it
        # has just laid out (tree_branch_keep), which is the only place the
        # drawn width of a branch is known.
        branch_show = input$nj_show_branch_label,
        branch_size = fitted$nj_branch_size,
        branch_color = input$nj_branch_color,
        # Tip points. A mapping onto them is drawn *on* the points, so it brings
        # the element with it whatever the switch says. The switch is locked on
        # while such a mapping exists (see the sidebar-state observer), and this
        # is what makes that true of the plot rather than only of the sidebar —
        # a restored Analysis can put a saved "off" into the mirror after the
        # mapping is already there.
        tippoint_show = isTRUE(fitted$nj_tippoint_show) ||
          .layers_want_tippoints(nj_layers()),
        tippoint_alpha = input$nj_tippoint_alpha,
        tippoint_size = fitted$nj_tippoint_size,
        tippoint_color = input$nj_tippoint_color,
        tippoint_shape = input$nj_tippoint_shape,
        # Clade highlights.
        nodelabel_show = input$nj_nodelabel_show,
        parentnodes = fitted$nj_parentnode %||% character(0),
        clade_color = input$nj_clade_scale,
        # Heatmap panel, and — only when it is drawing genes — the call
        # matrix it reads from. Kept out of the metadata table because it is one
        # column per gene and nothing maps it; see amr_matrix above.
        heatmaps = nj_heatmaps(),
        amr_matrix = if (
          any(vapply(
            nj_heatmaps(),
            function(h) identical(h$level, "gene"),
            logical(1)
          ))
        ) {
          amr_matrix()
        },
        # Elements toggles.
        rootedge_show = input$nj_rootedge_show,
        treescale_show = input$nj_treescale_show,
        axis_show = input$nj_axis_show,
        # Panel width, for the tip-label reserve (see .tiplab_frac).
        width_in = plot_width_in(),
        # Dimensions / legend.
        # Fixed. `as.ggplot()` spends (1 - zoom) of the canvas on a blank
        # border and shifts the plot inside it — compensation for content
        # running off the edge, which the axis reserves now prevent in every
        # layout. There is nothing left for them to correct, so they are no
        # longer the user's to set.
        zoom = 1,
        h = 0,
        v = 0,
        legend_orientation = LEGEND_ORIENTATION,
        legend_size = LEGEND_SIZE,
        # The tree's own aspect. The builder turns it into a plot height, which
        # for a radial layout is not this at all (see tree_legend_cols).
        aspect = fitted$nj_aspect_ratio,
        # Degrees of circle left open for the ring headers. Fitted on Generate
        # and adjustable after; only a radial layout uses it.
        open_angle = fitted$nj_open_angle
      )
    )

    # How big the canvas has to be for this plot's contents, in inches.
    #
    # The tree keeps TREE_PANEL_IN whatever happens; the annotations and the
    # legend are added *around* it. Height follows the tree's own budget and
    # aspect, so switching a legend on widens the image without stretching the
    # rows — the row pitch is what makes the tip labels legible, and it must not
    # move because something was added beside them.
    #
    # Capped at CANVAS_MAX_FACTOR: past that the annotations share what is left
    # rather than the canvas growing without limit.
    plot_canvas <- shiny$reactive({
      opts <- tree_opts()
      meta <- viz_metadata()
      md <- if (is.null(meta)) data.frame() else meta

      # Solved in tree_plot.R, beside the axis split it has to agree with: the
      # annotations' share is a fraction of the tree's *span*, not of the panel,
      # so the panel a given annotation set needs is not simply the sum.
      panel_in <- tree_panel_width_in(opts, md, TREE_PANEL_IN)

      circular <- identical(opts$layout, "circular") ||
        identical(opts$layout, "inward")
      # A circular tree is a disc, so its panel is square — and the annotation
      # rings grow it in *both* directions, not just across. Holding the height
      # at the budget while the width grew for the rings drew the disc as an
      # ellipse and put the outer ring off the bottom of the image.
      height_in <- if (circular) {
        panel_in
      } else {
        TREE_PANEL_IN * fitted$nj_aspect_ratio
      }

      # After the height, because how many columns the guides need depends on
      # it — and so, in turn, does how much width they claim.
      legend_in <- tree_legend_width_in(
        opts$layers,
        md,
        opts$legend_size,
        TREE_PANEL_IN,
        opts$heatmaps,
        height_in
      )
      canvas_in <- min(
        panel_in + legend_in,
        TREE_PANEL_IN * CANVAS_MAX_FACTOR
      )

      list(
        panel_in = TREE_PANEL_IN,
        canvas_in = canvas_in,
        height_in = height_in,
        # The aspect of the finished image, which is what the export and the
        # thumbnail need — not the tree's own aspect, since the canvas is wider.
        aspect = height_in / canvas_in
      )
    })

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

    # Recompute the tree. Fires on Generate, and also on the computation options
    # that now live in this engine's own sidebar rather than the tab's — the
    # missing-value handling, the algorithm and the imported sets. Those are on
    # the live side of the split, so they take effect when they change.
    #
    # The isolate selection is deliberately *not* here: it is a left-sidebar
    # control, and the tab only publishes it once Generate has applied it (see
    # applied_selection in visualization_plot.R), so it reaches this observer
    # through generate() alone.
    #
    # This is the expensive path — a few hundred isolates spend most of a second
    # in the distance matrix — so the trigger list is kept to inputs that really
    # change what is computed.
    shiny$observeEvent(
      list(generate(), na_handling(), algo(), imported_sets()),
      {
        if (!identical(plot_type(), "Tree")) {
          return()
        }
        # Nothing to compute before the first Generate; without this, touching an
        # option on a fresh tab would draw a tree the user never asked for.
        if (!isTRUE(generated()) && !isTRUE((generate() %||% 0L) > 0L)) {
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
        if (!is.null(tree)) {
          refit_layout(tree, notify = TRUE)
        }

        tree_obj(tree)

        generated(TRUE)
      }
    )

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
      update_scale_select(session, input_id, color_scales[cats], sel)
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
      if (
        any(vapply(layers, function(l) identical(l$field, field), logical(1)))
      ) {
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
      layer <- assign_mapping_layer(
        prof,
        layers,
        id = next_layer_id(),
        values = viz_metadata()[[field]],
        n_units = n_tips(),
        off = off_channels()
      )
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
      nj_layers(
        rebalance_layers(
          keep,
          profiles(),
          "tree",
          viz_metadata(),
          n_tips(),
          off_channels()
        )
      )
    })

    output$nj_layers_ui <- shiny$renderUI({
      layer_cards(
        ns,
        nj_layers(),
        "tree",
        "nj_layer_edit",
        "nj_layer_delete",
        legend_max = LEGEND_MAX_KEYS
      )
    })

    # Turning the tip labels off strands any mapping drawn on them, so the
    # engine re-places it — onto a tile strip, which is what is left. Without
    # this the layer stayed on a channel the plot no longer draws: no marks, no
    # legend, and a card in the sidebar claiming otherwise.
    shiny$observeEvent(
      off_channels(),
      {
        layers <- nj_layers()
        shiny$req(length(layers))
        moved <- rebalance_layers(
          layers,
          profiles(),
          "tree",
          viz_metadata(),
          n_tips(),
          off_channels()
        )
        if (!identical(moved, layers)) {
          nj_layers(moved)
        }
      },
      ignoreInit = TRUE
    )

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
      # Which aesthetics fit depends on the variable as its granularity leaves
      # it: a date grouped by year is six levels and can take a shape, where
      # the raw column is a continuum and cannot.
      values <- viz_metadata()[[l$field]]
      binned <- granularity_profile(prof, values, l$granularity)
      free <- union(
        l$aesthetic,
        eligible_aesthetics(binned, taken, "tree", n_tips(), off_channels())
      )
      blocked <- setdiff(names(AESTHETIC_LABELS), free)
      reasons <- Filter(
        Negate(is.null),
        lapply(blocked, function(a) aesthetic_block_reason(binned, a))
      )

      # Parsed, not raw: an ungrouped date reaches the scale as a continuum,
      # and out of SQLite it is a character column that no test for one can
      # recognise. Left raw, the palette offer came back with Qualitative on it.
      shown <- if (is_date_profile(prof)) {
        bin_date_values(values, l$granularity)
      } else {
        values
      }
      cats <- scale_categories_for(
        if (isTRUE(binned$continuous)) shown else as.character(shown),
        suitable_scale_categories(
          if (isTRUE(binned$continuous)) "Numeric" else "Factor",
          shown
        )
      )

      shiny$showModal(shiny$modalDialog(
        title = paste("Mapping:", l$title),
        size = "s",
        easyClose = TRUE,
        if (is_date_profile(prof)) {
          granularity_select(
            ns,
            "nj_layer_granularity",
            l$granularity,
            values = values
          )
        },
        pickerInput(
          ns("nj_layer_aesthetic"),
          "Show as",
          choices = setNames(free, unname(AESTHETIC_LABELS[free])),
          selected = l$aesthetic
        ),
        # Why the tile strip led the list. The per-tip channels are still
        # offered — this says what picking one costs at this many tips.
        if (crowded_tips(n_tips())) {
          shiny$div(
            class = "small text-muted mb-2",
            sprintf(
              paste(
                "%d tips is past the %d this tree can draw a readable tip",
                "point or label at. A tile strip keeps its width whatever",
                "the tip count; the others do not."
              ),
              n_tips(),
              TIP_MAPPING_MAX
            )
          )
        },
        if (length(reasons)) {
          shiny$div(
            class = "small text-muted mb-2",
            # Saying why an option is missing is the whole point: the old
            # picker just left it out.
            lapply(reasons, shiny$tags$div)
          )
        },
        if (l$aesthetic %in% COLOR_AESTHETICS) {
          scale_select(
            ns,
            "nj_layer_palette",
            categories = cats,
            selected = l$palette
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
        l <- set_layer_granularity(
          l,
          input$nj_layer_granularity,
          viz_metadata()[[l$field]]
        )
        # Pinned: rebalance_layers() must not undo a deliberate choice.
        l$auto <- FALSE
        l
      })
      # A pinned layer may now hold an aesthetic an automatic one had, so the
      # automatic ones move out of its way.
      nj_layers(
        rebalance_layers(
          layers,
          profiles(),
          "tree",
          viz_metadata(),
          n_tips(),
          off_channels()
        )
      )
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
          # radioGroupButtons' choiceValues = c(FALSE, TRUE) round-trip as the
          # strings "FALSE"/"TRUE" (the widget's HTML `value` attribute), never
          # as a logical — as.logical() is what makes isTRUE() mean anything here.
          if (isTRUE(as.logical(shiny$isolate(input$zoom_view)))) "is-zoom"
        ),
        id = ns("plot_stage"),
        prompt,
        loading,
        # height="auto" lets the container shrink-wrap the image, whose pixel
        # height renderPlot() derives from the panel width and the aspect-ratio
        # control below. The plotOutput default (height="400px") would instead
        # pin the box at 400px and clip the aspect-sized plot.
        shiny$plotOutput(ns("tree_plot"), height = "auto")
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
      # Both come from plot_canvas(), which grows the canvas for whatever the
      # plot has to show beside the tree rather than shrinking the tree to fit
      # it. Still no clientData anywhere in here, so no report from the browser
      # can trigger a redraw.
      width = function() {
        min(PLOT_MAX_PX, as.integer(plot_canvas()$canvas_in * PLOT_RES))
      },
      height = function() {
        min(PLOT_MAX_PX, as.integer(plot_canvas()$height_in * PLOT_RES))
      },
      res = PLOT_RES
    )

    # Fit ⇄ Zoom display mode, driven by this engine's own `zoom_view` switch
    # (right sidebar, tree_controls()). Purely toggles the .is-zoom class on the
    # mounted stage — the image itself is not re-rendered (see the tree-stage
    # CSS in app/styles/main.scss). ignoreInit: the initial state is already
    # stamped on the stage div by renderUI's isolate() read, so the first
    # (FALSE) value needs no toggle; this only fires on user changes.
    shiny$observeEvent(
      input$zoom_view,
      {
        shinyjs::toggleClass(
          id = "plot_stage",
          class = "is-zoom",
          condition = isTRUE(as.logical(input$zoom_view))
        )
      },
      ignoreInit = TRUE
    )

    # --- Heatmap panels ------------------------------------------------------
    #
    # Two panels, either or both, one per report abritamr writes: resistance
    # genes and virulence/stress genes. Both are the same measurement — the
    # call on one gene in one isolate — which is what makes a shared fill scale
    # mean something; they are two panels rather than one because they answer
    # different questions about the isolate and a reader looks for them
    # separately.
    #
    # The drug-class panel is gone. It drew one column per class holding
    # presence or absence, which is the *same* finding as its genes with the
    # detail thrown away: a class column is positive exactly when one of its
    # gene columns is. Two panels saying the same thing at different
    # resolutions is not a comparison, it is a duplicate — and it cost a column
    # run and a legend to say so.

    # The gene-level matrix the heatmap panels draw: one factor column per gene
    # symbol found in `amr_results`, holding the AMRFinderPlus method tier of
    # that isolate/gene (`amr_confidence_frame()`) — the same tiers and the same
    # source table the AMR-plot engine's own gene heatmap draws from, so a gene
    # reads at the identical tier and colour in both.
    # No req(): a database with no AMR screen is a value here, not a reason for
    # this (or the catalogue below) to stop.
    amr_matrix <- shiny$reactive({
      db_events$depend(db_rev, "amr", "isolates")
      path <- db_path()
      meta <- viz_metadata()
      if (
        is.null(path) ||
          !length(path) ||
          is.na(path) ||
          !nzchar(path) ||
          is.null(meta)
      ) {
        return(NULL)
      }
      amr_plot$amr_confidence_frame(amr_plot$load_amr_hits(path), meta$isolate)
    })

    # Every gene one panel could draw, with what the picker needs to describe
    # it: a label, the drug class to file it under, and a sub-text. Read
    # straight off `amr_results` (via `load_amr_hits`/`amr_gene_meta`) rather
    # than the metadata table, restricted to isolates actually in this tree —
    # a gene only some other isolate carries has nothing to show here.
    #
    # Both class vocabularies are carried, one column each, rather than one
    # `group` resolved from a setting: they are two readings of the same hits
    # (see amr_plot$AMR_CLASS_VOCABULARIES), the switch is per panel, and
    # re-reading the database every time a panel changes vocabulary would cost
    # a query to answer a question already in hand.
    .empty_catalog <- data.frame(
      col = character(0),
      label = character(0),
      rollup = character(0),
      amrfinder = character(0),
      element = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    )

    gene_catalog <- shiny$reactive({
      db_events$depend(db_rev, "amr", "isolates")
      path <- db_path()
      meta <- viz_metadata()
      if (
        is.null(path) ||
          !length(path) ||
          is.na(path) ||
          !nzchar(path) ||
          is.null(meta) ||
          !nrow(meta)
      ) {
        return(.empty_catalog)
      }
      hits <- amr_plot$load_amr_hits(path)
      hits <- hits[hits$isolate %in% meta$isolate, , drop = FALSE]
      if (!nrow(hits)) {
        return(.empty_catalog)
      }
      genes <- sort(unique(hits$gene_symbol))
      sections <- amr_plot$load_amr_sections(path)
      # The curated rollup class per gene — the same grouping the AMR-plot
      # engine's heatmap files its columns under by default — and AMRFinder's
      # own beside it. The element type is the same either way.
      rollup <- amr_plot$amr_gene_meta(hits, genes, sections, "rollup")
      amrfinder <- amr_plot$amr_gene_meta(hits, genes, sections, "amrfinder")
      counts <- vapply(
        genes,
        function(g) length(unique(hits$isolate[hits$gene_symbol == g])),
        integer(1)
      )
      data.frame(
        col = genes,
        label = genes,
        rollup = rollup$group,
        amrfinder = amrfinder$group,
        element = rollup$element_type,
        description = sprintf(
          "%d isolate%s",
          counts,
          ifelse(counts == 1L, "", "s")
        ),
        stringsAsFactors = FALSE
      )
    })

    # One element type's genes, filed under the vocabulary the panel asked for
    # and ordered by it — the column order is the class order, so switching
    # vocabulary re-files *and* re-orders the panel.
    element_catalog <- function(element, vocabulary = NULL) {
      cat <- gene_catalog()
      if (!nrow(cat)) {
        cat$group <- character(0)
        return(cat)
      }
      cat <- cat[cat$element == element, , drop = FALSE]
      key <- if (identical(vocabulary, "amrfinder")) "amrfinder" else "rollup"
      cat$group <- cat[[key]]
      cat[order(cat$group, cat$label), , drop = FALSE]
    }

    # Element types with at least one gene, in Resistance/Virulence/Stress
    # order — the order a heatmap added for each reads top to bottom in too.
    available_elements <- shiny$reactive({
      cat <- gene_catalog()
      if (!nrow(cat)) {
        return(character(0))
      }
      intersect(unname(amr_plot$AMR_ELEMENT_TYPES), unique(cat$element))
    })

    element_title <- function(element) {
      types <- amr_plot$AMR_ELEMENT_TYPES
      paste(names(types)[match(element, types)], "genes")
    }

    output$nj_heatmap_none <- shiny$renderUI({
      if (nrow(gene_catalog())) {
        return(NULL)
      }
      shiny$div(
        class = "text-muted small",
        "No AMR screening results in this database."
      )
    })

    # The "Add a heatmap" picker offers only element types the screen actually
    # found and that do not already have a panel — once all three are added
    # there is nothing left to pick, same as a mapping picker running out of
    # variables it makes sense to map twice.
    shiny$observe({
      added <- vapply(nj_heatmaps(), function(h) h$element, character(1))
      choices <- setdiff(available_elements(), added)
      types <- amr_plot$AMR_ELEMENT_TYPES
      updateVirtualSelect(
        inputId = "nj_heatmap_add",
        session = session,
        choices = setNames(choices, names(types)[match(choices, types)]),
        selected = character(0)
      )
    })

    shiny$observeEvent(input$nj_heatmap_add, {
      element <- input$nj_heatmap_add
      shiny$req(nzchar(element %||% ""))
      updateVirtualSelect(
        inputId = "nj_heatmap_add",
        session = session,
        selected = character(0)
      )

      layers <- nj_heatmaps()
      if (
        any(vapply(
          layers,
          function(h) identical(h$element, element),
          logical(1)
        ))
      ) {
        return()
      }
      cat <- element_catalog(element, HEATMAP_STYLE_DEFAULTS$vocabulary)
      if (!nrow(cat)) {
        return()
      }
      # Default settings: every gene of this element type in catalogue order,
      # on the shared confidence colours and unclustered — exactly as picking a
      # variable in Mapping starts the layer on its own automatic choices.
      nj_heatmaps(c(
        layers,
        list(c(
          list(
            id = next_heatmap_layer_id(),
            kind = "amr",
            level = "gene",
            element = element,
            cols = cat$col,
            labels = cat$label,
            classes = cat$group,
            palette = "Reds",
            title = element_title(element)
          ),
          HEATMAP_STYLE_DEFAULTS
        ))
      ))
    })

    shiny$observeEvent(input$nj_heatmap_delete, {
      nj_heatmaps(Filter(
        function(h) !identical(h$id, input$nj_heatmap_delete),
        nj_heatmaps()
      ))
    })

    output$nj_heatmap_layers_ui <- shiny$renderUI({
      # Passed as a function, not a frame: each card's picker lists its own
      # panel's element type filed under that panel's own vocabulary.
      .heatmap_layer_cards(
        ns,
        nj_heatmaps(),
        function(h) element_catalog(h$element, h$vocabulary)
      )
    })

    # ---- Settings shared by every panel --------------------------------------

    # The panel a card button names, or NULL when it names none.
    heatmap_by_id <- function(id) {
      hit <- Filter(function(h) identical(h$id, id), nj_heatmaps())
      if (length(hit)) hit[[1]] else NULL
    }

    # The clustering controls stay on screen whether or not clustering is on,
    # disabled until it is — and the class-names switch is their mirror, usable
    # only while it is off, since clustered columns are not in class order to
    # bracket. shinyjs rather than a conditionalPanel so the ordering options
    # are visible before the reader commits to them.
    #
    # The whole block is hidden while no panel exists: these apply to every
    # heatmap, and with none added they apply to nothing.
    sync_heatmap_shared_state <- function() {
      on <- isTRUE(input$nj_heatmap_cluster)
      for (id in c(
        "nj_heatmap_distance",
        "nj_heatmap_method",
        "nj_heatmap_strip",
        "nj_heatmap_dend"
      )) {
        shinyjs::toggleState(id, condition = on)
      }
      shinyjs::toggleState("nj_heatmap_class_names", condition = !on)
      shinyjs::toggle("nj_heatmap_shared", condition = length(nj_heatmaps()) > 0)
    }

    shiny$observe(sync_heatmap_shared_state())

    # The shared controls, written onto every panel at once.
    #
    # One observer rather than one per control: they all write the same record
    # list, and ten observers each rewriting it would cost ten redraws for a
    # change that is one. `%||%` on every read so a control that has not
    # round-tripped yet leaves the field as it was rather than nulling it.
    #
    # It also reads `nj_heatmaps()`, which is what stamps the current shared
    # settings onto a panel the moment it is added — a new matrix joins the
    # block already arranged like the ones beside it. The identical() guard is
    # what stops that self-reference from looping.
    shiny$observe({
      layers <- nj_heatmaps()
      if (!length(layers)) {
        return()
      }
      shared <- list(
        show_gene_names = isTRUE(input$nj_heatmap_gene_names),
        show_class_names = isTRUE(input$nj_heatmap_class_names),
        show_element_type = isTRUE(input$nj_heatmap_element),
        element_pos = input$nj_heatmap_element_pos %||% ELEMENT_POS_DEFAULT,
        cluster = isTRUE(input$nj_heatmap_cluster),
        show_class_strip = isTRUE(input$nj_heatmap_strip)
      )
      updated <- lapply(layers, function(h) {
        for (key in names(shared)) {
          h[[key]] <- shared[[key]]
        }
        h$cluster_distance <- input$nj_heatmap_distance %||% h$cluster_distance
        h$cluster_method <- input$nj_heatmap_method %||% h$cluster_method
        h$dend_depth <- input$nj_heatmap_dend %||% h$dend_depth
        h
      })
      if (!identical(updated, layers)) {
        nj_heatmaps(updated)
      }
    })

    # Classification is shared too, but changing it re-files every gene and
    # re-orders every panel's columns, so it rebuilds each panel's labels and
    # classes from the catalogue rather than just setting a field. Its own
    # observer because that rebuild must not run on every switch flip.
    shiny$observeEvent(
      input$nj_heatmap_vocabulary,
      {
        vocab <- input$nj_heatmap_vocabulary
        layers <- lapply(nj_heatmaps(), function(h) {
          if (identical(h$vocabulary, vocab)) {
            return(h)
          }
          h$vocabulary <- vocab
          cat <- element_catalog(h$element, vocab)
          if (nrow(cat)) {
            # The panel's *selection* survives; its order and its filing are
            # the new vocabulary's to decide.
            keep <- cat$col[cat$col %in% h$cols]
            idx <- match(keep, cat$col)
            h$cols <- keep
            h$labels <- cat$label[idx]
            h$classes <- cat$group[idx]
          }
          h
        })
        if (!identical(layers, nj_heatmaps())) {
          nj_heatmaps(layers)
        }
      },
      ignoreInit = TRUE
    )

    # One card's gene picker reports what the reader confirmed. The picker
    # lives in the card, so its input id is per panel and there is one observer
    # per panel — created once and kept, because the cards are redrawn whenever
    # the panel list changes and a fresh observer per redraw would apply the
    # same pick several times over.
    gene_observers <- new.env(parent = emptyenv())

    set_heatmap_genes <- function(id, chosen) {
      layers <- lapply(nj_heatmaps(), function(h) {
        if (!identical(h$id, id)) {
          return(h)
        }
        cat <- element_catalog(h$element, h$vocabulary)
        if (!nrow(cat)) {
          return(h)
        }
        # Catalogue order, not the picker's: unclustered, the catalogue files
        # the genes by drug class and the panel brackets each class under its
        # own run of columns, which has to be contiguous to be bracketed. An
        # empty selection means "every gene" rather than an empty matrix, same
        # as the picker's own placeholder says.
        keep <- cat$col[cat$col %in% (chosen %||% character(0))]
        if (!length(keep)) {
          keep <- cat$col
        }
        idx <- match(keep, cat$col)
        h$cols <- keep
        h$labels <- cat$label[idx]
        h$classes <- cat$group[idx]
        h
      })
      # Guarded so the value the browser reports back when a card is first
      # drawn — which is the selection the card was drawn *from* — is not a
      # change, and does not redraw the tree.
      if (!identical(layers, nj_heatmaps())) {
        nj_heatmaps(layers)
      }
    }

    shiny$observe({
      for (h in nj_heatmaps()) {
        key <- .heatmap_gene_input_id(h$id)
        if (!is.null(gene_observers[[key]])) {
          next
        }
        # `local()` so each observer closes over its own panel id rather than
        # over the loop variable, which by the time it fires is the last card.
        gene_observers[[key]] <- local({
          id <- h$id
          field <- key
          shiny$observeEvent(
            input[[field]],
            set_heatmap_genes(id, input[[field]]),
            ignoreInit = TRUE,
            # Clearing the picker sends NULL, and that is the "every gene"
            # case rather than nothing to do.
            ignoreNULL = FALSE
          )
        })
      }
    })

    # ---- One panel's colours -------------------------------------------------

    # Which panel the colour modal is open for. The gene picker needs no such
    # state: it is a control inside the card, so it already knows.
    coloring_heatmap <- shiny$reactiveVal(NULL)

    # Colour is the one thing left that is a single panel's own. Everything else
    # about how a panel is drawn — its labels, its clustering, which vocabulary
    # files its genes — is shared across the block and lives in the sidebar
    # accordions, so this dialog is one column rather than the two the old
    # combined one needed.
    #
    # Two ways to colour the same five tiers, and exactly one of them is live:
    # the four hand-picked swatches, or one sequential ramp spread across them.
    # A segmented control picks which, and swaps the body under it rather than
    # greying the other half — a disabled colour swatch still reads as a colour,
    # which is precisely the confusion to avoid here. Both sets of values are
    # kept on the record, so switching back and forth loses neither.
    shiny$observeEvent(input$nj_heatmap_colors, {
      h <- heatmap_by_id(input$nj_heatmap_colors)
      shiny$req(!is.null(h))
      coloring_heatmap(h$id)

      shiny$showModal(shiny$modalDialog(
        title = paste("Colours:", h$title),
        size = "m",
        easyClose = TRUE,
        radioGroupButtons(
          ns("nj_heatmap_color_mode"),
          "Confidence tiers",
          choiceNames = c("Pick each", "Colour scale"),
          choiceValues = c("tiers", "scale"),
          selected = h$color_mode %||% "tiers",
          justified = TRUE,
          size = "sm",
          width = "100%"
        ),
        # conditionalPanel rather than a server-side swap: the toggle is a
        # preview of the reader's own choice and has no business waiting on a
        # round-trip to redraw.
        shiny$conditionalPanel(
          condition = "input.nj_heatmap_color_mode == 'tiers'",
          ns = ns,
          shiny$div(
            class = "viz-color-grid",
            # Labelled by tier, strongest first, the order the panel's own
            # legend lists them in. Putative has no swatch: it is blended out
            # of Absent and Partial (amr_plot$amr_confidence_palette), same as
            # on the AMR tab.
            viz_color(ns, "nj_heatmap_present", "Perfect", h$color_present),
            viz_color(ns, "nj_heatmap_strong", "Strong", h$color_strong),
            viz_color(ns, "nj_heatmap_partial", "Partial", h$color_partial),
            viz_color(ns, "nj_heatmap_absent", "Absent", h$color_absent)
          )
        ),
        shiny$conditionalPanel(
          condition = "input.nj_heatmap_color_mode == 'scale'",
          ns = ns,
          # Sequential families only. The tiers are a ladder from Absent to
          # Perfect, and only a light-to-dark ramp reads as one — a qualitative
          # palette would colour five ordered things as five unrelated ones.
          scale_select(
            ns,
            "nj_heatmap_heat_scale",
            categories = "Sequential",
            selected = h$heat_scale %||% HEAT_SCALE_DEFAULT
          ),
          shiny$div(
            class = "text-muted fst-italic small mb-2",
            "Absent takes the lightest stop, Perfect the darkest."
          )
        ),
        shiny$tags$h6("Drug class strip", class = "viz-modal_heading"),
        # The strip only exists on a clustered panel, so with clustering off
        # there is nothing here to colour. Swapped for the reason rather than
        # greyed: a disabled pickerInput keeps its own button drawn as live, so
        # greying this one would say nothing at all.
        shiny$conditionalPanel(
          condition = "input.nj_heatmap_cluster",
          ns = ns,
          # Qualitative families only — it names drug classes, a factor —
          # starting on the shared default so a class matches the AMR tab until
          # the reader overrides it.
          scale_select(
            ns,
            "nj_heatmap_strip_scale",
            categories = "Qualitative",
            selected = h$strip_scale %||% CLASS_STRIP_SCALE
          )
        ),
        shiny$conditionalPanel(
          condition = "!input.nj_heatmap_cluster",
          ns = ns,
          shiny$div(
            class = "text-muted fst-italic small mb-2",
            "Turn on Cluster genes to colour the drug-class strip."
          )
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("nj_heatmap_apply"), "Apply")
        )
      ))
    })

    shiny$observeEvent(input$nj_heatmap_apply, {
      id <- coloring_heatmap()
      shiny$req(!is.null(id))
      layers <- lapply(nj_heatmaps(), function(h) {
        if (!identical(h$id, id)) {
          return(h)
        }
        # `%||%` on every read: the modal is torn down after Apply, so a field
        # whose widget never reported a value keeps what the panel already had
        # rather than becoming NULL and losing its colour. That also covers the
        # half of the dialog the segmented control had hidden — a
        # conditionalPanel's inputs stop reporting while hidden, and the values
        # they held must survive being switched away from.
        h$color_present <- input$nj_heatmap_present %||% h$color_present
        h$color_strong <- input$nj_heatmap_strong %||% h$color_strong
        h$color_partial <- input$nj_heatmap_partial %||% h$color_partial
        h$color_absent <- input$nj_heatmap_absent %||% h$color_absent
        h$color_mode <- input$nj_heatmap_color_mode %||% h$color_mode
        h$heat_scale <- input$nj_heatmap_heat_scale %||% h$heat_scale
        h$strip_scale <- input$nj_heatmap_strip_scale %||% h$strip_scale
        h
      })
      nj_heatmaps(layers)
      coloring_heatmap(NULL)
      shiny$removeModal()
    })

    # ---- Export contract ----------------------------------------------------
    # The tab's sidebar owns the export panel and the download itself; this
    # engine only says what it can produce and how to write it. Exported on the
    # preview's own aspect ratio so the tip labels keep the proportion they were
    # tuned to and whatever sits beside the tree gets the room it had on screen
    # — the *width* comes from the export panel, since that is the one thing the
    # user sets in physical units.
    export <- list(
      kind = "ggplot",
      label = "tree",
      ready = shiny$reactive(isTRUE(generated())),
      aspect = shiny$reactive(plot_canvas()$aspect),
      # What the chosen size will do to the smallest type on the figure.
      #
      # Scaling the design keeps it proportioned at any width, which is what
      # makes the export faithful — but proportion is not legibility. A dense
      # tree on a journal column is correctly drawn and still unreadable, and
      # the honest thing is to say so before the file is written rather than
      # to quietly enlarge the type and put the labels back on top of each
      # other.
      note = function(width_cm) {
        canvas <- plot_canvas()
        meta <- viz_metadata()
        md <- if (is.null(meta)) data.frame() else meta
        k <- (max(1, width_cm) / CM_PER_IN) / canvas$canvas_in
        pt <- tree_min_type_pt(scale_tree_opts(tree_opts(), k), md)
        if (!is.finite(pt) || pt >= MIN_PRINT_PT) {
          return(NULL)
        }
        sprintf(
          paste(
            "Smallest text would print at %.1f pt — under the %g pt most",
            "journals ask for. Export wider, or show fewer columns."
          ),
          pt,
          MIN_PRINT_PT
        )
      },
      # Rebuilt at the size it is going out at, not the size it was drawn at.
      # A ggplot cannot be rescaled: its type is in millimetres and the
      # reserves around it are fractions, so printing the preview at another
      # width moves one and not the other — which is how a 25cm export came out
      # with its tip labels colliding and a 40cm one with them lost in a
      # gutter. `scale_tree_opts()` scales the design instead, so the export is
      # the preview at another size rather than the preview stretched.
      save = function(file, format, opts) {
        canvas <- plot_canvas()
        target_in <- max(1, opts$width_cm) / CM_PER_IN
        built <- build_tree_ggtree(
          plot_inputs()$tree,
          plot_inputs()$metadata,
          scale_tree_opts(plot_inputs()$opts, target_in / canvas$canvas_in)
        )
        save_plot_export(
          built,
          file,
          format,
          width_cm = opts$width_cm,
          aspect = canvas$aspect,
          dpi = opts$dpi
        )
      }
    )

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
      list(
        zoom_view = isTRUE(as.logical(input$zoom_view)),
        .layers = nj_layers(),
        .heatmaps = nj_heatmaps()
      )
    ))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "nj_tiplab_show",
          "nj_show_branch_label",
          "nj_tippoint_show",
          "nj_nodelabel_show",
          "nj_rootedge_show",
          "nj_treescale_show",
          "nj_axis_show",
          # The heatmap block's shared controls. Real sidebar inputs since the
          # style modal was split up, so they snapshot and restore like any
          # other control rather than riding along inside the panel records.
          "nj_heatmap_gene_names",
          "nj_heatmap_class_names",
          "nj_heatmap_element",
          "nj_heatmap_cluster",
          "nj_heatmap_strip"
        ),
        selects = c("nj_tippoint_shape", "nj_heatmap_vocabulary"),
        # A virtualSelectInput now, so it cannot ride in `selects` — a picker
        # update message is ignored by the widget entirely.
        virtual_selects = "nj_layout",
        sliders = c(
          "nj_tiplab_size",
          "nj_branch_size",
          "nj_tippoint_alpha",
          "nj_tippoint_size",
          "nj_aspect_ratio",
          "nj_open_angle",
          "nj_heatmap_dend"
        ),
        colors = c(
          "nj_color",
          "nj_bg",
          "nj_tiplab_color",
          "nj_branch_color",
          "nj_tippoint_color",
          "nj_clade_scale"
        ),
        radio_groups = c("zoom_view", "nj_heatmap_element_pos")
      )

      # Plain selectInputs, not pickers, so they cannot ride in `selects` —
      # a picker's update message leaves a native <select> untouched.
      for (id in c("nj_heatmap_distance", "nj_heatmap_method")) {
        if (!is.null(vals[[id]])) {
          shiny$updateSelectInput(session, id, selected = vals[[id]])
        }
      }

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
            updateVirtualSelect(
              inputId = id,
              session = session,
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
          updateVirtualSelect(
            inputId = "nj_root_isolate",
            session = session,
            choices = c("Automatic", tips),
            selected = root
          )
        }
        n_tip <- length(tips)
        if (n_tip >= 3 && !is.null(vals$nj_parentnode)) {
          n_node <- if (identical(algo(), "UPGMA")) n_tip - 1L else n_tip - 2L
          nodes <- as.character(seq.int(n_tip + 1L, n_tip + n_node))
          updateVirtualSelect(
            inputId = "nj_parentnode",
            session = session,
            choices = nodes,
            selected = intersect(vals$nj_parentnode, nodes)
          )
        }
      }

      # Both come back from JSON as data.frames rather than lists of lists —
      # normalise before storing.
      layers <- normalize_layers(vals$.layers, LAYER_DEFAULTS, "tree")
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
        # A panel saved before ids existed (or one whose element type is no
        # longer offered) gets a fresh one — the edit/delete buttons address a
        # panel by id, and NA cannot tell two such panels apart.
        heatmaps <- lapply(seq_along(heatmaps), function(i) {
          h <- heatmaps[[i]]
          if (is.na(h$id %||% NA)) {
            h$id <- paste0("H", i)
          }
          h
        })
        nj_heatmaps(heatmaps)
        nj_heatmap_layer_seq(length(heatmaps))
      }
    }

    # Rebuild mapping layers from a pre-rewrite snapshot's flat keys. Each of
    # the three old switch/variable/scale triples becomes one layer on the
    # aesthetic it used to drive, and the tile strips become tile layers, so a
    # reopened Analysis draws what it drew when it was saved.
    .migrate_legacy_mapping <- function(vals) {
      legacy <- list(
        list(
          show = "nj_mapping_show",
          field = "nj_color_mapping",
          palette = "nj_tiplab_scale",
          aesthetic = "tiplab_color"
        ),
        list(
          show = "nj_tipcolor_mapping_show",
          field = "nj_tipcolor_mapping",
          palette = "nj_tippoint_scale",
          aesthetic = "tippoint_color"
        ),
        list(
          show = "nj_tipshape_mapping_show",
          field = "nj_tipshape_mapping",
          palette = NULL,
          aesthetic = "tippoint_shape"
        )
      )
      out <- list()
      prof <- profiles()
      add <- function(field, aesthetic, palette) {
        row <- profile_for(prof, field)
        if (is.null(row) || !isTRUE(row$groupable)) {
          return()
        }
        layer <- assign_mapping_layer(
          row,
          out,
          id = paste0("L", length(out) + 1L)
        )
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
      for (tile in normalize_layer_records(
        vals$.tiles,
        list(
          show = FALSE,
          variable = NA_character_,
          scale = "viridis"
        )
      ) %||%
        list()) {
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
      canvas <- plot_canvas()
      save_tree_plot(
        tree_plot_built(),
        file,
        "png",
        canvas$aspect,
        width = canvas$canvas_in,
        dpi = max(24, round(w / canvas$canvas_in))
      )
    }

    list(
      snapshot = snapshot,
      restore = restore,
      save_thumb = save_thumb,
      request_thumb = NULL,
      thumb_data = NULL,
      export = export
    )
  })
}
