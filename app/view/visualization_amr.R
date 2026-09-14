# app/view/visualization_amr.R
#
# Antimicrobial-resistance screening visualization submodule. Owns its own
# control panel, the plot render, and all AMR-specific reactive state. Mounted
# by app/view/visualization_plot.R as one plot tab's engine; the shared Generate
# button, plot type and per-isolate metadata are forwarded in as reactives.
#
# Like the Map and the Epi curve this engine computes no pairwise distances, so
# it takes local isolates only and ignores na_handling: an AMR screen exists for
# isolates typed in *this* database, and a staged peer's imported profile table
# carries allele identity and nothing else.
#
# Two views, chosen with one select (see PLOT_MODES):
#   * Gene heatmap    - isolates x genes, the successor to the ComplexHeatmap
#                       on the master branch. Cells carry AMRFinderPlus's own
#                       per-gene call confidence (see amr_method_confidence()
#                       in amr_plot.R), not plain presence/absence. Columns
#                       grouped by element type or drug class, or clustered.
#   * Prevalence      - ranked bars of how many isolates carry each gene or
#                       class. The view that survives a screen reporting several
#                       hundred genes.
#
# Both arrive from app/logic/amr_plot.R as ggplot objects, so - exactly as
# in the Epi engine - there is one renderImage, one download handler and one
# thumbnail function rather than a device block per format (which is what the
# master implementation had). Binning and filtering are cheap, so once Generate
# has been pressed the plot tracks the controls live; only clustering is
# expensive, and it sits behind its own matrix reactive so a purely cosmetic
# control does not re-run it.

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
    update_switch,
  ],
  rlang[`%||%`],
  shiny,
  shinyWidgets[
    pickerInput,
    pickerOptions,
    radioGroupButtons,
    updatePickerInput,
    updateRadioGroupButtons,
    updateVirtualSelect,
    virtualSelectInput
  ],
  stats[setNames],
)
box::use(
  app / logic / amr_plot,
  app / logic / date_bins[bin_date_values],
  app / logic / db_events,
  app /
    logic /
    field_profile[
      field_profiles_of = field_profiles,
      profile_for,
      scale_categories_for
    ],
  app / logic / functions[render_info],
  app /
    logic /
    mapping_engine[
      aesthetic_block_reason,
      assign_mapping_layer,
      granularity_profile,
      is_date_profile,
      max_layers,
      rebalance_layers,
      set_layer_granularity
    ],
  app / logic / viz_export[canvas_export, canvas_image, render_canvas_png],
  app /
    logic /
    viz_fit[ASPECT_MAX, TEXT_SIZE_DEFAULT, legibility_note, text_scale_percent],
  app /
    logic /
    viz_helpers[
      apply_controls,
      collect_input_snapshot,
      field_select,
      granularity_select,
      control_families,
      fit_hint,
      layer_action_btn,
      on_confirmed_reset,
      reset_button_row,
      scale_select,
      settled_inputs,
      suitable_scale_categories,
      text_size_slider,
      update_field_select,
      update_scale_select,
      viz_color,
      zoom_view_buttons
    ],
  app /
    logic /
    viz_layers[
      drop_layer,
      find_layer,
      layer_cards,
      layer_defaults,
      layer_has_field,
      layer_id_source,
      normalize_layers
    ],
)

# The two views. "heatmap" is the default because it is the one that answers
# the question the screen was run for: which isolate carries what.
PLOT_MODES <- c(
  `Gene heatmap` = "heatmap",
  Prevalence = "prevalence"
)
PLOT_MODE_DEFAULT <- "heatmap"

# How the heatmap's columns are arranged *inside* one element-type panel:
# clustered across the whole panel, like "Cluster isolates" does for rows, or
# - when they are not - grouped by drug class, the one arrangement that means
# something on every view this control is shown for. Grouped is not the same
# as unclustered: genes still cluster, just one drug class at a time, so a
# block's own internal order is never arbitrary either. There is no third
# position any more: "Element type" went the same way it did for the row axis
# (resistance, virulence and stress genes are now always drawn as separate
# panels side by side, so grouping by it would have been a control that
# changed nothing), and the old "None" (columns left in their natural,
# unarranged order) is gone too — see amr_cluster_cols and .column_layout in
# amr_plot.R.
COLUMN_GROUPING_DEFAULT <- "class"

# What a pre-restructure snapshot's amr_column_grouping ("class"/"cluster"/
# "none", plus the older "element") means for the switch that replaced it —
# used only by restore() below, translating a saved value before it falls
# through to the ordinary switch restore.
.legacy_cluster_cols <- function(saved) {
  identical(saved, "cluster")
}

# Which of the two drug-class vocabularies the gene heatmap groups and colours
# by. The vocabularies are in amr_plot.R beside the constant itself; this tab
# defaults to AMRFinderPlus rather than the curated rollup amr_plot.R leads with.
CLASS_VOCABULARIES <- amr_plot$AMR_CLASS_VOCABULARIES
CLASS_VOCABULARY_DEFAULT <- "amrfinder"

LEVELS <- c(Genes = "gene", `Drug classes` = "class")
LEVEL_DEFAULT <- "gene"

# Both axes open on Jaccard and Ward's. The argument, and the silhouette
# measurements behind it, are in amr_plot.R beside the constants themselves.
CLUSTER_DISTANCE_DEFAULT <- amr_plot$AMR_CLUSTER_DISTANCE_DEFAULT
CLUSTER_METHOD_DEFAULT <- amr_plot$AMR_CLUSTER_METHOD_DEFAULT

TOP_N_DEFAULT <- 30L

# What the aspect slider holds before any data is loaded. Generate replaces it
# with amr_auto_layout()'s answer for the matrix actually being drawn; this is
# only what the control shows until then.
ASPECT_DEFAULT <- 0.9

# The aspect slider's floor and grid. The floor is the prevalence chart's, the
# lowest either view is fitted to.
ASPECT_MIN <- amr_plot$AMR_PREVALENCE_MIN
ASPECT_STEP <- 0.05

# A ratio put on the aspect slider's own grid. The slider snaps whatever it is
# sent, and a snapped echo that differed from the value sent would read as the
# reader's own drag and stop the view following its fit.
.snap_aspect <- function(value) {
  snapped <- ASPECT_MIN + round((value - ASPECT_MIN) / ASPECT_STEP) * ASPECT_STEP
  round(min(max(snapped, ASPECT_MIN), ASPECT_MAX), 2)
}

# One depth for both dendrograms, in centimetres. They are read together and
# there was never a reason to give them different depths; 0 draws neither,
# keeping the clustering's row and column *order* while dropping the trees.
DEND_DEFAULT <- 1

# The medium this engine maps variables onto, in mapping_engine.R's terms: a
# repeatable colour strip beside the rows.
MEDIUM <- "amr"

LAYER_DEFAULTS <- layer_defaults(MEDIUM)

PRESENT_COLOR_DEFAULT <- "#000000"
STRONG_COLOR_DEFAULT <- "#8C6E3D"
PARTIAL_COLOR_DEFAULT <- "#E5C494"
ABSENT_COLOR_DEFAULT <- "#EFEFEF"
DEND_COLOR_DEFAULT <- "#000000"
TEXT_COLOR_DEFAULT <- "#000000"
BACKGROUND_DEFAULT <- "#FFFFFF"

# The element-type panels the gene heatmap can draw, as the Colors tab's
# per-panel confidence-colour modal names them. Each present panel gets one
# "Edit colours" button; its choices are stored in amr_element_colors(),
# keyed by these labels. "scale" is the starting colour mode — each panel on
# its element type's own sequential ramp (amr_plot$AMR_ELEMENT_HEAT_SCALES),
# the same arrangement the Tree's heatmap panels default to.
CONFIDENCE_ELEMENT_LABELS <- c(
  names(amr_plot$AMR_ELEMENT_TYPES),
  amr_plot$AMR_UNCLASSIFIED
)
CONFIDENCE_MODE_DEFAULT <- "scale"

CLASS_SCALE_DEFAULT <- "Set2"
BAR_SCALE_DEFAULT <- "Dark2"

# Only shown while the element-type / hit-quality filters actually bite: they
# read `amr_results`, which backs the gene heatmap, the gene-level prevalence
# bars, and — under the AMRFinderPlus vocabulary — the drug-class prevalence
# count too, since that reads amr_results the same way rather than the
# abritamr rollup.
COND_HITS <- paste(
  "input.amr_mode == 'heatmap' ||",
  "(input.amr_mode == 'prevalence' && input.amr_level == 'gene') ||",
  "(input.amr_mode == 'prevalence' && input.amr_level == 'class' &&",
  "input.amr_class_vocab == 'amrfinder')"
)
# The abritamr-only call-section filter: meaningless once the drug-class
# count reads amr_results (AMRFinderPlus vocabulary) instead of the rollup.
COND_SECTIONS <- paste(
  "input.amr_mode == 'prevalence' && input.amr_level == 'class' &&",
  "input.amr_class_vocab == 'rollup'"
)
# Which vocabulary the drug classes are filed under: the gene heatmap's own
# column grouping, and — when Prevalence counts by class — its own items.
COND_CLASS_VOCAB <- paste(
  "input.amr_mode == 'heatmap' ||",
  "(input.amr_mode == 'prevalence' && input.amr_level == 'class')"
)
COND_HEATMAPS <- "input.amr_mode != 'prevalence'"

# --- The sidebar's controls, by widget family --------------------------------
#
# Every control this panel renders, filed under the family whose update path
# restores it, and the value each is declared with. Together they drive both
# "Reset settings" and the Analysis restore -- see control_families() and the
# reset reference at the top of viz_helpers.R.
#
# `amr_layer_add` is absent because it clears itself the moment it is picked
# from. `amr_genes` is filed but carries no default: it is rendered server-side
# from the database's own gene list, so a reset rebuilds it (genes_rebuild)
# rather than sending it a value.
AMR_CONTROLS <- control_families(
  switches = c(
    "amr_show_row_names",
    "amr_show_col_names",
    "amr_show_element_names"
  ),
  pickers = c(
    "amr_mode",
    "amr_class_vocab",
    "amr_elements",
    "amr_sections",
    "amr_class_scale",
    "amr_bar_scale",
    "amr_cluster_distance",
    "amr_cluster_method"
  ),
  virtual_selects = "amr_genes",
  sliders = c(
    "amr_top_n",
    "amr_min_identity",
    "amr_min_coverage",
    "amr_aspect_ratio",
    "amr_text_size",
    "amr_dend_size"
  ),
  radio_groups = c(
    "amr_level",
    "amr_cluster_rows",
    "amr_cluster_cols",
    "zoom_view"
  ),
  colors = c("amr_dend_color", "amr_text_color", "amr_background_color")
)

# Every catalogued control's coded default, taken from the named constant where
# there is one so the reset and the widget cannot disagree.
#
# The cluster and zoom values are strings because radioGroupButtons'
# choiceValues are: the browser reports "TRUE", not TRUE, and
# updateRadioGroupButtons matches on the value as written.
AMR_CONTROL_DEFAULTS <- list(
  amr_mode = PLOT_MODE_DEFAULT,
  amr_level = LEVEL_DEFAULT,
  amr_top_n = TOP_N_DEFAULT,
  amr_class_vocab = CLASS_VOCABULARY_DEFAULT,
  amr_elements = unname(amr_plot$AMR_ELEMENT_TYPES),
  amr_sections = unname(amr_plot$AMR_SECTIONS),
  amr_min_identity = 0,
  amr_min_coverage = 0,
  amr_aspect_ratio = ASPECT_DEFAULT,
  amr_text_size = TEXT_SIZE_DEFAULT,
  amr_show_row_names = FALSE,
  amr_show_col_names = TRUE,
  amr_show_element_names = TRUE,
  amr_cluster_rows = "TRUE",
  amr_cluster_cols = "TRUE",
  amr_class_scale = CLASS_SCALE_DEFAULT,
  amr_cluster_distance = CLUSTER_DISTANCE_DEFAULT,
  amr_cluster_method = CLUSTER_METHOD_DEFAULT,
  amr_dend_size = DEND_DEFAULT,
  amr_bar_scale = BAR_SCALE_DEFAULT,
  amr_dend_color = DEND_COLOR_DEFAULT,
  amr_text_color = TEXT_COLOR_DEFAULT,
  amr_background_color = BACKGROUND_DEFAULT,
  zoom_view = "FALSE"
)

# --- AMR control tabs --------------------------------------------------------

# Which control tabs each view has anything to say in.
#
# Prevalence draws ranked bars off a count: it has no dendrogram to tune and no
# row strips to map, so two of the five tabs would open on nothing but a note
# explaining why they are empty. Its Layout tab stays, for the aspect ratio and
# the text size. Hiding the rest is the arrangement the Map makes for its modes.
#
# Keyed on each nav_panel's explicit `value`, never on its title: a tab renamed
# for the reader would otherwise stop matching here and silently stick — left
# visible in a view with nothing to put in it, or hidden for good.
TABS_BY_MODE <- list(
  heatmap = c("data", "layout", "clustering", "mapping", "colors"),
  prevalence = c("data", "layout", "colors")
)
ALL_TABS <- c("data", "layout", "clustering", "mapping", "colors")

# Show only the tabs the current view uses, and move off one that is being
# hidden. Done in the browser rather than with bslib's nav_show()/nav_hide() for
# the reason the Map gives at the same point: a server-side toggle can throw
# "Node cannot be found" into the middle of a render batch.
.mode_tabs_script <- function(ns) {
  js_arr <- function(x) paste0("[", paste0("'", x, "'", collapse = ","), "]")
  modes <- unname(PLOT_MODES)
  by_mode <- paste0(
    "{",
    paste0(
      modes,
      ":",
      vapply(modes, function(m) js_arr(TABS_BY_MODE[[m]]), character(1)),
      collapse = ","
    ),
    "}"
  )
  shiny$tags$script(shiny$HTML(paste0(
    "(function(){",
    "var modeSel='#",
    ns("amr_mode"),
    "';",
    "var tabsByMode=",
    by_mode,
    ";",
    "var allTabs=",
    js_arr(ALL_TABS),
    ";",
    "function apply(){",
    "var modeEl=document.querySelector(modeSel);if(!modeEl)return;",
    "var wrap=modeEl.closest('.viz-nav-wrap');if(!wrap)return;",
    "var vis=(tabsByMode[modeEl.value]||allTabs).slice();",
    "allTabs.forEach(function(v){",
    "var link=wrap.querySelector('.nav-link[data-value='+JSON.stringify(v)+']');",
    "if(link){var li=link.closest('.nav-item');",
    "if(li){li.style.display=(vis.indexOf(v)>=0)?'':'none';}}",
    "});",
    "var active=wrap.querySelector('.nav-link.active');",
    "if(active&&vis.indexOf(active.getAttribute('data-value'))<0){",
    "var f=wrap.querySelector('.nav-link[data-value='+JSON.stringify(vis[0])+']');",
    "if(f){f.click();}",
    "}",
    "}",
    "$(document).on('change',modeSel,apply);",
    "var n=0,t=setInterval(function(){n++;",
    "if(document.querySelector(modeSel)){apply();}",
    "if(n>40){clearInterval(t);}},300);",
    "})();"
  )))
}

amr_controls <- function(ns) {
  shiny$tagList(
    navset_tab(
      # Data -------------------------------------------------------------------
      nav_panel(
        "Data",
        value = "data",
        icon = shiny$icon("table-cells"),
        # What the bars count, which decides which of the two filters below
        # applies — the gene level reads `amr_results`, the class level either
        # the abritamr rollup or `amr_results` too, depending on the
        # vocabulary picker just below (see COND_CLASS_VOCAB).
        shiny$conditionalPanel(
          condition = "input.amr_mode == 'prevalence'",
          ns = ns,
          radioGroupButtons(
            ns("amr_level"),
            "Count by",
            choices = LEVELS,
            selected = LEVEL_DEFAULT,
            justified = TRUE,
            size = "sm"
          ),
          shiny$sliderInput(
            ns("amr_top_n"),
            "Show top",
            min = 5,
            max = 100,
            value = TOP_N_DEFAULT,
            step = 5,
            ticks = FALSE
          )
        ),
        # Which vocabulary the drug classes are filed under: the gene
        # heatmap's own column grouping, and — while Prevalence counts by
        # class — its own items. Kept as one control shared by both, so a
        # reader who switches it on one sees the same classes on the other.
        shiny$conditionalPanel(
          condition = COND_CLASS_VOCAB,
          ns = ns,
          pickerInput(
            ns("amr_class_vocab"),
            "Drug classes from",
            choices = CLASS_VOCABULARIES,
            selected = CLASS_VOCABULARY_DEFAULT
          )
        ),
        shiny$conditionalPanel(
          condition = COND_HITS,
          ns = ns,
          pickerInput(
            ns("amr_elements"),
            "Element types",
            choices = amr_plot$AMR_ELEMENT_TYPES,
            selected = unname(amr_plot$AMR_ELEMENT_TYPES),
            multiple = TRUE,
            width = "100%",
            options = pickerOptions(
              actionsBox = TRUE,
              title = "None",
              selectedTextFormat = "count > 2",
              countSelectedText = "{0} types",
              container = "body"
            )
          ),
          # Rendered server-side: the choices are this database's detected
          # genes, grouped by drug class, so the picker is built once they
          # are known rather than declared empty and back-filled — the
          # selection it carries is "every gene", which cannot be expressed
          # before the gene list exists. Same reasoning as the Epi engine's
          # stratify picker.
          shiny$uiOutput(ns("genes_ui")),
          accordion(
            open = FALSE,
            accordion_panel(
              "Minimum %",
              icon = shiny$icon("bars-staggered"),
              # AMRFinderPlus reports partial and low-identity hits alongside
              # confident ones and `amr_results` keeps both percentages, so the
              # reader can set the bar. Point mutations report neither and are
              # never filtered out by these (see filter_amr_hits). Bounds are
              # fit to this screen's own reported range server-side (see
              # fit_threshold_bounds) rather than declared as a flat 0-100 —
              # these are placeholders until that fit runs.
              tooltip(
                shiny$sliderInput(
                  ns("amr_min_identity"),
                  "Minimum % identity",
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 1,
                  ticks = FALSE
                ),
                paste(
                  "How closely the hit matches the reference gene's sequence.",
                  "Range fits what this screen actually reported."
                )
              ),
              tooltip(
                shiny$sliderInput(
                  ns("amr_min_coverage"),
                  "Minimum % coverage",
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 1,
                  ticks = FALSE
                ),
                paste(
                  "How much of the reference gene's length the hit spans.",
                  "Range fits what this screen actually reported."
                )
              )
            )
          )
        ),
        shiny$conditionalPanel(
          condition = COND_SECTIONS,
          ns = ns,
          pickerInput(
            ns("amr_sections"),
            "Call sections",
            choices = amr_plot$AMR_SECTIONS,
            selected = unname(amr_plot$AMR_SECTIONS),
            multiple = TRUE,
            width = "100%",
            options = pickerOptions(
              actionsBox = TRUE,
              title = "None",
              selectedTextFormat = "count > 2",
              countSelectedText = "{0} sections",
              container = "body"
            )
          ),
          shiny$helpText(
            class = "amr-help",
            "Matches are confident calls, partials incomplete ones.",
            "Virulence groups are reported separately from resistance."
          )
        )
      ),
      # Layout -----------------------------------------------------------------
      #
      # The aspect ratio and the text size apply to both views; the label
      # switches name the heatmap's rows and columns and are shown for it only.
      nav_panel(
        "Layout",
        value = "layout",
        icon = shiny$icon("sliders"),
        # Height per isolate, which is the one thing about the shape that is a
        # judgement rather than a fit: Generate solves it from the matrix so the
        # rows come out legible, but how tall a figure is worth having is the
        # reader's call. Every size that hangs off the row pitch — the isolate
        # labels above all — is re-solved against whatever is set here, so a
        # taller figure means larger labels rather than more whitespace.
        #
        # Everything else about the matrix's proportions — the label sizes, the
        # block-title size and the cell borders — is fitted to the shape of the
        # data by amr_plot$amr_auto_layout(), exactly as the Tree fits its own.
        # The six sliders that used to be here each had a right answer the
        # module could work out, and getting one of them wrong made the plot
        # unreadable in a way the reader then had to diagnose.
        shiny$sliderInput(
          ns("amr_aspect_ratio"),
          "Aspect ratio",
          min = ASPECT_MIN,
          max = ASPECT_MAX,
          value = ASPECT_DEFAULT,
          step = ASPECT_STEP,
          ticks = FALSE
        ),
        # Every piece of type on the plot: isolate and gene names, class and
        # element titles, strip names, bar names, axes and the legend. A bias on
        # the fit, not a size - see app/logic/viz_fit.R.
        text_size_slider(ns, "amr_text_size"),
        # A bar name is never left off (see amr_prevalence_layout()), so what is
        # explained here is a chart whose names had to be set too small.
        shiny$conditionalPanel(
          condition = "input.amr_mode == 'prevalence'",
          ns = ns,
          fit_hint(
            ns,
            "amr_bar_names_hint",
            "Too many bars to name legibly. Show fewer or raise the aspect ratio."
          )
        ),
        # All three read off the heatmap alone: not shown for Prevalence, which
        # has no isolate row or per-gene column to name.
        #
        # Whether the drug class itself is drawn as text or as a colour strip
        # is not a switch here at all — see amr_cluster_cols in the
        # Clustering tab, which decides that on its own (see .gene_panel's
        # top_annotation). A separate on/off switch here used to fight that
        # mechanism for the same row.
        shiny$conditionalPanel(
          condition = COND_HEATMAPS,
          ns = ns,
          input_switch(ns("amr_show_row_names"), "Show isolate names", FALSE),
          # A switch that is on but draws nothing is explained rather than left
          # to read as a bug: the fit leaves a label off where no legible size
          # fits the room it has (see amr_auto_layout()).
          fit_hint(
            ns,
            "amr_row_names_hint",
            "Too many isolates to name legibly. Try a taller aspect ratio."
          ),
          input_switch(ns("amr_show_col_names"), "Show gene names", TRUE),
          fit_hint(
            ns,
            "amr_col_names_hint",
            "Too many genes to name legibly. Filter the genes shown."
          ),
          # The element type (Resistance/Virulence/Stress) a panel's columns
          # belong to — always one label per panel regardless of how its
          # columns are grouped, drawn under the matrix rather than the drug
          # class titles above it so the two never compete for the same row.
          # See amr_elements in the Data tab for what decides which panels
          # exist, and column_title_side in .gene_panel for where this draws.
          input_switch(
            ns("amr_show_element_names"),
            "Show element-type labels",
            TRUE
          ),
          fit_hint(
            ns,
            "amr_element_names_hint",
            "A panel is too narrow to carry its element-type label."
          )
        )
      ),
      # Clustering -------------------------------------------------------------
      nav_panel(
        "Clustering",
        value = "clustering",
        icon = shiny$icon("sitemap"),
        radioGroupButtons(
          ns("amr_cluster_rows"),
          "Isolates",
          choiceNames = c("Cluster Isolates", "No Isolate Cluster"),
          choiceValues = c(TRUE, FALSE),
          selected = TRUE,
          justified = TRUE,
          size = "sm",
          width = "100%"
        ),
        # The gene axis is a different question — how alike two *genes* are
        # across the isolates, rather than two isolates across the genes — but
        # not a different enough one to earn its own distance and linkage
        # pickers: every reader who tuned one axis wanted the same pair on the
        # other. "Cluster Class" keeps the drug-class grouping rather than an
        # unarranged "None" — but genes still cluster within their own class
        # block there, so the pair below stays live for that case too. See
        # amr_cluster_cols in .column_layout.
        radioGroupButtons(
          ns("amr_cluster_cols"),
          "Genes",
          choiceNames = c("Cluster Class", "Cluster All"),
          choiceValues = c(FALSE, TRUE),
          selected = TRUE,
          justified = TRUE,
          size = "sm",
          width = "100%"
        ),
        # Only "Cluster All" draws the drug class as a colour strip rather than
        # as text block titles (see .gene_panel's top_annotation), so this scale
        # has nothing to style under "Cluster Class". Kept visible but disabled
        # by the sync observer below rather than hidden, so the reader sees it
        # exists before committing to the grouping.
        scale_select(ns, "amr_class_scale", categories = "Qualitative"),
        shiny$conditionalPanel(
          condition = paste(
            "input.amr_cluster_rows == 'TRUE' ||",
            "input.amr_cluster_cols == 'TRUE' ||",
            "input.amr_mode == 'heatmap'"
          ),
          ns = ns,
          pickerInput(
            ns("amr_cluster_distance"),
            "Distance",
            choices = amr_plot$AMR_CLUSTER_DISTANCES,
            selected = CLUSTER_DISTANCE_DEFAULT
          ),
          pickerInput(
            ns("amr_cluster_method"),
            "Linkage",
            choices = amr_plot$AMR_CLUSTER_METHODS,
            selected = CLUSTER_METHOD_DEFAULT
          )
        ),
        shiny$sliderInput(
          ns("amr_dend_size"),
          "Dendrogram depth (cm)",
          min = 0,
          max = 6,
          value = DEND_DEFAULT,
          step = 0.5,
          ticks = FALSE
        )
      ),
      # Mapping -------------------------------------------------------------
      nav_panel(
        "Mapping",
        value = "mapping",
        icon = shiny$icon("map-pin"),
        # The same arrangement as the Tree and the MST: pick a *variable* and
        # app/logic/mapping_engine.R decides the rest. Every variable this
        # database holds is offered, each carrying its own value count and
        # type, and each mapping becomes one colour strip beside the rows.
        field_select(ns, "amr_layer_add", "Map a variable"),
        shiny$uiOutput(ns("amr_layers_ui"))
      ),
      # Colors -------------------------------------------------------------
      #
      # The one tab every view shows, so its rows keep their own conditions.
      nav_panel(
        "Colors",
        value = "colors",
        icon = shiny$icon("palette"),
        shiny$conditionalPanel(
          condition = "input.amr_mode == 'prevalence'",
          ns = ns,
          scale_select(ns, "amr_bar_scale", categories = "Qualitative")
        ),
        # The gene-call confidence tiers are coloured per element-type panel,
        # not once for the whole screen: one "Edit colours" button per panel the
        # current screen carries, each opening a modal with the same colour-
        # scale / pick-each choice the Tree's heatmap panels use (see
        # amr_confidence_colors_ui and the modal in the server). The dendrogram,
        # text and background pickers below stay flat — they belong to the plot,
        # not to a panel.
        shiny$conditionalPanel(
          condition = COND_HEATMAPS,
          ns = ns,
          shiny$tags$label("Gene call confidence", class = "control-label"),
          shiny$uiOutput(ns("amr_confidence_colors_ui"))
        ),
        shiny$div(
          class = "viz-color-grid",
          shiny$conditionalPanel(
            condition = COND_HEATMAPS,
            ns = ns,
            viz_color(ns, "amr_dend_color", "Dendrogram", DEND_COLOR_DEFAULT)
          ),
          viz_color(ns, "amr_text_color", "Text", TEXT_COLOR_DEFAULT),
          viz_color(
            ns,
            "amr_background_color",
            "Background",
            BACKGROUND_DEFAULT
          )
        )
      )
    ),
    # Pinned under the tabs rather than filed inside one, as the Map pins its
    # own mode picker: it is the control that decides which tabs there are, so
    # it cannot live in a tab that one of its own values hides.
    shiny$div(
      class = "viz-mode-dropup reset-buttons",
      pickerInput(
        ns("amr_mode"),
        NULL,
        choices = PLOT_MODES,
        selected = PLOT_MODE_DEFAULT
      )
    ),
    zoom_view_buttons(ns),
    reset_button_row(
      ns,
      paste(
        "Re-solve the aspect ratio, the text sizes, which labels fit and the",
        "filter ranges for the matrix currently drawn. Colours and mappings",
        "are kept."
      )
    ),
    .mode_tabs_script(ns)
  )
}

#' @export
# `options_ui` is accepted for one signature across all engines; this one has no
# distance-computation controls, so the tab never passes any.
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
          amr_controls(ns)
        )
      )
    ),
    shinyjs::useShinyjs(),
    waiter::useWaiter(),
    # Loading overlay: shown when Generate (parent namespace) is clicked, scoped
    # to this engine's own stage id, and cleared when the plot re-renders — the
    # Epi engine's shiny:value variant, since this is an image output too.
    shiny$tags$script(
      shiny$HTML(
        paste0(
          "(function(){",
          "var gen='",
          generate_id,
          "';var out='",
          ns("amr_plot"),
          "';var stage='",
          ns("plot_stage"),
          "';var timer;",
          "function set(on){var s=document.getElementById(stage);if(!s)return;",
          # Ignore Generate clicks while this engine's panel is hidden (another
          # plot tab is active) — offsetParent is null when display:none.
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
  # Accepted for a uniform argument bundle from the plot tab; the AMR views plot
  # straight from the already-filtered viz_metadata(), so they need no separate
  # handling.
  selected_isolates = shiny$reactive(NULL),
  # Accepted for the same reason; no pairwise distances are computed over
  # allele calls here, so missing-value handling never applies (same as the Map
  # and the Epi curve).
  na_handling = shiny$reactive("ignore_na"),
  generate = shiny$reactive(0L),
  plot_type = shiny$reactive("MST")
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Whether Generate has been pressed for this engine. Once TRUE the plot
    # tracks the controls live — filtering and recolouring a matrix is
    # microseconds, and making them wait for another Generate press made them
    # look broken (the same reasoning as the Epi curve's epi_data()).
    generated <- shiny$reactiveVal(FALSE)

    # Bumped to rebuild the server-rendered gene picker (Reset settings).
    genes_rebuild <- shiny$reactiveVal(0L)
    # TRUE for exactly one rebuild: a reset must force the picker back to its
    # default, whereas a plain data-driven re-render keeps the current choice so
    # a deliberate selection sticks. Consumed on read — see the Epi engine's
    # stratify_force_default for the full reasoning.
    genes_force_default <- shiny$reactiveVal(FALSE)

    # The annotation strips, in draw order. Written only by explicit user action
    # (add, edit, delete) and by a restore — never by a re-render, which is what
    # the single annotation picker they replaced could not promise.
    amr_layers <- shiny$reactiveVal(list())
    # Ids are never reused: a stale card's button would otherwise address the
    # layer that replaced it.
    amr_layer_seq <- shiny$reactiveVal(0L)
    next_layer_id <- layer_id_source(amr_layer_seq)

    # Per-element-type gene-call confidence colours, keyed by the panel labels
    # in CONFIDENCE_ELEMENT_LABELS. Each entry is a list of `color_mode`
    # ("scale" or "tiers"), `heat_scale`, and the four tier hex colours. Written
    # only by the colour modal's Apply and by a restore; a label with no entry
    # falls back to element_cfg()'s coded defaults. Handed to the builder as
    # `element_colors` (see heatmap_opts and amr_plot$.panel_confidence_palette).
    amr_element_colors <- shiny$reactiveVal(list())

    # The stored config for one panel label, merged over its defaults so the
    # builder and the modal always see all five fields. "scale" mode starts on
    # the element type's own sequential ramp; "tiers" mode on the shared
    # confidence-tier defaults.
    element_cfg <- function(label) {
      stored <- amr_element_colors()[[label]] %||% list()
      list(
        color_mode = stored$color_mode %||% CONFIDENCE_MODE_DEFAULT,
        heat_scale = stored$heat_scale %||%
          amr_plot$amr_element_heat_scale(label),
        present_color = stored$present_color %||% PRESENT_COLOR_DEFAULT,
        strong_color = stored$strong_color %||% STRONG_COLOR_DEFAULT,
        partial_color = stored$partial_color %||% PARTIAL_COLOR_DEFAULT,
        absent_color = stored$absent_color %||% ABSENT_COLOR_DEFAULT
      )
    }

    # The element-type panels the current screen actually contains, by display
    # label — what the Colors tab draws a button for, and the panels the modal
    # can edit.
    present_elements <- shiny$reactive({
      mat <- presence_mat()
      if (!ncol(mat)) {
        return(character(0))
      }
      amr_plot$amr_element_blocks(mat)$titles
    })

    mode <- function() input$amr_mode %||% PLOT_MODE_DEFAULT

    # The sliders, read once the reader lets go. Each one either re-derives the
    # presence matrix (the two thresholds), re-counts the bars (top n) or
    # redraws a figure that takes seconds at a thousand isolates, so a drag
    # read live queued one rebuild per value it passed through.
    settled <- settled_inputs(
      input,
      c(
        "amr_min_identity",
        "amr_min_coverage",
        "amr_top_n",
        "amr_aspect_ratio",
        "amr_text_size",
        "amr_dend_size"
      )
    )

    # Which curation files a gene under a drug class, for both the heatmap's
    # column blocks and the gene picker's headings — they have to agree, or a
    # reader searches under a heading the plot does not draw.
    class_vocab <- function() {
      input$amr_class_vocab %||% CLASS_VOCABULARY_DEFAULT
    }

    # --- data ---------------------------------------------------------------

    amr_hits <- shiny$reactive({
      db_events$depend(db_rev, "amr", "isolates")
      shiny$req(db_path())
      amr_plot$load_amr_hits(db_path())
    })

    amr_sections <- shiny$reactive({
      db_events$depend(db_rev, "amr", "isolates")
      shiny$req(db_path())
      amr_plot$load_amr_sections(db_path())
    })

    # Fits both threshold sliders' min/max to what this screen actually
    # reported, so a database whose weakest hit is 92% identity does not offer
    # 90 wasted degrees of a slider that would filter nothing. Value moves to
    # the new floor too — that is the "no filter" position for the new range,
    # matching what 0 meant for the old flat 0-100 one. Falls back to 0-100
    # when there is no numeric data to fit (no screen loaded, or every hit's
    # metric is NA, e.g. point mutations only).
    fit_threshold_bounds <- function() {
      hits <- amr_hits()
      fit_one <- function(id, values) {
        b <- amr_plot$amr_threshold_bounds(values)
        shiny$updateSliderInput(
          session,
          id,
          min = b$min,
          max = b$max,
          value = b$value
        )
      }
      fit_one("amr_min_identity", hits$pct_identity)
      fit_one("amr_min_coverage", hits$pct_coverage)
    }

    # Opens the element-type picker on as many panels as fit under the gene cap
    # (see amr_plot$amr_capped_element_types()): all three panels of a large
    # screen are well over a hundred columns, too narrow for any gene name or
    # drug-class title. Seeded when a screen loads and on reset only; after
    # that the choice is the reader's.
    fit_element_types <- function() {
      updatePickerInput(
        session,
        "amr_elements",
        selected = amr_plot$amr_capped_element_types(shiny$isolate(amr_hits()))
      )
    }

    shiny$observeEvent(
      amr_hits(),
      {
        fit_threshold_bounds()
        fit_element_types()
      },
      ignoreNULL = FALSE
    )

    # Fits the "Show top" slider's range to how many genes or drug classes
    # this screen (and the current vocabulary, at class level) actually
    # offers, rather than some flat 5-100 that neither shrinks for a small
    # demo database nor grows for one with several hundred genes. Kept as its
    # own observer, distinct from fit_threshold_bounds() above, because it has
    # to re-run on a "Count by"/vocabulary switch too, not only on a new
    # screen loading.
    fit_top_n_bounds <- function() {
      level <- input$amr_level %||% LEVEL_DEFAULT
      n_items <- if (identical(level, "class")) {
        if (identical(class_vocab(), "amrfinder")) {
          hits <- amr_hits()
          length(unique(
            amr_plot$amr_gene_meta(
              hits,
              unique(hits$gene_symbol),
              sections = NULL,
              vocabulary = "amrfinder"
            )$group
          ))
        } else {
          length(unique(amr_sections()$drug_class))
        }
      } else {
        length(unique(amr_hits()$gene_symbol))
      }
      # Isolated for the same reason as apply_scale_choices() below: this
      # answers to how many items there are to rank, and a slider it refits
      # reports its own value straight back — depended on, that echo would
      # re-enter the observer that caused it.
      b <- amr_plot$amr_top_n_bounds(
        n_items,
        default = TOP_N_DEFAULT,
        current = shiny$isolate(input$amr_top_n)
      )
      shiny$updateSliderInput(
        session,
        "amr_top_n",
        min = b$min,
        max = b$max,
        value = b$value,
        step = b$step
      )
    }

    shiny$observe({
      amr_hits()
      amr_sections()
      input$amr_level
      input$amr_class_vocab
      fit_top_n_bounds()
    })

    # The isolates this plot covers: whatever the tab's Selection panel left in
    # the metadata table. Local only — an AMR screen is a property of a genome
    # assembly typed here.
    isolates <- shiny$reactive({
      meta <- viz_metadata()
      shiny$req(meta, nrow(meta) > 0)
      meta$isolate
    })

    filtered_hits <- shiny$reactive({
      amr_plot$filter_amr_hits(
        amr_hits(),
        element_types = input$amr_elements,
        min_identity = settled$amr_min_identity() %||% 0,
        min_coverage = settled$amr_min_coverage() %||% 0
      )
    })

    # An empty gene selection means "every gene", so clearing the picker shows
    # the whole screen rather than an empty plot.
    selected_genes <- shiny$reactive({
      g <- input$amr_genes
      if (!length(g)) NULL else g
    })

    # The expensive reactive. Everything cosmetic reads the built plot below,
    # so moving a colour picker never re-clusters.
    presence_mat <- shiny$reactive({
      amr_plot$amr_presence_matrix(
        filtered_hits(),
        isolates(),
        selected_genes(),
        sections = amr_sections(),
        vocabulary = class_vocab()
      )
    })

    prevalence_df <- shiny$reactive({
      amr_plot$amr_prevalence(
        filtered_hits(),
        amr_sections(),
        isolates(),
        level = input$amr_level %||% LEVEL_DEFAULT,
        top_n = settled$amr_top_n() %||% TOP_N_DEFAULT,
        keep_sections = input$amr_sections,
        vocabulary = class_vocab()
      )
    })

    # The bar chart's fit at a given ratio (NULL fits one), standing in for
    # amr_auto_layout() in that mode: the heatmap fit has a matrix to solve
    # against and this has one bar per row.
    prevalence_layout <- function(aspect) {
      df <- prevalence_df()
      amr_plot$amr_prevalence_layout(
        df$item,
        df$group,
        amr_plot$AMR_CANVAS_IN,
        aspect = aspect,
        text_scale = text_scale_percent(text_size_mirror())
      )
    }

    # The bar chart as drawn, at the ratio in force.
    prevalence_fit <- shiny$reactive(prevalence_layout(aspect_mirror()))

    # --- controls fitted to the data ----------------------------------------

    # Every gene the screen reported, grouped by element type and drug class.
    # Built from the unfiltered hits deliberately: rebuilding the picker every
    # time the identity slider moves would drop the reader's selection under
    # them.
    output$genes_ui <- shiny$renderUI({
      # The vocabulary picker itself, not class_vocab()'s fallback, and ahead
      # of everything else: every input is NULL for the flush before the
      # browser reports it, and falling back there builds the whole list once
      # against the default only to build it again when the picker checks in a
      # moment later — several hundred genes grouped, sorted and re-bound twice
      # for nothing.
      vocabulary <- shiny$req(input$amr_class_vocab)
      render_info("visualization_amr genes_ui")
      genes_rebuild()
      choices <- amr_plot$amr_gene_choices(
        amr_hits(),
        amr_sections(),
        vocabulary
      )
      if (!length(choices)) {
        return(NULL)
      }
      all_genes <- unlist(choices, use.names = FALSE)
      prev <- shiny$isolate(input$amr_genes)
      force_default <- shiny$isolate(genes_force_default())
      if (force_default) {
        genes_force_default(FALSE)
      }
      keep <- if (!force_default) intersect(prev, all_genes) else character()
      virtualSelectInput(
        ns("amr_genes"),
        "Genes",
        choices = choices,
        selected = if (length(keep)) keep else all_genes,
        multiple = TRUE,
        search = TRUE,
        # Without this, the header checkbox takes every gene the screen found,
        # not just the ones the search term currently matches — surprising when
        # the dropdown is showing a filtered subset.
        selectAllOnlyVisible = TRUE,
        searchPlaceholderText = "Search genes ...",
        # An empty selection means "every gene" (see selected_genes()), so the
        # empty state names that rather than reading "None".
        placeholder = "All genes",
        optionsCount = 10,
        noOfDisplayValues = 2,
        # Not formals — these reach the widget config through `...`. A screen
        # reporting several hundred genes cannot list them in the toggle, so
        # past two it counts them instead.
        optionsSelectedText = "genes selected",
        optionSelectedText = "gene selected",
        allOptionsSelectedText = "All genes",
        # Every pick re-derives the presence matrix and re-clusters it, so a
        # multi-gene selection is batched to the dropdown's close rather than
        # costing one recompute per click.
        updateOn = "close",
        dropboxWrapper = "body",
        showDropboxAsPopup = TRUE,
        popupDropboxBreakpoint = "10000px",
        width = "100%"
      )
    })

    # Every column's profile, for the variable picker and the mapping engine.
    # `isolate` names every row uniquely; it labels the matrix, it never colours
    # it.
    profiles <- shiny$reactive({
      meta <- viz_metadata()
      shiny$req(meta)
      prof <- field_profiles() %||%
        field_profiles_of(
          meta,
          mlst_cols = attr(meta, "mlst_cols"),
          amr_cols = attr(meta, "amr_cols"),
          custom_cols = attr(meta, "custom_cols")
        )
      prof[prof$field != "isolate", , drop = FALSE]
    })

    # Refilled here rather than declared in the UI, because updateVirtualSelect()
    # has no `...` and so cannot re-set hasOptionDescription. Columns that cannot
    # group the isolates stay listed but disabled, with the reason in their
    # sub-text.
    shiny$observe({
      prof <- profiles()
      shiny$req(nrow(prof))
      update_field_select(session, "amr_layer_add", prof)
    })

    # Picking a variable adds a strip; the engine decides its palette from the
    # variable's own profile and from what the other strips already hold.
    shiny$observeEvent(input$amr_layer_add, {
      field <- input$amr_layer_add
      shiny$req(nzchar(field %||% ""))
      # Cleared straight away so the same variable can be re-picked after a
      # delete, and so the selection cannot re-fire on a later flush.
      updateVirtualSelect(
        inputId = "amr_layer_add",
        session = session,
        selected = character(0)
      )

      layers <- amr_layers()
      if (layer_has_field(layers, field)) {
        return()
      }
      if (length(layers) >= max_layers(MEDIUM)) {
        shiny$showNotification(
          sprintf(
            paste(
              "%d annotation strips is the most the heatmap can show at once.",
              "Remove one first."
            ),
            max_layers(MEDIUM)
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
      amr_layers(c(layers, list(layer)))
    })

    # One delegated handler per action rather than one observer per row: an
    # observeEvent created inside renderUI is re-registered on every render, so
    # the ids push their own value into a single input instead.
    shiny$observeEvent(input$amr_layer_delete, {
      keep <- drop_layer(amr_layers(), input$amr_layer_delete)
      amr_layers(rebalance_layers(keep, profiles(), MEDIUM, viz_metadata()))
    })

    output$amr_layers_ui <- shiny$renderUI({
      render_info("visualization_amr amr_layers_ui")
      layer_cards(
        ns,
        amr_layers(),
        MEDIUM,
        "amr_layer_edit",
        "amr_layer_delete",
        empty_text = "No variables mapped."
      )
    })

    # --- Editing one strip ---------------------------------------------------

    editing <- shiny$reactiveVal(NULL)

    # The strip has one channel, so what is left to decide is the palette and,
    # for a date, the calendar interval it is grouped by.
    shiny$observeEvent(input$amr_layer_edit, {
      l <- find_layer(amr_layers(), input$amr_layer_edit)
      shiny$req(!is.null(l))
      prof <- profile_for(profiles(), l$field)
      shiny$req(!is.null(prof))
      editing(l$id)

      values <- viz_metadata()[[l$field]]
      # The palette has to suit the variable as the chosen granularity leaves
      # it: binned to months it is a category, not a continuum.
      binned <- granularity_profile(prof, values, l$granularity)
      # Parsed, not raw: an ungrouped date reaches the scale as a continuum, and
      # out of SQLite it is a character column that no test for one can
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
            "amr_layer_granularity",
            l$granularity,
            values = values
          )
        },
        scale_select(
          ns,
          "amr_layer_palette",
          categories = cats,
          selected = l$palette
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("amr_layer_apply"), "Apply")
        )
      ))
    })

    shiny$observeEvent(input$amr_layer_apply, {
      id <- editing()
      shiny$req(!is.null(id))
      layers <- lapply(amr_layers(), function(l) {
        if (!identical(l$id, id)) {
          return(l)
        }
        l$palette <- input$amr_layer_palette %||% l$palette
        l <- set_layer_granularity(
          l,
          input$amr_layer_granularity,
          viz_metadata()[[l$field]]
        )
        # Pinned: rebalance_layers() rebuilds automatic layers from scratch and
        # would discard the palette just chosen.
        l$auto <- FALSE
        l
      })
      amr_layers(rebalance_layers(layers, profiles(), MEDIUM, viz_metadata()))
      editing(NULL)
      shiny$removeModal()
    })

    # Each mapped variable's values keyed by isolate, ready for the builder.
    # Isolates the field is empty for are labelled rather than dropped (see
    # .row_annotation in amr_plot.R). A date is binned first, so the strip
    # carries as many colours as there are intervals, not as there are isolates.
    anno_layers <- shiny$reactive({
      meta <- viz_metadata()
      layers <- amr_layers()
      if (is.null(meta) || !length(layers)) {
        return(list())
      }
      out <- lapply(layers, function(l) {
        if (!l$field %in% names(meta)) {
          return(NULL)
        }
        vals <- meta[[l$field]]
        if (identical(l$transform, "as_date")) {
          vals <- bin_date_values(vals, l$granularity)
        }
        list(
          field = l$field,
          label = l$title,
          palette = l$palette,
          continuous = isTRUE(l$continuous),
          values = setNames(vals, meta$isolate)
        )
      })
      Filter(Negate(is.null), out)
    })

    # What each picker was last refilled with, so the same refill is never sent
    # twice. Plain environment rather than reactive state: nothing reads it but
    # the function below, and a reactive value here would be one more thing for
    # the refill to invalidate.
    scale_sent <- new.env(parent = emptyenv())

    # Restrict a colour-scale picker to the palettes that can carry the number
    # of categories currently mapped to it, and move the selection when the one
    # in force no longer can. Mirrors apply_scale_choices() in the Epi engine,
    # generalised over the picker id since this engine has two of them. `fit`
    # is swappable because the bar scale does not want the general ladder of
    # qualitative palettes amr_fit_scale() falls through — see
    # amr_bar_scale_fit().
    apply_scale_choices <- function(
      id,
      n,
      default,
      force_default = FALSE,
      fit = amr_plot$amr_fit_scale
    ) {
      choices <- amr_plot$amr_scale_choices(max(1L, as.integer(n)))
      # Read through isolate(), never as a dependency. updatePickerInput
      # rebuilds the control, and a rebuilt picker reports back twice — the
      # list's own first entry as it is rebuilt, then the value actually
      # selected. Depended on, those echoes re-enter the observer that sent
      # them, which refills the picker again: with a fitted palette that is not
      # the first on offer (Dark2 over Set1) the two chase each other for as
      # long as the session lives, R never goes idle, and the whole UI stays
      # behind the busy shield. What this answers to is the data, which the
      # observers below still read reactively.
      current <- shiny$isolate(input[[id]])
      selected <- if (force_default) fit(default, n) else fit(current, n)
      # A reset has to land whatever was sent before it, since the reader's own
      # pick since then is exactly what it is undoing.
      if (
        !force_default && identical(scale_sent[[id]], list(choices, selected))
      ) {
        return(invisible(NULL))
      }
      scale_sent[[id]] <- list(choices, selected)
      update_scale_select(session, id, choices, selected)
    }

    shiny$observe({
      meta <- attr(presence_mat(), "genes")
      n <- if (is.null(meta) || !nrow(meta)) 1L else length(unique(meta$group))
      apply_scale_choices("amr_class_scale", n, CLASS_SCALE_DEFAULT)
    })

    # The class colour scale only styles the strip "Cluster All" draws; under
    # "Cluster Class" the class is text and the scale has nothing to colour, so
    # disable it there rather than remove it from the Clustering tab. The
    # radioGroupButtons round-trips its boolean choiceValues as "TRUE"/"FALSE".
    shiny$observe({
      shinyjs::toggleState(
        id = "amr_class_scale",
        condition = isTRUE(as.logical(input$amr_cluster_cols))
      )
    })

    shiny$observe({
      df <- prevalence_df()
      n <- if (!nrow(df)) 1L else length(unique(df$group))
      apply_scale_choices(
        "amr_bar_scale",
        n,
        BAR_SCALE_DEFAULT,
        fit = amr_plot$amr_bar_scale_fit
      )
    })

    # --- confidence colours (per element-type panel) ----------------------

    # One "Edit colours" button per element-type panel the screen carries. The
    # panel set is the data's, not the reader's — the same list amr_plot splits
    # the heatmap into — so this is a button row rather than an add/remove list.
    output$amr_confidence_colors_ui <- shiny$renderUI({
      labels <- present_elements()
      if (!length(labels)) {
        return(shiny$div(
          class = "text-muted fst-italic mb-2 tree-layer-empty",
          "Generate the plot to colour its panels."
        ))
      }
      shiny$div(
        class = "tree-layer-list",
        lapply(labels, function(label) {
          cfg <- element_cfg(label)
          meta <- if (identical(cfg$color_mode, "scale")) {
            paste("Scale ·", cfg$heat_scale)
          } else {
            "Tier colours"
          }
          shiny$div(
            class = "tree-layer-card",
            shiny$div(
              class = "tree-layer_body",
              shiny$div(class = "tree-layer_title", title = label, label),
              shiny$div(class = "tree-layer_meta", meta)
            ),
            layer_action_btn(
              ns,
              "amr_confidence_colors",
              label,
              "palette",
              "Edit confidence colours"
            )
          )
        })
      )
    })

    # Which panel the colour modal is editing. NULL when it is closed.
    coloring_element <- shiny$reactiveVal(NULL)

    # Two ways to colour the same five confidence tiers, exactly one live at a
    # time — one sequential ramp spread across them, or the four tiers picked by
    # hand — swapped by a segmented control rather than greyed, since a disabled
    # swatch still reads as a colour. The same dialog, for the same reason, as
    # the Tree's per-panel heatmap colour modal.
    shiny$observeEvent(input$amr_confidence_colors, {
      label <- input$amr_confidence_colors
      shiny$req(label %in% CONFIDENCE_ELEMENT_LABELS)
      coloring_element(label)
      cfg <- element_cfg(label)

      shiny$showModal(shiny$modalDialog(
        title = paste("Confidence colours:", label),
        size = "m",
        easyClose = TRUE,
        radioGroupButtons(
          ns("amr_conf_mode"),
          "Confidence tiers",
          choiceNames = c("Colour scale", "Pick each"),
          choiceValues = c("scale", "tiers"),
          selected = cfg$color_mode,
          justified = TRUE,
          size = "sm",
          width = "100%"
        ),
        shiny$conditionalPanel(
          condition = "input.amr_conf_mode == 'tiers'",
          ns = ns,
          shiny$div(
            class = "viz-color-grid",
            # Strongest first, the order the panel's own legend lists them.
            # Putative has no swatch — it is blended out of Absent and Partial
            # (amr_plot$amr_confidence_palette), same as everywhere else.
            viz_color(ns, "amr_conf_present", "Perfect", cfg$present_color),
            viz_color(ns, "amr_conf_strong", "Strong", cfg$strong_color),
            viz_color(ns, "amr_conf_partial", "Partial", cfg$partial_color),
            viz_color(ns, "amr_conf_absent", "Absent", cfg$absent_color)
          )
        ),
        shiny$conditionalPanel(
          condition = "input.amr_conf_mode == 'scale'",
          ns = ns,
          # Sequential families only: the tiers are a ladder from Absent to
          # Perfect and only a light-to-dark ramp reads as one.
          scale_select(
            ns,
            "amr_conf_heat_scale",
            categories = "Sequential",
            selected = cfg$heat_scale
          ),
          shiny$div(
            class = "text-muted fst-italic small mb-2",
            "Absent takes the lightest stop, Perfect the darkest."
          )
        ),
        footer = shiny$tagList(
          shiny$modalButton("Cancel"),
          shiny$actionButton(ns("amr_conf_apply"), "Apply")
        )
      ))
    })

    # Commit the modal's choices onto the panel it was opened for. `%||%` on
    # every read: the half of the dialog the segmented control had hidden stops
    # reporting, and its values must survive being switched away from.
    shiny$observeEvent(input$amr_conf_apply, {
      label <- coloring_element()
      shiny$req(label %in% CONFIDENCE_ELEMENT_LABELS)
      current <- element_cfg(label)
      store <- amr_element_colors()
      store[[label]] <- list(
        color_mode = input$amr_conf_mode %||% current$color_mode,
        heat_scale = input$amr_conf_heat_scale %||% current$heat_scale,
        present_color = input$amr_conf_present %||% current$present_color,
        strong_color = input$amr_conf_strong %||% current$strong_color,
        partial_color = input$amr_conf_partial %||% current$partial_color,
        absent_color = input$amr_conf_absent %||% current$absent_color
      )
      amr_element_colors(store)
      coloring_element(NULL)
      shiny$removeModal()
    })

    # --- reset --------------------------------------------------------------

    # Reset settings: restore every control in this engine's own sidebar to its
    # coded default, and drop the state that is not a control at all -- the
    # mapping strips and the per-panel confidence colours.
    #
    # The catalogue (AMR_CONTROLS / AMR_CONTROL_DEFAULTS above) is what makes
    # that complete. This used to be shinyjs::reset() plus a hand-written patch
    # list, which returned the sliders and switches and silently left all six
    # pickerInputs and all four radio-group buttons -- the view mode, the
    # vocabulary, both filters, the clustering pair and the count level --
    # exactly as the reader had set them. See the reference at the top of
    # viz_helpers.R for which widget families that helper cannot reach and why.
    #
    # The data-fitted answers come after the catalogue, so the fit wins over the
    # coded placeholder: the two palette pickers whose *choices* are swapped in
    # at runtime, the filter sliders whose bounds are this screen's own reported
    # range, and the layout fit -- for the aspect ratio and the two label
    # switches the default *is* the fit, and returning a 250-isolate matrix to
    # the ratio meant for a few dozen would hand back a picture no setting in
    # the panel had produced.
    reset_amr_settings <- function() {
      amr_layers(list())
      amr_layer_seq(0L)
      amr_element_colors(list())
      editing(NULL)

      # The gene picker is rendered server-side from this database's own gene
      # list, so there is no value to send it: forcing the default for one
      # rebuild is how it goes back to "every gene". Set before the bump so the
      # re-render sees it.
      genes_force_default(TRUE)
      genes_rebuild(genes_rebuild() + 1L)

      apply_controls(session, AMR_CONTROL_DEFAULTS, AMR_CONTROLS)
      fit_element_types()
      aspect_mirror(ASPECT_DEFAULT)
      hand_aspect(list())
      text_size_mirror(TEXT_SIZE_DEFAULT)
      show_row_names_mirror(FALSE)
      show_col_names_mirror(TRUE)
      show_element_names_mirror(TRUE)

      apply_scale_choices(
        "amr_class_scale",
        1L,
        CLASS_SCALE_DEFAULT,
        force_default = TRUE
      )
      apply_scale_choices(
        "amr_bar_scale",
        1L,
        BAR_SCALE_DEFAULT,
        force_default = TRUE,
        fit = amr_plot$amr_bar_scale_fit
      )
      fit_threshold_bounds()
      fit_top_n_bounds()
      auto_fit_layout(unname(PLOT_MODES))
    }

    on_confirmed_reset(
      input,
      session,
      reset_amr_settings,
      "Mapped variables and the per-panel gene-call colours go with the rest."
    )

    # The layout fit, applied: the three label rows switched to whatever this
    # shape has room for, in both directions, and then the aspect ratio of the
    # given views returned to their fit. In that order because the labels
    # decide the height worth buying - isolate names want a deeper row than
    # bare bands - and the canvas width the gene names get.
    auto_fit_layout <- function(views = shiny$isolate(mode())) {
      refit_labels()
      refit_aspect(views)
    }

    # Auto-fit: re-solve the geometry for the matrix on screen -- the same solve
    # Generate runs, on demand.
    #
    # It exists because the aspect ratio is the reader's to set and goes stale
    # as soon as anything else about the matrix moves: filtering two hundred
    # genes down to twenty leaves a ratio that spreads the rows without growing
    # the type, and switching the isolate names on over a shape with no room for
    # them draws a smear. Only the geometry -- colours, mappings, the filters
    # and the clustering settings are deliberate choices and are not the fit's
    # to overrule.
    shiny$observeEvent(input$auto_fit, {
      if (!isTRUE(generated())) {
        shiny$showNotification(
          "Generate a plot first \u2014 there is nothing to fit yet.",
          type = "warning"
        )
        return()
      }
      # Text size is a bias on the fit, so re-solving the fit drops it: a
      # hand-set 160% left on a freshly fitted layout is not the engine's
      # answer for this matrix.
      set_text_size(TEXT_SIZE_DEFAULT)
      auto_fit_layout()
      shiny$showNotification(
        "Sizes, labels and spacing fitted to the plot on screen.",
        type = "message",
        duration = 5
      )
    })

    shiny$observeEvent(
      session_reset(),
      {
        generated(FALSE)
        reset_amr_settings()
      },
      ignoreInit = TRUE
    )

    # --- generate -----------------------------------------------------------

    # No plot will render when Generate bails, so the overlay's clearing event
    # never fires — hide it now rather than leaving the spinner up for 45s.
    bail <- function(...) {
      shiny$showNotification(..., type = "warning")
      shinyjs::removeClass(id = "plot_stage", class = "is-loading")
      generated(FALSE)
      invisible(NULL)
    }

    shiny$observeEvent(generate(), {
      if (!identical(plot_type(), "AMR")) {
        return()
      }
      meta <- viz_metadata()
      if (is.null(meta) || !nrow(meta)) {
        return(bail("No isolate metadata to plot."))
      }

      # AMR screening rides along with a typing run, so an absent table means
      # the run predates screening or screening was unavailable for the
      # species — point at where it gets produced rather than just reporting an
      # empty plot.
      if (!amr_plot$has_amr_data(db_path())) {
        return(bail(
          shiny$tagList(
            shiny$tags$b("No AMR screening in this database."),
            shiny$br(),
            paste(
              "Screening runs alongside cgMLST typing. Re-type these",
              "assemblies under Typing to populate it."
            )
          ),
          duration = 12
        ))
      }

      empty <- switch(
        mode(),
        prevalence = !nrow(prevalence_df()),
        !ncol(presence_mat())
      )
      if (isTRUE(empty)) {
        return(bail(paste(
          "Nothing to plot: no screening result for the selected isolates",
          "clears the current filters."
        )))
      }

      # Fitted before the plot is published, so the first draw is already at the
      # right ratio rather than being drawn once at the old one and again at the
      # new one. Both views go back to their fit: a Generate is a new plot.
      auto_fit_layout(unname(PLOT_MODES))
      generated(TRUE)
    })

    # --- plot ---------------------------------------------------------------

    # The canvas the plot is drawn on, in inches: a physical size, never the
    # width the browser reports, so no size report from the client can redraw
    # the plot and the preview, the export and a saved Analysis are one
    # drawing. The heatmap grows it sideways for its gene columns (see
    # amr_plot$amr_canvas_width_in()); the prevalence chart keeps the base.
    canvas_width <- function(col_names = show_col_names_mirror()) {
      if (identical(mode(), "prevalence")) {
        return(amr_plot$AMR_CANVAS_IN)
      }
      amr_plot$amr_canvas_width_in(
        ncol(presence_mat()),
        col_names,
        length(anno_layers())
      )
    }
    canvas_in <- shiny$reactive(canvas_width())

    # A radioGroupButtons round-trips its boolean choiceValues as the strings
    # "TRUE"/"FALSE", never as a logical — see the zoom_view comment further
    # down for why as.logical() is doing the work here rather than isTRUE().
    grouping <- function() {
      if (isTRUE(as.logical(input$amr_cluster_cols))) {
        "cluster"
      } else {
        COLUMN_GROUPING_DEFAULT
      }
    }

    # The aspect the plot is actually drawn at, and the only thing the render
    # reads for it — never input$amr_aspect_ratio directly.
    #
    # updateSliderInput does not set an input; it sends a message the browser
    # applies and echoes back a flush later. A plot built from the input alone
    # is therefore drawn once at the stale ratio and again at the fitted one.
    # Writing the fit into this mirror before the plot is published makes the
    # first draw the correct one, and the echo that follows assigns the value it
    # already holds, which shiny does not treat as a change. Same arrangement,
    # and the same reason, as the Tree's `fitted` mirrors.
    aspect_mirror <- shiny$reactiveVal(ASPECT_DEFAULT)

    # The ratio the reader set by hand in each view, keyed by view, or nothing
    # where that view still follows its fit - the Epi curve's
    # aspect_follows_fit, kept per view because a heatmap's ratio means nothing
    # to a bar chart. Generate and a reset clear both, Auto-fit the one on
    # screen, and a restore records the saved ratio against its view.
    hand_aspect <- shiny$reactiveVal(list())

    # A user drag lands in the same mirror the fit writes to and is remembered
    # as this view's own ratio. The echo of a fitted value is not a drag: it
    # was snapped to the slider's grid before it was sent, so it equals the
    # mirror already.
    #
    # Nor is a late echo of a ratio sent before the last one: the slider is read
    # once it settles, and a server busy drawing a thousand-isolate heatmap can
    # let one echo settle after the fit has already moved on. Every value sent
    # since the reader last dragged is therefore recognised as the server's own.
    aspects_sent <- numeric(0)
    shiny$observeEvent(settled$amr_aspect_ratio(), {
      value <- settled$amr_aspect_ratio()
      if (any(abs(aspects_sent - value) < 1e-9)) {
        return()
      }
      aspects_sent <<- numeric(0)
      if (!isTRUE(all.equal(shiny$isolate(aspect_mirror()), value))) {
        aspect_mirror(value)
        held <- shiny$isolate(hand_aspect())
        held[[shiny$isolate(mode())]] <- value
        hand_aspect(held)
      }
    })

    # Writes an aspect to the slider and to the mirror the plot reads.
    set_aspect <- function(value) {
      aspects_sent <<- c(aspects_sent, value)
      shiny$updateSliderInput(session, "amr_aspect_ratio", value = value)
      aspect_mirror(value)
    }

    # refit_labels() below writes a Generate-time auto-off straight in here
    # rather than only sending updateSwitchInput's message, which the render
    # would not see until the echo arrived a flush later. A reader's own
    # click lands here too, so turning it back on after an auto-off — the
    # whole point of seeding rather than disabling — takes effect on the
    # same draw.
    show_col_names_mirror <- shiny$reactiveVal(TRUE)

    # The isolate names are decided by the fit the same way, so they need the
    # same mirror.
    show_row_names_mirror <- shiny$reactiveVal(FALSE)

    shiny$observeEvent(input$amr_show_row_names, {
      value <- isTRUE(input$amr_show_row_names)
      if (!isTRUE(all.equal(shiny$isolate(show_row_names_mirror()), value))) {
        show_row_names_mirror(value)
      }
    })

    # The text size is reset by Auto-fit, so it echoes as the aspect does.
    text_size_mirror <- shiny$reactiveVal(TEXT_SIZE_DEFAULT)

    shiny$observeEvent(settled$amr_text_size(), {
      value <- settled$amr_text_size()
      if (!isTRUE(all.equal(shiny$isolate(text_size_mirror()), value))) {
        text_size_mirror(value)
      }
    })

    # Writes a text size to the slider and to the mirror the fit reads.
    set_text_size <- function(value) {
      shiny$updateSliderInput(session, "amr_text_size", value = value)
      text_size_mirror(value)
    }

    shiny$observeEvent(input$amr_show_col_names, {
      value <- isTRUE(input$amr_show_col_names)
      if (!isTRUE(all.equal(shiny$isolate(show_col_names_mirror()), value))) {
        show_col_names_mirror(value)
      }
    })

    # The element-type row is seeded off the same way and for the same reason,
    # so it needs the same mirror — see show_col_names_mirror above.
    show_element_names_mirror <- shiny$reactiveVal(TRUE)

    shiny$observeEvent(input$amr_show_element_names, {
      value <- isTRUE(input$amr_show_element_names)
      if (
        !isTRUE(all.equal(shiny$isolate(show_element_names_mirror()), value))
      ) {
        show_element_names_mirror(value)
      }
    })

    # Everything the fit needs to describe the matrix on screen. Split out
    # because it is asked for in two ways: with the label switches as they
    # stand (what gets drawn), and with the labels left for the fit to decide
    # (what Generate and Auto-fit seed the switches from).
    fit_args <- function(
      col_names = show_col_names_mirror(),
      row_names = show_row_names_mirror()
    ) {
      mat <- presence_mat()
      shiny$req(ncol(mat) > 0)
      blocks <- amr_plot$amr_column_blocks(mat, grouping())
      elements <- amr_plot$amr_element_blocks(mat, grouping())
      list(
        n_rows = nrow(mat),
        n_cols = ncol(mat),
        width_in = canvas_width(col_names),
        show_row_names = row_names,
        show_col_names = col_names,
        show_element_names = show_element_names_mirror(),
        row_label_chars = max(nchar(rownames(mat)), 1L),
        col_label_chars = max(nchar(colnames(mat)), 1L),
        block_titles = blocks$titles,
        block_cols = blocks$cols,
        element_titles = elements$titles,
        element_cols = elements$cols,
        dend_cm = settled$amr_dend_size() %||% DEND_DEFAULT,
        n_strips = length(anno_layers()),
        # Only whether the drug classes are named as text over each block or
        # coloured into a strip - the element-type row below the body answers
        # to element_titles above now, not to this. See amr_auto_layout().
        column_grouping = grouping(),
        text_scale = text_scale_percent(text_size_mirror()),
        # The guides the legend column will really draw, so the fit plans the
        # keys and the type size against them rather than against a guess.
        legend_guides = amr_plot$amr_legend_guides(mat, palette_opts())
      )
    }

    # Every size in the heatmap, solved against the ratio in force. This is what
    # replaced the sliders: every label size, the legend size, the cell border
    # width, which labels are legible and whether the block titles have to be
    # turned on their side. See amr_plot$amr_auto_layout().
    layout_fit <- shiny$reactive({
      do.call(
        amr_plot$amr_auto_layout,
        c(fit_args(), list(aspect = aspect_mirror()))
      )
    })

    # The ratio the fit chooses for the view on screen, on the slider's grid:
    # for the heatmap, for the label switches as they now stand. The coded
    # default suits a few dozen isolates and nothing else: a thousand at 0.9 is
    # a band of rows a fraction of a millimetre apart.
    fitted_aspect <- shiny$reactive({
      value <- if (identical(mode(), "prevalence")) {
        prevalence_layout(NULL)$fitted_aspect
      } else {
        do.call(amr_plot$amr_auto_layout, fit_args())$aspect
      }
      .snap_aspect(value)
    })

    # Keeps the ratio on the fit as the data, the labels, the text size and the
    # view change - or on the reader's own ratio for this view, which no filter
    # change countermands. Priority over the render, so a changed fit is drawn
    # once rather than at the old ratio first.
    #
    # In the bar chart, only what is ranked - the count-by level, or how many
    # bars are kept - reopens the ratio: those change how many rows there are
    # to seat. Text size and the threshold/section filters resize the chart's
    # contents within the ratio already in force instead (see
    # amr_prevalence_layout()'s hand-set-ratio branch), so they are read only
    # inside fitted_aspect() below, isolated from this observer, and never
    # move the slider on their own.
    shiny$observe(priority = 10, {
      shiny$req(generated())
      m <- mode()
      if (identical(m, "prevalence")) {
        input$amr_level
        settled$amr_top_n()
      }
      target <- hand_aspect()[[m]] %||%
        tryCatch(
          if (identical(m, "prevalence")) {
            shiny$isolate(fitted_aspect())
          } else {
            fitted_aspect()
          },
          error = function(e) NULL
        )
      shiny$req(target)
      if (!isTRUE(all.equal(shiny$isolate(aspect_mirror()), target))) {
        set_aspect(target)
      }
    })

    # Returns the given views to their fit, and the one on screen to its fitted
    # ratio on this flush. Sent unconditionally: guarding on
    # `input$amr_aspect_ratio` reads a value the browser may be about to
    # replace - a reset pushes the coded default first and re-fits in the same
    # flush, and two messages for one input in a flush coalesce to the last.
    refit_aspect <- function(views = shiny$isolate(mode())) {
      held <- shiny$isolate(hand_aspect())
      held[views] <- NULL
      hand_aspect(held)
      fitted <- shiny$isolate(tryCatch(fitted_aspect(), error = function(e) NULL))
      if (!is.null(fitted)) {
        set_aspect(fitted)
      }
    }

    # Writes a label switch and the mirror the render reads, in one flush.
    set_label_switch <- function(id, mirror, value) {
      update_switch(id, value = value, session = session)
      mirror(value)
    }

    # The three label rows, switched to what this shape has room for — in both
    # directions, because Generate, Auto-fit and a reset are all a request for
    # the layout this data needs: a label the room cannot hold is switched off,
    # and one it can hold is switched back on. The Tree's rule for its tip
    # labels; any other edit leaves the switches as the reader set them, and the
    # builder still leaves off whatever no longer fits (see the hints).
    #
    # Judged with the gene names wanted, so the canvas is measured at the width
    # it would grow to for them; the isolate names are the fit's to decide.
    refit_labels <- function() {
      # A screen whose filters leave no gene column still has bars to count;
      # there is nothing to name, and the switches are left as they are.
      mat <- shiny$isolate(tryCatch(presence_mat(), error = function(e) NULL))
      if (is.null(mat) || !ncol(mat)) {
        return(invisible(NULL))
      }
      fit <- do.call(
        amr_plot$amr_auto_layout,
        shiny$isolate(fit_args(col_names = TRUE, row_names = NA))
      )
      set_label_switch(
        "amr_show_row_names",
        show_row_names_mirror,
        isTRUE(fit$show_row_names)
      )
      set_label_switch(
        "amr_show_col_names",
        show_col_names_mirror,
        isTRUE(fit$cols_legible)
      )
      set_label_switch(
        "amr_show_element_names",
        show_element_names_mirror,
        isTRUE(fit$elements_legible)
      )
    }

    # Say why a switch that is on draws nothing: the fit left that label off
    # because no legible size fits the room it has.
    shiny$observe({
      fit <- if (identical(mode(), "prevalence")) {
        NULL
      } else {
        tryCatch(layout_fit(), error = function(e) NULL)
      }
      hint <- function(id, shown) {
        shinyjs::toggleClass(id, "d-none", condition = !isTRUE(shown))
      }
      hint(
        "amr_row_names_hint",
        !is.null(fit) && show_row_names_mirror() && !isTRUE(fit$legible)
      )
      hint(
        "amr_col_names_hint",
        !is.null(fit) && show_col_names_mirror() && !isTRUE(fit$cols_legible)
      )
      hint(
        "amr_element_names_hint",
        !is.null(fit) &&
          show_element_names_mirror() &&
          !isTRUE(fit$elements_legible)
      )
      hint(
        "amr_bar_names_hint",
        identical(mode(), "prevalence") &&
          isFALSE(tryCatch(prevalence_fit()$legible, error = function(e) NULL))
      )
    })

    # The colour choices the legend column is keyed from. Read by both the fit
    # (which plans the legend against them) and the builder (which draws it).
    palette_opts <- function() {
      list(
        # The flat tier keys are the fallback the builder blends Putative out
        # of for a panel with no per-element entry (a hand-built matrix, or a
        # label outside CONFIDENCE_ELEMENT_LABELS); every real panel is
        # coloured from element_colors below instead.
        present_color = PRESENT_COLOR_DEFAULT,
        strong_color = STRONG_COLOR_DEFAULT,
        partial_color = PARTIAL_COLOR_DEFAULT,
        absent_color = ABSENT_COLOR_DEFAULT,
        # One confidence-colour config per element-type panel, keyed by the
        # panel's display label (see element_cfg and the colour modal).
        element_colors = setNames(
          lapply(CONFIDENCE_ELEMENT_LABELS, element_cfg),
          CONFIDENCE_ELEMENT_LABELS
        ),
        class_scale = input$amr_class_scale %||% CLASS_SCALE_DEFAULT,
        anno_layers = anno_layers()
      )
    }

    # Everything the heatmap builder reads: the palette choices, the rest of
    # the controls, and the fit last so its sizes are the ones it takes.
    heatmap_opts <- function() {
      fit <- layout_fit()
      # One distance and one linkage, shared by both axes rather than a second
      # pair for genes — see the Clustering tab's comment on amr_cluster_cols.
      cluster_distance <- input$amr_cluster_distance %||%
        CLUSTER_DISTANCE_DEFAULT
      cluster_method <- input$amr_cluster_method %||% CLUSTER_METHOD_DEFAULT
      c(
        palette_opts(),
        list(
          # The cell border has no picker of its own: drawn any other color it
          # reads as a grid superimposed on the matrix rather than the thin gap
          # between cells it is meant to be, so it always matches the
          # background.
          grid_color = input$amr_background_color %||% BACKGROUND_DEFAULT,
          dend_color = input$amr_dend_color %||% DEND_COLOR_DEFAULT,
          text_color = input$amr_text_color %||% TEXT_COLOR_DEFAULT,
          column_grouping = grouping(),
          cluster_rows = isTRUE(as.logical(input$amr_cluster_rows)),
          cluster_distance = cluster_distance,
          cluster_method = cluster_method,
          col_cluster_distance = cluster_distance,
          col_cluster_method = cluster_method,
          dend_size = settled$amr_dend_size() %||% DEND_DEFAULT,
          # Read from the mirrors, never from the inputs directly - same reason
          # as aspect_mirror above: refit_labels()'s decision would otherwise
          # not reach the first draw until updateSwitchInput's echo arrived a
          # flush later.
          show_row_names = show_row_names_mirror(),
          show_col_names = show_col_names_mirror(),
          show_element_names = show_element_names_mirror()
        ),
        fit
      )
    }

    # Rebuilt live as the controls change; the matrix reactives above are what
    # re-run when a data control moves, this only redraws.
    amr_ggplot <- shiny$reactive({
      # Which Generate this is the answer to. Everything the plot is built
      # from is value-driven, so a Generate that changes nothing invalidates
      # nothing — re-confirming the same isolate set applies the same metadata
      # and the same selection — and the loading overlay comes down on this
      # output's own value event. Without a dependency here no plot is drawn
      # and the spinner runs to its client-side safety timeout over a picture
      # that was already correct. Pressing Generate is an explicit request for
      # the plot, so it draws one.
      generate()
      shiny$req(generated())
      background <- input$amr_background_color %||% BACKGROUND_DEFAULT

      if (identical(mode(), "prevalence")) {
        df <- prevalence_df()
        shiny$req(nrow(df) > 0)
        return(amr_plot$build_amr_prevalence(
          df,
          c(
            list(
              bar_scale = input$amr_bar_scale %||% BAR_SCALE_DEFAULT,
              text_color = input$amr_text_color %||% TEXT_COLOR_DEFAULT,
              background = background,
              n_isolates = length(isolates())
            ),
            prevalence_fit()
          )
        ))
      }

      # The size the finished image is bound for. ComplexHeatmap lays the
      # legends out against the device it is drawn on, so this has to be the
      # real canvas rather than grid.grabExpr's 7x7 default — see
      # amr_plot$amr_as_ggplot().
      canvas <- plot_canvas()

      mat <- presence_mat()
      shiny$req(ncol(mat) > 0)
      amr_plot$amr_as_ggplot(
        amr_plot$build_amr_heatmap(mat, heatmap_opts()),
        background,
        width_in = canvas$width_in,
        height_in = canvas$height_in
      )
    })

    # The plot output element is kept mounted so each Generate re-renders the
    # *same* output. The "press Generate" prompt is an overlay toggled
    # separately.
    output$plot_area <- shiny$renderUI({
      render_info("visualization_amr plot_area")
      prompt <- shiny$div(
        id = ns("viz_prompt"),
        class = "viz-plot-prompt",
        style = if (isTRUE(shiny$isolate(generated()))) {
          "display:none;"
        } else {
          NULL
        },
        shiny$icon("shield-virus", class = "viz-plot-icon"),
        shiny$p(
          "Configure the AMR options, then press ",
          shiny$tags$strong("Generate Plot"),
          "."
        )
      )

      loading <- shiny$div(
        class = "viz-loading",
        shiny$div(
          class = "spinner-custom viz-spinner-dark",
          waiter::spin_flower(),
          shiny$tags$h5("Generating plot …", class = "viz-loading_text")
        )
      )

      shiny$div(
        # The `is-zoom` modifier is re-applied on every (re)mount from the
        # current control value via isolate() — read without a reactive
        # dependency so toggling it never re-renders the plot; live toggles are
        # the observer's job. radioGroupButtons' choiceValues = c(FALSE, TRUE)
        # round-trip as the strings "FALSE"/"TRUE", never as a logical, which is
        # what as.logical() is doing here.
        class = paste(
          "viz-plot-stage amr-stage",
          if (isTRUE(as.logical(shiny$isolate(input$zoom_view)))) "is-zoom"
        ),
        id = ns("plot_stage"),
        prompt,
        loading,
        shiny$plotOutput(ns("amr_plot"), height = "auto")
      )
    })

    shiny$observeEvent(
      generated(),
      {
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
      },
      ignoreNULL = FALSE
    )

    # The plot's proportions, fitted rather than set: taller as the isolate
    # count grows, so a screen of two hundred is drawn as a tall matrix a reader
    # can tell the rows of rather than a wide band they cannot. Prevalence bars
    # are the exception — their rows are the bars, one line of type each, so
    # they scale with the bar count and not with the isolates.
    plot_aspect <- shiny$reactive({
      if (identical(mode(), "prevalence")) {
        return(prevalence_fit()$aspect)
      }
      layout_fit()$aspect
    })

    # The canvas in inches, for the render, the export and the thumbnail.
    plot_canvas <- shiny$reactive({
      width_in <- canvas_in()
      list(width_in = width_in, height_in = width_in * plot_aspect())
    })

    # Smallest type the plot on screen sets, for the export's legibility note.
    plot_min_pt <- function() {
      if (identical(mode(), "prevalence")) {
        prevalence_fit()$min_pt
      } else {
        layout_fit()$min_pt
      }
    }

    # Fit ⇄ Zoom display mode, driven by this engine's own `zoom_view` control
    # (right sidebar, amr_controls()). Purely toggles the .is-zoom class on the
    # mounted stage — the image is not re-rendered (see the amr-stage CSS in
    # app/styles/main.scss), which is what makes it free on a matrix that takes
    # a second to draw. ignoreInit: the initial state is already stamped on the
    # stage div by renderUI's isolate() read.
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

    # Rendered through ggsave (via renderImage) rather than renderPlot so the
    # whole frame takes the chosen background: ggsave derives the device
    # background from the theme's plot.background, whereas renderPlot's device
    # is hardwired white and forced once at module init, so it can never track
    # the colour picker. Same treatment, and the same reason, as the Epi curve.
    # The pixel size is the canvas at PLOT_RES.
    output$amr_plot <- shiny$renderImage(
      {
        render_info("visualization_amr amr_plot")
        canvas <- plot_canvas()
        canvas_image(
          amr_ggplot(),
          canvas$width_in,
          canvas$height_in,
          "AMR screening plot"
        )
      },
      deleteFile = TRUE
    )

    # ---- Export contract ----------------------------------------------------
    # The tab's sidebar owns the export panel and the download; this engine only
    # says what it can produce and how to write it. The file is the plot on
    # screen at the size it is on screen, and its name carries the view mode,
    # since the modes are different plots over the same data.
    export <- canvas_export(
      label = shiny$reactive(paste0("amr_", mode())),
      ready = shiny$reactive(isTRUE(generated())),
      canvas = plot_canvas,
      plot = amr_ggplot,
      note = function() {
        legibility_note(
          plot_min_pt(),
          if (identical(mode(), "prevalence")) {
            "Raise the text size or the aspect ratio, or show fewer bars."
          } else {
            "Raise the text size or the aspect ratio, or show fewer genes."
          }
        )
      }
    )

    # `plot_area` is a cheap renderUI gating the "press Generate" prompt, and
    # the plot output has to bind through it, so it stays live while hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)

    # The two controls this module renders rather than declares, kept live
    # because a restored plot's gene selection and its annotation strips are
    # applied through them - and by default neither is on screen when a reopened
    # tab restores. Shiny counts anything under a `display: none` ancestor (a
    # collapsed accordion panel, an inactive nav tab) as hidden, which suspends
    # the render outright; suspended, the control neither exists in the DOM for
    # an update*Input() to reach nor re-renders to pick a value up, so a saved
    # selection was silently dropped. See the matching note in
    # visualization_epi.R.
    for (id in c("genes_ui", "amr_layers_ui")) {
      shiny$outputOptions(output, id, suspendWhenHidden = FALSE)
    }
    # Server-side image with no client state to lose, so it may suspend while
    # its plot tab is in the background — as the Tree and the Epi curve do.
    shiny$outputOptions(output, "amr_plot", suspendWhenHidden = TRUE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    # Every amr_* control, plus the annotation strips and the per-panel
    # confidence colours, which are reactiveVal state rather than inputs.
    snapshot <- shiny$reactive(
      c(
        collect_input_snapshot(input, "amr_"),
        list(
          # No amr_ prefix: `zoom_view` is the shared display-mode control, named
          # the same here as in the Tree, so the prefix sweep never picks it up.
          # Saved as a logical, as the Tree saves it.
          zoom_view = isTRUE(as.logical(input$zoom_view)),
          .layers = amr_layers(),
          .element_colors = amr_element_colors()
        )
      )
    )

    # Rebuild an annotation strip from a pre-rewrite snapshot's flat keys. Each
    # saved AMR plot carried at most one, in amr_anno_field plus its granularity
    # and palette; rebuilding it here is what stops a saved analysis silently
    # losing its colour strip on first reopen.
    migrate_legacy_annotation <- function(vals) {
      field <- vals$amr_anno_field
      if (is.null(field) || !nzchar(field %||% "")) {
        return(NULL)
      }
      prof <- profile_for(profiles(), field)
      if (is.null(prof)) {
        return(NULL)
      }
      layer <- assign_mapping_layer(
        prof,
        list(),
        "L1",
        MEDIUM,
        viz_metadata()[[field]]
      )
      if (is.null(layer)) {
        return(NULL)
      }
      if (!is.null(vals$amr_anno_granularity)) {
        layer <- set_layer_granularity(
          layer,
          vals$amr_anno_granularity,
          viz_metadata()[[field]]
        )
      }
      if (!is.null(vals$amr_anno_scale)) {
        layer$palette <- vals$amr_anno_scale
      }
      layer$auto <- FALSE
      list(layer)
    }

    restore <- function(vals) {
      # Same catalogue a reset applies, holding saved values instead of the
      # coded ones. amr_cluster_rows/amr_cluster_cols round-trip as
      # radioGroupButtons' "TRUE"/"FALSE" strings, not switch booleans -- see
      # .legacy_cluster_cols just below for what a pre-restructure snapshot's
      # own boolean value means.
      apply_controls(session, vals, AMR_CONTROLS)

      # See .legacy_cluster_cols(). Such a snapshot's own
      # amr_col_cluster_distance/method, from when the two axes still had
      # separate pickers, is dropped rather than translated - the shared pair
      # above already carries a sensible value either way.
      if (
        is.null(vals$amr_cluster_cols) && !is.null(vals$amr_column_grouping)
      ) {
        updateRadioGroupButtons(
          session,
          "amr_cluster_cols",
          selected = .legacy_cluster_cols(vals$amr_column_grouping)
        )
      }

      # The mirror is what the render reads, and a restored slider reaches it
      # only via the browser's echo — which never arrives for a value that did
      # not change. Writing it here makes a restore take effect on this flush.
      if (!is.null(vals$amr_aspect_ratio)) {
        aspect_mirror(vals$amr_aspect_ratio)
        held <- list()
        held[[vals$amr_mode %||% PLOT_MODE_DEFAULT]] <- vals$amr_aspect_ratio
        hand_aspect(held)
      }
      if (!is.null(vals$amr_text_size)) {
        text_size_mirror(vals$amr_text_size)
      }
      if (!is.null(vals$amr_show_row_names)) {
        show_row_names_mirror(isTRUE(vals$amr_show_row_names))
      }
      if (!is.null(vals$amr_show_col_names)) {
        show_col_names_mirror(isTRUE(vals$amr_show_col_names))
      }
      if (!is.null(vals$amr_show_element_names)) {
        show_element_names_mirror(isTRUE(vals$amr_show_element_names))
      }

      layers <- normalize_layers(vals$.layers, LAYER_DEFAULTS, MEDIUM)
      if (is.null(layers)) {
        layers <- migrate_legacy_annotation(vals)
      }
      if (!is.null(layers)) {
        amr_layers(layers)
        amr_layer_seq(length(layers))
      }

      # Per-panel confidence colours. Only the labels the save actually carried
      # are restored; any panel it did not touch keeps element_cfg()'s default.
      # A pre-feature save has no `.element_colors` and its flat amr_*_color
      # keys, if any, are left behind — the panels open on their ramp defaults.
      stored <- vals$.element_colors
      if (is.list(stored)) {
        amr_element_colors(stored[
          intersect(names(stored), CONFIDENCE_ELEMENT_LABELS)
        ])
      }
    }

    # Thumbnail: server-render the current view on its own canvas, so it looks
    # like the plot it stands for, with the resolution carrying the requested
    # pixel width.
    save_thumb <- function(file, w, h) {
      canvas <- plot_canvas()
      render_canvas_png(
        amr_ggplot(),
        file,
        canvas$width_in,
        canvas$height_in,
        res = max(24, round(w / canvas$width_in))
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
