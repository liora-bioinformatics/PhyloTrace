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
# Three views, chosen with one select (see PLOT_MODES):
#   * Gene heatmap    - isolates x genes presence/absence, the successor to the
#                       ComplexHeatmap on the master branch. Columns grouped by
#                       element type or drug class, or clustered.
#   * Drug classes    - isolates x drug class, cells carrying abritamr's
#                       confident/partial distinction. Coarser, and the only
#                       view that shows that distinction at all.
#   * Prevalence      - ranked bars of how many isolates carry each gene or
#                       class. The view that survives a screen reporting several
#                       hundred genes.
#
# All three arrive from app/logic/amr_plot.R as ggplot objects, so - exactly as
# in the Epi engine - there is one renderImage, one download handler and one
# thumbnail function rather than a device block per format (which is what the
# master implementation had). Binning and filtering are cheap, so once Generate
# has been pressed the plot tracks the controls live; only clustering is
# expensive, and it sits behind its own matrix reactive so a purely cosmetic
# control does not re-run it.

box::use(
  bslib[
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
  app / logic / viz_export[save_plot_export],
  app /
    logic /
    viz_helpers[
      apply_input_snapshot,
      collect_input_snapshot,
      field_select,
      granularity_select,
      reset_viz_colors,
      scale_select,
      suitable_scale_categories,
      update_field_select,
      update_scale_select,
      viz_color
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

# The three views. "heatmap" is the default because it is the one that answers
# the question the screen was run for: which isolate carries what.
PLOT_MODES <- c(
  `Gene heatmap` = "heatmap",
  `Drug classes` = "classes",
  Prevalence = "prevalence"
)
PLOT_MODE_DEFAULT <- "heatmap"

# How the heatmap's columns are arranged *inside* one element-type panel:
# clustered, like "Cluster isolates" does for rows, or - when they are not -
# grouped by drug class, the one arrangement that means something on every
# view this control is shown for. There is no third position any more:
# "Element type" went the same way it did for the row axis (resistance,
# virulence and stress genes are now always drawn as separate panels side by
# side, so grouping by it would have been a control that changed nothing),
# and the old "None" (columns left in their natural, unarranged order) is
# gone too — see amr_cluster_cols and .column_layout in amr_plot.R.
COLUMN_GROUPING_DEFAULT <- "class"

# What a pre-restructure snapshot's amr_column_grouping ("class"/"cluster"/
# "none", plus the older "element") means for the switch that replaced it —
# used only by restore() below, translating a saved value before it falls
# through to the ordinary switch restore.
.legacy_cluster_cols <- function(saved) {
  identical(saved, "cluster")
}

# Which of the two drug-class vocabularies the gene heatmap groups and colours
# by. The vocabularies, and why the curated rollup leads, are in amr_plot.R
# beside the constant itself.
CLASS_VOCABULARIES <- amr_plot$AMR_CLASS_VOCABULARIES
CLASS_VOCABULARY_DEFAULT <- amr_plot$AMR_CLASS_VOCABULARY_DEFAULT

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

# One depth for both dendrograms, in centimetres. They are read together and
# there was never a reason to give them different depths; 0 draws neither,
# keeping the clustering's row and column *order* while dropping the trees.
DEND_DEFAULT <- 1.5

# The medium this engine maps variables onto, in mapping_engine.R's terms: a
# repeatable colour strip beside the rows.
MEDIUM <- "amr"

LAYER_DEFAULTS <- layer_defaults(MEDIUM)

PRESENT_COLOR_DEFAULT <- "#000000"
PARTIAL_COLOR_DEFAULT <- "#E5C494"
ABSENT_COLOR_DEFAULT <- "#EFEFEF"
GRID_COLOR_DEFAULT <- "#FFFFFF"
DEND_COLOR_DEFAULT <- "#000000"
TEXT_COLOR_DEFAULT <- "#000000"
BACKGROUND_DEFAULT <- "#FFFFFF"

CLASS_SCALE_DEFAULT <- "Set2"
BAR_SCALE_DEFAULT <- "Set2"

# Only shown while the element-type / hit-quality filters actually bite: they
# read `amr_results`, which backs the gene heatmap and the gene-level prevalence
# bars but not the abritamr rollup behind the drug-class views.
COND_HITS <- paste(
  "input.amr_mode == 'heatmap' ||",
  "(input.amr_mode == 'prevalence' && input.amr_level == 'gene')"
)
COND_SECTIONS <- paste(
  "input.amr_mode == 'classes' ||",
  "(input.amr_mode == 'prevalence' && input.amr_level == 'class')"
)
COND_HEATMAPS <- "input.amr_mode != 'prevalence'"

# --- AMR control tabs --------------------------------------------------------

# Which control tabs each view has anything to say in.
#
# Prevalence draws ranked bars off a count: it has no matrix to lay out, no
# dendrogram to tune and no row strips to map, so three of the five tabs would
# open on nothing but a note explaining why they are empty. Hiding them is the
# same arrangement the Map makes for its own modes.
#
# Keyed on each nav_panel's explicit `value`, never on its title: a tab renamed
# for the reader would otherwise stop matching here and silently stick — left
# visible in a view with nothing to put in it, or hidden for good.
TABS_BY_MODE <- list(
  heatmap = c("data", "layout", "clustering", "mapping", "colors"),
  classes = c("data", "layout", "clustering", "mapping", "colors"),
  prevalence = c("data", "colors")
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
        # applies — the gene level reads `amr_results`, the class level the
        # abritamr rollup.
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
          # Which vocabulary the column blocks are named in. Only the gene heatmap
          # has the choice to make: the drug-class view *is* the rollup, so there
          # is no second vocabulary to draw it in.
          shiny$conditionalPanel(
            condition = "input.amr_mode == 'heatmap'",
            ns = ns,
            pickerInput(
              ns("amr_class_vocab"),
              "Drug classes from",
              choices = CLASS_VOCABULARIES,
              selected = CLASS_VOCABULARY_DEFAULT
            )
          ),
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
            "Range fits what this screen actually reported"
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
            "Range fits what this screen actually reported"
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
      # Both views this tab is shown for are matrices, so nothing here needs a
      # per-mode condition of its own — see TABS_BY_MODE.
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
          min = 0.3,
          max = 8,
          value = ASPECT_DEFAULT,
          step = 0.1,
          ticks = FALSE
        ),
        input_switch(ns("amr_show_row_names"), "Show isolate names", FALSE),
        shiny$uiOutput(ns("row_name_warning"))
      ),
      # Clustering -------------------------------------------------------------
      nav_panel(
        "Clustering",
        value = "clustering",
        icon = shiny$icon("sitemap"),
        input_switch(ns("amr_cluster_rows"), "Cluster isolates", TRUE),
        # The gene axis is a different question — how alike two *genes* are
        # across the isolates, rather than two isolates across the genes — but
        # not a different enough one to earn its own distance and linkage
        # pickers: every reader who tuned one axis wanted the same pair on the
        # other. One shared pair below, shown whenever either switch is on, and
        # fed to whichever axis (or both) is actually clustering. Off, the
        # gene axis falls back to drug-class grouping rather than an
        # unarranged "None" — see amr_cluster_cols in .column_layout.
        input_switch(ns("amr_cluster_cols"), "Cluster genes", FALSE),
        shiny$conditionalPanel(
          condition = "input.amr_cluster_rows || input.amr_cluster_cols",
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
        shiny$uiOutput(ns("amr_layers_ui")),
        shiny$hr(),
        shiny$conditionalPanel(
          condition = "input.amr_mode == 'heatmap'",
          ns = ns,
          input_switch(
            ns("amr_show_class_anno"),
            "Show drug-class strip",
            TRUE
          ),
          scale_select(ns, "amr_class_scale", categories = "Qualitative")
        )
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
        shiny$div(
          class = "viz-color-grid",
          shiny$conditionalPanel(
            condition = COND_HEATMAPS,
            ns = ns,
            viz_color(ns, "amr_present_color", "Present", PRESENT_COLOR_DEFAULT)
          ),
          # Only the drug-class matrix has a third cell state to colour.
          shiny$conditionalPanel(
            condition = "input.amr_mode == 'classes'",
            ns = ns,
            viz_color(ns, "amr_partial_color", "Partial", PARTIAL_COLOR_DEFAULT)
          ),
          shiny$conditionalPanel(
            condition = COND_HEATMAPS,
            ns = ns,
            viz_color(ns, "amr_absent_color", "Absent", ABSENT_COLOR_DEFAULT),
            viz_color(ns, "amr_grid_color", "Cell border", GRID_COLOR_DEFAULT),
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
      class = "viz-mode-dropup",
      pickerInput(
        ns("amr_mode"),
        "View",
        choices = PLOT_MODES,
        selected = PLOT_MODE_DEFAULT
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

    mode <- function() input$amr_mode %||% PLOT_MODE_DEFAULT

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

    shiny$observeEvent(amr_hits(), fit_threshold_bounds(), ignoreNULL = FALSE)

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
        min_identity = input$amr_min_identity %||% 0,
        min_coverage = input$amr_min_coverage %||% 0
      )
    })

    # An empty gene selection means "every gene", so clearing the picker shows
    # the whole screen rather than an empty plot.
    selected_genes <- shiny$reactive({
      g <- input$amr_genes
      if (!length(g)) NULL else g
    })

    # The two expensive reactives. Everything cosmetic reads the built plot
    # below, so moving a colour picker never re-clusters.
    presence_mat <- shiny$reactive({
      amr_plot$amr_presence_matrix(
        filtered_hits(),
        isolates(),
        selected_genes(),
        sections = amr_sections(),
        vocabulary = class_vocab()
      )
    })

    class_mat <- shiny$reactive({
      amr_plot$amr_class_matrix(
        amr_sections(),
        isolates(),
        keep_sections = input$amr_sections
      )
    })

    prevalence_df <- shiny$reactive({
      amr_plot$amr_prevalence(
        filtered_hits(),
        amr_sections(),
        isolates(),
        level = input$amr_level %||% LEVEL_DEFAULT,
        top_n = input$amr_top_n %||% TOP_N_DEFAULT,
        keep_sections = input$amr_sections
      )
    })

    # --- controls fitted to the data ----------------------------------------

    # Every gene the screen reported, grouped by element type and drug class.
    # Built from the unfiltered hits deliberately: rebuilding the picker every
    # time the identity slider moves would drop the reader's selection under
    # them.
    output$genes_ui <- shiny$renderUI({
      render_info("visualization_amr genes_ui")
      genes_rebuild()
      choices <- amr_plot$amr_gene_choices(
        amr_hits(),
        amr_sections(),
        class_vocab()
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

    # Restrict a colour-scale picker to the palettes that can carry the number
    # of categories currently mapped to it, and move the selection when the one
    # in force no longer can. Mirrors apply_scale_choices() in the Epi engine,
    # generalised over the picker id since this engine has two of them.
    apply_scale_choices <- function(id, n, default, force_default = FALSE) {
      choices <- amr_plot$amr_scale_choices(max(1L, as.integer(n)))
      selected <- if (force_default) {
        amr_plot$amr_fit_scale(default, n)
      } else {
        amr_plot$amr_fit_scale(input[[id]], n)
      }
      update_scale_select(session, id, choices, selected)
    }

    shiny$observe({
      meta <- attr(presence_mat(), "genes")
      n <- if (is.null(meta) || !nrow(meta)) 1L else length(unique(meta$group))
      apply_scale_choices("amr_class_scale", n, CLASS_SCALE_DEFAULT)
    })

    shiny$observe({
      df <- prevalence_df()
      n <- if (!nrow(df)) 1L else length(unique(df$group))
      apply_scale_choices("amr_bar_scale", n, BAR_SCALE_DEFAULT)
    })

    # --- reset --------------------------------------------------------------

    # Restore every sidebar control to its coded default. Shared by this
    # engine's "Reset settings" button and the app-level session_reset path, so
    # both routes return the controls identically. See the reset checklist in
    # app/logic/viz_helpers.R for why a blanket shinyjs::reset() is not enough
    # on its own.
    reset_amr_settings <- function() {
      shinyjs::reset(id = "controls_wrap")

      # Bucket 2: colorPickr swatches, which shinyjs::reset() does not even
      # recognise.
      reset_viz_colors(
        session,
        amr_present_color = PRESENT_COLOR_DEFAULT,
        amr_partial_color = PARTIAL_COLOR_DEFAULT,
        amr_absent_color = ABSENT_COLOR_DEFAULT,
        amr_grid_color = GRID_COLOR_DEFAULT,
        amr_dend_color = DEND_COLOR_DEFAULT,
        amr_text_color = TEXT_COLOR_DEFAULT,
        amr_background_color = BACKGROUND_DEFAULT
      )

      # Bucket 5: the gene picker is rendered by renderUI, so shinyjs::reset()
      # has no page-load value to restore it from — rebuild it instead, forcing
      # the default for this one rebuild rather than preserving the current
      # choice. Set before the bump so the re-render sees it.
      genes_force_default(TRUE)
      genes_rebuild(genes_rebuild() + 1L)

      # Bucket 6: the annotation strips are reactiveVal state rather than an
      # input, so nothing shinyjs does touches them.
      amr_layers(list())
      amr_layer_seq(0L)
      editing(NULL)
      aspect_mirror(ASPECT_DEFAULT)

      # Bucket 4: controls whose *choices* are swapped in at runtime. Deferred
      # past shinyjs::reset()'s own asynchronous, stale restoration, which would
      # otherwise land a moment later and overwrite an immediate correction.
      shinyjs::delay(400, {
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
          force_default = TRUE
        )
        # Same reasoning: shinyjs::reset() restores the two threshold sliders
        # to their declared 0-100 placeholders, not the data-fitted range.
        fit_threshold_bounds()
      })
    }

    shiny$observeEvent(input$reset_settings, reset_amr_settings())

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
        classes = !ncol(class_mat()),
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
      # new one.
      refit_aspect()
      generated(TRUE)
    })

    # --- plot ---------------------------------------------------------------

    # The canvas the plot is laid out for, in inches. Read from the browser so
    # the fit knows the room it actually has; 9in is a reasonable desktop
    # sidebar-open width for the first render, before clientData has reported.
    canvas_in <- function() {
      w <- session$clientData[[paste0("output_", ns("amr_plot"), "_width")]]
      w <- suppressWarnings(as.numeric(w))
      if (!length(w) || !is.finite(w) || w <= 0) 9 else w / 96
    }

    # Which matrix the current view draws, so the fit and the plot agree on the
    # shape they are describing.
    current_mat <- function() {
      if (identical(mode(), "classes")) class_mat() else presence_mat()
    }

    grouping <- function() {
      if (isTRUE(input$amr_cluster_cols)) "cluster" else COLUMN_GROUPING_DEFAULT
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

    # A user drag lands in the same mirror the fit writes to, so the two are
    # indistinguishable downstream and whichever happened last simply wins.
    shiny$observeEvent(input$amr_aspect_ratio, {
      value <- input$amr_aspect_ratio
      if (!isTRUE(all.equal(shiny$isolate(aspect_mirror()), value))) {
        aspect_mirror(value)
      }
    })

    # Everything the fit needs to describe the matrix on screen. Split out
    # because it is asked for twice: once at the ratio the reader has set (what
    # gets drawn) and once with no ratio at all (what Generate seeds the slider
    # from).
    fit_args <- function() {
      mat <- current_mat()
      shiny$req(ncol(mat) > 0)
      blocks <- if (identical(mode(), "classes")) {
        list(titles = character(0), cols = integer(0))
      } else {
        amr_plot$amr_column_blocks(mat, grouping())
      }
      list(
        n_rows = nrow(mat),
        n_cols = ncol(mat),
        width_in = canvas_in(),
        show_row_names = isTRUE(input$amr_show_row_names),
        row_label_chars = max(nchar(rownames(mat)), 1L),
        col_label_chars = max(nchar(colnames(mat)), 1L),
        block_titles = blocks$titles,
        block_cols = blocks$cols,
        dend_cm = input$amr_dend_size %||% DEND_DEFAULT,
        n_strips = length(anno_layers())
      )
    }

    # Every size in the heatmap, solved against the ratio in force. This is what
    # replaced the sliders: both label sizes, the block-title size, the legend
    # size, the cell border width and whether the block titles have to be turned
    # on their side. See amr_plot$amr_auto_layout().
    layout_fit <- shiny$reactive({
      do.call(
        amr_plot$amr_auto_layout,
        c(fit_args(), list(aspect = aspect_mirror()))
      )
    })

    # Fit the aspect to the data, the way the Tree fits its own. The coded
    # default suits a few dozen isolates and nothing else: two hundred and fifty
    # at 0.9 is a band of rows a millimetre apart, and no hand-tuning could fix
    # it because the ratio needed is several times what the slider used to open
    # at. Seeded on Generate only — a filter change must not countermand a ratio
    # the reader has just set by hand.
    refit_aspect <- function() {
      fitted <- do.call(amr_plot$amr_auto_layout, shiny$isolate(fit_args()))
      value <- fitted$aspect
      if (!isTRUE(all.equal(shiny$isolate(input$amr_aspect_ratio), value))) {
        shiny$updateSliderInput(session, "amr_aspect_ratio", value = value)
      }
      aspect_mirror(value)
    }

    # The one place the automatic fit is allowed to answer back: at this many
    # isolates a name cannot be set large enough to read, and saying so is
    # better than drawing a grey smear and leaving the reader to work out why.
    output$row_name_warning <- shiny$renderUI({
      if (!isTRUE(input$amr_show_row_names)) {
        return(NULL)
      }
      fit <- shiny$req(layout_fit())
      if (isTRUE(fit$legible)) {
        return(NULL)
      }
      shiny$helpText(
        class = "amr-help",
        sprintf(
          paste(
            "%d isolates leave about %.1f pt per name, which is below what",
            "prints legibly. Narrow the selection or map a variable instead."
          ),
          nrow(current_mat()),
          fit$fontsize_row
        )
      )
    })

    heatmap_opts <- function() {
      fit <- layout_fit()
      # One distance and one linkage, shared by both axes rather than a second
      # pair for genes — see the Clustering tab's comment on amr_cluster_cols.
      cluster_distance <- input$amr_cluster_distance %||%
        CLUSTER_DISTANCE_DEFAULT
      cluster_method <- input$amr_cluster_method %||% CLUSTER_METHOD_DEFAULT
      c(
        list(
          present_color = input$amr_present_color %||% PRESENT_COLOR_DEFAULT,
          partial_color = input$amr_partial_color %||% PARTIAL_COLOR_DEFAULT,
          absent_color = input$amr_absent_color %||% ABSENT_COLOR_DEFAULT,
          grid_color = input$amr_grid_color %||% GRID_COLOR_DEFAULT,
          dend_color = input$amr_dend_color %||% DEND_COLOR_DEFAULT,
          text_color = input$amr_text_color %||% TEXT_COLOR_DEFAULT,
          column_grouping = grouping(),
          cluster_rows = isTRUE(input$amr_cluster_rows),
          cluster_distance = cluster_distance,
          cluster_method = cluster_method,
          col_cluster_distance = cluster_distance,
          col_cluster_method = cluster_method,
          dend_size = input$amr_dend_size %||% DEND_DEFAULT,
          show_row_names = isTRUE(input$amr_show_row_names),
          show_class_anno = isTRUE(input$amr_show_class_anno),
          class_scale = input$amr_class_scale %||% CLASS_SCALE_DEFAULT,
          anno_layers = anno_layers()
        ),
        # The fit comes last so its fontsize_*, grid_width and title_rot are the
        # ones the builder reads.
        fit
      )
    }

    # Rebuilt live as the controls change; the matrix reactives above are what
    # re-run when a data control moves, this only redraws.
    amr_ggplot <- shiny$reactive({
      shiny$req(generated())
      background <- input$amr_background_color %||% BACKGROUND_DEFAULT

      if (identical(mode(), "prevalence")) {
        df <- prevalence_df()
        shiny$req(nrow(df) > 0)
        return(amr_plot$build_amr_prevalence(
          df,
          list(
            bar_scale = input$amr_bar_scale %||% BAR_SCALE_DEFAULT,
            text_color = input$amr_text_color %||% TEXT_COLOR_DEFAULT,
            background = background,
            n_isolates = length(isolates())
          )
        ))
      }

      # The size the finished image is bound for. ComplexHeatmap lays the
      # legends out against the device it is drawn on, so this has to be the
      # real canvas rather than grid.grabExpr's 7x7 default — see
      # amr_plot$amr_as_ggplot().
      width_in <- canvas_in()
      height_in <- width_in * plot_aspect()

      if (identical(mode(), "classes")) {
        mat <- class_mat()
        shiny$req(ncol(mat) > 0)
        return(amr_plot$amr_as_ggplot(
          amr_plot$build_amr_class_heatmap(mat, heatmap_opts()),
          background,
          width_in = width_in,
          height_in = height_in
        ))
      }

      mat <- presence_mat()
      shiny$req(ncol(mat) > 0)
      amr_plot$amr_as_ggplot(
        amr_plot$build_amr_heatmap(mat, heatmap_opts()),
        background,
        width_in = width_in,
        height_in = height_in
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
        n <- nrow(prevalence_df())
        return(max(0.35, min(1.6, 0.22 + n * 0.035)))
      }
      layout_fit()$aspect
    })

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
    output$amr_plot <- shiny$renderImage(
      {
        render_info("visualization_amr amr_plot")
        p <- amr_ggplot()
        w <- session$clientData[[paste0("output_", ns("amr_plot"), "_width")]]
        w <- as.integer(w %||% 900L)
        h <- as.integer(w * plot_aspect())
        # Lay the plot out as if at 96dpi (so point sizes keep their meaning)
        # but render at the browser's device pixel ratio so the PNG stays crisp
        # on HiDPI screens.
        pr <- session$clientData$pixelratio %||% 1
        tmp <- tempfile(fileext = ".png")
        amr_plot$render_amr_png(
          p,
          tmp,
          width_px = w,
          height_px = h,
          res = 96,
          scale = pr
        )
        list(src = tmp, width = w, height = h, alt = "AMR screening plot")
      },
      deleteFile = TRUE
    )

    # ---- Export contract ----------------------------------------------------
    # The tab's sidebar owns the export panel and the download; this engine only
    # says what it can produce and how to write it. The file name carries the
    # view mode, since the three modes are different plots over the same data
    # and an exported heatmap should not be mistaken for a prevalence chart.
    export_aspect <- plot_aspect

    export <- list(
      kind = "ggplot",
      label = shiny$reactive(paste0("amr_", mode())),
      ready = shiny$reactive(isTRUE(generated())),
      aspect = export_aspect,
      save = function(file, format, opts) {
        save_plot_export(
          amr_ggplot(),
          file,
          format,
          width_cm = opts$width_cm,
          aspect = export_aspect(),
          dpi = opts$dpi
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
    # Every amr_* control, plus the annotation strips, which are reactiveVal
    # state rather than an input.
    snapshot <- shiny$reactive(
      c(
        collect_input_snapshot(input, "amr_"),
        list(
          # No amr_ prefix: `zoom_view` is the shared display-mode control, named
          # the same here as in the Tree, so the prefix sweep never picks it up.
          # Saved as a logical, as the Tree saves it.
          zoom_view = isTRUE(as.logical(input$zoom_view)),
          .layers = amr_layers()
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
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "amr_cluster_rows",
          "amr_cluster_cols",
          "amr_show_row_names",
          "amr_show_class_anno"
        ),
        selects = c(
          "amr_mode",
          "amr_class_vocab",
          "amr_cluster_distance",
          "amr_cluster_method",
          "amr_class_scale",
          "amr_bar_scale"
        ),
        sliders = c(
          "amr_top_n",
          "amr_min_identity",
          "amr_min_coverage",
          "amr_dend_size",
          "amr_aspect_ratio"
        ),
        colors = c(
          "amr_present_color",
          "amr_partial_color",
          "amr_absent_color",
          "amr_grid_color",
          "amr_dend_color",
          "amr_text_color",
          "amr_background_color"
        ),
        radio_groups = c("amr_level", "zoom_view"),
        pickers = c(
          "amr_elements",
          "amr_sections"
        ),
        # Server-rendered and rebuilt on a counter, but bound the same way
        # regardless of when its HTML lands — what sets it apart is the widget:
        # a virtual-select ignores updatePickerInput() outright.
        virtual_selects = "amr_genes"
      )

      # See .legacy_cluster_cols(). Such a snapshot's own
      # amr_col_cluster_distance/method, from when the two axes still had
      # separate pickers, is dropped rather than translated - the shared pair
      # above already carries a sensible value either way.
      if (
        is.null(vals$amr_cluster_cols) && !is.null(vals$amr_column_grouping)
      ) {
        update_switch(
          "amr_cluster_cols",
          value = .legacy_cluster_cols(vals$amr_column_grouping),
          session = session
        )
      }

      # The mirror is what the render reads, and a restored slider reaches it
      # only via the browser's echo — which never arrives for a value that did
      # not change. Writing it here makes a restore take effect on this flush.
      if (!is.null(vals$amr_aspect_ratio)) {
        aspect_mirror(vals$amr_aspect_ratio)
      }

      layers <- normalize_layers(vals$.layers, LAYER_DEFAULTS, MEDIUM)
      if (is.null(layers)) {
        layers <- migrate_legacy_annotation(vals)
      }
      if (!is.null(layers)) {
        amr_layers(layers)
        amr_layer_seq(length(layers))
      }
    }

    # Thumbnail: server-render the current view to a small PNG.
    save_thumb <- function(file, w, h) {
      amr_plot$render_amr_png(
        amr_ggplot(),
        file,
        width_px = w,
        height_px = h,
        res = 96,
        scale = 1
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
