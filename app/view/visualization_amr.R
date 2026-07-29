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
  ],
  shiny,
  shinyWidgets[
    pickerInput,
    pickerOptions,
    radioGroupButtons,
    updatePickerInput
  ],
  stats[setNames],
)
box::use(
  app / logic / amr_plot,
  app / logic / field_labels[field_label, grouped_field_choices],
  app / logic / functions[render_info],
  app /
    logic /
    viz_helpers[
      reset_viz_colors,
      scale_select,
      viz_color,
      collect_input_snapshot,
      apply_input_snapshot,
    ],
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# The three views. "heatmap" is the default because it is the one that answers
# the question the screen was run for: which isolate carries what.
PLOT_MODES <- c(
  `Gene heatmap` = "heatmap",
  `Drug classes` = "classes",
  Prevalence = "prevalence"
)
PLOT_MODE_DEFAULT <- "heatmap"

# How the heatmap's columns are arranged. Grouping and clustering are mutually
# exclusive on that axis (ComplexHeatmap cannot reconcile one dendrogram with a
# categorical split), so they share one control rather than being two switches
# that would silently override each other. See .column_layout in amr_plot.R.
COLUMN_GROUPINGS <- c(
  `Element type` = "element",
  `Drug class` = "class",
  Cluster = "cluster",
  None = "none"
)
COLUMN_GROUPING_DEFAULT <- "element"

# The drug-class matrix has no per-gene metadata to group its columns by, so it
# offers only the two arrangements that mean something there.
CLASS_COLUMN_GROUPINGS <- COLUMN_GROUPINGS[c("Cluster", "None")]

LEVELS <- c(Genes = "gene", `Drug classes` = "class")
LEVEL_DEFAULT <- "gene"

CLUSTER_METHOD_DEFAULT <- "average"
CLUSTER_DISTANCE_DEFAULT <- "binary"

TOP_N_DEFAULT <- 30L
DEND_DEFAULT <- 2
GRID_WIDTH_DEFAULT <- 1
ASPECT_DEFAULT <- 0.65
FONTSIZE_ROW_DEFAULT <- 10
FONTSIZE_COL_DEFAULT <- 10
FONTSIZE_TITLE_DEFAULT <- 14
FONTSIZE_LEGEND_DEFAULT <- 9

PRESENT_COLOR_DEFAULT <- "#66C2A5"
PARTIAL_COLOR_DEFAULT <- "#E5C494"
ABSENT_COLOR_DEFAULT <- "#EFEFEF"
GRID_COLOR_DEFAULT <- "#FFFFFF"
DEND_COLOR_DEFAULT <- "#000000"
TEXT_COLOR_DEFAULT <- "#000000"
BACKGROUND_DEFAULT <- "#FFFFFF"

ANNO_SCALE_DEFAULT <- "Set1"
CLASS_SCALE_DEFAULT <- "Set2"
BAR_SCALE_DEFAULT <- "Set2"

# The sentinel for "no metadata colour strip", matching the Epi engine's
# stratify picker.
NO_ANNOTATION <- ""

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

amr_controls <- function(ns) {
  shiny$tagList(
    navset_tab(
      # Data -------------------------------------------------------------------
      nav_panel(
        "Data",
        icon = shiny$icon("table-cells"),
        accordion(
          open = "View",
          accordion_panel(
            "View",
            icon = shiny$icon("chart-simple"),
            pickerInput(
              ns("amr_mode"),
              "View",
              choices = PLOT_MODES,
              selected = PLOT_MODE_DEFAULT
            ),
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
            )
          ),
          accordion_panel(
            "Elements",
            icon = shiny$icon("dna"),
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
              # are known rather than declared empty and back-filled (an empty
              # <select> initialises bootstrap-select disabled). Same reasoning
              # as the Epi engine's stratify picker.
              shiny$uiOutput(ns("genes_ui")),
              # AMRFinderPlus reports partial and low-identity hits alongside
              # confident ones and `amr_results` keeps both percentages, so the
              # reader can set the bar. Point mutations report neither and are
              # never filtered out by these (see filter_amr_hits).
              shiny$sliderInput(
                ns("amr_min_identity"),
                "Minimum % identity",
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("amr_min_coverage"),
                "Minimum % coverage",
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                ticks = FALSE
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
          )
        )
      ),
      # Layout -----------------------------------------------------------------
      nav_panel(
        "Layout",
        icon = shiny$icon("sliders"),
        accordion(
          open = "Clustering",
          accordion_panel(
            "Clustering",
            icon = shiny$icon("sitemap"),
            shiny$conditionalPanel(
              condition = COND_HEATMAPS,
              ns = ns,
              # Choices are swapped per view (the drug-class matrix has no gene
              # metadata to group by) — see the amr_mode observer, and the
              # reset checklist's bucket 4 for why the reset path must patch it
              # up on a delay.
              pickerInput(
                ns("amr_column_grouping"),
                "Group genes by",
                choices = COLUMN_GROUPINGS,
                selected = COLUMN_GROUPING_DEFAULT
              ),
              input_switch(ns("amr_cluster_rows"), "Cluster isolates", TRUE),
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
              ),
              shiny$sliderInput(
                ns("amr_dend_row"),
                "Isolate dendrogram (cm)",
                min = 0.5,
                max = 6,
                value = DEND_DEFAULT,
                step = 0.5,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("amr_dend_col"),
                "Column dendrogram (cm)",
                min = 0.5,
                max = 6,
                value = DEND_DEFAULT,
                step = 0.5,
                ticks = FALSE
              )
            ),
            shiny$conditionalPanel(
              condition = "input.amr_mode == 'prevalence'",
              ns = ns,
              shiny$helpText(
                class = "amr-help",
                "Clustering applies to the heatmap views. Prevalence bars are",
                "always ranked by count."
              )
            )
          ),
          accordion_panel(
            "Labels & Sizing",
            icon = shiny$icon("ruler-combined"),
            shiny$conditionalPanel(
              condition = COND_HEATMAPS,
              ns = ns,
              input_switch(ns("amr_show_row_names"), "Show isolate names", TRUE)
            ),
            # On (default) the label sizes step down as the matrix grows, which
            # is what master did and had no way to override. The steps stop
            # helping somewhere past a couple of hundred labels, so the sliders
            # take over when this is off.
            input_switch(ns("amr_auto_fontsize"), "Auto label size", TRUE),
            shiny$conditionalPanel(
              condition = "!input.amr_auto_fontsize",
              ns = ns,
              shiny$sliderInput(
                ns("amr_fontsize_row"),
                "Row label size",
                min = 3,
                max = 20,
                value = FONTSIZE_ROW_DEFAULT,
                step = 1,
                ticks = FALSE
              ),
              shiny$conditionalPanel(
                condition = COND_HEATMAPS,
                ns = ns,
                shiny$sliderInput(
                  ns("amr_fontsize_col"),
                  "Column label size",
                  min = 3,
                  max = 20,
                  value = FONTSIZE_COL_DEFAULT,
                  step = 1,
                  ticks = FALSE
                )
              )
            ),
            shiny$conditionalPanel(
              condition = COND_HEATMAPS,
              ns = ns,
              shiny$sliderInput(
                ns("amr_fontsize_title"),
                "Block title size",
                min = 8,
                max = 24,
                value = FONTSIZE_TITLE_DEFAULT,
                step = 1,
                ticks = FALSE
              ),
              shiny$sliderInput(
                ns("amr_grid_width"),
                "Cell border width",
                min = 0,
                max = 3,
                value = GRID_WIDTH_DEFAULT,
                step = 0.25,
                ticks = FALSE
              )
            ),
            shiny$sliderInput(
              ns("amr_fontsize_legend"),
              "Legend text size",
              min = 6,
              max = 18,
              value = FONTSIZE_LEGEND_DEFAULT,
              step = 1,
              ticks = FALSE
            ),
            shiny$sliderInput(
              ns("amr_aspect_ratio"),
              "Aspect ratio",
              min = 0.3,
              max = 1.4,
              value = ASPECT_DEFAULT,
              step = 0.05,
              ticks = FALSE
            )
          )
        )
      ),
      # Annotation -------------------------------------------------------------
      nav_panel(
        "Annotation",
        icon = shiny$icon("map-pin"),
        shiny$conditionalPanel(
          condition = COND_HEATMAPS,
          ns = ns,
          # Server-rendered for the same reason as the gene picker: the choices
          # are this database's metadata columns.
          shiny$uiOutput(ns("anno_ui")),
          scale_select(ns, "amr_anno_scale", categories = "Qualitative"),
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
        shiny$conditionalPanel(
          condition = "input.amr_mode == 'prevalence'",
          ns = ns,
          shiny$helpText(
            class = "amr-help",
            "Prevalence bars are coloured by element type or call section;",
            "pick their palette under Colors."
          )
        )
      ),
      # Colors -------------------------------------------------------------
      nav_panel(
        "Colors",
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
  session_reset = shiny$reactive(0L),
  viz_metadata = shiny$reactive(NULL),
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

    # Bumped to rebuild the server-rendered pickers (Reset settings).
    genes_rebuild <- shiny$reactiveVal(0L)
    anno_rebuild <- shiny$reactiveVal(0L)
    # TRUE for exactly one rebuild: a reset must force the picker back to its
    # default, whereas a plain data-driven re-render keeps the current choice so
    # a deliberate selection sticks. Consumed on read — see the Epi engine's
    # stratify_force_default for the full reasoning.
    genes_force_default <- shiny$reactiveVal(FALSE)
    anno_force_default <- shiny$reactiveVal(FALSE)

    mode <- function() input$amr_mode %||% PLOT_MODE_DEFAULT

    # --- data ---------------------------------------------------------------

    amr_hits <- shiny$reactive({
      shiny$req(db_path())
      amr_plot$load_amr_hits(db_path())
    })

    amr_sections <- shiny$reactive({
      shiny$req(db_path())
      amr_plot$load_amr_sections(db_path())
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
        selected_genes()
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
        top_n = input$amr_top_n %||% TOP_N_DEFAULT
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
      choices <- amr_plot$amr_gene_choices(amr_hits())
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
      pickerInput(
        ns("amr_genes"),
        "Genes",
        choices = choices,
        selected = if (length(keep)) keep else all_genes,
        multiple = TRUE,
        width = "100%",
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search genes ...",
          size = 10,
          title = "None",
          selectedTextFormat = "count > 2",
          countSelectedText = "{0} of {1} genes",
          container = "body"
        )
      )
    })

    # Candidate fields for the isolate colour strip: anything except the isolate
    # id itself.
    anno_fields <- shiny$reactive({
      meta <- viz_metadata()
      if (is.null(meta) || !length(names(meta))) {
        return(character())
      }
      setdiff(names(meta), "isolate")
    })

    output$anno_ui <- shiny$renderUI({
      render_info("visualization_amr anno_ui")
      fields <- anno_fields()
      anno_rebuild()
      if (!length(fields)) {
        return(NULL)
      }
      prev <- shiny$isolate(input$amr_anno_field)
      force_default <- shiny$isolate(anno_force_default())
      if (force_default) {
        anno_force_default(FALSE)
      }
      meta <- viz_metadata()
      # Categorised, human-readable labels (the derived classical-MLST and AMR
      # columns get their own groups); the value stays the raw column name.
      pickerInput(
        ns("amr_anno_field"),
        "Colour isolates by",
        choices = c(
          list(`No annotation` = NO_ANNOTATION),
          grouped_field_choices(
            fields,
            attr(meta, "mlst_cols"),
            attr(meta, "amr_cols")
          )
        ),
        selected = if (!force_default && isTRUE(prev %in% fields)) {
          prev
        } else {
          NO_ANNOTATION
        },
        width = "100%",
        options = pickerOptions(
          size = 10,
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search fields ...",
          container = "body"
        )
      )
    })

    # The chosen field's values, keyed by isolate, or NULL when nothing is
    # mapped. Isolates the field is empty for are labelled rather than dropped
    # (see .row_annotation in amr_plot.R).
    anno_values <- shiny$reactive({
      field <- input$amr_anno_field
      if (is.null(field) || identical(field, NO_ANNOTATION)) {
        return(NULL)
      }
      meta <- viz_metadata()
      if (is.null(meta) || !field %in% names(meta)) {
        return(NULL)
      }
      setNames(as.character(meta[[field]]), meta$isolate)
    })

    anno_label <- shiny$reactive({
      field <- input$amr_anno_field
      if (is.null(field) || identical(field, NO_ANNOTATION)) {
        return(NULL)
      }
      field_label(field)
    })

    # Restrict a colour-scale picker to the palettes that can carry the number
    # of categories currently mapped to it, and move the selection when the one
    # in force no longer can. Mirrors apply_scale_choices() in the Epi engine,
    # generalised over the picker id since this engine has three of them.
    apply_scale_choices <- function(id, n, default, force_default = FALSE) {
      choices <- amr_plot$amr_scale_choices(max(1L, as.integer(n)))
      selected <- if (force_default) {
        amr_plot$amr_fit_scale(default, n)
      } else {
        amr_plot$amr_fit_scale(input[[id]], n)
      }
      updatePickerInput(
        session,
        id,
        choices = choices,
        selected = selected
      )
    }

    shiny$observe({
      vals <- anno_values()
      n <- if (is.null(vals)) 1L else length(unique(vals))
      apply_scale_choices("amr_anno_scale", n, ANNO_SCALE_DEFAULT)
    })

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

    # The drug-class matrix has no gene metadata to group its columns by, so
    # only Cluster / None mean anything there. Swapping the choices (rather than
    # leaving dead options in the list) is what keeps the control honest; it
    # makes this a bucket-4 input for the reset path.
    shiny$observeEvent(
      input$amr_mode,
      {
        choices <- if (identical(mode(), "classes")) {
          CLASS_COLUMN_GROUPINGS
        } else {
          COLUMN_GROUPINGS
        }
        current <- input$amr_column_grouping
        updatePickerInput(
          session,
          "amr_column_grouping",
          choices = choices,
          selected = if (isTRUE(current %in% choices)) {
            current
          } else if (identical(mode(), "classes")) {
            "none"
          } else {
            COLUMN_GROUPING_DEFAULT
          }
        )
      },
      ignoreNULL = FALSE
    )

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

      # Bucket 5: both pickers are rendered by renderUI, so shinyjs::reset() has
      # no page-load value to restore them from — rebuild them instead, forcing
      # the default for this one rebuild rather than preserving the current
      # choice. Set before the bump so the re-render sees it.
      genes_force_default(TRUE)
      anno_force_default(TRUE)
      genes_rebuild(genes_rebuild() + 1L)
      anno_rebuild(anno_rebuild() + 1L)

      # Bucket 4: controls whose *choices* are swapped in at runtime. Deferred
      # past shinyjs::reset()'s own asynchronous, stale restoration, which would
      # otherwise land a moment later and overwrite an immediate correction.
      shinyjs::delay(400, {
        updatePickerInput(
          session,
          "amr_column_grouping",
          choices = COLUMN_GROUPINGS,
          selected = COLUMN_GROUPING_DEFAULT
        )
        apply_scale_choices(
          "amr_anno_scale",
          1L,
          ANNO_SCALE_DEFAULT,
          force_default = TRUE
        )
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

      generated(TRUE)
    })

    # --- plot ---------------------------------------------------------------

    # Label sizes: NULL lets the logic layer fit them to the matrix (master's
    # step function), which is what the "Auto label size" switch means.
    fontsize_row <- function() {
      if (isTRUE(input$amr_auto_fontsize)) {
        NULL
      } else {
        input$amr_fontsize_row %||% FONTSIZE_ROW_DEFAULT
      }
    }
    fontsize_col <- function() {
      if (isTRUE(input$amr_auto_fontsize)) {
        NULL
      } else {
        input$amr_fontsize_col %||% FONTSIZE_COL_DEFAULT
      }
    }

    heatmap_opts <- function() {
      list(
        present_color = input$amr_present_color %||% PRESENT_COLOR_DEFAULT,
        partial_color = input$amr_partial_color %||% PARTIAL_COLOR_DEFAULT,
        absent_color = input$amr_absent_color %||% ABSENT_COLOR_DEFAULT,
        grid_color = input$amr_grid_color %||% GRID_COLOR_DEFAULT,
        dend_color = input$amr_dend_color %||% DEND_COLOR_DEFAULT,
        text_color = input$amr_text_color %||% TEXT_COLOR_DEFAULT,
        grid_width = input$amr_grid_width %||% GRID_WIDTH_DEFAULT,
        column_grouping = input$amr_column_grouping %||%
          COLUMN_GROUPING_DEFAULT,
        cluster_rows = isTRUE(input$amr_cluster_rows),
        cluster_distance = input$amr_cluster_distance %||%
          CLUSTER_DISTANCE_DEFAULT,
        cluster_method = input$amr_cluster_method %||% CLUSTER_METHOD_DEFAULT,
        dend_row = input$amr_dend_row %||% DEND_DEFAULT,
        dend_col = input$amr_dend_col %||% DEND_DEFAULT,
        fontsize_row = fontsize_row(),
        fontsize_col = fontsize_col(),
        fontsize_title = input$amr_fontsize_title %||% FONTSIZE_TITLE_DEFAULT,
        fontsize_legend = input$amr_fontsize_legend %||%
          FONTSIZE_LEGEND_DEFAULT,
        show_row_names = isTRUE(input$amr_show_row_names),
        show_class_anno = isTRUE(input$amr_show_class_anno),
        class_scale = input$amr_class_scale %||% CLASS_SCALE_DEFAULT,
        anno_values = anno_values(),
        anno_label = anno_label(),
        anno_scale = input$amr_anno_scale %||% ANNO_SCALE_DEFAULT
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
            fontsize_row = fontsize_row(),
            fontsize_legend = input$amr_fontsize_legend %||%
              FONTSIZE_LEGEND_DEFAULT,
            n_isolates = length(isolates())
          )
        ))
      }

      if (identical(mode(), "classes")) {
        mat <- class_mat()
        shiny$req(ncol(mat) > 0)
        return(amr_plot$amr_as_ggplot(
          amr_plot$build_amr_class_heatmap(mat, heatmap_opts()),
          background
        ))
      }

      mat <- presence_mat()
      shiny$req(ncol(mat) > 0)
      amr_plot$amr_as_ggplot(
        amr_plot$build_amr_heatmap(mat, heatmap_opts()),
        background
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
        class = "viz-plot-stage amr-stage",
        id = ns("plot_stage"),
        prompt,
        loading,
        shiny$plotOutput(ns("amr_plot"), height = "auto"),
        # Hidden target the export action button clicks to start the download.
        shiny$div(
          class = "d-none",
          shiny$downloadButton(ns("download_amr"), "Download plot")
        )
      )
    })

    shiny$observeEvent(
      generated(),
      {
        shinyjs::toggle(id = "viz_prompt", condition = !isTRUE(generated()))
      },
      ignoreNULL = FALSE
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
        h <- as.integer(w * (input$amr_aspect_ratio %||% ASPECT_DEFAULT))
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

    output$download_amr <- shiny$downloadHandler(
      filename = function() {
        paste0(
          Sys.Date(),
          "_amr_",
          mode(),
          ".",
          input$amr_filetype %||% "png"
        )
      },
      content = function(file) {
        amr_plot$save_amr_plot(
          amr_ggplot(),
          file,
          input$amr_filetype %||% "png",
          input$amr_aspect_ratio %||% ASPECT_DEFAULT
        )
      }
    )

    # The export tab uses an action button; route it to the hidden download
    # link.
    shiny$observeEvent(input$amr_download, {
      if (!isTRUE(generated())) {
        shiny$showNotification(
          "Generate a plot before saving it.",
          type = "warning"
        )
        return()
      }
      shinyjs::click("download_amr")
    })

    # `plot_area` is a cheap renderUI gating the "press Generate" prompt, and
    # the plot output has to bind through it, so it stays live while hidden.
    shiny$outputOptions(output, "plot_area", suspendWhenHidden = FALSE)
    # Server-side image with no client state to lose, so it may suspend while
    # its plot tab is in the background — as the Tree and the Epi curve do.
    shiny$outputOptions(output, "amr_plot", suspendWhenHidden = TRUE)

    # ---- Dashboard "Save Analysis" contract ---------------------------------
    snapshot <- shiny$reactive(collect_input_snapshot(input, "amr_"))

    restore <- function(vals) {
      apply_input_snapshot(
        session,
        vals,
        switches = c(
          "amr_cluster_rows",
          "amr_show_row_names",
          "amr_auto_fontsize",
          "amr_show_class_anno"
        ),
        selects = c(
          "amr_mode",
          "amr_column_grouping",
          "amr_cluster_distance",
          "amr_cluster_method",
          "amr_anno_scale",
          "amr_class_scale",
          "amr_bar_scale",
          "amr_filetype"
        ),
        sliders = c(
          "amr_top_n",
          "amr_min_identity",
          "amr_min_coverage",
          "amr_dend_row",
          "amr_dend_col",
          "amr_fontsize_row",
          "amr_fontsize_col",
          "amr_fontsize_title",
          "amr_fontsize_legend",
          "amr_grid_width",
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
        radio_groups = "amr_level",
        # The gene and annotation pickers are server-rendered (bucket 5,
        # rebuilt on a counter), so these are best-effort: they land only once
        # the control exists. The element and section pickers are declared in
        # the UI and restore straight away.
        pickers = c(
          "amr_elements",
          "amr_sections",
          "amr_genes",
          "amr_anno_field"
        )
      )
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
      thumb_data = NULL
    )
  })
}
