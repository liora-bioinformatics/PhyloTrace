# app/view/typing.R

box::use(
  shiny[
    NS,
    moduleServer,
    reactive,
    reactiveVal,
    reactiveValues,
    observe,
    observeEvent,
    outputOptions,
    req,
    invalidateLater,
    renderUI,
    uiOutput,
    renderText,
    isolate,
    textOutput,
    verbatimTextOutput,
    div,
    p,
    span,
    tags,
    icon,
    actionButton,
    numericInput,
    radioButtons,
    checkboxInput,
    showNotification,
    showModal,
    modalDialog,
    modalButton,
    HTML
  ],
  stats[setNames],
  bslib[
    page_sidebar,
    sidebar,
    accordion,
    accordion_panel,
    accordion_panel_close,
    as_fill_item,
  ],
  shinyFiles[
    shinyFilesButton,
    shinyFileChoose,
    parseFilePaths,
    shinyDirButton,
    shinyDirChoose,
    parseDirPath,
  ],
  shinyWidgets[progressBar, updateProgressBar],
  shinyjs[
    runjs,
    useShinyjs,
    disabled,
    toggleState,
    disable,
    enable,
    addClass,
    removeClass
  ],
  DT[DTOutput, renderDT, datatable, dataTableProxy, replaceData, JS],
  htmltools[htmlEscape],
  fs[path_home],
)
box::use(
  app / logic / functions[render_info],
  app /
    logic /
    pymlst[
      conda_env,
      start_typing,
      parse_typing_log,
      parse_clamlst_meta,
      db_species,
      store_clamlst_results,
      existing_strains,
      scheme_size,
    ],
  app /
    logic /
    amr[
      amr_species,
      parse_amr_meta,
      store_amr_results,
    ],
)

# Accepted assembly extensions, mirroring the glob in loop-pymlst.sh.
genome_pattern <- "\\.(fasta|fa|fna)$"

# Bootstrap badge class per typing outcome. Bootstrap 5 (shipped with bslib)
# provides these `text-bg-*` utilities, so no custom CSS is needed.
status_badge <- function(status) {
  cls <- switch(
    status,
    Added = "text-bg-success",
    Duplicate = "text-bg-secondary",
    Incompatible = "text-bg-danger",
    Error = "text-bg-danger",
    Running = "text-bg-info",
    Pending = "text-bg-light text-dark",
    Stopped = "text-bg-warning",
    New = "text-bg-success",
    "Already present" = "text-bg-warning",
    "text-bg-light"
  )
  sprintf('<span class="badge %s">%s</span>', cls, status)
}

# Scheme-completeness (QC) badge: share of the scheme's loci called for a
# strain. Green when near-complete, amber for a mild shortfall, red when a large
# fraction is missing (a sign of a poor assembly or species mismatch).
completeness_badge <- function(pct) {
  if (is.na(pct)) {
    return("—")
  }
  cls <- if (pct >= 99) {
    "text-bg-success"
  } else if (pct >= 90) {
    "text-bg-warning"
  } else {
    "text-bg-danger"
  }
  sprintf('<span class="badge %s">%.1f%%</span>', cls, pct)
}

# Layout of the "Typing Results" table: one entry per column, in display order.
# `name` is the header label, `tip` the hover explanation, `class` an optional
# CSS class (see main.scss) that reserves horizontal space for the columns whose
# cell content changes while a run is in progress - without it the widest badge
# of the moment sets the column width and every column to its right shifts on
# each polling tick.
#
# Each of the three analyses that ride along in a run gets its own column
# reporting that analysis' own state and outcome: cgMLST allele calling
# (`wgMLST add`), classical 7-gene MLST (`claMLST search`) and AMR screening
# (abritamr). They appear in execution order, and the allele-calling metrics
# (loci found through completeness) sit directly behind the cgMLST column they
# belong to rather than after all three analyses.
results_columns <- list(
  list(
    name = "Strain",
    class = "pt-col-strain",
    tip = "Assembly file name, without extension."
  ),
  list(
    name = "cgMLST",
    class = "pt-col-cgmlst",
    tip = paste(
      "Allele calling against the loaded scheme.",
      "Added, Duplicate, Incompatible (no core gene matched) or Error."
    )
  ),
  list(
    name = "Loci found",
    tip = "Scheme loci located in the assembly by BLAT."
  ),
  list(
    name = "Alleles added",
    tip = "Loci whose allele was stored for this strain."
  ),
  list(
    name = "Partial",
    tip = "Loci matched over less than their full length."
  ),
  list(
    name = "Filled",
    tip = "Partial loci completed from the assembly and kept."
  ),
  list(
    name = "Removed",
    tip = "Loci dropped for low coverage or a failed coding-sequence test."
  ),
  list(
    name = "Completeness",
    tip = paste(
      "Share of the scheme's loci called.",
      "A low value points to a poor assembly or a species mismatch."
    )
  ),
  list(
    name = "MLST",
    class = "pt-col-mlst",
    tip = paste(
      "Classical 7-gene MLST: the Sequence Type, Novel (complete but",
      "unregistered profile) or Partial (some loci not called)."
    )
  ),
  list(
    name = "AMR",
    class = "pt-col-amr",
    tip = paste(
      "Resistance screening (abritamr). Hits count all detected elements:",
      "acquired resistance, virulence and stress/metal genes."
    )
  ),
  list(
    name = "Finished",
    class = "pt-col-time",
    tip = "Clock time at which the strain's last step ended."
  ),
  list(
    name = "Elapsed",
    tip = "Total time for this strain: cgMLST, classical MLST and AMR."
  ),
  list(
    name = "Detail",
    tip = "What the strain is doing, or why it was not added."
  )
)

# Header row for the results table, built as a DT `container` so each header
# cell can carry its explanation (a Bootstrap tooltip, falling back to the
# browser's native `title` tooltip) and its width-reserving class.
results_header <- tags$table(
  class = "display",
  tags$thead(tags$tr(
    lapply(results_columns, function(col) {
      tags$th(
        class = col$class,
        title = col$tip,
        `data-bs-toggle` = "tooltip",
        `data-bs-placement` = "bottom",
        col$name
      )
    })
  ))
)

# Attach a Bootstrap tooltip to every header cell that carries an explanation.
# Runs once per render (the header survives replaceData, so the live polling
# updates never re-trigger it). `container: body` keeps the tooltip out of the
# accordion body, which scrolls and would otherwise clip it. Falls back to the
# native title tooltip if Bootstrap's JS is unavailable.
header_tooltips <- JS(
  "function(settings, json) {",
  "  if (!window.bootstrap || !window.bootstrap.Tooltip) return;",
  "  $(this.api().table().header())",
  "    .find('th[data-bs-toggle=\"tooltip\"]')",
  "    .each(function() {",
  "      window.bootstrap.Tooltip.getOrCreateInstance(",
  "        this, { container: 'body', trigger: 'hover' });",
  "    });",
  "}"
)

# One step of the typing pipeline, as a sidebar accordion panel. Every genome
# goes through the same steps in this order - pick the input, then the three
# analyses - and each panel owns the settings for its step, so the accordion
# is the process: numbered, in execution order, and named (from step 2 on)
# exactly like the results table's columns, which ties a setting to the step -
# and the column - it governs.
#
# `info_id` is the namespaced id of a click target that opens this step's
# in-depth explanation as a modal (server-side observeEvent, see
# step_info_modal below) rather than a hover tooltip or an always-visible
# paragraph: a step's full explanation is longer than a tooltip comfortably
# holds, and the sidebar is only 320px wide, so keeping it opt-in is what keeps
# the panel a single line tall. `.tooltip-bttn` is the existing "looks like an
# icon, is a button" style already used elsewhere in the app (see
# visualization_plot.R's missing-values help); `.info-modal` (main.scss) is its
# matching modal style.
#
# bslib renders `title` *inside* its own accordion-toggle <button>, which rules
# out a real shiny::actionButton() here twice over:
#
# 1. Browsers do not allow a <button> nested inside another <button> - the outer
#    one is silently closed the moment the HTML parser meets the inner one,
#    corrupting the whole panel's markup before any JS runs. So the trigger is
#    a <span> carrying its input id in a data attribute.
#
# 2. That span is still a *descendant* of the toggle button, and Bootstrap's
#    collapse plugin catches clicks with one delegated listener that walks the
#    clicked element's ancestors for `[data-bs-toggle="collapse"]`. Clicking
#    the icon would therefore open the modal *and* toggle the panel.
#    step_info_click_script below stops that; see its own comment.
step_panel <- function(number, label, info_id, ...) {
  accordion_panel(
    title = tags$span(
      class = "typing-step-title",
      span(label),
      tags$span(
        class = "tooltip-bttn typing-step-info",
        `data-info-id` = info_id,
        role = "button",
        tabindex = "0",
        `aria-label` = "More information",
        onkeydown = paste(
          "if(event.key==='Enter'||event.key===' '){",
          "event.preventDefault();this.click();}"
        ),
        icon("circle-info")
      )
    ),
    value = label,
    icon = span(class = "typing-step-num", number),
    ...
  )
}

# Makes each step's info icon open its modal without also toggling the panel it
# sits in.
#
# Bootstrap's collapse plugin listens for clicks on `document`, so a listener
# on the icon itself is too late to head it off - by the time the event reaches
# the icon in the bubble phase, the panel has already toggled. The one place
# that runs *earlier* is the capture phase on `window`, which sits above
# `document` in the capture path: stopping propagation there means the click
# never reaches Bootstrap's listener at all.
#
# That also stops the click reaching the icon, so Shiny's own click binding
# would never see it either - hence setting the input value directly here. The
# counter mirrors what actionButton() reports, and `priority: 'event'` makes
# every click fire observeEvent even if the value were to repeat.
#
# Keeping the icon in its natural place in the header (rather than repositioning
# it out of the button) is what keeps it painted correctly across collapse and
# re-expand.
step_info_click_script <- tags$script(HTML(
  "window.addEventListener('click', function(e) {",
  "  var t = e.target.closest && e.target.closest('.typing-step-info');",
  "  if (!t) return;",
  "  e.stopPropagation();",
  "  e.preventDefault();",
  "  if (!window.Shiny || !window.Shiny.setInputValue) return;",
  "  var n = (parseInt(t.dataset.clicks || '0', 10) || 0) + 1;",
  "  t.dataset.clicks = n;",
  "  window.Shiny.setInputValue(t.dataset.infoId, n, {priority: 'event'});",
  "}, true);"
))

# Human-readable per-strain analysis duration: seconds -> "3.9s" or "1m 04s".
format_elapsed <- function(secs) {
  if (is.na(secs)) {
    return("—")
  }
  if (secs < 60) {
    return(sprintf("%.1fs", secs))
  }
  sprintf("%dm %02ds", secs %/% 60, round(secs %% 60))
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    useShinyjs(),
    fillable = TRUE,
    sidebar = sidebar(
      title = NULL,
      width = 320,
      # The pipeline, one panel per step, in the order a genome runs them.
      # Genome input is step 1 - picking the assemblies is as much a required
      # step as any analysis that follows, so it gets a number and a panel like
      # the rest rather than sitting above the accordion as a control of its
      # own. All panels start open: with `multiple = TRUE` they don't compete
      # for space the way the results-table accordion's log/results/selection
      # panels do, so there is no need to hide any of them by default.
      accordion(
        class = "typing-steps",
        open = TRUE,
        multiple = TRUE,
        step_panel(
          1,
          "Genome input",
          ns("info_genome"),
          # File and folder do the same job - resolving to a list of
          # assemblies - so they share one row rather than each claiming a
          # full-width row of its own.
          div(
            class = "row g-2",
            div(
              class = "col-6",
              shinyFilesButton(
                ns("genome_file"),
                "File",
                title = "Choose a genome assembly",
                icon = icon("file-lines"),
                buttonType = "default",
                multiple = FALSE,
                class = "w-100",
                root = path_home()
              )
            ),
            div(
              class = "col-6",
              shinyDirButton(
                ns("genome_dir"),
                "Folder",
                title = "Choose a folder of assemblies",
                icon = icon("folder-open"),
                buttonType = "default",
                class = "w-100",
                root = path_home()
              )
            )
          )
        ),
        step_panel(
          2,
          "cgMLST",
          ns("info_cgmlst"),
          # The scheme is what cgMLST is called against, so its size (the
          # completeness metric's denominator) is shown here rather than as a
          # standalone sidebar line with no obvious home.
          uiOutput(ns("scheme_info")),
          # Both thresholds are fractions in [0, 1], so they pair naturally in
          # one row - and the panel stays a single line tall when open.
          div(
            class = "row g-2",
            div(
              class = "col-6",
              numericInput(
                ns("identity"),
                "Min. identity",
                value = 0.95,
                min = 0,
                max = 1,
                step = 0.01,
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              numericInput(
                ns("coverage"),
                "Min. coverage",
                value = 0.9,
                min = 0,
                max = 1,
                step = 0.01,
                width = "100%"
              )
            )
          )
        ),
        step_panel(
          3,
          "Classical MLST",
          ns("info_mlst"),
          radioButtons(
            ns("cla_repo"),
            "Scheme source",
            choices = c("PubMLST" = "pubmlst", "Pasteur" = "pasteur"),
            selected = "pubmlst",
            inline = TRUE
          )
        ),
        step_panel(
          4,
          "AMR screening",
          ns("info_amr"),
          checkboxInput(
            ns("run_amr"),
            "Run screening",
            value = TRUE
          )
        )
      ),
      step_info_click_script,
      disabled(actionButton(
        ns("start"),
        "Start Analysis",
        icon = icon("play")
      )),
      disabled(actionButton(
        ns("terminate"),
        "Terminate",
        icon = icon("stop"),
        # pt-no-lock: the one control that must stay clickable while a long run
        # is in progress, so it opts out of the global input shield (busy-shield.js).
        class = "btn-danger pt-no-lock"
      ))
    ),
    div(
      progressBar(
        ns("progress"),
        value = 0,
        total = 1,
        display_pct = TRUE,
        status = "primary",
        striped = TRUE,
        title = "Typing progress"
      ),
      textOutput(ns("status_line"))
    ),
    as_fill_item(
      accordion(
        id = ns("typing_accordion"),
        open = "Selected Genomes",
        multiple = FALSE,
        class = "typing-accordion",
        accordion_panel(
          title = tags$span(
            class = "typing-accordion-title",
            span("Selected Genomes"),
            uiOutput(ns("selection_summary"), inline = TRUE)
          ),
          value = "Selected Genomes",
          icon = icon("dna"),
          DTOutput(ns("selection_table"))
        ),
        accordion_panel(
          "Process Log",
          icon = icon("terminal"),
          verbatimTextOutput(ns("log"))
        ),
        accordion_panel(
          "Results",
          icon = icon("table"),
          DTOutput(ns("results_table"))
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Typing reactive values
    Typing <- reactiveValues(
      genome_input = NULL,
      files = character(0),
      strains = character(0),
      # Subset of files / strains actually piped into a run: assemblies already
      # present in the database are dropped, so only new genomes are typed.
      queued_files = character(0),
      queued_strains = character(0),
      proc = NULL,
      log_file = NULL,
      # Path to this run's interim claMLST reference DB (built by the typing
      # script, read once for reference sequences / metadata, then deleted).
      cla_db = NULL,
      # Base output dir for this run's AMR screens (one subdir per strain, read
      # once on finalize into amr_results / amr_summary, then deleted).
      amr_out = NULL,
      status = "idle",
      results = NULL,
      # Whether the current run includes classical MLST / AMR screening. Kept
      # separate from cla_db / amr_out (which are cleared on finalize) so the
      # results table can keep telling those columns apart from "not run" once
      # the run is over.
      cla_enabled = FALSE,
      amr_enabled = FALSE,
      terminated = FALSE,
      refresh = 0L
    )
    log_text <- reactiveVal("")

    # Delete an interim claMLST reference DB (and any SQLite side files). Safe to
    # call with NULL / NA / a non-existent path.
    cleanup_cla_db <- function(path) {
      if (!is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path)) {
        unlink(c(path, paste0(path, c("-wal", "-shm"))))
      }
    }

    # Delete this run's AMR output directory (per-strain abritamr outputs). Safe
    # to call with NULL / NA / a non-existent path.
    cleanup_amr_out <- function(path) {
      if (!is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path)) {
        unlink(path, recursive = TRUE)
      }
    }

    # Non-reactive mirror of the live typing process. onSessionEnded() runs
    # outside the reactive context (it cannot read Typing$proc), so we keep a
    # plain handle here to guarantee the process tree is killed when the app
    # window closes - otherwise pymlst is orphaned and keeps the database
    # locked, breaking the next launch.
    live_proc <- NULL
    # Non-reactive mirror of this run's interim claMLST DB path, for the same
    # out-of-reactive-context cleanup on window close.
    live_cla_db <- NULL
    # Non-reactive mirror of this run's AMR output dir, cleaned up the same way.
    live_amr_out <- NULL
    session$onSessionEnded(function() {
      if (!is.null(live_proc) && live_proc$is_alive()) {
        tryCatch(live_proc$kill_tree(), error = function(e) NULL)
      }
      cleanup_cla_db(live_cla_db)
      cleanup_amr_out(live_amr_out)
    })

    # Reset server reactives on session reset
    observeEvent(
      session_reset(),
      {
        if (!is.null(Typing$proc) && Typing$proc$is_alive()) {
          Typing$terminated <- TRUE
          tryCatch(Typing$proc$kill_tree(), error = function(e) NULL)
        }
        live_proc <<- NULL
        cleanup_cla_db(live_cla_db)
        live_cla_db <<- NULL
        cleanup_amr_out(live_amr_out)
        live_amr_out <<- NULL
        Typing$genome_input <- NULL
        Typing$files <- character(0)
        Typing$strains <- character(0)
        Typing$queued_files <- character(0)
        Typing$queued_strains <- character(0)
        Typing$proc <- NULL
        Typing$log_file <- NULL
        Typing$cla_db <- NULL
        Typing$amr_out <- NULL
        Typing$cla_enabled <- FALSE
        Typing$amr_enabled <- FALSE
        Typing$status <- "idle"
        Typing$results <- NULL
        Typing$terminated <- FALSE
        Typing$refresh <- 0L
        log_text("")
        updateProgressBar(session, "progress", value = 0, total = 1)
        runjs(sprintf(
          "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
          ns("progress")
        ))
        runjs(sprintf(
          "(function(){
           var acc = document.getElementById('%s');
           if (!acc) return;
           var item = acc.querySelector('[data-value=\"Selected Genomes\"]');
           if (!item) return;
           var btn = item.querySelector('.accordion-button.collapsed');
           if (btn) btn.click();
         })();",
          ns("typing_accordion")
        ))
      },
      ignoreInit = TRUE
    )

    # Define roots
    roots <- c(Home = path_home(), Root = "/")

    # Condition helper
    or_default <- function(x, default) {
      if (is.null(x) || length(x) != 1 || is.na(x)) default else x
    }

    # Valid db checker reactive
    valid_db <- reactive({
      path <- db_path()
      !is.null(path) &&
        length(path) == 1 &&
        is.character(path) &&
        !is.na(path) &&
        file.exists(path)
    })

    # A selection (file or folder) resolves to the genome_input handed to the
    # typing script plus the ordered list of assemblies / strain names it will
    # produce (strain name = file name without extension, as in loop-pymlst.sh).
    set_selection <- function(path) {
      if (identical(Typing$status, "running")) {
        return(invisible())
      }

      files <- if (dir.exists(path)) {
        list.files(path, pattern = genome_pattern, full.names = TRUE)
      } else {
        path
      }

      Typing$genome_input <- path
      Typing$files <- files
      Typing$strains <- sub("\\.[^.]*$", "", basename(files))
      Typing$results <- NULL
      log_text("")
      updateProgressBar(session, "progress", value = 0, total = 1)
    }

    # File / folder choosers
    shinyFileChoose(
      input,
      "genome_file",
      roots = roots,
      defaultRoot = "Home",
      filetypes = c("fasta", "fa", "fna"),
      session = session
    )
    shinyDirChoose(
      input,
      "genome_dir",
      roots = roots,
      defaultRoot = "Home",
      session = session
    )

    observeEvent(input$genome_file, {
      path <- parseFilePaths(roots, input$genome_file)$datapath
      req(length(path), is.character(path), file.exists(path))
      set_selection(path)
    })

    observeEvent(input$genome_dir, {
      path <- parseDirPath(roots, input$genome_dir)
      req(length(path), is.character(path), dir.exists(path))
      set_selection(path)
    })

    # Strains already in the loaded database; re-queried after each run so the
    # selection table's "Already present" flags stay current.
    existing <- reactive({
      Typing$refresh
      existing_strains(db_path())
    })

    # Total loci in the loaded scheme (denominator of the completeness metric).
    scheme_total <- reactive(scheme_size(db_path()))

    # Loaded-scheme summary shown to the user (total number of loci).
    output$scheme_info <- renderUI({
      total <- scheme_total()
      if (is.na(total) || total == 0) {
        return(NULL)
      }
      div(
        class = "text-muted small",
        "Scheme size: ",
        tags$strong(format(total, big.mark = ",")),
        " loci"
      )
    })

    # In-depth explanation for one sidebar step, opened by its info button
    # (see step_panel). A modal rather than a tooltip since the explanation is
    # longer than a hover bubble comfortably holds; `.info-modal` (main.scss) is
    # the same "large tooltip" style already used elsewhere in the app (e.g.
    # visualization_plot.R's missing-values help).
    step_info_modal <- function(title, ...) {
      showModal(div(
        class = "info-modal",
        modalDialog(
          title = title,
          tags$dl(...),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      ))
    }

    observeEvent(input$info_genome, {
      step_info_modal(
        "Genome input",
        tags$dt("Supported formats"),
        tags$dd("Assembled genomes as .fasta, .fa or .fna files."),
        tags$dt("Single file or folder"),
        tags$dd(
          "Pick one assembly, or a folder - scanned non-recursively - of",
          "several."
        ),
        tags$dt("Duplicate detection"),
        tags$dd(
          "Genomes already in the database - matched by strain name, the",
          "file name without extension - are detected automatically and",
          "skipped."
        ),
        tags$dt("Pipeline"),
        tags$dd("Every selected genome then runs the three steps below, in order.")
      )
    })

    observeEvent(input$info_cgmlst, {
      step_info_modal(
        "cgMLST",
        tags$dt("Allele calling"),
        tags$dd(
          "Each genome is BLAT-searched against the loaded scheme and added",
          "via wgMLST add."
        ),
        tags$dt("Min. identity"),
        tags$dd(
          "Minimum sequence identity for BLAT to call a locus: the fraction",
          "of identical bases between the assembly and the reference allele",
          "(-minIdentity). Raise it for stricter matches; lower it to recover",
          "more divergent alleles."
        ),
        tags$dt("Min. coverage"),
        tags$dd(
          "Minimum fraction of a reference locus the alignment must span to",
          "keep a hit (aligned length / locus length). Hits below this are",
          "discarded; partial hits above it are aligned and gap-filled."
        ),
        tags$dt("Result columns"),
        tags$dd(
          "Drives the cgMLST, Loci found, Alleles added, Partial, Filled,",
          "Removed and Completeness columns in Typing Results."
        )
      )
    })

    observeEvent(input$info_mlst, {
      step_info_modal(
        "Classical MLST",
        tags$dt("Sequence Type"),
        tags$dd(
          "Derived per genome right after allele calling (claMLST search),",
          "using the same identity / coverage thresholds as cgMLST."
        ),
        tags$dt("Scheme source"),
        tags$dd(
          "Repository the 7-gene scheme is fetched from. If the chosen",
          "repository has no scheme for the species, the other is tried",
          "automatically; if neither has it, the ST is left blank."
        ),
        tags$dt("When it's skipped"),
        tags$dd("No classical MLST is attempted when the scheme has no resolvable species.")
      )
    })

    observeEvent(input$info_amr, {
      step_info_modal(
        "AMR screening",
        tags$dt("Screening"),
        tags$dd(
          "Runs last, after cgMLST and classical MLST, with abritamr /",
          "AMRFinderPlus."
        ),
        tags$dt("Detected elements"),
        tags$dd(
          "Acquired resistance genes, virulence factors and stress/metal",
          "elements. Resistance point mutations are added when the scheme's",
          "species is supported."
        ),
        tags$dt("Reference database"),
        tags$dd(
          "Matches are looked up against NCBI's curated AMRFinderPlus",
          "reference gene database, so hits come back as standardised gene",
          "names rather than raw sequence matches."
        ),
        tags$dt("Storage"),
        tags$dd("Results are stored in the database (amr_results / amr_summary).")
      )
    })

    # Selected Genomes accordion header summary: counts of files that would
    # actually be queued for typing vs. ones already in the database (mirrors
    # the "New" / "Already present" split shown per-row in the selection table).
    output$selection_summary <- renderUI({
      n_total <- length(Typing$strains)
      if (n_total == 0) {
        return(NULL)
      }
      n_present <- sum(Typing$strains %in% existing())
      n_new <- n_total - n_present
      tags$span(
        class = "typing-accordion-summary",
        tags$span(class = "badge text-bg-success", paste(n_new, "New")),
        if (n_present > 0) {
          tags$span(
            class = "badge text-bg-warning",
            paste(n_present, "Already present")
          )
        }
      )
    })
    outputOptions(output, "selection_summary", suspendWhenHidden = FALSE)

    # Enable/disable controls based on current status.
    # While running: lock file/folder pickers (needs both disable() + CSS class
    # because shinyFiles buttons are not standard inputs) and parameter inputs.
    # Terminate mirrors the running state; Start requires a valid ready state.
    observe({
      running <- identical(Typing$status, "running")
      ready <- isTRUE(valid_db()) &&
        length(Typing$strains) > 0 &&
        !running
      toggleState("start", condition = ready)
      toggleState("terminate", condition = running)

      if (running) {
        disable("genome_file")
        addClass("genome_file", "custom-disable")
        disable("genome_dir")
        addClass("genome_dir", "custom-disable")
        disable("identity")
        disable("coverage")
        disable("cla_repo")
        disable("run_amr")
      } else {
        enable("genome_file")
        removeClass("genome_file", "custom-disable")
        enable("genome_dir")
        removeClass("genome_dir", "custom-disable")
        enable("identity")
        enable("coverage")
        enable("cla_repo")
        enable("run_amr")
      }
    })

    # Builds the display data frame for the Selected Genomes table. Called by
    # both the initial renderDT and the live replaceData observer so the column
    # structure is always identical.
    build_selection_df <- function(files, strains, results, existing_strains) {
      is_present <- strains %in% existing_strains
      if (!is.null(results) && nrow(results) > 0) {
        status_map <- setNames(results$status, results$strain)
        statuses <- status_map[strains]
        statuses[is.na(statuses)] <- "Pending"
        # Already-present genomes are excluded from the run, so they never
        # appear in `results`; keep their flag rather than showing "Pending".
        statuses[is_present] <- "Already present"
      } else {
        statuses <- ifelse(is_present, "Already present", "New")
      }
      # `status_map[strains]` yields NA-named elements for any selected strain
      # absent from `results` (e.g. already-present genomes excluded from the
      # queue). Those NA names would propagate into data.frame() as row names and
      # trigger "row names contain missing values", so drop the names - the table
      # is rendered with rownames = FALSE regardless.
      data.frame(
        File = basename(files),
        Status = unname(vapply(statuses, status_badge, character(1))),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # TRUE while at least one file is selected; updated only when the
    # presence of files changes so renderDT is not re-triggered by polling.
    selection_has_files <- reactiveVal(FALSE)
    observe({
      has <- length(Typing$files) > 0
      if (!identical(has, isolate(selection_has_files()))) {
        selection_has_files(has)
      }
    })

    # Structural render only — fires when files appear or disappear.
    # All data reads are wrapped in isolate() so 700 ms polling ticks don't
    # cause a full re-render; live status updates are pushed via replaceData.
    output$selection_table <- renderDT({
      render_info("output$selection_table")

      if (!selection_has_files()) {
        return(datatable(
          data.frame(" " = "No genomes selected yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      datatable(
        isolate(
          build_selection_df(
            Typing$files,
            Typing$strains,
            Typing$results,
            existing()
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(dom = "t", paging = FALSE, ordering = FALSE)
      )
    })

    selection_proxy <- dataTableProxy("selection_table", session = session)

    # Push status updates in-place so the accordion body's scroll position
    # is preserved across polling ticks and post-run refreshes.
    observe({
      req(selection_has_files())
      replaceData(
        selection_proxy,
        build_selection_df(
          Typing$files,
          Typing$strains,
          Typing$results,
          existing()
        ),
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    # Live log
    output$log <- renderText({
      text <- log_text()
      if (!nzchar(text)) "No typing run yet." else text
    })

    # Status line under the progress bar. Reports the module's whole run
    # status - idle, failed-to-start, terminated, complete, or (while running)
    # the live strain / phase - so this one line replaces the sidebar's old
    # standalone status badge; it always renders some text so the line never
    # goes blank and collapses the layout under it.
    #
    # Genomes are processed strictly one at a time, and within a genome the
    # three analyses run in sequence, so exactly one (strain, phase) pair is
    # live at any moment. Reporting only `status == "Running"` used to leave the
    # line blank for most of a genome's run time: that status covers allele
    # calling alone, and the strain flips to its final cgMLST outcome as soon as
    # the AMR sentinel appears - so the line vanished for the whole classical
    # MLST + AMR screening stretch, which is by far the longest part.
    #
    # The phases are read back off the same per-strain log state the results
    # table uses, most-advanced first:
    #   AMR screening   - the strain's "AMR: screening" sentinel is out
    #   Classical MLST  - allele calling reached its DONE line (`cg_done`) but
    #                     the section has not been closed out yet
    #   Allele calling  - the section is open and cgMLST is still working
    output$status_line <- renderText({
      results <- Typing$results
      if (identical(Typing$status, "terminated")) {
        return("Run terminated.")
      }
      # The process failed before any genome could start, so there is no
      # per-strain phase to read back - report the run-level outcome directly.
      if (identical(Typing$status, "failed")) {
        return("Typing failed to start.")
      }
      if (is.null(results) || nrow(results) == 0) {
        return("Idle")
      }

      screening <- which(
        !is.na(results$amr_status) & results$amr_status == "screening"
      )
      running <- which(results$status == "Running")

      if (length(screening)) {
        return(paste("AMR screening:", results$strain[screening[1]]))
      }
      if (length(running)) {
        i <- running[1]
        phase <- if (isTRUE(Typing$cla_enabled) && isTRUE(results$cg_done[i])) {
          "Classical MLST"
        } else {
          "Allele calling"
        }
        return(paste0(phase, ": ", results$strain[i]))
      }
      if (identical(Typing$status, "done")) {
        return("All genomes processed.")
      }
      # Nothing is running (idle): no phase to report.
      if (!identical(Typing$status, "running")) {
        return("Idle")
      }
      # No phase is live. Either the script has not reached the first genome yet
      # - it is still building this run's classical MLST reference database,
      # which downloads the scheme and can take a while - or it is between two
      # genomes.
      if (all(results$status == "Pending")) {
        "Preparing run ..."
      } else {
        "Starting next genome ..."
      }
    })

    # Builds the display data frame for the results table. Kept as a local
    # function so both the initial renderDT and the live replaceData observer
    # produce identical column structure.
    build_results_df <- function(
      results,
      total,
      cla_on = FALSE,
      amr_on = FALSE
    ) {
      dash <- function(x) ifelse(is.na(x), "—", as.character(x))

      cg_done <- if ("cg_done" %in% names(results)) {
        results$cg_done
      } else {
        rep(FALSE, nrow(results))
      }

      # The three analyses run strictly in sequence within a strain's section of
      # the log (allele calling, then classical MLST, then AMR screening), so a
      # step that has produced no result yet is queued behind the ones before it
      # rather than finished: TRUE while this strain is still working.
      in_flight <- results$status %in% c("Pending", "Running")

      # Allele calling is over the moment pymlst prints its DONE line, but a
      # strain's log section is only closed out once the steps behind it report
      # in (see parse_typing_log), so `status` still reads "Running" all through
      # the classical MLST search and the AMR screen. Report the cgMLST outcome
      # as soon as it is actually known: otherwise cgMLST and MLST both sit at
      # "Running", which reads as two steps running at once. Only a successful
      # run reaches DONE, so this can never pre-empt a failure verdict.
      cg_status <- ifelse(
        results$status == "Running" & cg_done,
        "Added",
        results$status
      )

      # Completeness (loci called / scheme size) is a metric of allele calling
      # alone, so it is filled once allele calling is done - in step with the
      # cgMLST outcome and the other allele-calling columns - rather than being
      # held back until the whole strain, AMR screen included, has finished.
      completeness <- if (!is.na(total) && total > 0) {
        round(results$found / total * 100, 1)
      } else {
        rep(NA_real_, nrow(results))
      }
      completeness[cg_status != "Added"] <- NA_real_

      # `claMLST search` is a single call that prints nothing until it returns
      # the ST, so classical MLST has no progress output to key off. Its state is
      # read off the step it follows instead: queued while allele calling is
      # still going, running once allele calling hit its DONE line and no ST has
      # come back yet. Empty once the step is no longer waiting on anything.
      cla_state <- if (cla_on) {
        ifelse(!in_flight, "", ifelse(cg_done, "Running", "Pending"))
      } else {
        rep("", nrow(results))
      }

      # Classical MLST cell: the registered ST number when known, otherwise a
      # badge flagging a novel profile (complete but unregistered) or a partial
      # one (some loci uncalled). While the strain is still running it shows the
      # step's own state; "—" when no classical scheme was used or the search
      # returned nothing usable.
      st_cell <- function(status, st, state) {
        if (is.na(status)) {
          return(if (nzchar(state)) status_badge(state) else "—")
        }
        switch(
          status,
          known = as.character(st),
          novel = '<span class="badge text-bg-warning">Novel</span>',
          partial = '<span class="badge text-bg-secondary">Partial</span>',
          "—"
        )
      }
      st_column <- if ("cla_status" %in% names(results)) {
        vapply(
          seq_len(nrow(results)),
          function(i) {
            st_cell(results$cla_status[i], results$st[i], cla_state[i])
          },
          character(1)
        )
      } else {
        dash(results$st)
      }

      # AMR screening cell: a hit count once done, a live "screening …" badge
      # while abritamr runs, a failure flag, the queue state while the strain's
      # earlier steps are still running, or "—" when AMR was not run. The count
      # spans all detected elements (AMR + virulence + stress/metal).
      amr_cell <- function(status, n, pending) {
        if (is.null(status) || is.na(status)) {
          return(if (pending) status_badge("Pending") else "—")
        }
        switch(
          status,
          done = if (!is.na(n) && n > 0) {
            sprintf(
              '<span class="badge text-bg-success">%d hit%s</span>',
              n,
              if (n == 1L) "" else "s"
            )
          } else {
            '<span class="badge text-bg-secondary">None</span>'
          },
          screening = '<span class="badge text-bg-info">Screening …</span>',
          failed = '<span class="badge text-bg-danger">Failed</span>',
          "—"
        )
      }
      amr_column <- if ("amr_status" %in% names(results)) {
        vapply(
          seq_len(nrow(results)),
          function(i) {
            amr_cell(
              results$amr_status[i],
              results$amr_elements[i],
              amr_on && in_flight[i]
            )
          },
          character(1)
        )
      } else {
        rep("—", nrow(results))
      }

      # Strain names are assembly file names and can be long enough to crowd out
      # every other column, so they are clipped to a fixed width (see the
      # .pt-strain rule in main.scss) with the full name on hover.
      strain_names <- htmlEscape(results$strain, attribute = TRUE)
      strain_column <- sprintf(
        '<span class="pt-strain" title="%s">%s</span>',
        strain_names,
        strain_names
      )

      # Column names and their order must stay in step with `results_columns`,
      # which supplies the rendered header (labels, tooltips and width classes).
      data.frame(
        Strain = strain_column,
        cgMLST = vapply(cg_status, status_badge, character(1)),
        `Loci found` = dash(results$found),
        `Alleles added` = dash(results$added),
        Partial = dash(results$partial),
        Filled = dash(results$filled),
        Removed = dash(results$removed),
        Completeness = vapply(completeness, completeness_badge, character(1)),
        MLST = st_column,
        AMR = amr_column,
        Finished = dash(results$finished),
        Elapsed = vapply(results$elapsed, format_elapsed, character(1)),
        Detail = results$detail,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # TRUE while results exist (typing has started or has run); updated only
    # when results transition NULL → non-NULL or back, so renderDT below is
    # not re-triggered by the 700 ms polling updates.
    results_initialized <- reactiveVal(FALSE)
    observe({
      has <- !is.null(Typing$results) && nrow(Typing$results) > 0
      if (!identical(has, isolate(results_initialized()))) {
        results_initialized(has)
      }
    })

    # Per-strain results — structural render only.
    # Fires exactly twice per typing run: once when results are first seeded
    # (results_initialized flips TRUE) to build the correct column layout, and
    # once on reset (flips FALSE) to show the placeholder. Live data is pushed
    # via replaceData below, which preserves scroll position.
    output$results_table <- renderDT({
      render_info("output$results_table")

      if (!results_initialized()) {
        return(datatable(
          data.frame(" " = "No results yet.", check.names = FALSE),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      datatable(
        isolate(build_results_df(
          Typing$results,
          scheme_total(),
          Typing$cla_enabled,
          Typing$amr_enabled
        )),
        container = results_header,
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          initComplete = header_tooltips
        )
      )
    })

    results_proxy <- dataTableProxy("results_table", session = session)

    # Push live data updates without a full re-render so scroll position is
    # preserved across polling ticks.
    observe({
      results <- Typing$results
      req(!is.null(results), nrow(results) > 0)
      replaceData(
        results_proxy,
        build_results_df(
          results,
          scheme_total(),
          Typing$cla_enabled,
          Typing$amr_enabled
        ),
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    # Keep selection_table and results_table reactive while their panel is absent
    # from the DOM (after nav_remove on reset). Without this, outputs suspend when
    # the panel is removed and still hold the stale pre-reset cached value; when
    # the panel is re-inserted Shiny re-sends that stale cache first, causing a
    # one-frame flicker before the reset state arrives.
    outputOptions(output, "selection_table", suspendWhenHidden = FALSE)
    outputOptions(output, "results_table", suspendWhenHidden = FALSE)
    outputOptions(output, "log", suspendWhenHidden = FALSE)
    outputOptions(output, "status_line", suspendWhenHidden = FALSE)

    # Start typing
    observeEvent(input$start, {
      req(valid_db(), length(Typing$strains) > 0)
      if (identical(Typing$status, "running")) {
        return()
      }

      # Type only assemblies that are not already in the database; their
      # `wgMLST add` would otherwise be rejected as a duplicate, wasting time.
      keep <- !(Typing$strains %in% existing())
      Typing$queued_files <- Typing$files[keep]
      Typing$queued_strains <- Typing$strains[keep]

      if (length(Typing$queued_files) == 0) {
        showNotification(
          "All selected genomes are already present in the database.",
          type = "warning",
          duration = 5
        )
        return()
      }

      # Click the "Typing Results" accordion button
      runjs(sprintf(
        "(function(){
           var acc = document.getElementById('%s');
           if (!acc) return;
           var item = acc.querySelector('[data-value=\"Typing Results\"]');
           if (!item) return;
           var btn = item.querySelector('.accordion-button.collapsed');
           if (btn) btn.click();
         })();",
        ns("typing_accordion")
      ))

      Typing$log_file <- tempfile(fileext = ".log")
      file.create(Typing$log_file)
      log_text("")
      Typing$terminated <- FALSE
      # Seed an all-Pending table so the queue is visible immediately.
      Typing$results <- parse_typing_log(character(0), Typing$queued_strains)

      # Classical MLST rides along in the same run: the scheme's species (from
      # the mother DB) tells claMLST import which classical scheme to build; the
      # ST is derived per genome. The reference DB is built by the script at this
      # temp path and read back here on finalize (for reference sequences /
      # metadata), then deleted. NA species => classical MLST skipped entirely.
      species <- db_species(db_path())
      Typing$cla_db <- if (!is.na(species)) {
        tempfile(fileext = ".clamlst.db")
      } else {
        NULL
      }
      live_cla_db <<- Typing$cla_db
      Typing$cla_enabled <- !is.null(Typing$cla_db)

      # AMR screening rides along too (opt-out via the sidebar checkbox). Each
      # genome is screened into a per-strain subdir of this run's temp AMR dir,
      # read back on finalize. The scheme's species is mapped to an abritamr
      # `--species` token when supported (enabling point mutations); an
      # unsupported / unknown species falls back to acquired-genes-only.
      run_amr <- isTRUE(input$run_amr)
      amr_sp <- if (run_amr) amr_species(species) else NA_character_
      Typing$amr_out <- if (run_amr) {
        d <- tempfile("amr_")
        dir.create(d, recursive = TRUE, showWarnings = FALSE)
        d
      } else {
        NULL
      }
      live_amr_out <<- Typing$amr_out
      Typing$amr_enabled <- run_amr

      proc <- tryCatch(
        start_typing(
          db_path = db_path(),
          genome_files = Typing$queued_files,
          log_file = Typing$log_file,
          identity = or_default(input$identity, 0.95),
          coverage = or_default(input$coverage, 0.9),
          env = conda_env,
          species = species,
          repo = or_default(input$cla_repo, "pubmlst"),
          cla_db = if (is.null(Typing$cla_db)) NA_character_ else Typing$cla_db,
          amr_env = if (run_amr) conda_env else NA_character_,
          amr_species = amr_sp,
          amr_out = if (is.null(Typing$amr_out)) {
            NA_character_
          } else {
            Typing$amr_out
          }
        ),
        error = function(e) e
      )

      if (inherits(proc, "error")) {
        Typing$status <- "failed"
        showNotification(
          paste("Could not start typing:", conditionMessage(proc)),
          type = "error",
          duration = 6
        )
        return()
      }

      Typing$proc <- proc
      live_proc <<- proc
      Typing$status <- "running"
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.add('is-animating');",
        ns("progress")
      ))
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = length(Typing$queued_strains)
      )
      skipped <- length(Typing$strains) - length(Typing$queued_strains)
      showNotification(
        paste0(
          length(Typing$queued_strains),
          " genome(s) queued.",
          if (skipped > 0) {
            sprintf(" %d already present, skipped.", skipped)
          }
        ),
        type = "message",
        duration = 3
      )
    })

    # Terminate a running batch
    observeEvent(input$terminate, {
      req(identical(Typing$status, "running"), !is.null(Typing$proc))
      Typing$terminated <- TRUE
      tryCatch(Typing$proc$kill_tree(), error = function(e) NULL)
    })

    # Expose the current typing status for other modules to react to.
    typing_status <- reactive(Typing$status)

    # Incremented whenever a typing run (completed or terminated) has written at
    # least one new strain to the database
    db_updated <- reactiveVal(0L)

    # Poll the background process: tail the log, refresh the results table and
    # the progress bar, and finalise once the process exits. invalidateLater
    # returns control to the event loop each tick so updates reach the browser
    # live and the UI stays responsive while typing runs.
    observe({
      if (!identical(Typing$status, "running")) {
        return(NULL)
      }

      proc <- Typing$proc
      alive <- !is.null(proc) && proc$is_alive()

      lines <- if (!is.null(Typing$log_file) && file.exists(Typing$log_file)) {
        readLines(Typing$log_file, warn = FALSE)
      } else {
        character(0)
      }

      log_text(paste(lines, collapse = "\n"))

      results <- parse_typing_log(lines, Typing$queued_strains)
      Typing$results <- results
      done <- sum(
        results$status %in% c("Added", "Duplicate", "Incompatible", "Error")
      )
      updateProgressBar(
        session,
        "progress",
        value = done,
        total = max(1L, length(Typing$queued_strains))
      )

      if (alive) {
        invalidateLater(700, session)
        return(NULL)
      }

      # Finished (completed or killed)
      Typing$status <- if (isTRUE(Typing$terminated)) "terminated" else "done"
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
        ns("progress")
      ))
      # A killed run stops short of its queue, so the bar is left part-filled.
      # Recolour it to say so - a half-full primary bar otherwise reads as a run
      # that is still going.
      if (isTRUE(Typing$terminated)) {
        updateProgressBar(
          session,
          "progress",
          value = done,
          total = max(1L, length(Typing$queued_strains)),
          status = "warning"
        )
      }
      Typing$refresh <- Typing$refresh + 1L

      # Persist any classical MLST STs derived during this run into the mother
      # DB's classical_mlst table (one row per allele), together with the
      # per-allele reference sequence + schema marker read from this run's
      # interim reference DB, and run-level provenance (repository actually used,
      # scheme/species, reference release, pyMLST version, identity/coverage).
      # Best-effort and additive: rows without an ST are skipped and any failure
      # never affects cgMLST results. The interim reference DB is deleted straight
      # after - it is a disposable per-run artifact.
      meta <- parse_clamlst_meta(lines)
      tryCatch(
        store_clamlst_results(
          db_path = db_path(),
          results = results,
          cla_db_path = if (is.null(Typing$cla_db)) {
            NA_character_
          } else {
            Typing$cla_db
          },
          identity = or_default(input$identity, 0.95),
          coverage = or_default(input$coverage, 0.9),
          repository = meta$repository,
          scheme = meta$scheme,
          scheme_version = meta$scheme_version,
          pymlst_version = meta$pymlst_version
        ),
        error = function(e) NULL
      )
      cleanup_cla_db(Typing$cla_db)
      Typing$cla_db <- NULL
      live_cla_db <<- NULL

      added <- sum(results$status == "Added")
      duplicate <- sum(results$status == "Duplicate")
      failed <- sum(results$status %in% c("Incompatible", "Error"))

      # Persist AMR screening results for the strains that were actually added
      # (so amr_results never keys rows to an isolate the mother DB rejected).
      # Each strain's abritamr output was written to amr_out/<strain>; read it
      # back into amr_results / amr_summary with run-level provenance (tool /
      # AMRFinder DB versions, whether point mutations were enabled). Best-effort
      # and additive - any failure never affects cgMLST / classical MLST results.
      # The per-run AMR output dir is a disposable artifact, deleted right after.
      amr_enabled <- !is.null(Typing$amr_out)
      amr_screened <- 0L
      amr_elements <- 0L
      if (amr_enabled) {
        amr_meta <- parse_amr_meta(lines)
        organism <- amr_species(db_species(db_path()))
        point_mutations <- !is.na(organism)
        for (strain in results$strain[results$status == "Added"]) {
          stored <- tryCatch(
            store_amr_results(
              db_path = db_path(),
              strain = strain,
              amr_dir = file.path(Typing$amr_out, strain),
              abritamr_version = amr_meta$abritamr_version,
              amrfinder_version = amr_meta$amrfinder_version,
              amrfinder_db_version = amr_meta$amrfinder_db_version,
              organism = organism,
              point_mutations = point_mutations,
              identity = NA_real_
            ),
            error = function(e) FALSE
          )
          if (isTRUE(stored)) amr_screened <- amr_screened + 1L
        }
        # Total detected elements, scraped from the script's per-strain sentinel.
        el <- regmatches(
          lines,
          regexec("AMR: done .* \\(([0-9]+) elements\\)", lines)
        )
        amr_elements <- sum(vapply(
          el,
          function(m) if (length(m) > 1) as.integer(m[2]) else 0L,
          integer(1)
        ))
        cleanup_amr_out(Typing$amr_out)
        Typing$amr_out <- NULL
        live_amr_out <<- NULL
      }

      # Signal other modules that the DB has new data.
      if (added > 0L) {
        db_updated(db_updated() + 1L)
      }

      # A killed run freezes its table mid-flight: the genome that was being
      # worked on, and everything still queued behind it, will never produce a
      # result. Retire those rows to "Stopped" so no badge keeps claiming work is
      # in progress - the tables, their per-step columns (which key off Pending /
      # Running) and the selection list all follow from this one rewrite. It is
      # applied last, so the persistence above still sees the run as it happened.
      if (isTRUE(Typing$terminated)) {
        # A genome killed during its AMR screen has a closed cgMLST section, so
        # it is not "Running" - the live screening marker is what makes it
        # unfinished.
        screening <- !is.na(results$amr_status) &
          results$amr_status == "screening"
        frozen <- results$status %in% c("Pending", "Running") | screening
        # A genome killed after allele calling reached its DONE line was still
        # added to the database, so it keeps that verdict; only the steps behind
        # it are lost. Everything else never got a cgMLST result at all.
        results$status[frozen & results$cg_done] <- "Added"
        results$status[frozen & !results$cg_done] <- "Stopped"
        # The killed screen produced nothing, so drop its marker rather than let
        # the AMR column keep advertising work in progress.
        results$amr_status[screening] <- NA_character_
        results$detail[frozen] <- "Run terminated."
        Typing$results <- results
      }

      showNotification(
        HTML(paste(
          c(
            if (identical(Typing$status, "terminated")) {
              "Typing terminated"
            } else {
              "Typing complete"
            },
            sprintf(
              "%d added, %d duplicate, %d failed.",
              added,
              duplicate,
              failed
            ),
            if (amr_enabled) {
              sprintf(
                "AMR: %d genome(s) screened, %d element(s) detected.",
                amr_screened,
                amr_elements
              )
            }
          ),
          collapse = "<br>"
        )),
        type = if (failed > 0) "warning" else "message",
        duration = 5
      )
    })

    list(typing_status = typing_status, db_updated = db_updated)
  })
}
