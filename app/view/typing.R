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
    tagList,
    icon,
    actionButton,
    numericInput,
    radioButtons,
    checkboxInput,
    showNotification,
    showModal,
    removeModal,
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
  DT[
    DTOutput,
    renderDT,
    datatable,
    dataTableProxy,
    replaceData,
    selectRows,
    JS
  ],
  htmltools[htmlEscape],
  fs[path_home],
)
box::use(
  app / logic / db_events,
  app / logic / app_meta[APP_VERSION],
  app / logic / database_functions[sync_metadata_table],
  app / logic / functions[render_info],
  app / logic / logging[typing_log_file, log_event],
  app / logic / provenance[scheme_provenance, store_provenance],
  app /
    logic /
    mlst_repo[
      REPO_URLS,
      describe_resolution,
      resolve_mlst_scheme,
      write_scheme_spec,
    ],
  app /
    logic /
    pymlst[
      CG_COVERAGE_DEFAULT,
      CG_IDENTITY_DEFAULT,
      CLA_COVERAGE_DEFAULT,
      CLA_IDENTITY_DEFAULT,
      conda_env,
      start_typing,
      parse_typing_log,
      parse_clamlst_meta,
      parse_tool_meta,
      read_st_profiles,
      clamlst_refs,
      cg_outcome,
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
  app /
    logic /
    genome_hash[
      classify_genome,
      genome_hash_map,
      genome_hash_row,
      store_genome_hash,
    ],
)

# Accepted assembly extensions, mirroring the glob in loop-pymlst.sh.
genome_pattern <- "\\.(fasta|fa|fna)$"

# Only drive the progress bar during the pre-run genome check when the selection
# is large enough that the pass takes visible time; below this the bar just
# stays at 0% while the status line reads "Checking genomes ...".
check_progress_min <- 10L

# Above this many rows, a name-conflict advisory ("these will be skipped as
# duplicates") is enough potential data loss to pause the run for a look
# before committing, rather than auto-proceeding past a wall of toast text -
# below it the existing inline warning is legible enough on its own.
name_conflict_gate_min <- 5L

# Logger for the typing module: a thin "TYPING"-tagged wrapper over the central
# log_event() (app/logic/logging.R), so every line lands in the active session
# log file as well as the console. `event` is a short phrase naming what
# happened; `detail` (optional) carries the specifics (counts, paths, params).
log_typing <- function(event, detail = NULL) {
  log_event("TYPING", event, detail)
}

# Small standalone DT for a duplicate-advisory table (same_genome or
# name_conflict rows out of the pre-run check). Built with datatable()
# directly rather than renderDT so it drops into a modal or a static panel
# without output-id wiring of its own; paging/search only kick in once the
# list is long enough to need them, so a 2-row table still reads as a plain
# list rather than a padded-out widget.
duplicate_dt <- function(df, cols, headers) {
  datatable(
    df[, cols, drop = FALSE],
    colnames = headers,
    rownames = FALSE,
    selection = "none",
    class = "compact stripe hover",
    options = list(
      dom = if (nrow(df) > 10) "ftp" else "t",
      paging = nrow(df) > 10,
      searching = nrow(df) > 10,
      ordering = FALSE
    )
  )
}

# Bootstrap badge class per typing outcome. Bootstrap 5 (shipped with bslib)
# provides these `text-bg-*` utilities, so no custom CSS is needed.
status_badge <- function(status) {
  cls <- switch(
    status,
    Added = "text-bg-success",
    Duplicate = "text-bg-secondary",
    Excluded = "text-bg-dark",
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
      "unregistered profile), Partial (some loci not called) or Ambiguous",
      "(several STs fit the profile). A starred ST comes from the scheme's",
      "profile table, where the uncalled locus is recorded as absent."
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

  # Reports back whenever this module's panel becomes visible again, so the
  # server can rebuild the results table (see render_nonce). The tab bar lives
  # outside this module, so the trigger is a document-level listener that
  # simply checks whether our own table is on screen once a tab has been
  # shown - `offsetParent` is null for anything inside a hidden pane.
  panel_shown_script <- tags$script(HTML(sprintf(
    "$(document).on('shown.bs.tab', function() {
       var el = document.getElementById('%s');
       if (!el || el.offsetParent === null) return;
       if (!window.Shiny || !Shiny.setInputValue) return;
       Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
     });",
    ns("results_table"),
    ns("panel_shown")
  )))

  page_sidebar(
    useShinyjs(),
    panel_shown_script,
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
                value = CG_IDENTITY_DEFAULT,
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
                value = CG_COVERAGE_DEFAULT,
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
          ),
          # Thresholds of their own rather than the cgMLST pair: the searches
          # differ in kind, and pyMLST itself defaults `claMLST search` to a
          # looser identity (0.9) than `wgMLST add` (0.95). The seven classical
          # loci are searched from one fixed reference allele each, so a locus
          # whose reference happens to be divergent can miss a cutoff that suits
          # cgMLST's thousands of loci perfectly well.
          div(
            class = "row g-2",
            div(
              class = "col-6",
              numericInput(
                ns("cla_identity"),
                "Min. identity",
                value = CLA_IDENTITY_DEFAULT,
                min = 0,
                max = 1,
                step = 0.01,
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              numericInput(
                ns("cla_coverage"),
                "Min. coverage",
                value = CLA_COVERAGE_DEFAULT,
                min = 0,
                max = 1,
                step = 0.01,
                width = "100%"
              )
            )
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
        icon = icon("play"),
        # pt-no-lock: Start must not arm the global input shield
        # (busy-shield.js). The checking phase keeps Shiny continuously busy
        # (one genome per invalidateLater(0) tick), so a shield engaged by this
        # click would never see an idle gap to release on and would stay up for
        # the whole pass - covering Terminate. Nothing needs shielding here
        # anyway: every other control is disabled while a run owns the queue.
        class = "pt-no-lock"
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
          # .typing-scrollpane takes the scrolling off the accordion body so the
          # toolbar stays pinned above the list and the table's sticky header has
          # a scrollport with no padding above it (see main.scss).
          div(
            class = "typing-scrollpane",
            div(
              class = "typing-selection-toolbar",
              actionButton(
                ns("select_all"),
                "All",
                icon = icon("check-double"),
                class = "btn-sm"
              ),
              actionButton(
                ns("select_none"),
                "None",
                icon = icon("xmark"),
                class = "btn-sm"
              ),
              actionButton(
                ns("exclude_selected"),
                "Exclude selected",
                icon = icon("ban"),
                class = "btn-sm"
              ),
              actionButton(
                ns("include_selected"),
                "Include selected",
                icon = icon("rotate-left"),
                class = "btn-sm"
              )
            ),
            DTOutput(ns("selection_table"))
          )
        ),
        accordion_panel(
          "Process Log",
          icon = icon("terminal"),
          verbatimTextOutput(ns("log"))
        ),
        accordion_panel(
          "Results",
          icon = icon("table"),
          div(
            class = "typing-scrollpane",
            DTOutput(ns("results_table"))
          )
        ),
        accordion_panel(
          title = tags$span(
            class = "typing-accordion-title",
            span("Duplicate Advisories"),
            uiOutput(ns("duplicate_advisories_badge"), inline = TRUE)
          ),
          value = "Duplicate Advisories",
          icon = icon("clone"),
          div(
            class = "typing-scrollpane",
            DTOutput(ns("duplicate_advisories_table"))
          )
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = shiny::reactive(NULL),
  session_reset = shiny::reactive(0L),
  db_rev = db_events$new_bus()
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
      # Strains manually excluded from typing via the selection table, on top of
      # the automatic "already present" exclusion.
      excluded_strains = character(0),
      # same_genome rows (strain, other) from the last pre-run check: assemblies
      # already stored under a different isolate name. Drives the "Duplicate
      # Advisories" panel - unlike name_conflict these are typed normally, so
      # without this they would leave no record once the summary toast dismisses.
      same_genome_dup = NULL,
      proc = NULL,
      log_file = NULL,
      # Path to this run's interim claMLST reference DB (built by the typing
      # script, read once for reference sequences / metadata, then deleted).
      cla_db = NULL,
      # Path to the resolved scheme's download spec, handed to the script.
      cla_spec = NULL,
      # This run's scheme profile table, read back once the build step is over.
      cla_profiles = NULL,
      # This run's claMLST reference sequences / schema marker, read once.
      cla_refs = NULL,
      # The database's cgMLST scheme context, stamped onto every isolate typed
      # in this run.
      scheme_context = NULL,
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

    # Non-reactive scratch for the pre-run genome check. The checking observe
    # walks one genome per reactive tick (so the pass stays interruptible and
    # never blocks the event loop the way the old synchronous withProgress did);
    # its loop counter and accumulating result table live here, off the reactive
    # graph, so writing them each tick does not re-trigger the observe itself.
    chk <- new.env(parent = emptyenv())

    # Which strains each step has already been written to the mother database
    # for, off the reactive graph for the same reason as `chk`: the polling
    # observe both reads and writes it every tick. `tried` is per (step, strain)
    # attempt, so a strain is never written twice while the run is live; `done`
    # records the attempts that actually stored something, so the finalize sweep
    # can retry the rest once the pyMLST process - the only other writer - is
    # gone.
    persisted <- new.env(parent = emptyenv())
    reset_persisted <- function() {
      steps <- list(
        cla = character(0),
        amr = character(0),
        hash = character(0),
        prov = character(0)
      )
      persisted$tried <- steps
      persisted$done <- steps
    }
    reset_persisted()

    # Delete an interim claMLST reference DB (SQLite side files and the scheme's
    # profile table included). Safe to call with NULL / NA / a non-existent path.
    cleanup_cla_db <- function(path) {
      if (!is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path)) {
        unlink(c(path, paste0(path, c("-wal", "-shm", ".profiles.tsv"))))
      }
    }

    # Delete this run's AMR output directory (per-strain abritamr outputs). Safe
    # to call with NULL / NA / a non-existent path.
    cleanup_amr_out <- function(path) {
      if (!is.null(path) && length(path) == 1 && !is.na(path) && nzchar(path)) {
        unlink(path, recursive = TRUE)
      }
    }

    # Writes the results of this run into the mother database, one isolate and
    # one step at a time, as soon as that step is over for that isolate - the
    # treatment allele calling gets for free, since pyMLST writes its own calls
    # per genome. What is persisted is therefore always a prefix of the work
    # actually done: an interrupted run costs at most the genome in flight,
    # rather than leaving every finished isolate with allele calls but no ST, no
    # AMR profile and no assembly hash - a gap that cannot be filled later,
    # because a strain already in the database is refused by `wgMLST add`.
    #
    # Nothing is written for a strain the mother database has not accepted, so no
    # row is ever keyed to a rejected isolate. Every store is per isolate and
    # replaces that isolate's rows, so a repeated attempt is harmless; all are
    # best-effort, and a failure never affects allele calling.
    #
    # These writes share the database with the running pyMLST process, which is
    # why connect() arms a busy timeout. Contention is the exception rather than
    # the rule: a strain's steps report in while pyMLST is busy with the *next*
    # genome's BLAT search or is waiting on that strain's AMR screen, holding no
    # write lock either way. `retry = TRUE` re-attempts the strains that failed
    # anyway, and is used by the finalize sweep once the pyMLST process is gone
    # and nothing else is writing.
    persist_results <- function(results, lines, retry = FALSE) {
      if (is.null(results) || !nrow(results)) {
        return(invisible(NULL))
      }
      ready <- cg_outcome(results) == "Added"
      if (!any(ready)) {
        return(invisible(NULL))
      }

      pending <- function(step, strain) {
        seen <- if (retry) persisted$done[[step]] else persisted$tried[[step]]
        !(strain %in% seen)
      }
      record <- function(step, strain, stored) {
        persisted$tried[[step]] <- union(persisted$tried[[step]], strain)
        if (isTRUE(stored)) {
          persisted$done[[step]] <- union(persisted$done[[step]], strain)
        }
      }
      # Strains whose provenance row is refreshed at the end of this pass. On
      # the closing sweep that is every accepted strain, so the last figures a
      # step only knows once it is over - elapsed time, elements detected -
      # reach the row even for isolates written several polls ago.
      touched <- if (retry) results$strain[ready] else character(0)

      # Classical MLST: one row per locus, plus the per-allele reference sequence
      # and schema marker read from this run's interim reference DB and the
      # run-level provenance the script logged (repository actually used, scheme,
      # reference release, pyMLST version, thresholds).
      cla_rows <- which(ready & !is.na(results$cla_status))
      cla_rows <- cla_rows[vapply(
        results$strain[cla_rows],
        pending,
        logical(1),
        step = "cla"
      )]
      if (isTRUE(Typing$cla_enabled) && length(cla_rows)) {
        meta <- parse_clamlst_meta(lines)
        # The reference DB holds every allele of the scheme, so it is read once
        # per run rather than once per isolate.
        if (is.null(Typing$cla_refs) && !is.null(Typing$cla_db)) {
          Typing$cla_refs <- clamlst_refs(Typing$cla_db)
        }
        for (i in cla_rows) {
          stored <- tryCatch(
            store_clamlst_results(
              db_path = db_path(),
              results = results[i, , drop = FALSE],
              cla_db_path = if (is.null(Typing$cla_db)) {
                NA_character_
              } else {
                Typing$cla_db
              },
              identity = or_default(input$cla_identity, CLA_IDENTITY_DEFAULT),
              coverage = or_default(input$cla_coverage, CLA_COVERAGE_DEFAULT),
              repository = meta$repository,
              scheme = meta$scheme,
              scheme_version = meta$scheme_version,
              pymlst_version = meta$pymlst_version,
              refs = Typing$cla_refs
            ),
            error = function(e) FALSE
          )
          record("cla", results$strain[i], stored)
          touched <- union(touched, results$strain[i])
        }
      }

      # AMR: read back out of this strain's abritamr output directory, with the
      # tool / database versions and whether point mutations were enabled.
      amr_rows <- which(ready & !is.na(results$amr_status) & results$amr_status == "done")
      amr_rows <- amr_rows[vapply(
        results$strain[amr_rows],
        pending,
        logical(1),
        step = "amr"
      )]
      if (isTRUE(Typing$amr_enabled) && !is.null(Typing$amr_out) && length(amr_rows)) {
        amr_meta <- parse_amr_meta(lines)
        organism <- amr_species(db_species(db_path()))
        for (i in amr_rows) {
          strain <- results$strain[i]
          stored <- tryCatch(
            store_amr_results(
              db_path = db_path(),
              strain = strain,
              amr_dir = file.path(Typing$amr_out, strain),
              abritamr_version = amr_meta$abritamr_version,
              amrfinder_version = amr_meta$amrfinder_version,
              amrfinder_db_version = amr_meta$amrfinder_db_version,
              organism = organism,
              point_mutations = !is.na(organism),
              identity = NA_real_
            ),
            error = function(e) FALSE
          )
          record("amr", strain, stored)
          touched <- union(touched, strain)
        }
      }

      # Genome hash: the assembly each isolate was actually typed from. This is
      # the only provenance allele calling has - the calls say nothing about the
      # input they were called from.
      hash_rows <- which(ready)
      hash_rows <- hash_rows[vapply(
        results$strain[hash_rows],
        pending,
        logical(1),
        step = "hash"
      )]
      for (i in hash_rows) {
        strain <- results$strain[i]
        file <- Typing$queued_files[match(strain, Typing$queued_strains)]
        stored <- if (is.na(file)) {
          FALSE
        } else {
          tryCatch(
            store_genome_hash(db_path(), strain, file),
            error = function(e) FALSE
          )
        }
        record("hash", strain, stored)
        touched <- union(touched, strain)
      }

      # Provenance: how this isolate was typed - the assembly it came from, the
      # scheme and thresholds each analysis ran against, the tool versions
      # behind them and what allele calling made of it. Refreshed after any step
      # of this pass reported in, so the row grows with the isolate's results
      # instead of being assembled once at the end.
      if (length(touched)) {
        tools <- parse_tool_meta(lines)
        cla_meta <- parse_clamlst_meta(lines)
        amr_meta <- parse_amr_meta(lines)
        amr_organism <- if (isTRUE(Typing$amr_enabled)) {
          amr_species(db_species(db_path()))
        } else {
          NA_character_
        }
        # A database with no scheme overview leaves the locus count unknown, so
        # completeness stays NA rather than being derived from nothing.
        locus_count <- Typing$scheme_context$cg_locus_count
        if (is.null(locus_count)) {
          locus_count <- NA_real_
        }

        # What became of each step for this isolate. Without it a NULL column
        # is ambiguous: a step that was never part of the run, one that ran and
        # returned nothing, and one that failed all read the same, while the
        # run-level fields around them (scheme, tool versions) are filled in
        # either way. Left NA mid-run, when a step may simply not have reported
        # yet, and settled on the closing sweep.
        step_status <- function(enabled, observed, unfinished) {
          if (!isTRUE(enabled)) {
            "skipped"
          } else if (!is.na(observed)) {
            observed
          } else if (retry) {
            unfinished
          } else {
            NA_character_
          }
        }

        for (strain in touched) {
          i <- match(strain, results$strain)
          hashes <- genome_hash_row(db_path(), strain)
          # "screening" is a state the log passes through, never one to record.
          amr_observed <- results$amr_status[i]
          if (!is.na(amr_observed) && !(amr_observed %in% c("done", "failed"))) {
            amr_observed <- NA_character_
          }
          stored <- tryCatch(
            store_provenance(
              db_path(),
              strain,
              c(
                Typing$scheme_context,
                hashes,
                list(
                  run_id = if (is.null(Typing$log_file)) {
                    NA_character_
                  } else {
                    basename(Typing$log_file)
                  },
                  elapsed_seconds = results$elapsed[i],
                  phylotrace_version = APP_VERSION,
                  cg_identity = or_default(input$identity, CG_IDENTITY_DEFAULT),
                  cg_coverage = or_default(input$coverage, CG_COVERAGE_DEFAULT),
                  cg_loci_found = results$found[i],
                  cg_alleles_added = results$added[i],
                  cg_partial_genes = results$partial[i],
                  cg_filled_genes = results$filled[i],
                  cg_removed_genes = results$removed[i],
                  cg_completeness = if (
                    !is.na(locus_count) && locus_count > 0 && !is.na(results$found[i])
                  ) {
                    round(results$found[i] / locus_count * 100, 1)
                  } else {
                    NA_real_
                  },
                  cla_status = step_status(
                    Typing$cla_enabled,
                    results$cla_status[i],
                    "failed"
                  ),
                  cla_scheme = cla_meta$scheme,
                  cla_scheme_version = cla_meta$scheme_version,
                  cla_alembic_version = Typing$cla_refs$alembic,
                  cla_repository = cla_meta$repository,
                  cla_identity = or_default(input$cla_identity, CLA_IDENTITY_DEFAULT),
                  cla_coverage = or_default(input$cla_coverage, CLA_COVERAGE_DEFAULT),
                  amr_status = step_status(
                    Typing$amr_enabled,
                    amr_observed,
                    "incomplete"
                  ),
                  amr_organism = amr_organism,
                  amr_point_mutations = if (isTRUE(Typing$amr_enabled)) {
                    as.integer(!is.na(amr_organism))
                  } else {
                    NA_integer_
                  },
                  amr_elements = results$amr_elements[i],
                  amr_abritamr_version = amr_meta$abritamr_version,
                  amr_amrfinder_version = amr_meta$amrfinder_version,
                  amr_amrfinder_db_version = amr_meta$amrfinder_db_version,
                  pymlst_version = tools$pymlst,
                  blat_version = tools$blat,
                  mafft_version = tools$mafft
                )
              )
            ),
            error = function(e) FALSE
          )
          if (isTRUE(stored)) {
            persisted$done$prov <- union(persisted$done$prov, strain)
          }
        }
      }

      invisible(NULL)
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
        log_typing("Session ended: killing orphaned typing process")
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
          log_typing("Killing live process on reset")
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
        Typing$excluded_strains <- character(0)
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
        reset_persisted()
        log_text("")
        updateProgressBar(
          session,
          "progress",
          value = 0,
          total = 1,
          status = "primary"
        )
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
        log_typing("Selection ignored: run in progress", path)
        return(invisible())
      }

      is_dir <- dir.exists(path)
      files <- if (is_dir) {
        list.files(path, pattern = genome_pattern, full.names = TRUE)
      } else {
        path
      }

      log_typing(
        if (is_dir) "Folder selected" else "File selected",
        sprintf("%d genome(s) | %s", length(files), path)
      )

      Typing$genome_input <- path
      Typing$files <- files
      Typing$strains <- sub("\\.[^.]*$", "", basename(files))
      Typing$excluded_strains <- character(0)
      Typing$results <- NULL
      log_text("")
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = 1,
        status = "primary"
      )

      # A fresh pick from either the file or folder chooser goes through here,
      # so open "Selected Genomes" right where the new selection lands instead
      # of leaving the user to notice the summary badge changed while whatever
      # panel they had open (e.g. Results, from a previous run) stays shown.
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
    # selection table's "Already present" flags stay current. Also on any change
    # to the isolate pool from elsewhere - removing isolates in the Database
    # Browser used to leave this list claiming they were still present, so a
    # re-type of a removed isolate was flagged as a duplicate that no longer was.
    existing <- reactive({
      Typing$refresh
      db_events$depend(db_rev, "isolates")
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
        tags$dd(
          "Every selected genome then runs the three steps below, in order."
        )
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
          "Derived per genome right after allele calling (claMLST search).",
          "A number is shown only when exactly one ST matches; otherwise the",
          "profile is flagged Novel, Partial or Ambiguous. Where a scheme",
          "records a locus as absent (allele 0, e.g. pstS in Enterococcus",
          "faecium), the ST is read from its profile table and shown starred."
        ),
        tags$dt("Scheme source"),
        tags$dd(
          "Repository the 7-gene scheme is fetched from. The species is",
          "resolved to exactly one database and one scheme; if the chosen",
          "repository cannot do that, the other is tried, and if neither can,",
          "classical MLST is skipped for the run. Where one database holds two",
          "equally classical schemes (Acinetobacter baumannii's Oxford and",
          "Pasteur schemes are both on PubMLST), this choice also picks between",
          "them by name."
        ),
        tags$dt("Min. identity / Min. coverage"),
        tags$dd(
          "The same BLAT cutoffs as cgMLST, but for the seven classical loci",
          "and set separately - pyMLST itself defaults this search to a looser",
          "identity (0.9) than allele calling (0.95). Each locus is sought from",
          "one fixed reference allele, so a locus whose reference happens to be",
          "divergent can miss a strict cutoff and be reported uncalled while",
          "the other six type normally. A looser cutoff only affects whether a",
          "locus is found at all: the allele number still comes from an exact",
          "sequence match, so it cannot produce a wrong allele - an unmatched",
          "hit is reported as a new allele instead."
        ),
        tags$dt("When it's skipped"),
        tags$dd(
          "No classical MLST is attempted when the scheme has no resolvable species."
        )
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
        tags$dd(
          "Results are stored in the database (amr_results / amr_summary)."
        )
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
      is_present <- Typing$strains %in% existing()
      is_excluded <- Typing$strains %in% Typing$excluded_strains & !is_present
      n_present <- sum(is_present)
      n_excluded <- sum(is_excluded)
      n_new <- n_total - n_present - n_excluded
      tags$span(
        class = "typing-accordion-summary",
        tags$span(class = "badge text-bg-success", paste(n_new, "New")),
        if (n_present > 0) {
          tags$span(
            class = "badge text-bg-warning",
            paste(n_present, "Already present")
          )
        },
        if (n_excluded > 0) {
          tags$span(
            class = "badge text-bg-dark",
            paste(n_excluded, "Excluded")
          )
        }
      )
    })
    outputOptions(output, "selection_summary", suspendWhenHidden = FALSE)

    # Row-count badge on the "Duplicate Advisories" tab title, mirroring
    # selection_summary - lets a same_genome hit be noticed even after its
    # summary toast has auto-dismissed, without opening the panel.
    output$duplicate_advisories_badge <- renderUI({
      dup <- Typing$same_genome_dup
      n <- if (is.null(dup)) 0L else nrow(dup)
      if (n == 0L) {
        return(NULL)
      }
      tags$span(
        class = "typing-accordion-summary",
        tags$span(class = "badge text-bg-warning", paste(n, "Duplicate"))
      )
    })
    outputOptions(output, "duplicate_advisories_badge", suspendWhenHidden = FALSE)

    # Full same_genome list from the last check pass (see launch_typing).
    output$duplicate_advisories_table <- renderDT({
      render_info("output$duplicate_advisories_table")
      dup <- Typing$same_genome_dup
      if (is.null(dup) || nrow(dup) == 0) {
        return(datatable(
          data.frame(
            " " = "No duplicate assemblies flagged in the last check.",
            check.names = FALSE
          ),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        ))
      }
      duplicate_dt(
        dup,
        c("strain", "other"),
        c("Selected assembly", "Already stored as")
      )
    })
    outputOptions(output, "duplicate_advisories_table", suspendWhenHidden = FALSE)

    # Enable/disable controls based on current status.
    # While running: lock file/folder pickers (needs both disable() + CSS class
    # because shinyFiles buttons are not standard inputs) and parameter inputs.
    # Terminate mirrors the running state; Start requires a valid ready state.
    observe({
      # "checking" is the pre-run hashing phase - it owns the queue just like a
      # live run, so it locks the same controls and keeps Terminate live.
      running <- identical(Typing$status, "running") ||
        identical(Typing$status, "checking")
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
        disable("cla_identity")
        disable("cla_coverage")
        disable("run_amr")
      } else {
        enable("genome_file")
        removeClass("genome_file", "custom-disable")
        enable("genome_dir")
        removeClass("genome_dir", "custom-disable")
        enable("identity")
        enable("coverage")
        enable("cla_repo")
        enable("cla_identity")
        enable("cla_coverage")
        enable("run_amr")
      }
    })

    # Exclude / include act on the rows ticked in the selection table, so they
    # stay dead until something is ticked (and while a run owns the queue). Kept
    # apart from the observer above so ticking a row does not re-send that whole
    # block of enable/disable messages for the sidebar on every click.
    observe({
      running <- identical(Typing$status, "running") ||
        identical(Typing$status, "checking")
      picked <- !running && length(input$selection_table_rows_selected) > 0
      toggleState("exclude_selected", condition = picked)
      toggleState("include_selected", condition = picked)
      toggleState(
        "select_all",
        condition = !running && length(Typing$strains) > 0
      )
      toggleState("select_none", condition = picked)
    })

    # Builds the display data frame for the Selected Genomes table. Called by
    # both the initial renderDT and the live replaceData observer so the column
    # structure is always identical.
    build_selection_df <- function(
      files,
      strains,
      results,
      existing_strains,
      excluded_strains
    ) {
      is_present <- strains %in% existing_strains
      # Manually excluded strains are dropped from the queue same as
      # already-present ones; "Already present" wins if both apply.
      is_excluded <- strains %in% excluded_strains & !is_present
      if (!is.null(results) && nrow(results) > 0) {
        status_map <- setNames(results$status, results$strain)
        statuses <- status_map[strains]
        statuses[is.na(statuses)] <- "Pending"
        # Already-present genomes are excluded from the run, so they never
        # appear in `results`; keep their flag rather than showing "Pending".
        statuses[is_present] <- "Already present"
        statuses[is_excluded] <- "Excluded"
      } else {
        statuses <- ifelse(
          is_present,
          "Already present",
          ifelse(is_excluded, "Excluded", "New")
        )
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
            existing(),
            Typing$excluded_strains
          )
        ),
        rownames = FALSE,
        escape = FALSE,
        # Rows are selected here only as a picker for the Exclude/Include
        # buttons below - the actual exclusion state lives in
        # Typing$excluded_strains and is reflected via the Status column.
        selection = list(mode = "multiple", target = "row"),
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
          existing(),
          Typing$excluded_strains
        ),
        resetPaging = FALSE,
        rownames = FALSE
      )
    })

    # Exclude / include the rows currently picked in the selection table.
    # Picking is a plain row click (mode = "multiple"); the buttons commit that
    # pick into Typing$excluded_strains and clear it so the next click starts
    # fresh.
    observeEvent(input$exclude_selected, {
      rows <- input$selection_table_rows_selected
      req(length(rows))
      log_typing("Excluding strains", sprintf("%d row(s)", length(rows)))
      Typing$excluded_strains <- union(
        Typing$excluded_strains,
        Typing$strains[rows]
      )
      selectRows(selection_proxy, NULL)
    })

    observeEvent(input$include_selected, {
      rows <- input$selection_table_rows_selected
      req(length(rows))
      log_typing("Re-including strains", sprintf("%d row(s)", length(rows)))
      Typing$excluded_strains <- setdiff(
        Typing$excluded_strains,
        Typing$strains[rows]
      )
      selectRows(selection_proxy, NULL)
    })

    # Row-picker helpers: tick every row / clear the tick, independent of the
    # Exclude/Include commit step above.
    observeEvent(input$select_all, {
      selectRows(selection_proxy, seq_along(Typing$strains))
    })

    observeEvent(input$select_none, {
      selectRows(selection_proxy, NULL)
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
      # Pre-run hashing phase. Static text on purpose: the progress bar already
      # counts genomes as they are checked, so there is no need to re-render this
      # line (and re-flush the module) on every genome in the loop.
      if (identical(Typing$status, "checking")) {
        return("Checking genomes ...")
      }
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

      # Report the cgMLST outcome as soon as it is actually known (see
      # cg_outcome): otherwise cgMLST and MLST both sit at "Running", which reads
      # as two steps running at once. The same verdict decides when a strain's
      # results may be written to the mother database.
      cg_status <- cg_outcome(results)

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
      # read off the steps around it instead: queued while allele calling is
      # still going, running once allele calling hit its DONE line, and over once
      # the strain has settled or the step *behind* it has started - any AMR
      # sentinel means the search has already returned, whether or not it
      # returned anything usable.
      cla_state <- if (cla_on) {
        settled <- !in_flight | !is.na(results$amr_status)
        ifelse(settled, "", ifelse(cg_done, "Running", "Pending"))
      } else {
        rep("", nrow(results))
      }

      # Classical MLST cell: the registered ST number when known, otherwise a
      # badge flagging a novel profile (complete but unregistered), a partial one
      # (some loci uncalled) or an ambiguous one (several STs fit the call). Only
      # a "known" call has a single ST, so no other state ever prints a number.
      # While the strain is still running it shows the step's own state; "—" when
      # no classical scheme was used or the search returned nothing usable.
      st_cell <- function(status, st, state) {
        if (is.na(status)) {
          return(if (nzchar(state)) status_badge(state) else "—")
        }
        switch(
          status,
          known = as.character(st),
          # Same ST, reached through the scheme's profile table because a locus
          # the scheme itself records as absent was not called.
          inferred = sprintf(
            '<span title="Locus absent - ST read from the scheme profile">%s*</span>',
            as.character(st)
          ),
          novel = '<span class="badge text-bg-warning">Novel</span>',
          partial = '<span class="badge text-bg-secondary">Partial</span>',
          ambiguous = '<span class="badge text-bg-secondary">Ambiguous</span>',
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
    # Fires when results are first seeded (results_initialized flips TRUE) to
    # build the correct column layout, on reset (flips FALSE) to show the
    # placeholder, and whenever the panel is revealed again (render_nonce), so
    # a first render that landed in a hidden pane is rebuilt on a visible one.
    # Live data is pushed via replaceData below, which preserves scroll
    # position.
    output$results_table <- renderDT({
      render_info("output$results_table")
      render_nonce()

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

    # Rebuild the results table from scratch whenever the panel is revealed
    # again (input$panel_shown, fired by the script in ui()).
    #
    # A run seeds its first results row - and so triggers the table's one
    # structural render - while the user may already have switched to another
    # tab. That first render lands in a hidden pane and can end up without its
    # rows; the live replaceData updates that follow only replace data in a
    # table that was built correctly, so they never repair it, and the table
    # still reads empty when the tab is revisited. Re-rendering on reveal is
    # what makes that recoverable: it rebuilds against the current data on a
    # pane that is actually visible, which is the same path that always works
    # when the user simply stays put.
    render_nonce <- reactiveVal(0L)
    observeEvent(
      input$panel_shown,
      {
        if (isTRUE(results_initialized())) {
          render_nonce(isolate(render_nonce()) + 1L)
        }
      },
      ignoreInit = TRUE
    )

    # Start typing
    observeEvent(input$start, {
      req(valid_db(), length(Typing$strains) > 0)
      log_typing(
        "Start requested",
        sprintf("%d strain(s) selected", length(Typing$strains))
      )
      # A run is already owning the queue - either still hashing its selection
      # (checking) or typing it (running); ignore the click.
      if (
        identical(Typing$status, "running") ||
          identical(Typing$status, "checking")
      ) {
        log_typing("Start ignored: run already in progress", Typing$status)
        return()
      }

      # Only manual exclusions are filtered here. A name already in the
      # database is NOT dropped at this point any more: it might be a genuine
      # no-op retype (same digest, safe to skip) or a real name collision
      # (different digest under a taken name), and only the digest check below
      # can tell those apart - dropping by name alone made every name-conflict
      # or retype classification below unreachable, since it never let a known
      # name survive into `checked`. See launch_typing(), which narrows the
      # queue down to what actually gets typed once that check is in.
      keep <- !(Typing$strains %in% Typing$excluded_strains)
      Typing$queued_files <- Typing$files[keep]
      Typing$queued_strains <- Typing$strains[keep]
      log_typing(
        "Queue filtered",
        sprintf(
          "%d queued, %d excluded",
          length(Typing$queued_strains),
          sum(Typing$strains %in% Typing$excluded_strains)
        )
      )

      # Nothing survives the filter: bail out before the digest pass below, which
      # would otherwise flash its "Checking genomes" progress panel for the
      # instant it takes to walk an empty queue. Reusing one notification id
      # keeps repeated clicks from stacking copies of the same message.
      if (length(Typing$queued_files) == 0) {
        log_typing("Start aborted: nothing queued after filter")
        showNotification(
          "All selected genomes are excluded.",
          id = ns("empty_queue"),
          type = "warning",
          duration = 5
        )
        return()
      }

      # Content digests of every selected assembly (see app/logic/genome_hash.R).
      # This is what actually distinguishes a harmless retype (name known,
      # digest identical - dropped silently in launch_typing()) from a real
      # name_conflict (name known, digest differs - gated or reported there)
      # from a same_genome hit (new name, digest matches something else -
      # reported but never blocks; re-typing an assembly under a new name is
      # legitimate, and only the metadata can settle whether two records are
      # really the same epidemiological isolate).
      #
      # Hashing several hundred assemblies takes long enough that doing it inline
      # would freeze the event loop for the whole pass and could not be
      # cancelled. So it becomes its own phase: seed the non-reactive check state
      # here, flip to "checking", and let the checking observe below walk one
      # genome per tick (filling the progress bar to 100%, interruptible via
      # Terminate) before handing off to launch_typing().
      chk$recorded <- genome_hash_map(db_path())
      chk$known <- existing()
      chk$out <- data.frame(
        strain = Typing$queued_strains,
        file = Typing$queued_files,
        digest = NA_character_,
        status = "new",
        other = NA_character_,
        stringsAsFactors = FALSE
      )
      chk$i <- 0L
      chk$n <- length(Typing$queued_strains)
      # Small selections check almost instantly, so advancing the bar just makes
      # it flicker to 100% and back; only drive it for larger batches.
      chk$show_bar <- chk$n >= check_progress_min

      Typing$terminated <- FALSE
      Typing$status <- "checking"
      log_typing(
        "Checking phase started",
        sprintf("hashing %d genome(s)", chk$n)
      )
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.add('is-animating');",
        ns("progress")
      ))
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = if (chk$show_bar) chk$n else 1,
        # A prior terminated run may have left the bar recoloured "warning"
        # (see the finalize handler below) - shinyWidgets only touches the
        # bar's class list when `status` is non-NULL, so every fresh start
        # must restate "primary" itself or the old colour (and its black
        # text) carries over silently.
        status = "primary"
      )
    })

    # Walk the queued selection one genome per reactive tick, classifying each
    # against the database (see classify_genome). invalidateLater(0) yields to
    # the event loop between genomes, so the pass never blocks the UI and a
    # Terminate click lands within one genome's hashing time. When the queue is
    # exhausted, hand the classification off to launch_typing(); if Terminate
    # fired, abandon the pass and reset to idle.
    observe({
      if (!identical(Typing$status, "checking")) {
        return(NULL)
      }

      if (isTRUE(Typing$terminated)) {
        log_typing("Checking phase terminated", sprintf("%d/%d done", chk$i, chk$n))
        reset_to_idle()
        showNotification(
          "Genome check terminated.",
          id = ns("check_terminated"),
          type = "warning",
          duration = 4
        )
        return(NULL)
      }

      # Whole selection classified: proceed to the typing run.
      if (chk$i >= chk$n) {
        log_typing(
          "Checking phase complete",
          sprintf(
            "%d new, %d same-genome, %d retype, %d name-conflict, %d unknown",
            sum(chk$out$status == "new"),
            sum(chk$out$status == "same_genome"),
            sum(chk$out$status == "retype"),
            sum(chk$out$status == "name_conflict"),
            sum(chk$out$status == "unknown")
          )
        )
        launch_typing(chk$out)
        return(NULL)
      }

      i <- chk$i + 1L
      r <- classify_genome(
        chk$out$strain[i],
        chk$out$file[i],
        chk$recorded,
        chk$known
      )
      chk$out$digest[i] <- r$digest
      chk$out$status[i] <- r$status
      chk$out$other[i] <- r$other
      chk$i <- i

      # Advance the bar (a targeted widget message, not a reactive write, so it
      # does not re-flush the module) and queue the next genome. Skipped for
      # small selections, which stay at 0%. invalidateLater drives the loop.
      if (chk$show_bar) {
        updateProgressBar(session, "progress", value = i, total = chk$n)
      }
      invalidateLater(0, session)
    })

    # Shared reset back to idle once a run is abandoned after the progress bar
    # has already started animating: the checking-terminated path above and the
    # name-conflict gate's Cancel button below both need it.
    reset_to_idle <- function() {
      Typing$status <- "idle"
      updateProgressBar(session, "progress", value = 0, total = 1, status = "primary")
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
        ns("progress")
      ))
    }

    # Kick off the typing run once its selection has been checked. `checked` is
    # the classification table from the checking phase; its advisory verdicts are
    # surfaced here. same_genome never blocks (pyMLST types it fine under the new
    # name) - its full list is handed to the "Duplicate Advisories" panel, since
    # unlike name_conflict it leaves no trace in results_table afterwards. A
    # small name_conflict batch is likewise let through with just a heads-up
    # (pyMLST rejects those itself), but a batch bigger than
    # name_conflict_gate_min is treated as a signal something is off with the
    # whole selection (e.g. an accidental re-run of an already-typed batch), so
    # it pauses for an explicit Continue/Cancel before any process starts. Split
    # out of the Start handler so the checking phase can call it asynchronously
    # on completion rather than everything running in one blocking event.
    launch_typing <- function(checked) {
      same_genome <- checked[checked$status == "same_genome", , drop = FALSE]
      Typing$same_genome_dup <- if (nrow(same_genome)) same_genome else NULL
      if (nrow(same_genome)) {
        log_typing(
          "Advisory: assemblies already in DB under another name",
          sprintf("%d", nrow(same_genome))
        )
        showNotification(
          HTML(paste0(
            "<strong>",
            nrow(same_genome),
            " selected assembly/assemblies already in the database under ",
            "another name.</strong><br>",
            "<em>Identical assembly, not necessarily the same isolate - see ",
            "the \"Duplicate Advisories\" panel for the full list before ",
            "treating these as duplicates.</em>"
          )),
          type = "warning",
          duration = 8
        )
      }

      # Narrow the queue to what actually gets typed: "new" and "same_genome"
      # only. "retype" (name known, digest identical - a genuine no-op) and
      # name_taken rows (below) are dropped, now that the digest check has told
      # them apart - see the Start handler, which used to make this call by
      # name alone before any of this was known. Set unconditionally, before
      # the gate below can return early: begin_typing_run() reads these back
      # whether it is called from here directly or later from "Continue typing".
      to_type <- checked[checked$status %in% c("new", "same_genome"), , drop = FALSE]
      Typing$queued_files <- to_type$file
      Typing$queued_strains <- to_type$strain

      if (nrow(to_type) == 0) {
        log_typing("Start aborted: nothing left to type after the check")
        showNotification(
          paste(
            "Every selected genome was already present (unchanged or",
            "name-conflicting) - nothing left to type."
          ),
          id = ns("empty_queue"),
          type = "warning",
          duration = 5
        )
        reset_to_idle()
        return()
      }

      # "unknown" (name taken, no recorded hash to compare against - e.g. an
      # isolate typed before genome hashing existed) gets the same treatment as
      # a confirmed name_conflict: either way the name is already taken, so
      # pyMLST's `wgMLST add` rejects the file regardless of what its content
      # turns out to be.
      name_taken <- checked[
        checked$status %in% c("name_conflict", "unknown"),
        ,
        drop = FALSE
      ]
      if (nrow(name_taken) > name_conflict_gate_min) {
        log_typing(
          "Advisory: name conflicts require confirmation",
          sprintf("%d (gate threshold %d)", nrow(name_taken), name_conflict_gate_min)
        )
        showModal(modalDialog(
          title = "Name conflicts detected",
          size = "l",
          easyClose = FALSE,
          tags$p(sprintf(
            paste(
              "%d selected assembly/assemblies already share a name with an",
              "isolate in the database. These will be skipped as duplicates",
              "and will NOT be typed - rename them to type them, or continue",
              "to type the rest of the selection."
            ),
            nrow(name_taken)
          )),
          div(
            class = "typing-scrollpane",
            duplicate_dt(name_taken, "strain", "Skipped as duplicate")
          ),
          footer = tagList(
            actionButton(ns("cancel_typing_gate"), "Cancel"),
            actionButton(
              ns("confirm_typing_gate"),
              "Continue typing",
              class = "btn-warning"
            )
          )
        ))
        return()
      }

      if (nrow(name_taken)) {
        log_typing(
          "Advisory: name conflicts skipped as duplicates",
          sprintf("%d", nrow(name_taken))
        )
        showNotification(
          HTML(paste0(
            "<strong>",
            nrow(name_taken),
            " selected assembly/assemblies ",
            "already share a name with an isolate in the database:</strong><br>",
            paste(htmlEscape(name_taken$strain), collapse = "<br>"),
            "<br><em>These are skipped as duplicates and will NOT be typed. ",
            "Rename them to type them.</em>"
          )),
          type = "error",
          duration = NULL
        )
      }

      begin_typing_run()
    }

    # Resume from the name-conflict confirmation gate: dismiss the modal and
    # either abandon the run (back to idle, as if Start was never clicked) or
    # proceed exactly as a sub-threshold batch would have.
    observeEvent(input$cancel_typing_gate, {
      removeModal()
      log_typing("Name-conflict gate cancelled")
      reset_to_idle()
    })

    observeEvent(input$confirm_typing_gate, {
      removeModal()
      log_typing("Name-conflict gate confirmed - proceeding")
      begin_typing_run()
    })

    # Second half of launch_typing(): actually starts the background typing
    # process against the already-queued, already-checked selection. Reached
    # either straight from launch_typing() or, once a name-conflict gate was
    # shown, from the "Continue typing" button above.
    begin_typing_run <- function() {
      # Click the "Results" accordion button
      runjs(sprintf(
        "(function(){
           var acc = document.getElementById('%s');
           if (!acc) return;
           var item = acc.querySelector('[data-value=\"Results\"]');
           if (!item) return;
           var btn = item.querySelector('.accordion-button.collapsed');
           if (btn) btn.click();
         })();",
        ns("typing_accordion")
      ))

      # Persistent per-run log under app_local_share_path/logs (see
      # logging.R). loop-pymlst.sh streams its combined stdout/stderr - the raw
      # output of every external command in the pipeline - here, and it is kept
      # after the run rather than discarded like the old tempfile.
      Typing$log_file <- typing_log_file()
      file.create(Typing$log_file)
      log_typing("Run log file", Typing$log_file)
      log_text("")
      Typing$terminated <- FALSE
      # Nothing of this run has reached the database yet.
      reset_persisted()
      # The scheme every isolate of this run is typed against, read once and
      # stamped onto each provenance row: a snapshot, so a later scheme refresh
      # cannot rewrite what these calls were made against.
      Typing$scheme_context <- scheme_provenance(db_path())
      Typing$cla_refs <- NULL
      # Seed an all-Pending table so the queue is visible immediately.
      Typing$results <- parse_typing_log(character(0), Typing$queued_strains)

      # Classical MLST rides along in the same run: the scheme's species (from
      # the mother DB) is resolved to exactly one repository scheme here, the
      # script downloads and builds it at this temp path, and the ST is derived
      # per genome. The reference DB is read back on finalize (for reference
      # sequences / metadata), then deleted. No species or no scheme resolvable
      # => classical MLST is skipped for the whole run.
      species <- db_species(db_path())
      cla_scheme <- if (!is.na(species)) {
        # Blocks on the repository APIs for a moment - logged so a slow or
        # unreachable repository is visible as such rather than as a stall.
        log_typing("Resolving classical MLST scheme", species)
        resolve_mlst_scheme(
          species,
          repos = union(or_default(input$cla_repo, "pubmlst"), names(REPO_URLS))
        )
      } else {
        list(resolved = FALSE, species = NA_character_, attempts = character(0))
      }
      Typing$cla_db <- if (isTRUE(cla_scheme$resolved)) {
        tempfile(fileext = ".clamlst.db")
      } else {
        NULL
      }
      Typing$cla_spec <- if (isTRUE(cla_scheme$resolved)) {
        write_scheme_spec(cla_scheme, tempfile(fileext = ".clamlst.spec"))
      } else {
        NULL
      }
      live_cla_db <<- Typing$cla_db
      Typing$cla_enabled <- !is.null(Typing$cla_db)

      # Why classical MLST will or won't run this time, worded for the run's own
      # transcript rather than the internal session log - the only log a user
      # inspecting a finished run actually has open. Written straight into the
      # log file now, before the subprocess launches: start_typing() connects
      # the subprocess's stdout to this same path in *append* mode precisely so
      # a note written here survives instead of being raced by the child's own
      # first write.
      cat(
        strrep("-", 48), "\n",
        describe_resolution(cla_scheme), "\n",
        sep = "",
        file = Typing$log_file,
        append = TRUE
      )

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

      log_typing(
        "Launching typing run",
        sprintf(
          paste(
            "%d genome(s) | species=%s | cgMLST id=%s cov=%s |",
            "claMLST=%s (%s) id=%s cov=%s | AMR=%s (%s)"
          ),
          length(Typing$queued_files),
          if (is.na(species)) "NA" else species,
          or_default(input$identity, CG_IDENTITY_DEFAULT),
          or_default(input$coverage, CG_COVERAGE_DEFAULT),
          if (Typing$cla_enabled) "on" else "off",
          if (isTRUE(cla_scheme$resolved)) {
            sprintf("%s: %s", cla_scheme$repository, cla_scheme$scheme)
          } else {
            "no scheme"
          },
          or_default(input$cla_identity, CLA_IDENTITY_DEFAULT),
          or_default(input$cla_coverage, CLA_COVERAGE_DEFAULT),
          if (run_amr) "on" else "off",
          if (is.na(amr_sp)) "acquired-only" else amr_sp
        )
      )

      proc <- tryCatch(
        start_typing(
          db_path = db_path(),
          genome_files = Typing$queued_files,
          log_file = Typing$log_file,
          identity = or_default(input$identity, CG_IDENTITY_DEFAULT),
          coverage = or_default(input$coverage, CG_COVERAGE_DEFAULT),
          env = conda_env,
          species = species,
          cla_identity = or_default(input$cla_identity, CLA_IDENTITY_DEFAULT),
          cla_coverage = or_default(input$cla_coverage, CLA_COVERAGE_DEFAULT),
          cla_db = if (is.null(Typing$cla_db)) NA_character_ else Typing$cla_db,
          cla_spec = if (is.null(Typing$cla_spec)) {
            NA_character_
          } else {
            Typing$cla_spec
          },
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
        log_typing("Typing failed to start", conditionMessage(proc))
        Typing$status <- "failed"
        # The bar has been animating since the checking phase - stop it and
        # empty it so a failed start does not leave a striped bar running.
        runjs(sprintf(
          "var el = document.getElementById('%s'); if (el) el.classList.remove('is-animating');",
          ns("progress")
        ))
        updateProgressBar(
          session,
          "progress",
          value = 0,
          total = 1,
          status = "primary"
        )
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
      log_typing(
        "Typing process started",
        sprintf("pid=%s, log=%s", proc$get_pid(), Typing$log_file)
      )
      runjs(sprintf(
        "var el = document.getElementById('%s'); if (el) el.classList.add('is-animating');",
        ns("progress")
      ))
      updateProgressBar(
        session,
        "progress",
        value = 0,
        total = length(Typing$queued_strains),
        status = "primary"
      )
      # Break the skipped count down the same way as the selection_summary
      # badges (already present vs. manually excluded), so this message never
      # contradicts what the badges just showed.
      n_present <- sum(Typing$strains %in% existing())
      n_excluded <- length(Typing$strains) -
        length(Typing$queued_files) -
        n_present
      showNotification(
        paste0(
          length(Typing$queued_strains),
          " genome(s) queued.",
          if (n_present > 0) sprintf(" %d already present,", n_present),
          if (n_excluded > 0) sprintf(" %d excluded,", n_excluded),
          if (n_present > 0 || n_excluded > 0) " skipped."
        ),
        type = "message",
        duration = 3
      )
    }

    # Terminate a running or checking batch. During checking there is no process
    # yet - the checking observe sees the flag and abandons the pass; during a
    # run the process tree is killed and the poll loop finalises the results.
    observeEvent(input$terminate, {
      if (identical(Typing$status, "checking")) {
        log_typing("Terminate requested during checking phase")
        Typing$terminated <- TRUE
        return()
      }
      req(identical(Typing$status, "running"), !is.null(Typing$proc))
      log_typing("Terminate requested: killing typing process")
      Typing$terminated <- TRUE
      tryCatch(Typing$proc$kill_tree(), error = function(e) NULL)
    })

    # Incremented whenever a typing run (completed or terminated) has written at
    # least one new strain to the database. Read by main.R, which turns it into
    # the "New isolates added" prompt offering a full database reload — the
    # heavy path that also re-runs the allele-hash backfill and releases the
    # modules that hold back automatic refresh while they carry unsaved edits.
    # What the other modules re-read on their own goes through db_rev instead.
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

      # The scheme's profile table lands next to the reference DB once the build
      # step is through; read it once per run, then every poll can resolve the
      # STs claMLST left undetermined.
      if (is.null(Typing$cla_profiles) && !is.null(Typing$cla_db)) {
        Typing$cla_profiles <- read_st_profiles(
          paste0(Typing$cla_db, ".profiles.tsv")
        )
      }

      results <- parse_typing_log(
        lines,
        Typing$queued_strains,
        Typing$cla_profiles
      )
      Typing$results <- results

      # Bank each isolate's results the moment its step is over, rather than
      # holding the whole run's worth until the process exits.
      persist_results(results, lines)
      # Genomes whose whole pipeline is over - allele calling, classical MLST and
      # the AMR screen alike. The bar counts genomes, so a genome still being
      # screened is not one of them; the phase it is in is named by the status
      # line underneath instead of being rounded up into the bar.
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
      log_typing(
        "Process exited",
        sprintf(
          "status=%s, %d/%d genome(s) finished",
          Typing$status,
          done,
          length(Typing$queued_strains)
        )
      )
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

      # Everything the run produced has been banked isolate by isolate as it
      # came in; this is the closing sweep. It re-attempts only what failed
      # earlier - by now the pyMLST process is gone, so a write that lost the
      # race for the database lock can go through - and picks up the last
      # genome, whose steps finished after the final poll. Both the interim
      # reference DB and the AMR output directory are still around at this
      # point, so a retry has everything it needs; they are deleted below.
      persist_results(results, lines, retry = TRUE)

      meta <- parse_clamlst_meta(lines)
      if (Typing$cla_enabled) {
        log_typing(
          "Classical MLST results persisted",
          sprintf(
            "%d isolate(s) | repo=%s scheme=%s",
            length(persisted$done$cla),
            meta$repository,
            meta$scheme
          )
        )
      }
      cleanup_cla_db(Typing$cla_db)
      if (!is.null(Typing$cla_spec)) {
        unlink(Typing$cla_spec)
      }
      Typing$cla_db <- NULL
      Typing$cla_spec <- NULL
      Typing$cla_profiles <- NULL
      Typing$cla_refs <- NULL
      live_cla_db <<- NULL

      added <- sum(results$status == "Added")
      duplicate <- sum(results$status == "Duplicate")
      failed <- sum(results$status %in% c("Incompatible", "Error"))
      log_typing(
        "Run outcome",
        sprintf("%d added, %d duplicate, %d failed", added, duplicate, failed)
      )

      log_typing(
        "Genome hashes persisted",
        sprintf("%d strain(s)", length(persisted$done$hash))
      )

      amr_enabled <- !is.null(Typing$amr_out)
      amr_screened <- length(persisted$done$amr)
      amr_elements <- 0L
      if (amr_enabled) {
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
        log_typing(
          "AMR results persisted",
          sprintf(
            "%d screened, %d element(s) detected",
            amr_screened,
            amr_elements
          )
        )
        # The per-run AMR output dir is a disposable artifact: every screen it
        # holds has been read into amr_results / amr_summary by now.
        cleanup_amr_out(Typing$amr_out)
        Typing$amr_out <- NULL
        live_amr_out <<- NULL
      }

      # Bring the metadata table up to date - one row per isolate `added`
      # above just put into `mlst` - before announcing the change below.
      # Explicit and synchronous, not left for whichever reactive across
      # whichever module happens to read the metadata table next: that used
      # to be how this row got backfilled, and since two different modules
      # could each be the one to trigger it, one could catch a table the
      # other had not backfilled yet and the two would disagree about which
      # isolates existed. Doing it here means every reader downstream of the
      # bump - regardless of which one runs first - sees the same, already
      # -synced table. See `sync_metadata_table()`'s docs for the full case.
      if (added > 0L) {
        sync_metadata_table(db_path())
      }

      # Signal other modules that the DB has new data.
      #
      # Announced once here, at the end of the run, rather than per strain: the
      # allele calls are written by pyMLST in a separate process, so for the
      # whole run the database is a moving target that no reader should be
      # invalidated onto. Writing early and announcing early are separate
      # concerns - the per-isolate persistence above deliberately stays silent.
      # By this point the pyMLST process has exited, every in-process write has
      # committed and the metadata sync above has run, so this is the first
      # moment the database is both changed and consistent.
      #
      # `schema` because new isolates bring new allele rows into `sequences`;
      # `metadata` because the sync above just back-filled a row per new
      # isolate. `amr` only when screening actually stored something.
      if (added > 0L) {
        domains <- c("isolates", "metadata", "schema")
        if (amr_screened > 0L) {
          domains <- c(domains, "amr")
        }
        do.call(db_events$bump, c(list(db_rev), as.list(domains)))
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

    # TRUE while a run is under way, for modules that must not read the
    # database as if it were settled.
    #
    # "checking" as well as "running": the genome-hash pass is the last moment
    # before pyMLST starts writing, and a distance computation begun there
    # would still be in flight when it does. `db_updated` says "new isolates
    # have landed, and the database is consistent again"; this says the
    # opposite, for the window in between - during a run pyMLST writes `mlst`
    # from a separate process, and nothing bumps db_rev until finalize, so a
    # reader that queried `mlst` directly right now would see half a run.
    typing_active <- reactive(
      Typing$status %in% c("checking", "running")
    )

    list(db_updated = db_updated, typing_active = typing_active)
  })
}
