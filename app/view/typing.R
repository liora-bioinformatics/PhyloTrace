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
    tooltip,
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
  DT[DTOutput, renderDT, datatable, dataTableProxy, replaceData],
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
      title = div(
        class = "typing-sidebar-title",
        div(class = "sidebar-title", "Add Isolates"),
        tooltip(
          icon("circle-info", class = "text-muted"),
          paste(
            "Select a single assembled genome (.fasta, .fa, .fna) or a folder",
            "of assemblies to type against the loaded scheme."
          ),
          placement = "right"
        )
      ),
      width = 320,
      uiOutput(ns("scheme_info")),
      shinyFilesButton(
        ns("genome_file"),
        "Select File",
        title = "Choose a genome assembly",
        icon = icon("file-lines"),
        buttonType = "default",
        multiple = FALSE,
        root = path_home()
      ),
      shinyDirButton(
        ns("genome_dir"),
        "Select Folder",
        title = "Choose a folder of assemblies",
        icon = icon("folder-open"),
        buttonType = "default",
        root = path_home()
      ),
      accordion(
        open = TRUE,
        accordion_panel(
          "Parameters",
          icon = icon("sliders"),
          numericInput(
            ns("identity"),
            tooltip(
              span("Min. identity ", icon("circle-info", class = "text-muted")),
              paste(
                "Minimum sequence identity for BLAT to call a locus: the",
                "fraction of identical bases between the assembly and the",
                "reference allele (passed to BLAT as -minIdentity). Raise it for",
                "stricter matches; lower it to recover more divergent alleles."
              )
            ),
            value = 0.95,
            min = 0,
            max = 1,
            step = 0.01
          ),
          numericInput(
            ns("coverage"),
            tooltip(
              span("Min. coverage ", icon("circle-info", class = "text-muted")),
              paste(
                "Minimum fraction of a reference locus the alignment must span",
                "to keep a hit (aligned length / locus length). Hits below this",
                "are discarded; partial hits above it are aligned and gap-filled."
              )
            ),
            value = 0.9,
            min = 0,
            max = 1,
            step = 0.01
          ),
          radioButtons(
            ns("cla_repo"),
            tooltip(
              span(
                "Classical MLST source ",
                icon("circle-info", class = "text-muted")
              ),
              paste(
                "Online repository used to fetch the classical 7-gene MLST",
                "scheme for this species (via claMLST import). The Sequence Type",
                "(ST) is derived alongside cgMLST typing. If the selected",
                "repository has no scheme for the species, the other one is tried",
                "automatically; if neither has it, the ST is left blank."
              )
            ),
            choices = c("PubMLST" = "pubmlst", "Pasteur" = "pasteur"),
            selected = "pubmlst",
            inline = TRUE
          ),
          checkboxInput(
            ns("run_amr"),
            tooltip(
              span(
                "Screen for antimicrobial resistance ",
                icon("circle-info", class = "text-muted")
              ),
              paste(
                "Screen each newly typed genome for antimicrobial resistance",
                "with abritamr / AMRFinderPlus, alongside cgMLST typing.",
                "Detects acquired resistance genes, virulence factors and",
                "stress/metal elements; resistance point mutations are added",
                "when the scheme's species is supported. Results are stored in",
                "the database (amr_results / amr_summary)."
              )
            ),
            value = TRUE
          )
        )
      ),
      disabled(actionButton(
        ns("start"),
        "Start Typing",
        icon = icon("play")
      )),
      disabled(actionButton(
        ns("terminate"),
        "Terminate",
        icon = icon("stop"),
        class = "btn-danger"
      )),
      uiOutput(ns("status_badge"))
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
      textOutput(ns("current_strain"))
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
          "Typing Log",
          icon = icon("terminal"),
          verbatimTextOutput(ns("log"))
        ),
        accordion_panel(
          "Typing Results",
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
      } else {
        enable("genome_file")
        removeClass("genome_file", "custom-disable")
        enable("genome_dir")
        removeClass("genome_dir", "custom-disable")
        enable("identity")
        enable("coverage")
        enable("cla_repo")
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

    # Current strain / phase line under the progress bar
    output$current_strain <- renderText({
      results <- Typing$results
      if (is.null(results)) {
        return("")
      }
      running <- results$strain[results$status == "Running"]
      if (length(running)) {
        paste("Typing:", running[1])
      } else if (identical(Typing$status, "done")) {
        "All genomes processed."
      } else {
        ""
      }
    })

    # Status badge in the sidebar
    output$status_badge <- renderUI({
      spec <- switch(
        Typing$status,
        idle = c("secondary", "Idle"),
        running = c("info", "Running ..."),
        done = c("success", "Complete"),
        terminated = c("warning", "Terminated"),
        failed = c("danger", "Failed"),
        c("secondary", Typing$status)
      )
      div(
        class = "mt-2",
        span(class = paste0("badge text-bg-", spec[1]), spec[2])
      )
    })

    # Builds the display data frame for the results table. Kept as a local
    # function so both the initial renderDT and the live replaceData observer
    # produce identical column structure.
    build_results_df <- function(results, total) {
      completeness <- if (!is.na(total) && total > 0) {
        round(results$found / total * 100, 1)
      } else {
        rep(NA_real_, nrow(results))
      }
      completeness[results$status != "Added"] <- NA_real_

      dash <- function(x) ifelse(is.na(x), "—", as.character(x))

      # Classical MLST cell: the registered ST number when known, otherwise a
      # badge flagging a novel profile (complete but unregistered) or a partial
      # one (some loci uncalled). "—" while pending / when no classical result.
      st_cell <- function(status, st) {
        if (is.na(status)) {
          return("—")
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
          function(i) st_cell(results$cla_status[i], results$st[i]),
          character(1)
        )
      } else {
        dash(results$st)
      }

      # AMR screening cell: a hit count once done, a live "screening …" badge
      # while abritamr runs, a failure flag, or "—" when AMR was not run. The
      # count spans all detected elements (AMR + virulence + stress/metal).
      amr_cell <- function(status, n) {
        if (is.null(status) || is.na(status)) {
          return("—")
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
          function(i) amr_cell(results$amr_status[i], results$amr_elements[i]),
          character(1)
        )
      } else {
        rep("—", nrow(results))
      }

      data.frame(
        Strain = results$strain,
        Status = vapply(results$status, status_badge, character(1)),
        ST = st_column,
        AMR = amr_column,
        `Loci found` = dash(results$found),
        `Alleles added` = dash(results$added),
        Partial = dash(results$partial),
        Filled = dash(results$filled),
        Removed = dash(results$removed),
        Completeness = vapply(completeness, completeness_badge, character(1)),
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
        isolate(build_results_df(Typing$results, scheme_total())),
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE
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
        build_results_df(results, scheme_total()),
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
    outputOptions(output, "status_badge", suspendWhenHidden = FALSE)

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
