# app/view/db_wizard.R
#
# Guided setup of a new PhyloTrace database: species, cgMLST scheme, optional
# classical MLST scheme, then review, AMR screening and where to save it. All
# scheme data comes from files the user supplies.

box::use(
  bslib,
  rlang[`%||%`],
  shiny,
  shinyWidgets[virtualSelectInput],
  shinyjs[hide, html, toggleState, useShinyjs],
  stats[setNames],
  waiter[Waiter, spin_flower],
)

box::use(
  app / logic / amr[amr_organisms, amr_species],
  app / logic / db_build[cancel_db_build, finish_db_build, start_db_build],
  app / logic / functions[render_info],
  app / logic / scheme_files,
  app / logic / scheme_index[CORE_TYPES, schemes_for_species],
  app / logic / scheme_sources[safe_url],
  app / logic / session_restart[restart_session],
  app / logic / setup_ui,
  app / logic / species,
  app / view / file_dialog,
  app / view / mlst_import,
  app / view / scheme_source,
)

# Setup steps in order: panel value and stepper label.
STEPS <- c(
  species = "Species",
  cgmlst = "cgMLST scheme",
  mlst = "MLST scheme",
  save = "Review & create"
)

# Navbar tab of the setup, reopened when a new setup is started.
WIZARD_TAB <- "db_wizard_panel"

# Columns of a locus annotation table the cgMLST overview can show.
MAPPING_FIELDS <- c(
  locus = "Locus ID",
  gene = "Gene",
  product = "Product",
  start = "Start",
  length = "Length"
)

# Shown on a task until the allele files have been read.
NEEDS_ALLELES <- "Available once the allele files have been read."

# Display labels of the abritamr organism tokens ("Escherichia" covers a genus).
organism_choices <- function() {
  tokens <- amr_organisms()
  labels <- ifelse(
    grepl("_", tokens, fixed = TRUE),
    gsub("_", " ", tokens, fixed = TRUE),
    paste(tokens, "(genus)")
  )
  setNames(tokens, labels)
}

# Label/value rows of a setup summary; `missing` rows are flagged.
summary_table <- function(rows) {
  shiny$div(
    class = "setup-summary",
    lapply(rows, function(r) {
      shiny$div(
        class = paste("setup-summary_row", if (isTRUE(r$missing)) "is-missing"),
        shiny$span(class = "setup-summary_label", r$label),
        shiny$span(class = "setup-summary_value", r$value)
      )
    })
  )
}

# Step 1: the species, from the catalog or entered by hand.
species_step <- function(ns) {
  shiny$div(
    class = "setup-columns",
    shiny$div(
      class = "setup-form",
      setup_ui$setup_section(
        "Species",
        shiny$div(
          class = "setup-help",
          "A database holds isolates of one species, or of a group of closely",
          " related species typed with the same scheme."
        ),
        shiny$radioButtons(
          ns("species_mode"),
          NULL,
          choices = c(
            "Species catalog" = "catalog",
            "Other species or species group" = "custom"
          ),
          inline = TRUE,
          width = "100%"
        ),
        shiny$conditionalPanel(
          "input.species_mode == 'catalog'",
          ns = ns,
          virtualSelectInput(
            ns("species_pick"),
            "Species",
            choices = species$species_catalog()$label,
            selected = NULL,
            autoSelectFirstOption = FALSE,
            search = TRUE,
            placeholder = "Select a species ...",
            searchPlaceholderText = "Search species ...",
            optionsCount = 10,
            # Opens as a popup over the window, like the other pickers in the
            # app, rather than a dropdown spanning the page.
            dropboxWrapper = "body",
            showDropboxAsPopup = TRUE,
            popupDropboxBreakpoint = "10000px",
            width = "100%"
          )
        ),
        shiny$conditionalPanel(
          "input.species_mode == 'custom'",
          ns = ns,
          shiny$div(
            class = "setup-grid allow-free-text",
            shiny$textInput(ns("genus"), "Genus", placeholder = "Klebsiella", width = "100%"),
            shiny$textInput(
              ns("epithets"),
              "Species",
              placeholder = "pneumoniae, variicola, quasipneumoniae",
              width = "100%"
            )
          ),
          shiny$div(
            class = "setup-help",
            "Separate several species with commas to define a species group.",
            " Leave the species empty for a genus-wide database."
          )
        )
      )
    ),
    shiny$div(
      class = "setup-report",
      shiny$uiOutput(ns("species_card")),
      shiny$uiOutput(ns("species_schemes"))
    )
  )
}

# Step 2: the cgMLST scheme as four numbered tasks - source and files on the
# left, the optional annotation and the scheme's details on the right.
cgmlst_step <- function(ns) {
  shiny$tagList(
    shiny$uiOutput(ns("cg_task_states")),
    shiny$div(
      class = "setup-columns",
      shiny$div(
        class = "setup-form",
        setup_ui$setup_task(
          ns("cg_t1"),
          1,
          "Scheme source",
          shiny$uiOutput(ns("cg_published")),
          scheme_source$ui(ns("cg_source"), types = CORE_TYPES)
        ),
        setup_ui$setup_task(
          ns("cg_t2"),
          2,
          "Allele files",
          shiny$div(
            class = "setup-pick",
            file_dialog$ui(ns("cg_alleles"), "Select allele files", "dna"),
            shiny$uiOutput(ns("cg_alleles_label"), inline = TRUE)
          ),
          shiny$div(
            class = "setup-help",
            "One FASTA file per locus (ZIP archive, folder or individual files), or",
            " one FASTA file with one sequence per locus. The first allele of each",
            " locus becomes its reference."
          ),
          shiny$uiOutput(ns("cg_report")),
          locked_hint = "Select and confirm the scheme source first."
        )
      ),
      shiny$div(
        class = "setup-form",
        setup_ui$setup_task(
          ns("cg_t3"),
          3,
          "Locus annotation",
          optional = TRUE,
          locked_hint = NEEDS_ALLELES,
          shiny$div(
            class = "setup-pick",
            file_dialog$ui(ns("cg_table"), "Select locus table", "table"),
            shiny$uiOutput(ns("cg_table_label"), inline = TRUE)
          ),
          shiny$div(
            class = "setup-help",
            "Gene names, products and positions shown in the cgMLST overview (CSV,",
            " TSV or Excel). Loci without a row remain unannotated."
          ),
          shiny$uiOutput(ns("cg_mapping"))
        ),
        setup_ui$setup_task(
          ns("cg_t4"),
          4,
          "Scheme details",
          locked_hint = NEEDS_ALLELES,
          shiny$div(
            class = "setup-grid allow-free-text",
            shiny$textInput(ns("cg_name"), "Scheme name", width = "100%"),
            shiny$textInput(ns("cg_version"), "Version", placeholder = "e.g. 1.0", width = "100%"),
            shiny$textInput(
              ns("cg_url"),
              "Scheme web page (optional)",
              placeholder = "https://",
              width = "100%"
            ),
            shiny$textInput(ns("cg_seed"), "Seed genome (optional)", width = "100%"),
            shiny$numericInput(
              ns("cg_distance"),
              "Cluster threshold (optional)",
              value = NA,
              min = 0,
              step = 1,
              width = "100%"
            ),
            shiny$textInput(
              ns("cg_citation"),
              "Scheme publication (optional)",
              placeholder = "Authors, journal, year",
              width = "100%"
            )
          )
        )
      )
    )
  )
}

# Step 3: the optional classical MLST scheme.
mlst_step <- function(ns) {
  shiny$tagList(
    shiny$div(
      class = "setup-toggle",
      bslib$input_switch(ns("mlst_on"), "Add a classical MLST scheme", value = FALSE),
      shiny$div(
        class = "setup-help",
        "Optional. With a scheme, every typed isolate is also assigned its",
        " sequence type (ST). A scheme can also be added to the database later."
      )
    ),
    shiny$conditionalPanel("input.mlst_on", ns = ns, mlst_import$ui(ns("mlst")))
  )
}

# Step 4: summary, AMR screening, storage location and the build itself.
save_step <- function(ns) {
  shiny$div(
    class = "setup-columns",
    shiny$div(
      class = "setup-form",
      setup_ui$setup_section("Summary", shiny$uiOutput(ns("summary"))),
      setup_ui$setup_section(
        "AMR screening",
        shiny$uiOutput(ns("amr_report")),
        shiny$tags$details(
          class = "setup-advanced",
          shiny$tags$summary("Change the point-mutation catalogue"),
          shiny$selectInput(
            ns("amr_organism"),
            NULL,
            choices = c("None (acquired genes only)" = "", organism_choices()),
            width = "100%"
          )
        )
      ),
      setup_ui$setup_section(
        "Storage location",
        shiny$div(
          class = "setup-pick",
          file_dialog$ui(ns("target"), "Choose location and name", "floppy-disk"),
          shiny$uiOutput(ns("target_label"), inline = TRUE)
        ),
        shiny$div(
          class = "storage-location-hint",
          shiny$icon("circle-info"),
          " Choose a folder on this computer. Databases kept on network or",
          " shared drives (NFS, SMB, cloud-synced folders) can be corrupted."
        ),
        shiny$div(
          class = "setup-actions",
          shiny$actionButton(
            ns("create"),
            "Create Database",
            icon = shiny$icon("database"),
            class = "btn-primary"
          )
        ),
        shiny$uiOutput(ns("create_hint"))
      )
    ),
    shiny$div(class = "setup-report", shiny$uiOutput(ns("result")))
  )
}

# The end of a setup: the database exists, and it can only be loaded or a new
# setup started - never edited into a different database.
done_step <- function(ns) {
  shiny$div(
    class = "setup-done",
    shiny$uiOutput(ns("done")),
    shiny$div(
      class = "setup-actions setup-done_actions",
      shiny$actionButton(
        ns("load_db"),
        "Load Database",
        icon = shiny$icon("angles-right"),
        class = "btn-primary btn-attention"
      ),
      shiny$actionButton(
        ns("new_setup"),
        "Create Another Database",
        icon = shiny$icon("plus")
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_fillable(
    useShinyjs(),
    bslib$as_fill_carrier(shiny$div(
      id = ns("wizard"),
      class = "setup-wizard",
      bslib$card(
        fill = TRUE,
        bslib$card_header(class = "setup-header", shiny$uiOutput(ns("stepper"))),
        bslib$card_body(
          bslib$navset_hidden(
            id = ns("steps"),
            bslib$nav_panel_hidden("species", species_step(ns)),
            bslib$nav_panel_hidden("cgmlst", cgmlst_step(ns)),
            bslib$nav_panel_hidden("mlst", mlst_step(ns)),
            bslib$nav_panel_hidden("save", save_step(ns)),
            bslib$nav_panel_hidden("done", done_step(ns))
          )
        ),
        bslib$card_footer(
          id = ns("footer"),
          class = "setup-footer",
          shiny$actionButton(ns("back"), "Back", icon = shiny$icon("arrow-left")),
          shiny$uiOutput(ns("step_hint"), class = "setup-footer_hint"),
          shiny$actionButton(
            ns("next_step"),
            "Next",
            icon = shiny$icon("arrow-right"),
            class = "btn-primary"
          )
        )
      )
    ))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    state <- shiny$reactiveValues(
      step = 1L,
      reached = 1L,
      # Parsed cgMLST scheme, or the error reading it raised.
      cg = NULL,
      # Locus annotation table, or the error reading it raised.
      cg_table = NULL,
      target = NULL,
      # Scheme name last proposed from the species, so a name the user typed
      # is never overwritten but a stale proposal is.
      proposed_name = "",
      # The running build: process handle, plan and start time.
      build = NULL,
      # Error of the last failed build, shown on the review step.
      error = NULL,
      # The finished database; once set, the setup is closed.
      result = NULL
    )

    # Plain mirror of the running build, so session teardown - outside any
    # reactive context - can still stop it and remove its unfinished database.
    live_build <- NULL
    session$onSessionEnded(function() cancel_db_build(live_build))

    cg_source <- scheme_source$server("cg_source")
    mlst <- mlst_import$server("mlst", species = shiny$reactive(chosen_species()$name))

    cg_paths <- file_dialog$server(
      "cg_alleles",
      mode = "file",
      title = "Select the cgMLST allele files",
      pattern = scheme_files$ALLELE_FILE_RE,
      multiple = TRUE,
      allow_dir = TRUE,
      hint = paste(
        "Select a ZIP archive or FASTA files (Ctrl-click for several), or open",
        "the folder that holds the locus files and choose 'Use this folder'."
      )
    )
    cg_table_path <- file_dialog$server(
      "cg_table",
      mode = "file",
      title = "Select the locus annotation table",
      pattern = scheme_files$TABLE_FILE_RE
    )
    target_path <- file_dialog$server(
      "target",
      mode = "save",
      title = "Save the new database",
      default_name = function() species$default_db_name(shiny$isolate(chosen_species()$name)),
      extension = "db",
      hint = paste(
        "Choose a folder on this computer's own disk. Network and cloud-synced",
        "folders can corrupt a database."
      )
    )

    # Species -----------------------------------------------------------------

    # The species as it will be stored: name, taxonomy id (catalog species
    # only), catalog record and, when it cannot be used yet, the reason.
    chosen_species <- shiny$reactive({
      if (identical(input$species_mode, "custom")) {
        epithets <- strsplit(input$epithets %||% "", "[,;/[:space:]]+")[[1]]
        epithets <- epithets[nzchar(epithets)]
        problem <- species$species_problem(input$genus, epithets)
        return(list(
          name = if (is.na(problem)) species$compose_species(input$genus, epithets) else NA,
          taxid = NA_integer_,
          record = NULL,
          problem = if (is.na(problem)) NULL else problem
        ))
      }
      label <- input$species_pick
      if (is.null(label) || !nzchar(label)) {
        return(list(name = NA, taxid = NA_integer_, record = NULL, problem = "Select a species."))
      }
      record <- species$species_record(label)
      list(
        name = label,
        taxid = as.integer(record$ncbi_taxid %||% NA),
        record = record,
        problem = NULL
      )
    })

    # Species card: name, taxonomy and description of a catalog species, or the
    # composed name of a custom one.
    output$species_card <- shiny$renderUI({
      render_info("output$species_card")
      sp <- chosen_species()
      if (is.na(sp$name)) {
        return(setup_ui$setup_note(paste(
          "Select the species or species group of this database. It determines",
          "how isolates are labelled and which point mutations AMR screening reports."
        )))
      }
      record <- sp$record
      image <- species$species_image(sp$name)
      ranks <- c("phylum", "class", "order", "family", "genus")
      shiny$div(
        class = "species-card_article",
        if (!is.na(image)) {
          shiny$div(class = "species-card_media", shiny$tags$img(src = image, alt = sp$name))
        },
        shiny$div(
          class = "species-details",
          shiny$div(
            class = "species-details_header",
            shiny$span(shiny$em(sp$name), class = "species-details_name"),
            if (!is.null(record$rank)) shiny$span(record$rank, class = "species-details_rank"),
            if (!is.na(sp$taxid)) {
              shiny$a(
                href = paste0(
                  "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
                  sp$taxid
                ),
                target = "_blank",
                rel = "noopener",
                class = "species-details_taxid",
                paste0("NCBI:txid", sp$taxid)
              )
            }
          ),
          if (!is.null(record)) {
            shiny$div(
              class = "species-details_lineage",
              lapply(ranks, function(rank) {
                value <- record$lineage[[rank]]
                if (!is.null(value)) {
                  shiny$div(
                    class = "species-details_chip",
                    shiny$span(toupper(rank), class = "species-details_chip-rank"),
                    shiny$span(value, class = "species-details_chip-value")
                  )
                }
              })
            )
          }
        ),
        if (!is.null(record$summary)) shiny$p(record$summary, class = "species-details_summary")
      )
    })

    # Published schemes for the chosen species, from the index shipped with the
    # app; no provider is contacted.
    published <- shiny$reactive(schemes_for_species(chosen_species()$name))

    # Where schemes for the species are published.
    output$species_schemes <- shiny$renderUI({
      render_info("output$species_schemes")
      sp <- chosen_species()
      shiny$req(!is.na(sp$name))
      rows <- published()
      setup_ui$setup_section(
        "Available schemes",
        if (nrow(rows)) {
          shiny$tagList(
            shiny$div(
              class = "setup-help",
              "Download the scheme files from the provider's page; they are",
              " imported in the next steps."
            ),
            setup_ui$scheme_links(rows)
          )
        } else {
          setup_ui$setup_note(paste(
            "No published scheme is listed for this species. Check the providers'",
            "sites (cgMLST.org, PubMLST, Institut Pasteur, Chewie-NS) or use an",
            "in-house scheme."
          ))
        }
      )
    })

    # Core genome schemes of the species, offered in the source task.
    core_published <- shiny$reactive({
      rows <- published()
      rows[rows$type %in% CORE_TYPES, , drop = FALSE]
    })

    output$cg_published <- shiny$renderUI({
      rows <- core_published()
      if (nrow(rows)) {
        shiny$tagList(
          shiny$div(class = "setup-subtitle", "Available for this species"),
          setup_ui$scheme_links(rows, use_input = ns("cg_use"))
        )
      }
    })

    # Fills the provider, name and page of a published scheme into the form.
    shiny$observeEvent(input$cg_use, {
      row <- core_published()[input$cg_use$i, , drop = FALSE]
      shiny$req(nrow(row) == 1L)
      shiny$updateSelectInput(session, "cg_source-provider", selected = row$provider)
      shiny$updateTextInput(session, "cg_name", value = row$scheme)
      shiny$updateTextInput(session, "cg_url", value = row$url)
    })

    # Follows the species: proposes a scheme name and the AMR catalogue.
    shiny$observeEvent(chosen_species()$name, {
      name <- chosen_species()$name
      shiny$req(!is.na(name))
      current <- trimws(input$cg_name %||% "")
      if (!nzchar(current) || identical(current, state$proposed_name)) {
        state$proposed_name <- paste(gsub("_", " ", species$default_db_name(name)), "cgMLST")
        shiny$updateTextInput(session, "cg_name", value = state$proposed_name)
      }
      organism <- amr_species(name)
      shiny$updateSelectInput(
        session,
        "amr_organism",
        selected = if (is.na(organism)) "" else organism
      )
    })

    # cgMLST scheme -----------------------------------------------------------

    # Reads the picked allele files into the scheme's reference set.
    shiny$observeEvent(cg_paths(), {
      shiny$withProgress(message = "Reading the scheme ...", value = 0, {
        state$cg <- tryCatch(
          scheme_files$read_cg_scheme(
            cg_paths(),
            progress = function(done, total) {
              shiny$setProgress(done / total, detail = sprintf("%d of %d loci", done, total))
            }
          ),
          error = function(e) e
        )
      })
    })

    # Reads the picked locus annotation table.
    shiny$observeEvent(cg_table_path(), {
      state$cg_table <- tryCatch(
        scheme_files$read_scheme_table(cg_table_path()),
        error = function(e) e
      )
    })

    # Drops the locus annotation again.
    shiny$observeEvent(input$cg_table_clear, {
      state$cg_table <- NULL
    })

    output$cg_alleles_label <- shiny$renderUI(setup_ui$picked_label(cg_paths()))
    output$cg_table_label <- shiny$renderUI({
      if (is.null(state$cg_table)) {
        setup_ui$picked_label(NULL, "No table selected")
      } else {
        shiny$tagList(
          setup_ui$picked_label(cg_table_path()),
          shiny$actionLink(ns("cg_table_clear"), "Remove", icon = shiny$icon("xmark"))
        )
      }
    })

    # TRUE once the allele files have been read into a usable scheme.
    cg_ready <- shiny$reactive(is.list(state$cg) && !inherits(state$cg, "error"))

    # TRUE once the scheme and the annotation table are both readable.
    has_table <- shiny$reactive(is.data.frame(state$cg_table) && cg_ready())

    # The annotation columns as guessed from the table and the scheme's loci.
    guessed_mapping <- shiny$reactive({
      shiny$req(has_table())
      scheme_files$guess_locus_columns(state$cg_table, state$cg$loci$locus)
    })

    # Column pickers of the annotation table, preset to the guess.
    output$cg_mapping <- shiny$renderUI({
      if (inherits(state$cg_table, "error")) {
        return(setup_ui$setup_note(conditionMessage(state$cg_table), "bad"))
      }
      if (!has_table()) {
        return(NULL)
      }
      guess <- guessed_mapping()
      headers <- names(state$cg_table)
      columns <- c("(not in the table)" = "", setNames(headers, headers))
      shiny$div(
        class = "setup-grid setup-mapping",
        lapply(names(MAPPING_FIELDS), function(field) {
          shiny$selectInput(
            ns(paste0("map_", field)),
            MAPPING_FIELDS[[field]],
            choices = columns,
            selected = guess[[field]] %||% "",
            width = "100%"
          )
        })
      )
    })

    # The annotation columns in effect: the user's pick, else the guess.
    mapping <- shiny$reactive({
      guess <- guessed_mapping()
      picks <- lapply(names(MAPPING_FIELDS), function(field) {
        pick <- input[[paste0("map_", field)]]
        if (is.null(pick)) {
          guess[[field]]
        } else if (pick %in% names(state$cg_table)) {
          pick
        } else {
          NA_character_
        }
      })
      setNames(picks, names(MAPPING_FIELDS))
    })

    # The `targets` table: every locus, annotated where the table has a row.
    cg_targets <- shiny$reactive({
      shiny$req(cg_ready())
      loci <- state$cg$loci$locus
      if (has_table()) {
        scheme_files$build_targets(loci, state$cg_table, mapping())
      } else {
        scheme_files$build_targets(loci)
      }
    })

    # Check of the allele files, right under the picker: figures and rejects.
    output$cg_report <- shiny$renderUI({
      render_info("output$cg_report")
      cg <- state$cg
      if (is.null(cg)) {
        return(NULL)
      }
      if (inherits(cg, "error")) {
        return(setup_ui$setup_note(conditionMessage(cg), "bad"))
      }
      rejected <- nrow(cg$issues)
      flipped <- sum(cg$loci$flipped)
      shiny$div(
        class = "setup-check",
        setup_ui$stat_row(
          c("Loci ready" = nrow(cg$loci), "Files read" = cg$n_files, "Rejected" = rejected),
          tones = c("good", "", if (rejected) "warn" else "")
        ),
        if (nrow(cg$loci) < 50L) {
          setup_ui$setup_note(
            paste(
              "This is a very small scheme. A cgMLST scheme usually has several",
              "hundred to a few thousand loci."
            ),
            "warn"
          )
        },
        if (flipped) {
          setup_ui$setup_note(sprintf(
            "%d reference sequence(s) stored reverse-complemented to read as coding sequence.",
            flipped
          ))
        },
        if (rejected) {
          setup_ui$setup_section("Rejected loci", setup_ui$issue_list(cg$issues))
        } else {
          setup_ui$setup_note("All loci passed the checks.", "good")
        }
      )
    })

    # What the cgMLST step still needs, NULL when complete.
    cg_problem <- shiny$reactive({
      src <- cg_source()
      url <- trimws(input$cg_url %||% "")
      distance <- input$cg_distance
      if (is.null(src)) {
        "Select the scheme provider."
      } else if (!isTRUE(src$confirmed)) {
        "Confirm the provider's terms of use."
      } else if (is.null(state$cg)) {
        "Select the allele files."
      } else if (inherits(state$cg, "error")) {
        "The allele files could not be read."
      } else if (inherits(state$cg_table, "error")) {
        "The locus table could not be read. Select another one or remove it."
      } else if (!nzchar(trimws(input$cg_name %||% ""))) {
        "Enter a scheme name."
      } else if (nzchar(url) && !safe_url(url)) {
        "The scheme web page must be an http(s) address."
      } else if (length(distance) == 1L && !is.na(distance) && distance < 0) {
        "The cluster threshold cannot be negative."
      }
    })

    # State of the four cgMLST tasks: each unlocks once the one it builds on is
    # complete, so the order of work is visible at a glance.
    output$cg_task_states <- setup_ui$task_states(function() {
      source_done <- isTRUE(cg_source()$confirmed)
      files_done <- source_done && cg_ready()
      annotated <- if (has_table()) cg_targets()$annotated else 0L
      setNames(
        c(
          if (source_done) "done" else "current",
          if (!source_done) "locked" else if (files_done) "done" else "current",
          if (!files_done) "locked" else if (annotated > 0L) "done" else "open",
          if (!files_done) "locked" else if (is.null(cg_problem())) "done" else "current"
        ),
        ns(c("cg_t1", "cg_t2", "cg_t3", "cg_t4"))
      )
    })

    output$cg_t1_status <- shiny$renderUI({
      src <- cg_source()
      if (isTRUE(src$confirmed)) setup_ui$task_status(src$label)
    })
    output$cg_t2_status <- shiny$renderUI({
      if (cg_ready()) {
        setup_ui$task_status(sprintf("%s loci", format(nrow(state$cg$loci), big.mark = ",")))
      } else if (inherits(state$cg, "error")) {
        setup_ui$task_status("Not readable", "bad")
      }
    })
    output$cg_t3_status <- shiny$renderUI({
      if (has_table()) {
        targets <- cg_targets()
        setup_ui$task_status(
          sprintf(
            "%s of %s loci annotated",
            format(targets$annotated, big.mark = ","),
            format(nrow(targets$targets), big.mark = ",")
          ),
          if (targets$annotated == 0L) "warn" else ""
        )
      }
    })

    # AMR ---------------------------------------------------------------------

    # The organism point mutations are screened for; "" for genes only.
    amr_organism <- shiny$reactive(input$amr_organism %||% "")

    # What AMR screening will report. The catalogue follows from the species,
    # as typing always chose it; the override is for the rare mismatch.
    output$amr_report <- shiny$renderUI({
      render_info("output$amr_report")
      name <- chosen_species()$name
      suggested <- if (is.na(name)) NA_character_ else amr_species(name)
      organism <- amr_organism()
      label <- names(organism_choices())[match(organism, organism_choices())]
      shiny$tagList(
        if (nzchar(organism)) {
          setup_ui$setup_note(
            sprintf(
              paste(
                "Acquired resistance, virulence and stress genes, plus resistance",
                "point mutations from the AMRFinderPlus catalogue for %s."
              ),
              label
            ),
            "good"
          )
        } else if (!is.na(name) && is.na(suggested)) {
          setup_ui$setup_note(sprintf(
            paste(
              "Acquired resistance, virulence and stress genes. AMRFinderPlus has",
              "no point-mutation catalogue for %s, so resistance caused by point",
              "mutations is not reported."
            ),
            name
          ))
        } else {
          setup_ui$setup_note("Acquired resistance, virulence and stress genes only.")
        },
        if (!is.na(suggested) && !identical(suggested, organism)) {
          setup_ui$setup_note(
            sprintf(
              "The catalogue matching this species is %s.",
              gsub("_", " ", suggested)
            ),
            "warn"
          )
        }
      )
    })

    # Navigation --------------------------------------------------------------

    # What the given step still needs before the setup can move past it.
    step_problem <- function(step) {
      switch(
        names(STEPS)[step],
        species = chosen_species()$problem,
        cgmlst = cg_problem(),
        mlst = if (isTRUE(input$mlst_on) && !isTRUE(mlst()$ready)) mlst()$problem,
        save = if (is.null(state$target)) "Choose the storage location."
      )
    }

    # First step that is still incomplete, NA when the whole setup is ready.
    first_open_step <- shiny$reactive({
      open <- which(!vapply(seq_along(STEPS), function(i) is.null(step_problem(i)), logical(1)))
      if (length(open)) open[1] else NA_integer_
    })

    # TRUE once the database is created: the setup is closed from then on.
    finished <- shiny$reactive(!is.null(state$result))

    # Shows a step and remembers how far the setup has come.
    go <- function(step) {
      if (finished()) {
        return(invisible(NULL))
      }
      step <- min(max(step, 1L), length(STEPS))
      state$step <- step
      state$reached <- max(state$reached, step)
      bslib$nav_select("steps", selected = names(STEPS)[step], session = session)
    }

    shiny$observeEvent(input$next_step, {
      shiny$req(is.null(step_problem(state$step)))
      go(state$step + 1L)
    })
    shiny$observeEvent(input$back, go(state$step - 1L))

    # Stepper links jump back to any step already visited.
    lapply(seq_along(STEPS), function(i) {
      shiny$observeEvent(input[[paste0("goto_", i)]], {
        if (i <= state$reached) go(i)
      })
    })

    # Progress indicator across the steps; all done and inert once finished.
    output$stepper <- shiny$renderUI({
      closed <- finished()
      shiny$tags$ol(
        class = "setup-steps",
        lapply(seq_along(STEPS), function(i) {
          done <- closed || (i < state$step && is.null(step_problem(i)))
          shiny$tags$li(
            class = paste(
              "setup-step",
              if (!closed && i == state$step) "is-active",
              if (done) "is-done",
              if (!closed && i <= state$reached) "is-reachable"
            ),
            shiny$actionLink(
              ns(paste0("goto_", i)),
              shiny$tagList(
                shiny$span(class = "setup-step_number", if (done) shiny$icon("check") else i),
                shiny$span(class = "setup-step_label", STEPS[[i]])
              )
            )
          )
        })
      )
    })

    # What the current step still needs, next to the navigation buttons.
    output$step_hint <- shiny$renderUI({
      problem <- step_problem(state$step)
      if (!is.null(problem) && state$step < length(STEPS)) {
        shiny$span(shiny$icon("circle-info"), " ", problem)
      }
    })

    # Keeps the navigation and action buttons in line with the setup's state.
    shiny$observe({
      last <- state$step == length(STEPS)
      toggleState("back", state$step > 1L)
      toggleState("next_step", !last && is.null(step_problem(state$step)))
      toggleState("create", is.na(first_open_step()) && is.null(state$build))
    })

    # Review and build --------------------------------------------------------

    # Takes the location picked in the save dialog.
    shiny$observeEvent(target_path(), {
      state$target <- target_path()
      state$error <- NULL
    })

    output$target_label <- shiny$renderUI(
      setup_ui$picked_label(state$target, "No location chosen")
    )

    # Everything the database will be built from, as one plan.
    plan <- shiny$reactive({
      sp <- chosen_species()
      src <- cg_source()
      cg <- state$cg
      url <- trimws(input$cg_url %||% "")
      list(
        species = sp$name,
        taxid = sp$taxid,
        target = state$target,
        amr_organism = amr_organism(),
        cg = list(
          loci = cg$loci,
          targets = cg_targets()$targets,
          info = list(
            name = substr(trimws(input$cg_name), 1L, 200L),
            source = src$id,
            source_label = src$label,
            version = substr(trimws(input$cg_version %||% ""), 1L, 100L),
            url = if (nzchar(url)) url else src$url,
            seed_genome = substr(trimws(input$cg_seed %||% ""), 1L, 300L),
            cluster_distance = input$cg_distance,
            citation = substr(trimws(input$cg_citation %||% ""), 1L, 2000L),
            provider_citation = src$citation,
            acknowledgement = src$acknowledgement,
            terms_url = src$terms_url,
            files = paste(basename(cg_paths()[1]), collapse = "")
          )
        ),
        mlst = if (isTRUE(input$mlst_on)) mlst()[c("scheme", "info")]
      )
    })

    # Summary rows of the setup, with what is still missing spelled out.
    summary_rows <- shiny$reactive({
      sp <- chosen_species()
      cg_ok <- is.null(cg_problem())
      mlst_on <- isTRUE(input$mlst_on)
      organism <- amr_organism()
      list(
        list(
          label = "Species",
          value = if (is.na(sp$name)) "Not selected" else shiny$em(sp$name),
          missing = is.na(sp$name)
        ),
        list(
          label = "cgMLST scheme",
          value = if (cg_ok) {
            sprintf(
              "%s - %s loci, %s annotated (%s)",
              trimws(input$cg_name),
              format(nrow(state$cg$loci), big.mark = ","),
              format(cg_targets()$annotated, big.mark = ","),
              cg_source()$label
            )
          } else {
            cg_problem()
          },
          missing = !cg_ok
        ),
        list(
          label = "Classical MLST",
          value = if (!mlst_on) {
            "None (sequence types are not determined)"
          } else if (isTRUE(mlst()$ready)) {
            sprintf(
              "%s - %d loci, %s sequence types",
              mlst()$info$name,
              length(mlst()$scheme$loci),
              format(mlst()$scheme$summary[["profiles"]], big.mark = ",")
            )
          } else {
            mlst()$problem
          },
          missing = mlst_on && !isTRUE(mlst()$ready)
        ),
        list(
          label = "AMR screening",
          value = if (nzchar(organism)) {
            paste("Genes and point mutations for", gsub("_", " ", organism))
          } else {
            "Acquired genes only"
          }
        )
      )
    })

    # The review's summary; AMR screening has its own section right below it.
    output$summary <- shiny$renderUI({
      render_info("output$summary")
      rows <- summary_rows()
      summary_table(Filter(function(r) r$label != "AMR screening", rows))
    })

    # Names the step that still blocks the build.
    output$create_hint <- shiny$renderUI({
      open <- first_open_step()
      if (!is.na(open)) {
        shiny$div(
          class = "setup-help",
          shiny$icon("circle-info"),
          sprintf(" %s: %s", STEPS[[open]], step_problem(open))
        )
      }
    })

    # Starts the build in the background: pyMLST can take a while on a large
    # scheme, and running it synchronously would freeze the session.
    shiny$observeEvent(input$create, {
      shiny$req(is.null(state$build), is.na(first_open_step()), !finished())
      build_plan <- plan()
      build <- tryCatch(start_db_build(build_plan), error = function(e) e)
      if (inherits(build, "error")) {
        shiny$showNotification(
          paste("The database could not be created:", conditionMessage(build)),
          type = "error",
          duration = 8
        )
        return()
      }

      waiter <- Waiter$new(
        id = ns("wizard"),
        html = shiny$div(
          class = "spinner-custom",
          spin_flower(),
          shiny$div(
            shiny$h5("Creating database ..."),
            shiny$div(id = "scheme-load", basename(build_plan$target)),
            shiny$div(id = ns("build_elapsed"), "0:00 elapsed"),
            shiny$tags$button(
              type = "button",
              class = "btn btn-primary",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
                ns("cancel_build")
              ),
              "Cancel"
            )
          )
        )
      )
      waiter$show()

      live_build <<- build
      state$error <- NULL
      state$build <- list(
        build = build,
        plan = build_plan,
        # What the review showed, kept with the build: the finished screen
        # describes the database that was created, not later inputs.
        summary = summary_rows(),
        waiter = waiter,
        started = Sys.time(),
        cancelled = FALSE
      )
    })

    # Cancels the running build; the poll below reports it and cleans up.
    shiny$observeEvent(input$cancel_build, {
      shiny$req(!is.null(state$build))
      state$build$cancelled <- TRUE
      tryCatch(state$build$build$proc$kill_tree(), error = function(e) NULL)
    })

    # Polls the running build once a second: shows the elapsed time while it
    # runs, then completes the database and reports the outcome.
    shiny$observe({
      job <- state$build
      if (is.null(job)) {
        return(NULL)
      }
      if (job$build$proc$is_alive()) {
        secs <- as.integer(difftime(Sys.time(), job$started, units = "secs"))
        html("build_elapsed", sprintf("%d:%02d elapsed", secs %/% 60, secs %% 60))
        shiny$invalidateLater(1000, session)
        return(NULL)
      }

      state$build <- NULL
      live_build <<- NULL
      on.exit(job$waiter$hide())

      if (isTRUE(job$cancelled)) {
        cancel_db_build(job$build)
        shiny$showNotification("Database creation cancelled.", type = "warning", duration = 5)
        return(NULL)
      }

      finished_build <- tryCatch(finish_db_build(job$build, job$plan), error = function(e) e)
      if (inherits(finished_build, "error")) {
        state$error <- conditionMessage(finished_build)
        shiny$showNotification(
          paste("Creating the database failed:", conditionMessage(finished_build)),
          type = "error",
          duration = 10
        )
        return(NULL)
      }

      # From here on the setup is closed: the inputs no longer describe a
      # database that can be created, only the one that was.
      state$result <- c(finished_build, list(summary = job$summary))
      bslib$nav_select("steps", selected = "done", session = session)
      hide("footer")
    })

    # Review notes, or why the last attempt failed.
    output$result <- shiny$renderUI({
      render_info("output$result")
      if (!is.null(state$error)) {
        return(setup_ui$setup_note(state$error, "bad"))
      }
      setup_ui$setup_note(paste(
        "Check the summary, choose the storage location and create the database.",
        "The database file is written only once it is complete."
      ))
    })

    # The created database: where it is, what it holds, what was skipped.
    output$done <- shiny$renderUI({
      render_info("output$done")
      res <- state$result
      shiny$req(res)
      shiny$tagList(
        shiny$div(
          class = "setup-done_head",
          shiny$icon("circle-check"),
          shiny$div(
            shiny$h4("Database created"),
            shiny$tags$code(res$path)
          )
        ),
        setup_ui$stat_row(
          c("Loci in the reference" = res$loci, "Skipped by pyMLST" = length(res$skipped)),
          tones = c("good", if (length(res$skipped)) "warn" else "")
        ),
        summary_table(res$summary),
        if (length(res$skipped)) {
          setup_ui$setup_section(
            "Skipped loci",
            shiny$div(class = "setup-issue_loci", paste(res$skipped, collapse = ", "))
          )
        },
        shiny$div(
          class = "setup-help",
          "The setup of a created database cannot be changed. Load it to add",
          " isolates, or start a new setup for another database."
        )
      )
    })

    # A new setup starts from a fresh session on this tab, so nothing of the
    # previous one - inputs, picked files, checks - can carry over into it.
    shiny$observeEvent(input$new_setup, restart_session(tab = WIZARD_TAB))

    # Server return values
    shiny$reactiveValues(
      load_db = shiny$reactive(input$load_db),
      db_location = shiny$reactive(state$result$path)
    )
  })
}
