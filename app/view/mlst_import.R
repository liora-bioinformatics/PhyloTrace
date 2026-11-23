# app/view/mlst_import.R
#
# Import of a classical MLST scheme from files the user downloaded: a profile
# table and one allele FASTA per locus. Validates the pair and hands back the
# assembled scheme; used by the database setup and to add a scheme to an
# existing database.

box::use(
  bslib,
  rlang[`%||%`],
  shiny,
  stats[setNames],
)

box::use(
  app / logic / scheme_files,
  app / logic / scheme_guide[guide_modal],
  app / logic / scheme_index[schemes_for_species],
  app / logic / scheme_sources[version_problem],
  app / logic / setup_ui,
  app / logic / species[default_db_name],
  app / view / file_dialog,
  app / view / scheme_source,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  # Three numbered tasks: the source and the scheme's details on the left (a
  # published scheme's "Use details" fills both), the files and their check on
  # the right.
  shiny$tagList(
    shiny$uiOutput(ns("task_states")),
    bslib$layout_columns(
      class = "setup-columns",
      col_widths = bslib$breakpoints(sm = 12, lg = c(6, 6)),
      fill = FALSE,
      shiny$div(
        class = "setup-form",
        setup_ui$setup_task(
          ns("t1"),
          1,
          "Scheme source",
          shiny$uiOutput(ns("published")),
          scheme_source$ui(ns("source"), types = "MLST"),
          hint = paste(
            "The provider the scheme files come from, and your confirmation that",
            "you may use them under its terms. 'Use details' on a published scheme",
            "fills in its provider and name."
          )
        ),
        setup_ui$setup_task(
          ns("t2"),
          2,
          "Scheme details",
          locked_hint = "Select and confirm the scheme source first.",
          hint = paste(
            "Recorded with the scheme. Only the name is required; a version or",
            "download date tells later which release of the scheme was used."
          ),
          shiny$div(
            class = "setup-grid allow-free-text",
            shiny$textInput(ns("name"), "Scheme name", width = "100%"),
            shiny$textInput(
              ns("version"),
              "Version or download date",
              placeholder = "e.g. 2026-11-16",
              width = "100%"
            )
          )
        )
      ),
      shiny$div(
        class = "setup-form",
        setup_ui$setup_task(
          ns("t3"),
          3,
          "Scheme files",
          locked_hint = "Complete the scheme details first.",
          hint = paste(
            "Profile table: an ST column and one column per locus (tab- or",
            "comma-separated, or Excel), as offered by PubMLST and BIGSdb. Allele",
            "files: one FASTA file per locus, named like the locus (adk.fasta), as",
            "files, a folder or a ZIP archive - or one FASTA file with headers such",
            "as >adk_1."
          ),
          shiny$div(
            class = "setup-pick",
            file_dialog$ui(ns("profiles"), "Profile table", "table"),
            shiny$uiOutput(ns("profiles_label"), inline = TRUE)
          ),
          shiny$div(
            class = "setup-pick",
            file_dialog$ui(ns("alleles"), "Allele files", "dna"),
            shiny$uiOutput(ns("alleles_label"), inline = TRUE)
          ),
          shiny$uiOutput(ns("report"))
        )
      )
    )
  )
}

# Scheme name proposed for a species, "" without one.
species_name <- function(species) {
  if (is.null(species) || is.na(species)) {
    return("")
  }
  paste(gsub("_", " ", default_db_name(species)), "MLST")
}

#' @export
server <- function(id, species = shiny$reactive(NA_character_), reset = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    source <- scheme_source$server("source", reset = reset)

    # Scheme name last proposed, from the species or a published scheme.
    proposed_name <- shiny$reactiveVal("")

    # Clears the form: name and version here, provider and files in their
    # own modules (Reload Database drops transient state, see CLAUDE.md). The
    # name goes back to the species' proposal.
    if (!is.null(reset)) {
      shiny$observeEvent(reset(), {
        proposed_name(species_name(species()))
        shiny$updateTextInput(session, "name", value = proposed_name())
        shiny$updateTextInput(session, "version", value = "")
      }, ignoreInit = TRUE)
    }

    # Classical MLST schemes published for the species, from the shipped index.
    published <- shiny$reactive(schemes_for_species(species(), type = "MLST"))

    # Where the species' MLST schemes can be downloaded.
    output$published <- shiny$renderUI({
      rows <- published()
      if (nrow(rows)) {
        shiny$tagList(
          setup_ui$scheme_links(
            rows,
            use_input = session$ns("use"),
            scroll = TRUE,
            guide_input = session$ns("guide")
          )
        )
      }
    })


    # Download guide of a published scheme.
    shiny$observeEvent(input$guide, {
      row <- published()[input$guide$i, , drop = FALSE]
      shiny$req(nrow(row) == 1L)
      shiny$showModal(guide_modal(row))
    })

    # Fills the provider and name of a published scheme into the form.
    shiny$observeEvent(input$use, {
      row <- published()[input$use$i, , drop = FALSE]
      shiny$req(nrow(row) == 1L)
      shiny$updateSelectInput(
        session,
        "source-provider",
        selected = row$provider
      )
      # A filled-in name counts as a proposal, so it follows a change of species.
      proposed_name(paste(row$species, row$scheme))
      shiny$updateTextInput(session, "name", value = proposed_name())
    })

    profile_path <- file_dialog$server(
      "profiles",
      mode = "file",
      title = "Select the MLST profile table",
      pattern = scheme_files$TABLE_FILE_RE,
      reset = reset
    )
    allele_paths <- file_dialog$server(
      "alleles",
      mode = "file",
      title = "Select the MLST allele files",
      pattern = scheme_files$ALLELE_FILE_RE,
      multiple = TRUE,
      allow_dir = TRUE,
      hint = paste(
        "Select the FASTA files (Ctrl-click for several) or a ZIP archive,",
        "or open the folder that holds them and choose 'Use this folder'."
      ),
      reset = reset
    )

    output$profiles_label <- shiny$renderUI(setup_ui$picked_label(profile_path()))
    output$alleles_label <- shiny$renderUI(setup_ui$picked_label(allele_paths()))

    # Proposes a scheme name from the species. A name the user typed is kept;
    # an earlier proposal follows a change of species.
    shiny$observeEvent(species(), {
      current <- trimws(input$name %||% "")
      if (
        !is.na(species()) &&
          (!nzchar(current) || identical(current, proposed_name()))
      ) {
        proposed_name(species_name(species()))
        shiny$updateTextInput(session, "name", value = proposed_name())
      }
    })

    # The profile table as read, or the error reading it raised.
    profiles <- shiny$reactive({
      path <- profile_path()
      if (is.null(path)) {
        return(NULL)
      }
      tryCatch(scheme_files$read_mlst_profiles(path), error = function(e) e)
    })

    # The assembled scheme, an error, or NULL while files are still missing.
    scheme <- shiny$reactive({
      prof <- profiles()
      paths <- allele_paths()
      if (is.null(prof) || inherits(prof, "error") || is.null(paths)) {
        return(NULL)
      }
      shiny$withProgress(message = "Reading MLST alleles ...", value = 0.5, {
        tryCatch(
          scheme_files$assemble_mlst_scheme(
            prof,
            scheme_files$read_mlst_alleles(paths, prof$candidates)
          ),
          error = function(e) e
        )
      })
    })

    # What is still missing before the scheme can be stored, NULL when ready;
    # in task order.
    problem <- shiny$reactive({
      src <- source()
      sch <- scheme()
      if (is.null(src)) {
        "Select the scheme provider."
      } else if (!isTRUE(src$confirmed)) {
        "Confirm the provider's terms of use."
      } else if (!nzchar(trimws(input$name %||% ""))) {
        "Enter a scheme name."
      } else if (!is.null(version_problem(input$version))) {
        version_problem(input$version)
      } else if (is.null(profile_path())) {
        "Select the profile table."
      } else if (inherits(profiles(), "error")) {
        conditionMessage(profiles())
      } else if (is.null(allele_paths())) {
        "Select the allele files."
      } else if (inherits(sch, "error")) {
        conditionMessage(sch)
      }
    })

    # Validation report: figures, loci and everything the import skipped.
    output$report <- shiny$renderUI({
      prof <- profiles()
      sch <- scheme()
      if (inherits(prof, "error")) {
        return(setup_ui$setup_note(conditionMessage(prof), "bad"))
      }
      if (inherits(sch, "error")) {
        return(setup_ui$setup_note(conditionMessage(sch), "bad"))
      }
      if (is.null(sch)) {
        return(shiny$tagList(
          if (!is.null(prof)) {
            setup_ui$setup_note(
              sprintf(
                "Profile table read: %d locus columns (%s).",
                length(prof$candidates),
                paste(prof$candidates, collapse = ", ")
              )
            )
          }
        ))
      }
      shiny$div(
        class = "setup-check",
        setup_ui$stat_row(
          c(
            "Loci" = sch$summary[["loci"]],
            "Sequence types" = sch$summary[["profiles"]],
            "Alleles" = sch$summary[["alleles"]]
          ),
          tones = "good"
        ),
        shiny$div(class = "setup-loci", paste(sch$loci, collapse = "  ·  ")),
        if (nrow(sch$issues)) {
          setup_ui$setup_section(
            "Skipped during import",
            setup_ui$issue_list(sch$issues)
          )
        } else {
          setup_ui$setup_note("Profiles and alleles are consistent.", "good")
        }
      )
    })

    # TRUE once both files have been read into a consistent scheme.
    files_ready <- shiny$reactive({
      sch <- scheme()
      !is.null(sch) && !inherits(sch, "error")
    })

    # State of the three tasks; each unlocks once the one before is complete.
    output$task_states <- setup_ui$task_states(function() {
      source_done <- isTRUE(source()$confirmed)
      details_done <- source_done &&
        nzchar(trimws(input$name %||% "")) &&
        is.null(version_problem(input$version))
      setNames(
        c(
          if (source_done) "done" else "current",
          if (!source_done) {
            "locked"
          } else if (details_done) {
            "done"
          } else {
            "current"
          },
          if (!details_done) {
            "locked"
          } else if (files_ready()) {
            "done"
          } else {
            "current"
          }
        ),
        session$ns(c("t1", "t2", "t3"))
      )
    })

    output$t1_status <- shiny$renderUI({
      src <- source()
      if (isTRUE(src$confirmed)) setup_ui$task_status(src$label)
    })
    output$t2_status <- shiny$renderUI({
      if (isTRUE(source()$confirmed)) {
        setup_ui$task_status(trimws(input$name %||% ""))
      }
    })
    output$t3_status <- shiny$renderUI({
      if (files_ready()) {
        sch <- scheme()
        setup_ui$task_status(sprintf(
          "%d loci, %s sequence types",
          sch$summary[["loci"]],
          format(sch$summary[["profiles"]], big.mark = ",")
        ))
      } else if (inherits(scheme(), "error") || inherits(profiles(), "error")) {
        setup_ui$task_status("Not readable", "bad")
      }
    })

    # The import as the caller stores it.
    shiny$reactive({
      issue <- problem()
      src <- source()
      if (!is.null(issue)) {
        return(list(ready = FALSE, problem = issue))
      }
      list(
        ready = TRUE,
        scheme = scheme(),
        info = list(
          name = substr(trimws(input$name), 1L, 200L),
          source = src$id,
          source_label = src$label,
          version = substr(trimws(input$version %||% ""), 1L, 100L),
          species = species(),
          url = src$url,
          terms_url = src$terms_url,
          acknowledgement = src$acknowledgement,
          citation = src$citation,
          files = paste(
            basename(c(profile_path(), allele_paths()[1])),
            collapse = ", "
          )
        )
      )
    })
  })
}
