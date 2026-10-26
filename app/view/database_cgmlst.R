# app/view/database_cgmlst.R

#' "cgMLST Overview" interface of the Database menu: the scheme, its loci,
#' how completely the isolates were called and the provenance and assembly
#' diagnostics of every isolate in the database.

box::use(
  DT[DTOutput, renderDT],
  bslib[nav_panel, navset_card_tab],
  shiny,
  shinyWidgets[updatePickerInput],
  stats[median, setNames],
  utils[head, write.csv],
)

box::use(
  app /
    logic /
    cgmlst_overview[LOW_COMPLETENESS, load_cgmlst_overview],
  app /
    logic /
    database_functions[load_allele, load_locus_alleles, locus_fasta],
  app / logic / db_events,
  app / logic / db_guard[db_failed, guard_db, guard_db_read],
  app / logic / functions[copy_to_clipboard, render_info, stat_tile],
  app / logic / overview_ui,
)

# DataTables shown with a loading overlay until their first draw.
TABLE_IDS <- c("cg_loci", "cg_isolates")

# Bars shown before the remainder is folded into one "other" row.
TOP_ALLELES <- 6L
TOP_MISSING <- 15L

fmt <- overview_ui$fmt

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  navset_card_tab(
    nav_panel(
      "Scheme Info",
      overview_ui$split_layout(
        overview_ui$fill_card(
          "Scheme Metadata",
          DTOutput(ns("scheme_table"), fill = TRUE)
        ),
        overview_ui$fill_card(
          "Typing Summary",
          shiny$uiOutput(ns("typing_summary")),
          hint = sprintf(
            paste(
              "Completeness is the share of the scheme's loci called per isolate;",
              "below %s%% an isolate is flagged in Diagnostics."
            ),
            format(LOW_COMPLETENESS)
          )
        )
      )
    ),
    nav_panel(
      "Loci Info",
      overview_ui$loci_layout(ns, "cg_loci", index_label = "Hash", index_icon = "fingerprint")
    ),
    nav_panel(
      "Distribution",
      overview_ui$split_layout(
        list(
          overview_ui$fill_card(
            "Missing Loci per Isolate",
            shiny$uiOutput(ns("missing_bars")),
            hint = "Typed isolates by how many of the scheme's loci were left without a call."
          ),
          overview_ui$fill_card(
            "Most Often Missing Loci",
            shiny$uiOutput(ns("top_missing")),
            hint = paste(
              "Share of typed isolates without a call at the locus; loci missed this",
              "often may be absent from the population or hard to assemble."
            )
          )
        ),
        overview_ui$fill_card(
          "Alleles per Locus",
          shiny$uiOutput(ns("allele_bins")),
          hint = paste(
            "Loci by number of distinct alleles in this database, the scheme's",
            "reference allele included."
          )
        )
      )
    ),
    nav_panel(
      "Diagnostics",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "cg_isolates",
          "Isolate Provenance",
          "export_isolates",
          "Loading isolate provenance ..."
        ),
        list(
          overview_ui$fill_card("Checks", shiny$uiOutput(ns("checks"))),
          overview_ui$fill_card("Typing Settings", shiny$uiOutput(ns("settings")))
        )
      )
    )
  )
}

# Handles server logic for the scheme metadata and the typing summary.
scheme_info_server <- function(input, output, overview) {
  output$scheme_table <- renderDT({
    render_info("output$scheme_table")
    scheme <- overview()$scheme
    if (!is.data.frame(scheme) || !nrow(scheme)) {
      return(overview_ui$message_table(paste(
        "No 'Scheme Overview' table found. <br> Try rebuilding",
        "the schema in the <strong>Create Scheme</strong> module"
      )))
    }
    # Values are kept as stored: the scheme URL arrives as a link.
    overview_ui$key_value_table(as.character(scheme$key), as.character(scheme$value))
  })

  # Headline figures across every isolate, then how complete their profiles are.
  output$typing_summary <- shiny$renderUI({
    render_info("output$typing_summary")
    ov <- overview()
    iso <- ov$isolates
    typed <- iso[iso$typed, , drop = FALSE]
    if (!nrow(typed)) {
      return(overview_ui$empty_hint("This database holds no typed isolates yet."))
    }
    med <- median(typed$completeness, na.rm = TRUE)

    shiny$tagList(
      overview_ui$stat_grid(
        stat_tile("Isolates", fmt(nrow(typed)), "vial"),
        stat_tile(
          "Loci",
          if (is.na(ov$locus_count)) "-" else fmt(ov$locus_count),
          "table-columns"
        ),
        stat_tile("Alleles", fmt(sum(ov$loci$`Allele Count`)), "dna"),
        stat_tile(
          "Median Completeness",
          if (is.na(med)) "-" else sprintf("%.1f%%", med),
          "circle-check"
        )
      ),
      overview_ui$section_title("Profile Completeness"),
      overview_ui$bar_rows(
        ov$completeness$label,
        ov$completeness$n,
        nrow(typed),
        tones = ov$completeness$tone
      )
    )
  })
}

# Handles server logic for the loci table, allele usage and sequence viewer.
loci_info_server <- function(input, output, session, db_path, overview, session_reset) {
  # Sequence and hash of the displayed allele, cached so the copy buttons do not
  # have to re-query the database.
  seq_cache <- shiny$reactiveVal(NULL)

  # Reset module state when the user returns to the landing screen.
  shiny$observeEvent(session_reset(), seq_cache(NULL), ignoreInit = TRUE)

  display_cols <- function(df) df[, setdiff(names(df), ".gene"), drop = FALSE]

  output$cg_loci <- renderDT({
    render_info("output$cg_loci")
    li <- overview()$loci
    if (!nrow(li)) {
      return(overview_ui$message_table(
        "No loci found. Re-download the scheme to populate loci info."
      ))
    }
    overview_ui$data_table(
      display_cols(li),
      selection = list(mode = "single", selected = 1)
    )
  })

  # The selected locus row (its `.gene` keys the allele lookups).
  selected_row <- shiny$reactive({
    li <- overview()$loci
    row <- input$cg_loci_rows_selected
    shiny$req(nrow(li) > 0, length(row) == 1)
    li[row, , drop = FALSE]
  })

  # Distinct alleles of the selected locus with their in-database usage.
  alleles <- shiny$reactive({
    shiny$req(db_path())
    gene <- selected_row()$.gene
    guard_db_read("Loading the locus alleles", load_locus_alleles(db_path(), gene))
  })

  # Refresh the allele dropdown whenever the selected locus changes.
  shiny$observeEvent(alleles(), {
    df <- alleles()
    shiny$req(nrow(df) > 0)
    total <- sum(df$count)
    labels <- ifelse(
      df$present,
      sprintf(
        "Allele %s - %s isolate(s) (%.1f%%)",
        df$seqid,
        fmt(df$count),
        100 * df$count / max(total, 1)
      ),
      sprintf("Allele %s - not present", df$seqid)
    )
    updatePickerInput(
      session,
      "allele_select",
      label = paste("Selected Locus:", selected_row()$Locus),
      choices = setNames(as.character(df$seqid), labels),
      selected = as.character(df$seqid[1])
    )
  })

  # Most frequent alleles of the selected locus, the rest folded into one row.
  output$allele_bars <- shiny$renderUI({
    df <- alleles()
    df <- df[df$present, , drop = FALSE]
    shiny$req(nrow(df) > 0)
    render_info("output$allele_bars")
    bars <- overview_ui$fold_top(
      paste("Allele", df$seqid),
      df$count,
      TOP_ALLELES,
      "%d other alleles"
    )
    shiny$div(
      class = "overview-allele-bars",
      overview_ui$section_title("Allele Frequency"),
      overview_ui$bar_rows(
        bars$labels,
        bars$counts,
        selected_row()$`Isolates Called`,
        tones = bars$tones
      )
    )
  })

  output$allele_sequence <- shiny$renderUI({
    shiny$req(db_path(), input$allele_select)
    allele <- guard_db_read(
      "Loading the allele sequence",
      load_allele(db_path(), as.integer(input$allele_select))
    )
    shiny$req(allele)
    seq_cache(allele)
    render_info("output$allele_sequence")
    overview_ui$sequence_view(input$allele_select, allele$sequence, allele$hash)
  })

  # Copy actions -----------------------------------------------------------
  shiny$observeEvent(input$copy_seq, {
    shiny$req(seq_cache())
    copy_to_clipboard(seq_cache()$sequence)
  })

  # The allele's hash identifies it across databases, unlike its local id; the
  # id stands in only where the database has no hash for it yet.
  shiny$observeEvent(input$copy_idx, {
    allele <- seq_cache()
    shiny$req(allele)
    copy_to_clipboard(if (is.na(allele$hash)) input$allele_select else allele$hash)
  })

  # Downloads --------------------------------------------------------------
  output$download_locus <- shiny$downloadHandler(
    filename = function() paste0(selected_row()$Locus, ".fasta"),
    content = function(file) {
      fasta <- guard_db(
        "Exporting the locus FASTA",
        locus_fasta(db_path(), selected_row()$.gene)
      )
      # No file written means the browser reports the download as failed,
      # rather than saving an empty FASTA.
      shiny$req(!db_failed(fasta), length(fasta) > 0)
      writeLines(fasta, file)
    }
  )

  output$export_loci <- shiny$downloadHandler(
    filename = function() "cgmlst_loci.csv",
    content = function(file) {
      li <- overview()$loci
      shiny$req(nrow(li) > 0)
      write.csv(display_cols(li), file, row.names = FALSE)
    }
  )

  shiny$outputOptions(output, "allele_sequence", suspendWhenHidden = FALSE)
}

# Handles server logic for the completeness and allele distributions.
distribution_server <- function(input, output, overview) {
  n_typed <- shiny$reactive(sum(overview()$isolates$typed))

  # How many of the scheme's loci each typed isolate is missing.
  output$missing_bars <- shiny$renderUI({
    render_info("output$missing_bars")
    bins <- overview()$missing_bins
    if (!n_typed()) {
      return(overview_ui$empty_hint("This database holds no typed isolates yet."))
    }
    shiny$tagList(
      overview_ui$bar_rows(
        bins$label,
        bins$n,
        n_typed(),
        tones = ifelse(bins$label == "None", "success", "primary")
      )
    )
  })

  # Loci most often left uncalled, as a share of the typed isolates.
  output$top_missing <- shiny$renderUI({
    render_info("output$top_missing")
    li <- overview()$loci
    li <- li[li$Missing > 0, , drop = FALSE]
    if (!nrow(li)) {
      return(overview_ui$empty_hint("Every locus was called in every typed isolate."))
    }
    li <- head(li[order(-li$Missing), , drop = FALSE], TOP_MISSING)
    shiny$tagList(
      overview_ui$bar_rows(li$Locus, li$Missing, n_typed(), tones = rep("warning", nrow(li)))
    )
  })

  # How many distinct alleles the database holds per locus.
  output$allele_bins <- shiny$renderUI({
    render_info("output$allele_bins")
    ov <- overview()
    if (!nrow(ov$loci)) {
      return(overview_ui$empty_hint("No loci to show."))
    }
    shiny$tagList(
      overview_ui$bar_rows(
        ifelse(
          ov$allele_bins$label == "1",
          "1 allele",
          paste(ov$allele_bins$label, "alleles")
        ),
        ov$allele_bins$n,
        nrow(ov$loci),
        unit = "loci"
      )
    )
  })
}

# Per-isolate provenance table, flagged rows first.
isolate_rows <- function(iso) {
  flags <- character(nrow(iso))
  add_flag <- function(hit, text) {
    flags[hit] <<- ifelse(nzchar(flags[hit]), paste(flags[hit], text, sep = "; "), text)
  }
  add_flag(iso$low_completeness, "low completeness")
  add_flag(iso$duplicate, "duplicate assembly")
  add_flag(iso$other_version, "other scheme version")
  add_flag(iso$odd_size, "unusual size")
  add_flag(iso$other_thresholds, "other thresholds")
  add_flag(iso$fragmented, "fragmented")
  add_flag(iso$typed & !iso$in_prov, "no provenance")

  out <- data.frame(
    Isolate = iso$isolate,
    Flags = flags,
    `Completeness (%)` = iso$completeness,
    Loci = iso$loci,
    Missing = iso$missing,
    # pyMLST's "Added N new MLST genes": calls written for the isolate.
    `Genes Added` = iso$alleles_added,
    Partial = iso$partial,
    Filled = iso$filled,
    Removed = iso$removed,
    Contigs = iso$contigs,
    `Length (bp)` = iso$length,
    Identity = iso$identity,
    Coverage = iso$coverage,
    `Scheme Version` = iso$scheme_version,
    pyMLST = iso$pymlst_version,
    BLAT = iso$blat_version,
    MAFFT = iso$mafft_version,
    `Typed At` = substr(iso$typed_at, 1, 19),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[order(!nzchar(flags), out$Isolate), , drop = FALSE]
}

# One entry per combination of settings isolates were typed with.
settings_list <- function(settings, current_version) {
  shiny$div(
    class = "overview-groups",
    lapply(seq_len(nrow(settings)), function(i) {
      version <- settings$scheme_version[i]
      shiny$div(
        class = "overview-group",
        shiny$div(
          class = "overview-group_head",
          shiny$span(
            if (!isTRUE(settings$recorded[i])) {
              "Provenance not recorded"
            } else if (is.na(version)) {
              "Scheme version not recorded"
            } else {
              paste("Scheme version", version)
            }
          ),
          shiny$div(
            class = "overview-group_badges",
            if (!is.na(version) && identical(version, current_version)) {
              shiny$span(class = "overview-badge is-primary", "Current")
            }
          )
        ),
        shiny$div(
          class = "overview-group_meta",
          if (!is.na(settings$thresholds[i])) {
            shiny$span(shiny$icon("sliders"), settings$thresholds[i])
          },
          if (!is.na(settings$tools[i])) {
            shiny$span(shiny$icon("toolbox"), settings$tools[i])
          },
          shiny$span(shiny$icon("vial"), sprintf("%s isolates", fmt(settings$isolates[i])))
        )
      )
    })
  )
}

# Handles server logic for the provenance checks, settings and isolate table.
diagnostics_server <- function(input, output, overview) {
  isolates <- shiny$reactive(isolate_rows(overview()$isolates))

  output$cg_isolates <- renderDT({
    render_info("output$cg_isolates")
    rows <- isolates()
    if (!nrow(rows)) {
      return(overview_ui$message_table("This database holds no isolates yet."))
    }
    overview_ui$data_table(rows)
  })

  output$checks <- shiny$renderUI({
    render_info("output$checks")
    checks <- overview()$checks
    if (!nrow(checks)) {
      return(overview_ui$empty_hint("Nothing to check yet - no isolate has been typed."))
    }
    overview_ui$checks_ui(checks)
  })

  output$settings <- shiny$renderUI({
    render_info("output$settings")
    ov <- overview()
    if (!nrow(ov$settings)) {
      return(overview_ui$empty_hint("No isolate has been typed yet."))
    }
    settings_list(ov$settings, ov$scheme_version)
  })

  output$export_isolates <- shiny$downloadHandler(
    filename = function() "cgmlst_isolate_provenance.csv",
    content = function(file) {
      rows <- isolates()
      shiny$req(nrow(rows) > 0)
      write.csv(rows, file, row.names = FALSE)
    }
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  session_reset = shiny$reactive(0L),
  db_rev = db_events$new_bus()
) {
  shiny$moduleServer(id, function(input, output, session) {
    # Every cgMLST figure of the database, summarised once per change.
    # `isolates` because typing, removal and merges move every count; `schema`
    # because a merge or restore can replace the scheme itself.
    overview <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "isolates", "schema")
      guard_db_read("Loading the cgMLST overview", load_cgmlst_overview(db_path()))
    })

    # Put the loading overlays back while a newly opened database's tables draw.
    shiny$observeEvent(db_path(), overview_ui$reset_table_overlays(session$ns, TABLE_IDS))

    scheme_info_server(input, output, overview)
    loci_info_server(input, output, session, db_path, overview, session_reset)
    distribution_server(input, output, overview)
    diagnostics_server(input, output, overview)
  })
}
