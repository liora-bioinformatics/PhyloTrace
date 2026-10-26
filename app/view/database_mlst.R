# app/view/database_mlst.R

#' "MLST Overview" interface of the Database menu: the classical MLST twin of
#' the cgMLST Overview, with the scheme, its loci, the ST distribution and the
#' provenance diagnostics of every classical call in the database.

box::use(
  DT[DTOutput, renderDT],
  bslib[nav_panel, navset_card_tab],
  htmltools[htmlEscape],
  shiny,
  shinyWidgets[show_toast, updateVirtualSelect],
  stats[setNames],
  utils[write.csv],
)

box::use(
  app / logic / database_functions[load_db_species],
  app / logic / db_events,
  app / logic / db_guard[db_failed, guard_db, guard_db_read],
  app / logic / functions[copy_to_clipboard, render_info, stat_tile],
  app /
    logic /
    mlst_overview[
      CALLED_STATUSES,
      STATUS_LABELS,
      UNRESOLVED_STATUSES,
      cla_locus_fasta,
      load_cla_allele_sequence,
      load_mlst_overview,
      locus_alleles,
      simpson_diversity,
    ],
  app / logic / overview_ui,
)

REPO_NAMES <- c(pubmlst = "PubMLST", pasteur = "Institut Pasteur (BIGSdb)")
REPO_HOMES <- c(pubmlst = "https://pubmlst.org", pasteur = "https://bigsdb.pasteur.fr")

# DataTables shown with a loading overlay until their first draw.
TABLE_IDS <- c("cla_loci", "cla_sts", "cla_isolates")

# Bars shown before the remainder is folded into one "other" row.
TOP_STS <- 12L
TOP_ALLELES <- 6L

fmt <- overview_ui$fmt

# The repository's display name, or the stored token when it is unknown.
repo_name <- function(repo) {
  if (is.na(repo)) {
    return(NA_character_)
  }
  if (repo %in% names(REPO_NAMES)) REPO_NAMES[[repo]] else repo
}

# Why there are no classical calls to show, as one HTML sentence.
empty_reason <- function(ov, species) {
  species <- if (is.null(species) || is.na(species)) {
    "this species"
  } else {
    sprintf("<em>%s</em>", htmlEscape(species))
  }
  skipped <- sum(ov$status$n[ov$status$status == "skipped"])
  if (identical(ov$state, "skipped")) {
    return(sprintf(
      paste(
        "No classical MLST scheme is available for %s: neither PubMLST nor",
        "Institut Pasteur hosts a scheme that resolves to it unambiguously, so",
        "classical MLST was skipped for %s isolate(s)."
      ),
      species,
      fmt(skipped)
    ))
  }
  if (!is.null(ov$primary)) {
    return(sprintf(
      paste(
        "Classical MLST ran against <strong>%s</strong>, but no allele profiles",
        "were stored. See <strong>Diagnostics</strong> for the recorded calls."
      ),
      htmlEscape(ov$primary$scheme)
    ))
  }
  paste(
    "No classical MLST results in this database. Isolates typed in PhyloTrace",
    "receive a classical MLST call whenever PubMLST or Institut Pasteur hosts a",
    "scheme for", species, "."
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  navset_card_tab(
    nav_panel(
      "Scheme Info",
      overview_ui$split_layout(
        overview_ui$fill_card(
          "Scheme Metadata",
          DTOutput(ns("scheme_table"), fill = TRUE),
          hint = paste(
            "The classical MLST scheme most isolates were called against (the",
            "primary scheme), as recorded with their calls. Latest Release is the",
            "newest profile-table date among them."
          )
        ),
        overview_ui$fill_card(
          "Typing Summary",
          shiny$uiOutput(ns("typing_summary")),
          hint = paste(
            "Diversity is Simpson's index (Hunter-Gaston) over isolates with an ST:",
            "the chance that two of them carry different STs."
          )
        )
      )
    ),
    nav_panel(
      "Loci Info",
      overview_ui$loci_layout(
        ns,
        "cla_loci",
        table_hint = paste(
          "Loci of the primary scheme. Observed Alleles: distinct registered",
          "alleles called. Missing: typed isolates without a call at the locus.",
          "Unresolved: novel (new) or multi-hit calls. Top Allele: the most",
          "frequent allele and its share."
        )
      )
    ),
    nav_panel(
      "Distribution",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "cla_sts",
          "Sequence Types",
          "export_sts",
          "Loading sequence types ...",
          hint = paste(
            "One row per ST of the primary scheme with its allele profile. Call is",
            "Inferred where the ST was found in the scheme's profile table rather",
            "than reported by the search itself."
          )
        ),
        list(
          overview_ui$fill_card(
            "ST Frequency",
            shiny$uiOutput(ns("st_bars")),
            hint = paste(
              "Most common STs of the primary scheme. Shares are of the isolates",
              "with a stored allele profile on that scheme."
            )
          ),
          overview_ui$fill_card(
            "Allele Diversity",
            shiny$uiOutput(ns("locus_bars")),
            hint = "Distinct registered alleles called per locus in this database."
          )
        )
      )
    ),
    nav_panel(
      "Diagnostics",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "cla_isolates",
          "Isolate Provenance",
          "export_isolates",
          "Loading isolate provenance ...",
          hint = paste(
            "How every isolate was called: status, ST, scheme release, thresholds",
            "and tool version. Flagged rows come first; each flag is explained in Checks."
          )
        ),
        list(
          overview_ui$fill_card(
            "Checks",
            shiny$uiOutput(ns("checks")),
            hint = overview_ui$CHECKS_HINT
          ),
          overview_ui$fill_card(
            "Scheme Releases",
            shiny$uiOutput(ns("releases")),
            hint = paste(
              "Isolates grouped by the scheme and profile-table release they were",
              "called against. Several releases of one scheme leave known STs valid,",
              "but unresolved calls on an older one may resolve on retyping; a",
              "second scheme makes STs incomparable."
            )
          )
        )
      )
    )
  )
}

# Key/value rows describing the primary scheme, values as HTML.
scheme_rows <- function(ov, species) {
  pr <- ov$primary
  calls <- ov$isolates[ov$isolates$on_primary & ov$isolates$has_calls, , drop = FALSE]
  esc <- function(x) if (is.null(x) || is.na(x)) NA_character_ else htmlEscape(x)
  distinct <- function(x) {
    x <- unique(x[!is.na(x)])
    if (!length(x)) NA_character_ else paste(x, collapse = ", ")
  }

  repo <- pr$repository
  repository <- if (!is.na(repo) && repo %in% names(REPO_HOMES)) {
    sprintf(
      "<a href='%s' target='_blank'>%s</a>",
      htmlEscape(REPO_HOMES[[repo]], attribute = TRUE),
      htmlEscape(repo_name(repo))
    )
  } else {
    esc(repo_name(repo))
  }
  release <- if (length(pr$versions) > 1) {
    sprintf("%s (%d releases in use)", esc(pr$latest), length(pr$versions))
  } else {
    esc(pr$latest)
  }
  loci <- ov$loci$Locus
  thresholds <- unique(sprintf(
    "identity %s, coverage %s",
    format(calls$identity, drop0trailing = TRUE, trim = TRUE),
    format(calls$coverage, drop0trailing = TRUE, trim = TRUE)
  )[!is.na(calls$identity)])
  dates <- sort(substr(calls$called_at[!is.na(calls$called_at)], 1, 10))

  c(
    "Scheme" = esc(pr$scheme),
    "Repository" = repository,
    "Species" = esc(species),
    "Latest Release" = release,
    "Loci" = if (length(loci)) {
      sprintf("%d (%s)", length(loci), esc(paste(loci, collapse = ", ")))
    } else {
      NA_character_
    },
    "Search Thresholds" = if (length(thresholds) == 1) {
      esc(thresholds)
    } else if (length(thresholds) > 1) {
      "Mixed - see <strong>Diagnostics</strong>"
    } else {
      NA_character_
    },
    "pyMLST" = esc(distinct(calls$pymlst_version)),
    "Typed" = if (length(dates)) {
      esc(if (dates[1] == dates[length(dates)]) {
        dates[1]
      } else {
        paste(dates[1], "to", dates[length(dates)])
      })
    } else {
      NA_character_
    }
  )
}

# Handles server logic for the scheme metadata and the typing summary.
scheme_info_server <- function(input, output, overview, species, reason) {
  output$scheme_table <- renderDT({
    render_info("output$scheme_table")
    ov <- overview()
    if (!identical(ov$state, "ok") || is.null(ov$primary)) {
      return(overview_ui$message_table(reason()))
    }
    rows <- scheme_rows(ov, species())
    overview_ui$key_value_table(names(rows), unname(rows))
  })

  # Headline figures across every isolate, then how their calls came out.
  output$typing_summary <- shiny$renderUI({
    render_info("output$typing_summary")
    ov <- overview()
    iso <- ov$isolates
    total <- nrow(iso)
    if (!total) {
      return(overview_ui$empty_hint("This database holds no isolates yet."))
    }
    with_st <- sum(!is.na(iso$st))
    diversity <- simpson_diversity(ov$sts$Isolates)

    overview_ui$sections(
      overview_ui$stat_grid(
        stat_tile("Typed", sprintf("%s / %s", fmt(sum(iso$has_calls)), fmt(total)), "vial"),
        stat_tile("ST Assigned", sprintf("%.1f%%", 100 * with_st / total), "circle-check"),
        stat_tile("Distinct STs", fmt(nrow(ov$sts)), "fingerprint"),
        stat_tile(
          "Diversity",
          if (is.na(diversity)) "-" else sprintf("%.3f", diversity),
          "chart-simple"
        )
      ),
      overview_ui$section(
        "Call Status",
        overview_ui$bar_rows(ov$status$label, ov$status$n, total, tones = ov$status$tone)
      )
    )
  })
}

# Handles server logic for the loci table, allele usage and sequence viewer.
loci_info_server <- function(
  input,
  output,
  session,
  db_path,
  overview,
  reason,
  session_reset
) {
  seq_cache <- shiny$reactiveVal(NULL)

  # The allele shown in the sequence viewer with the locus it belongs to.
  # Allele numbers repeat across loci, and on a new locus the selected row
  # changes before the picker is refilled; following the pair instead of both
  # separately keeps the viewer from drawing the new locus with the previous
  # locus's allele first.
  picked <- shiny$reactiveVal(NULL)

  # Reset module state when the user returns to the landing screen.
  shiny$observeEvent(session_reset(), {
    seq_cache(NULL)
    picked(NULL)
  }, ignoreInit = TRUE)

  display_cols <- function(df) df[, setdiff(names(df), ".gene"), drop = FALSE]

  output$cla_loci <- renderDT({
    render_info("output$cla_loci")
    ov <- overview()
    if (!nrow(ov$loci)) {
      return(overview_ui$message_table(reason()))
    }
    overview_ui$data_table(
      display_cols(ov$loci),
      selection = list(mode = "single", selected = 1),
      filter = "none"
    )
  })

  # The selected locus row (its `.gene` keys the allele lookups).
  selected_row <- overview_ui$selected_row(input, "cla_loci", function() overview()$loci)

  # Allele usage of the selected locus, from the overview already in memory.
  alleles <- shiny$reactive({
    locus_alleles(overview()$calls, selected_row()$.gene)
  })

  # Refresh the allele dropdown whenever the selected locus changes.
  shiny$observeEvent(alleles(), {
    df <- alleles()
    shiny$req(nrow(df) > 0)
    labels <- sprintf(
      "Allele %s - %s isolate(s) (%.1f%%)",
      df$allele,
      fmt(df$count),
      df$share
    )
    updateVirtualSelect(
      "allele_select",
      session = session,
      label = paste("Selected Locus:", selected_row()$Locus),
      choices = setNames(df$allele, labels),
      selected = df$allele[1]
    )
    picked(list(gene = selected_row()$.gene, allele = df$allele[1]))
  })

  # Follow the picker when another allele of the locus is chosen. Its echo of
  # the allele set above leaves `picked` as it is.
  shiny$observeEvent(input$allele_select, {
    shiny$req(input$allele_select)
    picked(list(gene = selected_row()$.gene, allele = input$allele_select))
  })

  # Most frequent alleles of the selected locus, the rest folded into one row.
  output$allele_bars <- shiny$renderUI({
    df <- alleles()
    shiny$req(nrow(df) > 0)
    render_info("output$allele_bars")
    bars <- overview_ui$fold_top(
      paste("Allele", df$allele),
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
    shiny$req(db_path(), picked())
    pr <- overview()$primary
    shiny$req(pr)
    pick <- picked()
    render_info("output$allele_sequence")

    sequence <- guard_db_read(
      "Loading the allele sequence",
      load_cla_allele_sequence(
        db_path(),
        pick$gene,
        pick$allele,
        pr$scheme,
        pr$repository
      )
    )
    seq_cache(sequence)
    if (is.null(sequence)) {
      return(shiny$div(
        class = "species-details_empty",
        sprintf(
          "No reference sequence stored for allele %s - novel and multi-hit calls carry none.",
          pick$allele
        )
      ))
    }
    overview_ui$sequence_view(sequence)
  })

  # Copy actions -----------------------------------------------------------
  # Confirms a copy once the browser reports the clipboard write succeeded.
  shiny$observeEvent(input$copied, {
    show_toast(
      title = "Copied to clipboard",
      type = "success",
      position = "bottom-end",
      timer = 3000
    )
  })

  shiny$observeEvent(input$copy_seq, {
    shiny$req(seq_cache())
    copy_to_clipboard(seq_cache(), session$ns("copied"))
  })

  shiny$observeEvent(input$copy_idx, {
    shiny$req(input$allele_select)
    copy_to_clipboard(input$allele_select, session$ns("copied"))
  })

  # Downloads --------------------------------------------------------------
  output$download_locus <- shiny$downloadHandler(
    filename = function() paste0(selected_row()$Locus, ".fasta"),
    content = function(file) {
      pr <- overview()$primary
      fasta <- guard_db(
        "Exporting the locus FASTA",
        cla_locus_fasta(db_path(), selected_row()$.gene, pr$scheme, pr$repository)
      )
      # No file written means the browser reports the download as failed,
      # rather than saving an empty FASTA.
      shiny$req(!db_failed(fasta), length(fasta) > 0)
      writeLines(fasta, file)
    }
  )

  output$export_loci <- shiny$downloadHandler(
    filename = function() "mlst_loci.csv",
    content = function(file) {
      li <- overview()$loci
      shiny$req(nrow(li) > 0)
      write.csv(display_cols(li), file, row.names = FALSE)
    }
  )

  shiny$outputOptions(output, "allele_sequence", suspendWhenHidden = FALSE)
}

# Handles server logic for the ST table and the frequency charts.
distribution_server <- function(input, output, overview, reason) {
  output$cla_sts <- renderDT({
    render_info("output$cla_sts")
    ov <- overview()
    if (!nrow(ov$sts)) {
      return(overview_ui$message_table(if (identical(ov$state, "ok")) {
        "No isolate of the primary scheme has a Sequence Type assigned yet."
      } else {
        reason()
      }))
    }
    sts <- ov$sts
    names(sts)[names(sts) == "Share"] <- "Share (%)"
    overview_ui$data_table(sts)
  })

  # The most common STs among isolates of the primary scheme, followed by the
  # remaining STs and the isolates no ST could be assigned to.
  output$st_bars <- shiny$renderUI({
    render_info("output$st_bars")
    ov <- overview()
    iso <- ov$isolates[ov$isolates$on_primary & ov$isolates$has_calls, , drop = FALSE]
    if (!nrow(iso)) {
      return(overview_ui$empty_hint(reason()))
    }
    bars <- overview_ui$fold_top(
      paste("ST", ov$sts$ST),
      ov$sts$Isolates,
      TOP_STS,
      "%d other STs"
    )
    no_st <- sum(is.na(iso$st))
    if (no_st) {
      bars$labels <- c(bars$labels, "No ST assigned")
      bars$counts <- c(bars$counts, no_st)
      bars$tones <- c(bars$tones, "warning")
    }
    shiny$tagList(
      overview_ui$bar_rows(bars$labels, bars$counts, nrow(iso), tones = bars$tones)
    )
  })

  # Distinct registered alleles observed per locus.
  output$locus_bars <- shiny$renderUI({
    render_info("output$locus_bars")
    li <- overview()$loci
    if (!nrow(li)) {
      return(overview_ui$empty_hint("No loci to show."))
    }
    shiny$tagList(
      overview_ui$bar_rows(
        li$Locus,
        li$`Observed Alleles`,
        0,
        notes = rep("alleles", nrow(li))
      )
    )
  })

  output$export_sts <- shiny$downloadHandler(
    filename = function() "mlst_sequence_types.csv",
    content = function(file) {
      sts <- overview()$sts
      shiny$req(nrow(sts) > 0)
      write.csv(sts, file, row.names = FALSE)
    }
  )
}

# Per-isolate provenance table, flagged rows first.
isolate_rows <- function(ov) {
  iso <- ov$isolates
  flags <- character(nrow(iso))
  add_flag <- function(hit, text) {
    flags[hit] <<- ifelse(nzchar(flags[hit]), paste(flags[hit], text, sep = "; "), text)
  }
  add_flag(!iso$on_primary & !is.na(iso$scheme), "other scheme")
  add_flag(
    iso$older_release & iso$status %in% UNRESOLVED_STATUSES,
    "unresolved on older release"
  )
  add_flag(iso$other_thresholds, "other thresholds")
  add_flag(iso$prov_differs, "provenance differs")
  add_flag(!iso$has_calls & iso$status %in% CALLED_STATUSES, "no alleles stored")

  n_loci <- length(ov$loci$Locus)
  out <- data.frame(
    Isolate = iso$isolate,
    Status = ifelse(
      iso$status %in% names(STATUS_LABELS),
      STATUS_LABELS[iso$status],
      iso$status
    ),
    Flags = flags,
    ST = iso$st,
    Scheme = iso$scheme,
    Repository = vapply(iso$repository, repo_name, character(1), USE.NAMES = FALSE),
    Release = iso$version,
    # Out of the primary scheme's loci only where the isolate was typed against
    # it; another scheme has its own locus count.
    `Loci Called` = ifelse(
      !iso$has_calls,
      NA_character_,
      ifelse(
        iso$on_primary & n_loci > 0,
        sprintf("%d / %d", iso$n_loci, n_loci),
        as.character(iso$n_loci)
      )
    ),
    Identity = iso$identity,
    Coverage = iso$coverage,
    pyMLST = iso$pymlst_version,
    `Called At` = substr(iso$called_at, 1, 19),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[order(!nzchar(flags), out$Isolate), , drop = FALSE]
}

# One entry per scheme release isolates were typed against.
release_list <- function(rel) {
  shiny$div(
    class = "overview-groups",
    lapply(seq_len(nrow(rel)), function(i) {
      shiny$div(
        class = "overview-group",
        shiny$div(
          class = "overview-group_head",
          shiny$span(rel$Scheme[i]),
          shiny$div(
            class = "overview-group_badges",
            if (isTRUE(rel$Primary[i])) {
              shiny$span(class = "overview-badge is-primary", "Primary")
            }
          )
        ),
        shiny$div(
          class = "overview-group_meta",
          shiny$span(shiny$icon("server"), repo_name(rel$Repository[i])),
          shiny$span(
            shiny$icon("calendar"),
            if (is.na(rel$Release[i])) "no release" else rel$Release[i]
          ),
          shiny$span(shiny$icon("vial"), overview_ui$count_label(rel$Isolates[i])),
          shiny$span(shiny$icon("circle-check"), sprintf("%s with ST", fmt(rel$`With ST`[i])))
        )
      )
    })
  )
}

# Handles server logic for the provenance checks, releases and isolate table.
diagnostics_server <- function(input, output, overview) {
  isolates <- shiny$reactive(isolate_rows(overview()))

  output$cla_isolates <- renderDT({
    render_info("output$cla_isolates")
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

  output$releases <- shiny$renderUI({
    render_info("output$releases")
    rel <- overview()$releases
    if (!nrow(rel)) {
      return(overview_ui$empty_hint("No isolate has been typed against a classical scheme."))
    }
    release_list(rel)
  })

  output$export_isolates <- shiny$downloadHandler(
    filename = function() "mlst_isolate_provenance.csv",
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
    # Every classical MLST record of the database, summarised once per change.
    # `isolates` because typing, removal and merges add or drop calls; `schema`
    # because a restore can replace them wholesale.
    overview <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "isolates", "schema")
      guard_db_read("Loading the classical MLST overview", load_mlst_overview(db_path()))
    })

    # Species of the database's scheme, named in the "no scheme" explanation.
    species <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "schema")
      guard_db_read("Loading the scheme", load_db_species(db_path()))
    })

    # Put the loading overlays back while a newly opened database's tables draw.
    shiny$observeEvent(db_path(), overview_ui$reset_table_overlays(session$ns, TABLE_IDS))

    # Why there are no classical calls to show, shared by every tab.
    reason <- shiny$reactive(empty_reason(overview(), species()))

    scheme_info_server(input, output, overview, species, reason)
    loci_info_server(input, output, session, db_path, overview, reason, session_reset)
    distribution_server(input, output, overview, reason)
    diagnostics_server(input, output, overview)
  })
}
