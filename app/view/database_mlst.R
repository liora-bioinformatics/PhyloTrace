# app/view/database_mlst.R

#' "MLST Overview" interface of the Database menu: the classical MLST twin of
#' the cgMLST Overview, with the scheme, its loci, the ST distribution and the
#' provenance diagnostics of every classical call in the database.

box::use(
  DT[DTOutput, JS, datatable, renderDT],
  bslib[
    as_fill_carrier,
    as_fill_item,
    as_fillable_container,
    card,
    card_body,
    card_header,
    nav_panel,
    navset_card_tab,
  ],
  htmltools[HTML, htmlEscape],
  shiny,
  shinyWidgets[pickerInput, pickerOptions, updatePickerInput],
  shinyjs[runjs],
  stats[setNames],
  utils[head, write.csv],
)

box::use(
  app / logic / database_functions[load_db_species],
  app / logic / db_events,
  app / logic / db_guard[db_failed, guard_db, guard_db_read],
  app / logic / functions[color_sequence, copy_to_clipboard, render_info, stat_tile],
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
)

REPO_NAMES <- c(pubmlst = "PubMLST", pasteur = "Institut Pasteur (BIGSdb)")
REPO_HOMES <- c(pubmlst = "https://pubmlst.org", pasteur = "https://bigsdb.pasteur.fr")

# DataTables shown with a loading overlay until their first draw.
TABLE_IDS <- c("cla_loci", "cla_sts", "cla_isolates")

# Bars shown before the remainder is folded into one "other" row.
TOP_STS <- 12L
TOP_ALLELES <- 6L

# Clears a table's loading overlay once DataTables has drawn it.
DRAW_SIGNAL <- JS(
  "function(settings) {",
  "  $(this.api().table().node())",
  "    .closest('.loci-table-body').addClass('is-loaded');",
  "}"
)

# Thousands-separated count.
fmt <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)

# The repository's display name, or the stored token when it is unknown.
repo_name <- function(repo) {
  if (is.na(repo)) {
    return(NA_character_)
  }
  if (repo %in% names(REPO_NAMES)) REPO_NAMES[[repo]] else repo
}

# Card header with a CSV export button, as on the cgMLST loci table.
table_header <- function(title, button_id) {
  card_header(
    class = "loci-info-header",
    title,
    shiny$downloadButton(
      button_id,
      "Export CSV",
      icon = shiny$icon("file-csv"),
      class = "btn-sm loci-export-btn"
    )
  )
}

# Fill/scroll card body holding a DataTable behind a loading overlay.
table_body <- function(ns, id, label) {
  card_body(
    id = ns(paste0(id, "_body")),
    class = "loci-table-body",
    shiny$div(
      class = "loci-table-loading",
      shiny$div(
        class = "loci-loading-inner",
        shiny$div(class = "loci-spinner"),
        shiny$p(label, class = "loci-loading_text")
      )
    ),
    DTOutput(ns(id), fill = TRUE)
  )
}

# One-cell table carrying a message in place of data, as the cgMLST overview
# does for a database without scheme tables.
message_table <- function(text) {
  datatable(
    data.frame(" " = text, check.names = FALSE),
    rownames = FALSE,
    colnames = "",
    escape = FALSE,
    selection = "none",
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      initComplete = DRAW_SIGNAL
    )
  )
}

# Scrolling, filterable data table in the style of the cgMLST loci table.
data_table <- function(df, selection = "none", filter = "top") {
  datatable(
    df,
    rownames = FALSE,
    filter = filter,
    selection = selection,
    class = "stripe row-border order-column",
    options = list(
      dom = "ti",
      paging = FALSE,
      scrollX = TRUE,
      scrollY = "1px",
      scrollCollapse = TRUE,
      columnDefs = list(list(className = "dt-left", targets = "_all")),
      initComplete = DRAW_SIGNAL
    )
  )
}

# Muted placeholder for a panel with nothing to show.
empty_hint <- function(text) {
  shiny$div(class = "db-empty-hint mlst-empty", shiny$icon("circle-info"), shiny$p(HTML(text)))
}

# Uppercase caption above a block inside a card.
section_title <- function(text) shiny$div(class = "mlst-section-title", text)

# Horizontal bars for a handful of counts: label, a bar scaled to the largest
# count, the count and its share of `total`. `notes` replaces the share column
# where a share means nothing; hovering a row shows its exact figures.
bar_rows <- function(labels, counts, total, tones = NULL, notes = NULL) {
  peak <- max(c(counts, 1))
  shiny$div(
    class = "mlst-bars",
    lapply(seq_along(counts), function(i) {
      share <- if (total > 0) 100 * counts[i] / total else 0
      note <- if (is.null(notes)) sprintf("%.1f%%", share) else notes[i]
      shiny$div(
        class = "mlst-bar",
        title = if (is.null(notes)) {
          sprintf("%s: %s of %s isolates (%.1f%%)", labels[i], fmt(counts[i]), fmt(total), share)
        } else {
          sprintf("%s: %s %s", labels[i], fmt(counts[i]), note)
        },
        shiny$span(class = "mlst-bar_label", labels[i]),
        shiny$div(
          class = "mlst-bar_track",
          shiny$div(
            class = paste("mlst-bar_fill", if (!is.null(tones)) paste0("is-", tones[i])),
            style = sprintf("width: %.1f%%;", 100 * counts[i] / peak)
          )
        ),
        shiny$span(class = "mlst-bar_value", fmt(counts[i])),
        shiny$span(class = "mlst-bar_share", note)
      )
    })
  )
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

# Renders the UI layout for the Scheme Info tab.
scheme_info_ui <- function(ns) {
  as_fill_carrier(
    shiny$div(
      class = "scheme-info-layout",
      as_fill_carrier(
        shiny$div(
          class = "scheme-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("Scheme Metadata"),
              card_body(DTOutput(ns("scheme_table"), fill = TRUE))
            )
          )
        )
      ),
      as_fill_carrier(
        shiny$div(
          class = "scheme-aside mlst-aside",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("Typing Summary"),
              card_body(shiny$uiOutput(ns("typing_summary")))
            )
          )
        )
      )
    )
  )
}

# Renders the UI layout for the Loci Info tab.
loci_info_ui <- function(ns) {
  as_fill_carrier(
    shiny$div(
      class = "loci-info-layout",
      as_fill_carrier(
        shiny$div(
          class = "loci-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              table_header("Loci Table", ns("export_loci")),
              table_body(ns, "cla_loci", "Loading loci table ...")
            )
          )
        )
      ),
      as_fillable_container(
        shiny$div(
          class = "loci-controls",
          pickerInput(
            ns("allele_select"),
            label = "Select locus from table",
            choices = character(0),
            options = pickerOptions(
              liveSearch = TRUE,
              size = 10,
              liveSearchPlaceholder = "Search alleles ...",
              container = "body"
            )
          ),
          shiny$uiOutput(ns("allele_bars")),
          as_fill_carrier(
            shiny$div(
              class = "loci-allele-panel",
              shiny$div(
                class = "loci-allele-header",
                shiny$div(
                  class = "loci-allele-actions",
                  shiny$actionButton(
                    ns("copy_seq"),
                    "Sequence",
                    icon = shiny$icon("copy"),
                    class = "btn-sm"
                  ),
                  shiny$actionButton(
                    ns("copy_idx"),
                    "Allele",
                    icon = shiny$icon("hashtag"),
                    class = "btn-sm"
                  ),
                  shiny$downloadButton(
                    ns("download_locus"),
                    "Locus",
                    icon = shiny$icon("download"),
                    class = "btn-sm"
                  )
                )
              ),
              as_fill_item(
                shiny$div(class = "sequence", shiny$uiOutput(ns("allele_sequence")))
              )
            )
          )
        )
      )
    )
  )
}

# Renders the UI layout for the Distribution tab.
distribution_ui <- function(ns) {
  as_fill_carrier(
    shiny$div(
      class = "scheme-info-layout",
      as_fill_carrier(
        shiny$div(
          class = "scheme-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              table_header("Sequence Types", ns("export_sts")),
              table_body(ns, "cla_sts", "Loading sequence types ...")
            )
          )
        )
      ),
      as_fill_carrier(
        shiny$div(
          class = "scheme-aside mlst-aside",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("ST Frequency"),
              card_body(shiny$uiOutput(ns("st_bars")))
            )
          ),
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("Allele Diversity"),
              card_body(shiny$uiOutput(ns("locus_bars")))
            )
          )
        )
      )
    )
  )
}

# Renders the UI layout for the Diagnostics tab.
diagnostics_ui <- function(ns) {
  as_fill_carrier(
    shiny$div(
      class = "scheme-info-layout",
      as_fill_carrier(
        shiny$div(
          class = "scheme-info-main",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              table_header("Isolate Provenance", ns("export_isolates")),
              table_body(ns, "cla_isolates", "Loading isolate provenance ...")
            )
          )
        )
      ),
      as_fill_carrier(
        shiny$div(
          class = "scheme-aside mlst-aside",
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("Checks"),
              card_body(shiny$uiOutput(ns("checks")))
            )
          ),
          as_fill_item(
            card(
              fill = TRUE,
              full_screen = TRUE,
              card_header("Scheme Releases"),
              card_body(shiny$uiOutput(ns("releases")))
            )
          )
        )
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  navset_card_tab(
    nav_panel("Scheme Info", scheme_info_ui(ns)),
    nav_panel("Loci Info", loci_info_ui(ns)),
    nav_panel("Distribution", distribution_ui(ns)),
    nav_panel("Diagnostics", diagnostics_ui(ns))
  )
}

# Key/value rows describing the primary scheme, as HTML-safe strings.
scheme_rows <- function(ov, species) {
  pr <- ov$primary
  calls <- ov$isolates[ov$isolates$on_primary & ov$isolates$has_calls, , drop = FALSE]
  esc <- function(x) if (is.null(x) || is.na(x)) NA_character_ else htmlEscape(x)
  link <- function(url, label = url) {
    if (is.null(url) || is.na(url)) {
      return(NA_character_)
    }
    sprintf(
      "<a href='%s' target='_blank'>%s</a>",
      htmlEscape(url, attribute = TRUE),
      htmlEscape(label)
    )
  }
  distinct <- function(x) {
    x <- unique(x[!is.na(x)])
    if (!length(x)) NA_character_ else paste(x, collapse = ", ")
  }

  repo <- pr$repository
  repository <- if (!is.na(repo) && repo %in% names(REPO_HOMES)) {
    link(REPO_HOMES[[repo]], repo_name(repo))
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

  rows <- c(
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
  rows <- rows[!is.na(rows)]
  data.frame(
    key = sprintf("<strong>%s</strong>", names(rows)),
    value = unname(rows),
    stringsAsFactors = FALSE
  )
}

# Handles server logic for the scheme metadata and the typing summary.
scheme_info_server <- function(input, output, overview, species, reason) {
  output$scheme_table <- renderDT({
    render_info("output$scheme_table")
    ov <- overview()
    if (!identical(ov$state, "ok") || is.null(ov$primary)) {
      return(message_table(reason()))
    }
    rows <- scheme_rows(ov, species())
    datatable(
      rows,
      class = "stripe row-border order-column",
      colnames = rep("", ncol(rows)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    )
  })

  # Headline figures across every isolate, then how their calls came out.
  output$typing_summary <- shiny$renderUI({
    render_info("output$typing_summary")
    ov <- overview()
    iso <- ov$isolates
    total <- nrow(iso)
    if (!total) {
      return(empty_hint("This database holds no isolates yet."))
    }
    with_st <- sum(!is.na(iso$st))
    diversity <- simpson_diversity(ov$sts$Isolates)

    shiny$tagList(
      shiny$div(
        class = "mlst-stats",
        stat_tile("Typed", sprintf("%s / %s", fmt(sum(iso$has_calls)), fmt(total)), "vial"),
        stat_tile("ST Assigned", sprintf("%.1f%%", 100 * with_st / total), "circle-check"),
        stat_tile("Distinct STs", fmt(nrow(ov$sts)), "fingerprint"),
        stat_tile(
          "Diversity",
          if (is.na(diversity)) "-" else sprintf("%.3f", diversity),
          "chart-simple"
        )
      ),
      section_title("Call Status"),
      bar_rows(ov$status$label, ov$status$n, total, tones = ov$status$tone),
      shiny$p(
        class = "mlst-note",
        "Diversity is Simpson's index (Hunter-Gaston) over isolates with an ST:",
        "the chance that two of them carry different STs."
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

  # Reset module state when the user returns to the landing screen.
  shiny$observeEvent(session_reset(), seq_cache(NULL), ignoreInit = TRUE)

  display_cols <- function(df) df[, setdiff(names(df), ".gene"), drop = FALSE]

  output$cla_loci <- renderDT({
    render_info("output$cla_loci")
    ov <- overview()
    if (!nrow(ov$loci)) {
      return(message_table(reason()))
    }
    data_table(
      display_cols(ov$loci),
      selection = list(mode = "single", selected = 1),
      filter = "none"
    )
  })

  # The selected locus row (its `.gene` keys the allele lookups).
  selected_row <- shiny$reactive({
    li <- overview()$loci
    row <- input$cla_loci_rows_selected
    shiny$req(nrow(li) > 0, length(row) == 1)
    li[row, , drop = FALSE]
  })

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
    updatePickerInput(
      session,
      "allele_select",
      label = paste("Selected Locus:", selected_row()$Locus),
      choices = setNames(df$allele, labels),
      selected = df$allele[1]
    )
  })

  # Most frequent alleles of the selected locus, the rest folded into one row.
  output$allele_bars <- shiny$renderUI({
    df <- alleles()
    shiny$req(nrow(df) > 0)
    render_info("output$allele_bars")
    called <- selected_row()$`Isolates Called`
    shown <- head(df, TOP_ALLELES)
    labels <- paste("Allele", shown$allele)
    counts <- shown$count
    tones <- rep("primary", nrow(shown))
    if (nrow(df) > TOP_ALLELES) {
      labels <- c(labels, sprintf("%d other alleles", nrow(df) - TOP_ALLELES))
      counts <- c(counts, sum(df$count[-seq_len(TOP_ALLELES)]))
      tones <- c(tones, "secondary")
    }
    shiny$div(
      class = "mlst-allele-bars",
      section_title("Allele Frequency"),
      bar_rows(labels, counts, called, tones = tones)
    )
  })

  output$allele_sequence <- shiny$renderUI({
    shiny$req(db_path(), input$allele_select)
    pr <- overview()$primary
    shiny$req(pr)
    gene <- selected_row()$.gene
    render_info("output$allele_sequence")

    sequence <- guard_db_read(
      "Loading the allele sequence",
      load_cla_allele_sequence(
        db_path(),
        gene,
        input$allele_select,
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
          input$allele_select
        )
      ))
    }

    HTML(paste0(
      "<div class=\"fasta-header\">&gt;",
      htmlEscape(paste0(gene, "_", input$allele_select)),
      "</div>",
      color_sequence(sequence)
    ))
  })

  # Copy actions -----------------------------------------------------------
  shiny$observeEvent(input$copy_seq, {
    shiny$req(seq_cache())
    copy_to_clipboard(seq_cache())
  })

  shiny$observeEvent(input$copy_idx, {
    shiny$req(input$allele_select)
    copy_to_clipboard(input$allele_select)
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
      return(message_table(if (identical(ov$state, "ok")) {
        "No isolate of the primary scheme has a Sequence Type assigned yet."
      } else {
        reason()
      }))
    }
    sts <- ov$sts
    names(sts)[names(sts) == "Share"] <- "Share (%)"
    data_table(sts)
  })

  # The most common STs among isolates of the primary scheme, followed by the
  # remaining STs and the isolates no ST could be assigned to.
  output$st_bars <- shiny$renderUI({
    render_info("output$st_bars")
    ov <- overview()
    iso <- ov$isolates[ov$isolates$on_primary & ov$isolates$has_calls, , drop = FALSE]
    if (!nrow(iso)) {
      return(empty_hint(reason()))
    }
    sts <- ov$sts
    shown <- head(sts, TOP_STS)
    labels <- paste("ST", shown$ST)
    counts <- shown$Isolates
    tones <- rep("primary", nrow(shown))
    if (nrow(sts) > TOP_STS) {
      labels <- c(labels, sprintf("%d other STs", nrow(sts) - TOP_STS))
      counts <- c(counts, sum(sts$Isolates[-seq_len(TOP_STS)]))
      tones <- c(tones, "secondary")
    }
    no_st <- sum(is.na(iso$st))
    if (no_st) {
      labels <- c(labels, "No ST assigned")
      counts <- c(counts, no_st)
      tones <- c(tones, "warning")
    }
    shiny$tagList(
      bar_rows(labels, counts, nrow(iso), tones = tones),
      shiny$p(
        class = "mlst-note",
        sprintf("Shares are of the %s isolates with a stored profile.", fmt(nrow(iso)))
      )
    )
  })

  # Distinct registered alleles observed per locus.
  output$locus_bars <- shiny$renderUI({
    render_info("output$locus_bars")
    li <- overview()$loci
    if (!nrow(li)) {
      return(empty_hint("No loci to show."))
    }
    shiny$tagList(
      bar_rows(
        li$Locus,
        li$`Observed Alleles`,
        0,
        notes = rep("alleles", nrow(li))
      ),
      shiny$p(
        class = "mlst-note",
        "Distinct registered alleles called per locus in this database."
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

# Handles server logic for the provenance checks, releases and isolate table.
diagnostics_server <- function(input, output, overview) {
  isolates <- shiny$reactive(isolate_rows(overview()))

  output$cla_isolates <- renderDT({
    render_info("output$cla_isolates")
    rows <- isolates()
    if (!nrow(rows)) {
      return(message_table("This database holds no isolates yet."))
    }
    data_table(rows)
  })

  output$checks <- shiny$renderUI({
    render_info("output$checks")
    checks <- overview()$checks
    if (!nrow(checks)) {
      return(empty_hint("Nothing to check yet - no isolate has been typed."))
    }
    tone_icon <- c(
      danger = "circle-xmark",
      warning = "triangle-exclamation",
      info = "circle-info",
      success = "circle-check"
    )
    shiny$div(
      class = "mlst-checks",
      lapply(seq_len(nrow(checks)), function(i) {
        shiny$div(
          class = paste0("mlst-check is-", checks$tone[i]),
          shiny$icon(tone_icon[[checks$tone[i]]]),
          shiny$div(
            class = "mlst-check_body",
            shiny$div(
              class = "mlst-check_title",
              shiny$span(checks$check[i]),
              if (!is.na(checks$n[i])) {
                shiny$span(class = "mlst-check_count", fmt(checks$n[i]))
              }
            ),
            shiny$p(class = "mlst-check_detail", checks$detail[i])
          )
        )
      })
    )
  })

  output$releases <- shiny$renderUI({
    render_info("output$releases")
    rel <- overview()$releases
    if (!nrow(rel)) {
      return(empty_hint("No isolate has been typed against a classical scheme."))
    }
    shiny$div(
      class = "mlst-releases",
      lapply(seq_len(nrow(rel)), function(i) {
        shiny$div(
          class = "mlst-release",
          shiny$div(
            class = "mlst-release_head",
            shiny$span(rel$Scheme[i]),
            shiny$div(
              class = "mlst-release_badges",
              if (isTRUE(rel$Primary[i])) shiny$span(class = "mlst-badge is-primary", "Primary")
            )
          ),
          shiny$div(
            class = "mlst-release_meta",
            shiny$span(shiny$icon("server"), repo_name(rel$Repository[i])),
            shiny$span(
              shiny$icon("calendar"),
              if (is.na(rel$Release[i])) "no release" else rel$Release[i]
            ),
            shiny$span(shiny$icon("vial"), sprintf("%s isolates", fmt(rel$Isolates[i]))),
            shiny$span(shiny$icon("circle-check"), sprintf("%s with ST", fmt(rel$`With ST`[i])))
          )
        )
      })
    )
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
    ns <- session$ns

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
    shiny$observeEvent(db_path(), {
      runjs(sprintf(
        "$('%s').removeClass('is-loaded');",
        paste0("#", ns(paste0(TABLE_IDS, "_body")), collapse = ", ")
      ))
    })

    # Why there are no classical calls to show, shared by every tab.
    reason <- shiny$reactive(empty_reason(overview(), species()))

    scheme_info_server(input, output, overview, species, reason)
    loci_info_server(input, output, session, db_path, overview, reason, session_reset)
    distribution_server(input, output, overview, reason)
    diagnostics_server(input, output, overview)
  })
}
