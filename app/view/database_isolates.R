# app/view/database_isolates.R

#' "Isolate Overview" interface of the Database menu: where the isolates came
#' from, the assemblies they were typed from, how those assemblies are
#' distributed and the record diagnostics of every isolate and typing run.

box::use(
  DT[DTOutput, renderDT],
  bslib[as_fill_carrier, as_fill_item, as_fillable_container, nav_panel, navset_card_tab],
  htmltools[htmlEscape],
  shiny,
  stats[median],
  utils[write.csv],
)

box::use(
  app / logic / app_meta[APP_VERSION],
  app / logic / database_functions[load_db_species],
  app / logic / db_events,
  app / logic / db_guard[guard_db_read],
  app / logic / functions[copy_to_clipboard, render_info, stat_tile],
  app /
    logic /
    isolate_overview[
      CONTIGUITY,
      LOCAL_LABEL,
      format_bp,
      format_duration,
      isolate_flags,
      load_isolate_overview,
      one_isolate_checks,
    ],
  app / logic / overview_ui,
)

# DataTables shown with a loading overlay until their first draw.
TABLE_IDS <- c("iso_table", "iso_runs")

# Bars shown before the remainder is folded into one "other" row.
TOP_SOURCES <- 6L

fmt <- overview_ui$fmt

NO_ISOLATES <- "This database holds no isolates yet."

NO_ASSEMBLIES <- paste(
  "No assembly statistics on record - the isolates were typed before PhyloTrace",
  "recorded assemblies, or imported without their records."
)

CONTIGUITY_HINT <- paste(
  "Isolates by contig count of their assembly. Complete: up to 10 contigs, typical",
  "of long-read or hybrid assemblies. Draft: 11-200, typical of short reads.",
  "Fragmented: 201-500. Highly fragmented: over 500, where loci go missing."
)

# "a" when every value agrees, a pointer to Diagnostics when they differ.
one_or_mixed <- function(x) {
  x <- unique(x[!is.na(x)])
  if (!length(x)) {
    NA_character_
  } else if (length(x) == 1) {
    htmlEscape(x)
  } else {
    "Mixed - see <strong>Diagnostics</strong>"
  }
}

# Median and range of a figure, formatted by `f`; NA when nothing is recorded.
median_range <- function(x, f) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_character_)
  }
  if (length(x) == 1) {
    return(f(x))
  }
  sprintf("%s (range %s - %s)", f(median(x)), f(min(x)), f(max(x)))
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  navset_card_tab(
    nav_panel(
      "Collection Info",
      overview_ui$split_layout(
        overview_ui$fill_card(
          "Collection Metadata",
          DTOutput(ns("collection_table"), fill = TRUE),
          hint = paste(
            "Where the isolates came from, when and with what they were typed, and",
            "the assemblies they were typed from, as recorded per isolate."
          )
        ),
        overview_ui$fill_card(
          "Collection Summary",
          shiny$uiOutput(ns("collection_summary")),
          hint = paste(
            CONTIGUITY_HINT,
            "Origin separates isolates typed in this database from imported ones."
          )
        )
      )
    ),
    nav_panel("Isolates", isolates_ui(ns)),
    nav_panel(
      "Distribution",
      overview_ui$split_layout(
        list(
          overview_ui$fill_card(
            "Assembly Length",
            shiny$uiOutput(ns("size_bars")),
            hint = paste(
              "Isolates by how far their assembly length is off the median of the",
              "collection; beyond 20% an assembly is flagged as unusual."
            )
          ),
          overview_ui$fill_card(
            "Contigs per Assembly",
            shiny$uiOutput(ns("contig_bars")),
            hint = CONTIGUITY_HINT
          )
        ),
        overview_ui$fill_card(
          "Typing Timeline",
          shiny$uiOutput(ns("timeline_bars")),
          hint = paste(
            "Isolates by when they were typed: per month, per quarter beyond a year",
            "and per year beyond three. Without a typing record, the date the",
            "assembly was hashed or the isolate was added stands in."
          )
        )
      )
    ),
    nav_panel(
      "Diagnostics",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "iso_runs",
          "Typing Runs",
          "export_runs",
          "Loading typing runs ...",
          hint = paste(
            "One row per typing run (its log file), with the assemblies it typed.",
            "A run whose contig counts or lengths stand apart from the others",
            "often had another sequencing technology or assembler behind it."
          )
        ),
        list(
          overview_ui$fill_card(
            "Checks",
            shiny$uiOutput(ns("checks")),
            hint = overview_ui$CHECKS_HINT
          ),
          overview_ui$fill_card(
            "Typing Environment",
            shiny$uiOutput(ns("environments")),
            hint = paste(
              "Isolates grouped by the PhyloTrace version that typed them and the",
              "algorithm their assembly was hashed with; Current marks this version."
            )
          )
        )
      )
    )
  )
}

# Renders the UI layout for the Isolates tab: the isolate table next to the
# selected isolate's flags and record details.
isolates_ui <- function(ns) {
  as_fill_carrier(shiny$div(
    class = "loci-info-layout",
    as_fill_carrier(shiny$div(
      class = "loci-info-main",
      overview_ui$table_card(
        ns,
        "iso_table",
        "Isolates",
        "export_isolates",
        "Loading isolates ...",
        hint = paste(
          "Every isolate with its origin, the assembly it was typed from and its",
          "typing run. Flagged rows come first; select a row for its details."
        )
      )
    )),
    as_fillable_container(shiny$div(
      class = "loci-controls",
      shiny$uiOutput(ns("isolate_checks")),
      as_fill_carrier(shiny$div(
        class = "loci-allele-panel",
        shiny$div(
          class = "loci-allele-header",
          shiny$div(
            class = "loci-allele-actions",
            shiny$actionButton(ns("copy_name"), "Name", icon = shiny$icon("tag"), class = "btn-sm"),
            shiny$actionButton(
              ns("copy_digest"),
              "Digest",
              icon = shiny$icon("fingerprint"),
              class = "btn-sm"
            ),
            shiny$actionButton(
              ns("copy_sha"),
              "SHA-256",
              icon = shiny$icon("file-shield"),
              class = "btn-sm"
            )
          )
        ),
        as_fill_item(shiny$div(class = "overview-detail", shiny$uiOutput(ns("isolate_detail"))))
      ))
    ))
  ))
}

# Key/value rows describing the collection, values as HTML.
collection_rows <- function(ov, species) {
  iso <- ov$isolates
  esc <- function(x) if (is.null(x) || !length(x) || is.na(x)) NA_character_ else htmlEscape(x)
  n <- nrow(iso)
  imported <- iso$source[iso$imported]
  recorded <- iso[!iso$no_assembly, , drop = FALSE]
  distinct <- length(unique(paste(recorded$algorithm, recorded$digest)))
  runs <- unique(iso$run_id[!is.na(iso$run_id)])
  dates <- sort(iso$typed_date[!is.na(iso$typed_date)])

  c(
    "Species" = esc(species),
    "Isolates" = fmt(n),
    "Origin" = if (!length(imported)) {
      "All typed in this database"
    } else {
      sprintf(
        "%s typed here, %s imported from %s",
        fmt(n - length(imported)),
        fmt(length(imported)),
        esc(paste(sort(unique(imported)), collapse = ", "))
      )
    },
    "Typing Period" = if (length(dates)) {
      esc(if (dates[1] == dates[length(dates)]) {
        format(dates[1])
      } else {
        paste(format(dates[1]), "to", format(dates[length(dates)]))
      })
    } else {
      NA_character_
    },
    "Typing Runs" = if (length(runs)) fmt(length(runs)) else NA_character_,
    "PhyloTrace" = one_or_mixed(iso$phylotrace),
    "Assembly Records" = sprintf("%s of %s isolates", fmt(nrow(recorded)), fmt(n)),
    "Distinct Assemblies" = if (nrow(recorded)) {
      if (distinct < nrow(recorded)) {
        sprintf(
          "%s (%s isolates share an assembly - see <strong>Diagnostics</strong>)",
          fmt(distinct),
          fmt(sum(iso$duplicate))
        )
      } else {
        fmt(distinct)
      }
    } else {
      NA_character_
    },
    "Digest Algorithm" = one_or_mixed(recorded$algorithm),
    "Assembly Length" = esc(median_range(iso$length, format_bp)),
    "Contigs" = esc(median_range(iso$contigs, fmt)),
    "Typing Time" = if (any(!is.na(iso$elapsed))) {
      esc(sprintf("%s per isolate (median)", format_duration(median(iso$elapsed, na.rm = TRUE))))
    } else {
      NA_character_
    }
  )
}

# Handles server logic for the collection metadata and summary.
collection_info_server <- function(input, output, overview, species) {
  output$collection_table <- renderDT({
    render_info("output$collection_table")
    ov <- overview()
    if (!identical(ov$state, "ok")) {
      return(overview_ui$message_table(NO_ISOLATES))
    }
    rows <- collection_rows(ov, species())
    overview_ui$key_value_table(names(rows), unname(rows))
  })

  # Headline figures, how contiguous the assemblies are and where the isolates
  # came from.
  output$collection_summary <- shiny$renderUI({
    render_info("output$collection_summary")
    ov <- overview()
    iso <- ov$isolates
    n <- nrow(iso)
    if (!n) {
      return(overview_ui$empty_hint(NO_ISOLATES))
    }
    med_contigs <- if (any(!is.na(iso$contigs))) median(iso$contigs, na.rm = TRUE) else NA
    origin <- overview_ui$fold_top(ov$origin$label, ov$origin$n, TOP_SOURCES, "%d other sources")
    origin$tones[origin$labels == LOCAL_LABEL] <- "success"

    shiny$tagList(
      overview_ui$stat_grid(
        stat_tile("Isolates", fmt(n), "vial"),
        stat_tile("Assemblies", sprintf("%s / %s", fmt(sum(!iso$no_assembly)), fmt(n)), "dna"),
        stat_tile(
          "Median Length",
          if (is.na(ov$median_length)) "-" else format_bp(ov$median_length),
          "ruler-horizontal"
        ),
        stat_tile(
          "Median Contigs",
          if (is.na(med_contigs)) "-" else fmt(med_contigs),
          "puzzle-piece"
        )
      ),
      overview_ui$section_title("Assembly Contiguity"),
      overview_ui$bar_rows(ov$contiguity$label, ov$contiguity$n, n, tones = ov$contiguity$tone),
      shiny$div(class = "overview-section-gap"),
      overview_ui$section_title("Origin"),
      overview_ui$bar_rows(origin$labels, origin$counts, n, tones = origin$tones)
    )
  })
}

# Per-isolate table, flagged rows first.
isolate_rows <- function(iso) {
  flags <- isolate_flags(iso)
  contiguity <- CONTIGUITY$label[match(iso$contiguity, CONTIGUITY$key)]
  out <- data.frame(
    Isolate = iso$isolate,
    Flags = flags,
    Source = ifelse(iso$imported, iso$source, LOCAL_LABEL),
    Assembly = contiguity,
    Contigs = iso$contigs,
    `Length (bp)` = iso$length,
    `Mean Contig (bp)` = iso$mean_contig,
    Run = iso$run_id,
    `Typed At` = substr(iso$typed_at, 1, 19),
    `Typing Time (s)` = round(iso$elapsed, 1),
    PhyloTrace = iso$phylotrace,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[order(!nzchar(flags), out$Isolate), , drop = FALSE]
}

# One isolate's records as label/value rows: its origin, the assembly on record
# and how it was typed.
detail_rows <- function(row, median_length, median_contigs) {
  off <- if (is.na(row$length) || is.na(median_length)) {
    ""
  } else {
    sprintf(" (%+.1f%% vs. median)", 100 * (row$length - median_length) / median_length)
  }
  contiguity <- CONTIGUITY$label[match(row$contiguity, CONTIGUITY$key)]
  rows <- c(
    Source = if (row$imported) paste("Imported from", row$source) else LOCAL_LABEL,
    Assembly = if (is.na(contiguity)) {
      NA
    } else {
      sprintf("%s (%s contigs)", contiguity, fmt(row$contigs))
    },
    Length = if (is.na(row$length)) NA else paste0(fmt(row$length), " bp", off),
    Contigs = if (is.na(row$contigs) || is.na(median_contigs)) {
      NA
    } else {
      sprintf("%s (median %s)", fmt(row$contigs), fmt(median_contigs))
    },
    `Mean Contig` = if (is.na(row$mean_contig)) NA else paste(fmt(row$mean_contig), "bp"),
    `File Size` = if (is.na(row$file_bytes)) {
      NA
    } else {
      sprintf("%.2f MB", row$file_bytes / 1024^2)
    },
    Digest = row$digest,
    `Typed From` = if (isTRUE(row$changed)) row$prov_digest else NA,
    `File SHA-256` = row$file_sha256,
    Algorithm = row$algorithm,
    `Hashed At` = substr(row$hashed_at, 1, 19),
    `Same Assembly As` = row$twins,
    Run = row$run_id,
    `Typed At` = substr(row$typed_at, 1, 19),
    `Typing Time` = format_duration(row$elapsed),
    PhyloTrace = row$phylotrace
  )
  rows <- rows[!is.na(rows) & nzchar(rows)]
  shiny$tags$dl(
    lapply(names(rows), function(k) shiny$tagList(shiny$tags$dt(k), shiny$tags$dd(rows[[k]])))
  )
}

# Handles server logic for the isolate table, the selected isolate's flags,
# details and copy buttons.
isolates_server <- function(input, output, overview) {
  rows <- shiny$reactive(isolate_rows(overview()$isolates))

  output$iso_table <- renderDT({
    render_info("output$iso_table")
    r <- rows()
    if (!nrow(r)) {
      return(overview_ui$message_table(NO_ISOLATES))
    }
    overview_ui$data_table(r, selection = list(mode = "single", selected = 1))
  })

  # Index of the selected isolate in the overview's isolate table.
  selected <- shiny$reactive({
    r <- rows()
    row <- input$iso_table_rows_selected
    shiny$req(nrow(r) > 0, length(row) == 1, row <= nrow(r))
    i <- match(r$Isolate[row], overview()$isolates$isolate)
    shiny$req(!is.na(i))
    i
  })

  # The selected isolate's flags, or a confirmation when it has none.
  output$isolate_checks <- shiny$renderUI({
    i <- selected()
    render_info("output$isolate_checks")
    checks <- one_isolate_checks(overview()$isolates, i)
    if (!nrow(checks)) {
      checks <- data.frame(
        check = "No flags",
        tone = "success",
        detail = "Nothing unusual about this isolate's assembly or records.",
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
    overview_ui$checks_ui(checks)
  })

  output$isolate_detail <- shiny$renderUI({
    i <- selected()
    render_info("output$isolate_detail")
    iso <- overview()$isolates
    med <- function(x) if (any(!is.na(x))) median(x, na.rm = TRUE) else NA_real_
    shiny$tagList(
      overview_ui$section_title(iso$isolate[i]),
      detail_rows(iso[i, , drop = FALSE], med(iso$length), med(iso$contigs))
    )
  })

  # Copy actions -----------------------------------------------------------
  shiny$observeEvent(input$copy_name, {
    copy_to_clipboard(overview()$isolates$isolate[selected()])
  })

  shiny$observeEvent(input$copy_digest, {
    digest <- overview()$isolates$digest[selected()]
    shiny$req(!is.na(digest))
    copy_to_clipboard(digest)
  })

  shiny$observeEvent(input$copy_sha, {
    sha <- overview()$isolates$file_sha256[selected()]
    shiny$req(!is.na(sha))
    copy_to_clipboard(sha)
  })

  output$export_isolates <- shiny$downloadHandler(
    filename = function() "isolates.csv",
    content = function(file) {
      r <- rows()
      shiny$req(nrow(r) > 0)
      write.csv(r, file, row.names = FALSE)
    }
  )
}

# Handles server logic for the assembly length, contig and timeline charts.
distribution_server <- function(input, output, overview) {
  n_isolates <- shiny$reactive(nrow(overview()$isolates))

  # Isolates by how far their assembly length is off the collection's median.
  output$size_bars <- shiny$renderUI({
    render_info("output$size_bars")
    ov <- overview()
    sized <- sum(ov$size_bins$n)
    if (!n_isolates()) {
      return(overview_ui$empty_hint(NO_ISOLATES))
    }
    if (!sized) {
      return(overview_ui$empty_hint(NO_ASSEMBLIES))
    }
    shiny$tagList(
      overview_ui$section_title(sprintf("Median %s", format_bp(ov$median_length))),
      overview_ui$bar_rows(ov$size_bins$label, ov$size_bins$n, sized, tones = ov$size_bins$tone)
    )
  })

  # Isolates by how many contigs their assembly is split into.
  output$contig_bars <- shiny$renderUI({
    render_info("output$contig_bars")
    bins <- overview()$contig_bins
    counted <- sum(bins$n)
    if (!n_isolates()) {
      return(overview_ui$empty_hint(NO_ISOLATES))
    }
    if (!counted) {
      return(overview_ui$empty_hint(NO_ASSEMBLIES))
    }
    overview_ui$bar_rows(
      ifelse(bins$label == "1", "1 contig", paste(bins$label, "contigs")),
      bins$n,
      counted,
      tones = bins$tone
    )
  })

  # Isolates typed per month or year, the undated ones last.
  output$timeline_bars <- shiny$renderUI({
    render_info("output$timeline_bars")
    ov <- overview()
    n <- n_isolates()
    if (!n) {
      return(overview_ui$empty_hint(NO_ISOLATES))
    }
    tl <- ov$timeline
    undated <- sum(is.na(ov$isolates$typed_date))
    if (!nrow(tl)) {
      return(overview_ui$empty_hint("No typing date is recorded for any isolate."))
    }
    labels <- c(tl$label, if (undated) "Not dated")
    shiny$tagList(
      overview_ui$bar_rows(
        labels,
        c(tl$n, if (undated) undated),
        n,
        tones = c(rep("primary", nrow(tl)), if (undated) "secondary")
      )
    )
  })
}

# One entry per PhyloTrace version and digest algorithm behind the records.
environment_list <- function(env) {
  shiny$div(
    class = "overview-groups",
    lapply(seq_len(nrow(env)), function(i) {
      version <- env$phylotrace[i]
      shiny$div(
        class = "overview-group",
        shiny$div(
          class = "overview-group_head",
          shiny$span(
            if (is.na(version)) "PhyloTrace version not recorded" else paste("PhyloTrace", version)
          ),
          shiny$div(
            class = "overview-group_badges",
            if (identical(version, APP_VERSION)) {
              shiny$span(class = "overview-badge is-primary", "Current")
            }
          )
        ),
        shiny$div(
          class = "overview-group_meta",
          shiny$span(
            shiny$icon("fingerprint"),
            if (is.na(env$algorithm[i])) "No assembly digest" else env$algorithm[i]
          ),
          shiny$span(shiny$icon("vial"), sprintf("%s isolates", fmt(env$isolates[i]))),
          if (env$runs[i] > 0) {
            shiny$span(
              shiny$icon("list-check"),
              sprintf("%s run%s", fmt(env$runs[i]), if (env$runs[i] == 1) "" else "s")
            )
          }
        )
      )
    })
  )
}

# Handles server logic for the run table, checks and typing environments.
diagnostics_server <- function(input, output, overview) {
  output$iso_runs <- renderDT({
    render_info("output$iso_runs")
    ov <- overview()
    if (!nrow(ov$runs)) {
      return(overview_ui$message_table(NO_ISOLATES))
    }
    overview_ui$data_table(ov$runs)
  })

  output$checks <- shiny$renderUI({
    render_info("output$checks")
    checks <- overview()$checks
    if (!nrow(checks)) {
      return(overview_ui$empty_hint(paste("Nothing to check yet -", tolower(NO_ISOLATES))))
    }
    overview_ui$checks_ui(checks)
  })

  output$environments <- shiny$renderUI({
    render_info("output$environments")
    env <- overview()$environments
    if (!nrow(env)) {
      return(overview_ui$empty_hint(paste(
        "No isolate has a typing or assembly record - typed before PhyloTrace",
        "recorded them, or imported without them."
      )))
    }
    environment_list(env)
  })

  output$export_runs <- shiny$downloadHandler(
    filename = function() "typing_runs.csv",
    content = function(file) {
      runs <- overview()$runs
      shiny$req(nrow(runs) > 0)
      write.csv(runs, file, row.names = FALSE)
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
    # Every isolate record of the database, summarised once per change.
    # `isolates` because typing, removal and merges add, drop or rewrite
    # isolates with their assembly and provenance records; `schema` because a
    # restore can replace them wholesale.
    overview <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "isolates", "schema")
      guard_db_read("Loading the isolate overview", load_isolate_overview(db_path()))
    })

    # Species of the database's scheme, shown with the collection.
    species <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "schema")
      guard_db_read("Loading the scheme", load_db_species(db_path()))
    })

    # Put the loading overlays back while a newly opened database's tables draw.
    shiny$observeEvent(db_path(), overview_ui$reset_table_overlays(session$ns, TABLE_IDS))

    collection_info_server(input, output, overview, species)
    isolates_server(input, output, overview)
    distribution_server(input, output, overview)
    diagnostics_server(input, output, overview)
  })
}
