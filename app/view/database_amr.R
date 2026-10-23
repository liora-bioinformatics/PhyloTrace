# app/view/database_amr.R

#' "AMR Overview" interface of the Database menu: the screening setup, every
#' detected element with its call quality, the Resistance, Virulence and Stress
#' distributions and the provenance diagnostics of every AMR screen in the
#' database.

box::use(
  DT[DTOutput, renderDT],
  bslib[as_fill_carrier, as_fill_item, as_fillable_container, nav_panel, navset_card_tab],
  htmltools[htmlEscape],
  shiny,
  shinyWidgets[pickerInput, pickerOptions, radioGroupButtons, updatePickerInput],
  stats[median, setNames],
  utils[write.csv],
)

box::use(
  app /
    logic /
    amr_overview[
      ELEMENT_TYPES,
      SCREEN_LABELS,
      element_hits,
      load_amr_overview,
    ],
  app / logic / database_functions[load_db_species],
  app / logic / db_events,
  app / logic / db_guard[guard_db_read],
  app / logic / functions[render_info, stat_tile],
  app / logic / overview_ui,
)

# DataTables shown with a loading overlay until their first draw.
TABLE_IDS <- c("amr_elements", "amr_profiles", "amr_isolates")

# Bars shown before the remainder is folded into one "other" row.
TOP_GROUPS <- 12L

# Tone of each call-quality tier, best first.
TIER_TONES <- c(
  Perfect = "success",
  Strong = "primary",
  Partial = "warning",
  Putative = "secondary"
)

# What a type's groups are called on screen.
GROUP_TITLES <- c(
  Resistance = "Drug Classes",
  Virulence = "Virulence Factors",
  Stress = "Stress Groups"
)

fmt <- overview_ui$fmt

# "Yes"/"No" for a logical, a dash where unknown.
yes_no <- function(x) ifelse(is.na(x), "-", ifelse(x, "Yes", "No"))

# Why there are no screening results to show, as one HTML sentence.
empty_reason <- function(ov) {
  if (identical(ov$state, "skipped")) {
    return(sprintf(
      "AMR screening was switched off for all %s isolate(s) when they were typed.",
      fmt(sum(ov$isolates$status == "skipped"))
    ))
  }
  paste(
    "No AMR screening results in this database. Isolates typed in PhyloTrace are",
    "screened with abritamr (AMRFinderPlus) unless screening is switched off in",
    "<strong>Add Isolates</strong>."
  )
}

# Renders the UI layout for the Elements tab: the element table next to the
# isolate picker, the call-quality chart and the selected call's details.
elements_ui <- function(ns) {
  as_fill_carrier(shiny$div(
    class = "loci-info-layout",
    as_fill_carrier(shiny$div(
      class = "loci-info-main",
      overview_ui$table_card(
        ns,
        "amr_elements",
        "Elements",
        "export_elements",
        "Loading elements ..."
      )
    )),
    as_fillable_container(shiny$div(
      class = "loci-controls",
      pickerInput(
        ns("hit_select"),
        label = "Select element from table",
        choices = character(0),
        options = pickerOptions(
          liveSearch = TRUE,
          size = 10,
          liveSearchPlaceholder = "Search isolates ...",
          container = "body"
        )
      ),
      shiny$uiOutput(ns("element_bars")),
      as_fill_carrier(shiny$div(
        class = "loci-allele-panel",
        as_fill_item(shiny$div(class = "overview-detail", shiny$uiOutput(ns("hit_detail"))))
      ))
    ))
  ))
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  navset_card_tab(
    nav_panel(
      "Screening Info",
      overview_ui$split_layout(
        overview_ui$fill_card(
          "Screening Metadata",
          DTOutput(ns("screening_table"), fill = TRUE)
        ),
        overview_ui$fill_card("Screening Summary", shiny$uiOutput(ns("screening_summary")))
      )
    ),
    nav_panel("Elements", elements_ui(ns)),
    nav_panel(
      "Distribution",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "amr_profiles",
          "Profiles",
          "export_profiles",
          "Loading profiles ...",
          controls = radioGroupButtons(
            ns("dist_type"),
            label = NULL,
            choices = unname(ELEMENT_TYPES),
            selected = ELEMENT_TYPES[["AMR"]],
            size = "sm"
          )
        ),
        list(
          overview_ui$fill_card("Element Groups", shiny$uiOutput(ns("group_bars"))),
          overview_ui$fill_card("Elements per Isolate", shiny$uiOutput(ns("count_bars")))
        )
      )
    ),
    nav_panel(
      "Diagnostics",
      overview_ui$split_layout(
        overview_ui$table_card(
          ns,
          "amr_isolates",
          "Isolate Screening",
          "export_isolates",
          "Loading isolate screening ..."
        ),
        list(
          overview_ui$fill_card("Checks", shiny$uiOutput(ns("checks"))),
          overview_ui$fill_card("Screening Settings", shiny$uiOutput(ns("settings")))
        )
      )
    )
  )
}

# Key/value rows describing the screening setup, values as HTML.
screening_rows <- function(ov, species) {
  screened <- ov$isolates[ov$isolates$screened, , drop = FALSE]
  esc <- function(x) if (is.null(x) || !length(x) || is.na(x)) NA_character_ else htmlEscape(x)
  one_or_mixed <- function(x) {
    x <- unique(x[!is.na(x)])
    if (!length(x)) {
      NA_character_
    } else if (length(x) == 1) {
      esc(x)
    } else {
      "Mixed - see <strong>Diagnostics</strong>"
    }
  }
  pm <- screened$point_mutations[!is.na(screened$point_mutations)]
  organism <- unique(screened$organism[!is.na(screened$organism)])
  organism_row <- if (length(organism) > 1) {
    "Mixed - see <strong>Diagnostics</strong>"
  } else if (length(organism) == 1) {
    sprintf(
      "%s (%s)",
      esc(gsub("_", " ", organism)),
      if (all(pm)) "point mutations screened" else "point mutations partly screened"
    )
  } else if (length(pm)) {
    "None - acquired genes only, no point mutations"
  } else {
    NA_character_
  }
  hits <- ov$hits
  found <- vapply(
    unname(ELEMENT_TYPES),
    function(type) {
      sub <- sort(unique(hits$subtype[hits$type == type]))
      if (!length(sub)) {
        return(NA_character_)
      }
      sprintf("%s (%s)", type, tolower(paste(sub, collapse = ", ")))
    },
    character(1)
  )
  found <- found[!is.na(found)]
  dates <- sort(substr(screened$screened_at[!is.na(screened$screened_at)], 1, 10))

  c(
    "Screening Tools" = one_or_mixed(ifelse(
      is.na(screened$amrfinder),
      NA_character_,
      sprintf(
        "abritamr %s, AMRFinderPlus %s",
        ifelse(is.na(screened$abritamr), "-", screened$abritamr),
        screened$amrfinder
      )
    )),
    "AMRFinderPlus Database" = one_or_mixed(screened$db_version),
    "Organism" = organism_row,
    "Species" = esc(species),
    "Elements Found" = if (length(found)) esc(paste(found, collapse = "; ")) else NA_character_,
    "Distinct Elements" = fmt(nrow(ov$elements)),
    "Screened" = sprintf("%s of %s isolates", fmt(nrow(screened)), fmt(nrow(ov$isolates))),
    "Screening Dates" = if (length(dates)) {
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

# Handles server logic for the screening metadata and summary.
screening_info_server <- function(input, output, overview, species, reason) {
  output$screening_table <- renderDT({
    render_info("output$screening_table")
    ov <- overview()
    if (!identical(ov$state, "ok")) {
      return(overview_ui$message_table(reason()))
    }
    rows <- screening_rows(ov, species())
    overview_ui$key_value_table(names(rows), unname(rows))
  })

  # Headline figures, how the screens came out and which element types the
  # screened isolates carry.
  output$screening_summary <- shiny$renderUI({
    render_info("output$screening_summary")
    ov <- overview()
    iso <- ov$isolates
    if (!nrow(iso)) {
      return(overview_ui$empty_hint("This database holds no isolates yet."))
    }
    screened <- iso[iso$screened, , drop = FALSE]
    n <- nrow(screened)
    med_classes <- if (n) median(screened$classes) else NA
    types <- c(
      "Acquired genes" = sum(screened$acquired > 0),
      "Point mutations" = sum(screened$point > 0),
      "Virulence factors" = sum(screened$virulence > 0),
      "Stress elements" = sum(screened$stress > 0)
    )

    shiny$tagList(
      overview_ui$stat_grid(
        stat_tile("Screened", sprintf("%s / %s", fmt(n), fmt(nrow(iso))), "vial"),
        stat_tile(
          "With Resistance",
          if (n) sprintf("%.1f%%", 100 * sum(screened$resistance > 0) / n) else "-",
          "shield-virus"
        ),
        stat_tile(
          "Median Drug Classes",
          if (is.na(med_classes)) "-" else format(med_classes),
          "pills"
        ),
        stat_tile("Distinct Elements", fmt(nrow(ov$elements)), "dna")
      ),
      overview_ui$section_title("Screening Status"),
      overview_ui$bar_rows(ov$status$label, ov$status$n, nrow(iso), tones = ov$status$tone),
      if (n) {
        shiny$tagList(
          shiny$div(class = "overview-section-gap"),
          overview_ui$section_title("Element Types"),
          overview_ui$bar_rows(names(types), unname(types), n),
          overview_ui$overview_note(
            "Screened isolates carrying at least one element of each type;",
            "acquired genes and point mutations are both resistance. Drug classes",
            "count every reported determinant, intrinsic ones included."
          )
        )
      }
    )
  })
}

# Handles server logic for the element table, call quality and hit details.
elements_server <- function(input, output, session, overview, reason) {
  output$amr_elements <- renderDT({
    render_info("output$amr_elements")
    ov <- overview()
    if (!nrow(ov$elements)) {
      return(overview_ui$message_table(if (identical(ov$state, "ok")) {
        "No element was found in any screened isolate."
      } else {
        reason()
      }))
    }
    overview_ui$data_table(ov$elements, selection = list(mode = "single", selected = 1))
  })

  # The selected element row.
  selected <- shiny$reactive({
    el <- overview()$elements
    row <- input$amr_elements_rows_selected
    shiny$req(nrow(el) > 0, length(row) == 1)
    el[row, , drop = FALSE]
  })

  # Every call of the selected element, best first.
  calls <- shiny$reactive({
    el <- selected()
    element_hits(overview()$hits, el$Element, el$Type)
  })

  # Offer the isolates carrying the selected element, best call first.
  shiny$observeEvent(calls(), {
    h <- calls()
    shiny$req(nrow(h) > 0)
    ids <- as.character(seq_len(nrow(h)))
    labels <- sprintf(
      "%s - %s (%s%% identity)",
      h$isolate,
      h$tier,
      format(round(h$pct_identity, 1), nsmall = 1)
    )
    updatePickerInput(
      session,
      "hit_select",
      label = sprintf(
        "Selected Element: %s (%s isolates)",
        selected()$Element,
        fmt(selected()$Isolates)
      ),
      choices = setNames(ids, labels),
      selected = ids[1]
    )
  })

  # Isolates carrying the selected element by their best call of it.
  output$element_bars <- shiny$renderUI({
    el <- selected()
    render_info("output$element_bars")
    tiers <- names(TIER_TONES)
    counts <- vapply(tiers, function(t) el[[t]], integer(1))
    shiny$div(
      class = "overview-allele-bars",
      overview_ui$section_title("Call Quality"),
      overview_ui$bar_rows(tiers, counts, el$Isolates, tones = unname(TIER_TONES))
    )
  })

  # The selected call: where it sits and how well it matches its reference.
  output$hit_detail <- shiny$renderUI({
    h <- calls()
    i <- suppressWarnings(as.integer(input$hit_select))
    shiny$req(nrow(h) > 0, length(i) == 1, !is.na(i), i <= nrow(h))
    render_info("output$hit_detail")
    hit <- h[i, , drop = FALSE]
    num <- function(x) if (is.na(x)) "-" else paste0(format(round(x, 2), nsmall = 2), "%")
    rows <- c(
      Isolate = hit$isolate,
      Element = hit$gene_symbol,
      Name = hit$sequence_name,
      Type = paste(hit$type, "-", hit$subtype),
      Class = if (is.na(hit$subclass) || identical(hit$subclass, hit$class)) {
        hit$class
      } else {
        paste(hit$class, "/", hit$subclass)
      },
      Call = sprintf("%s (%s)", hit$tier, hit$method),
      Identity = num(hit$pct_identity),
      Coverage = num(hit$pct_coverage),
      Contig = hit$contig,
      Position = if (is.na(hit$start)) {
        NA
      } else {
        sprintf("%s-%s (%s)", hit$start, hit$stop, hit$strand)
      },
      Reference = if (is.na(hit$ref_accession)) {
        NA
      } else {
        paste(hit$ref_accession, hit$ref_name, sep = " - ")
      }
    )
    rows <- rows[!is.na(rows) & nzchar(rows)]
    shiny$tags$dl(
      lapply(names(rows), function(k) {
        shiny$tagList(shiny$tags$dt(k), shiny$tags$dd(rows[[k]]))
      })
    )
  })

  output$export_elements <- shiny$downloadHandler(
    filename = function() "amr_elements.csv",
    content = function(file) {
      el <- overview()$elements
      shiny$req(nrow(el) > 0)
      write.csv(el, file, row.names = FALSE)
    }
  )
}

# Handles server logic for the per-type profiles, groups and counts.
distribution_server <- function(input, output, overview, reason) {
  type <- shiny$reactive(if (is.null(input$dist_type)) ELEMENT_TYPES[["AMR"]] else input$dist_type)

  output$amr_profiles <- renderDT({
    render_info("output$amr_profiles")
    ov <- overview()
    profiles <- ov$profiles[[type()]]
    if (!identical(ov$state, "ok") || is.null(profiles) || !nrow(profiles)) {
      return(overview_ui$message_table(reason()))
    }
    if (type() != ELEMENT_TYPES[["AMR"]]) {
      profiles$Classes <- NULL
    }
    overview_ui$data_table(profiles)
  })

  # Isolates per group of the selected type, the rest folded into one row.
  output$group_bars <- shiny$renderUI({
    render_info("output$group_bars")
    ov <- overview()
    groups <- ov$groups[[type()]]
    n <- sum(ov$isolates$screened)
    if (!n || is.null(groups) || !nrow(groups)) {
      return(overview_ui$empty_hint(sprintf("No %s elements found.", tolower(type()))))
    }
    bars <- overview_ui$fold_top(groups$group, groups$isolates, TOP_GROUPS, "%d other groups")
    shiny$tagList(
      overview_ui$section_title(GROUP_TITLES[[type()]]),
      overview_ui$bar_rows(bars$labels, bars$counts, n, tones = bars$tones),
      overview_ui$overview_note(
        "Screened isolates carrying at least one element of each group; an isolate",
        "can count towards several."
      )
    )
  })

  # Screened isolates by how many distinct elements of the type they carry.
  output$count_bars <- shiny$renderUI({
    render_info("output$count_bars")
    ov <- overview()
    counts <- ov$counts[[type()]]
    n <- sum(ov$isolates$screened)
    if (!n) {
      return(overview_ui$empty_hint(reason()))
    }
    shiny$tagList(
      overview_ui$bar_rows(counts$label, counts$n, n),
      overview_ui$overview_note(
        sprintf("Distinct %s elements per screened isolate.", tolower(type()))
      )
    )
  })

  output$export_profiles <- shiny$downloadHandler(
    filename = function() sprintf("amr_%s_profiles.csv", tolower(type())),
    content = function(file) {
      profiles <- overview()$profiles[[type()]]
      shiny$req(nrow(profiles) > 0)
      write.csv(profiles, file, row.names = FALSE)
    }
  )
}

# Per-isolate screening table, flagged rows first.
isolate_rows <- function(iso) {
  flags <- character(nrow(iso))
  add_flag <- function(hit, text) {
    flags[hit] <<- ifelse(nzchar(flags[hit]), paste(flags[hit], text, sep = "; "), text)
  }
  pm <- iso$point_mutations[iso$screened & !is.na(iso$point_mutations)]
  add_flag(iso$lost, "results missing")
  add_flag(iso$status %in% c("failed", "incomplete"), "screen failed")
  add_flag(iso$other_db, "other database")
  add_flag(
    any(pm) & iso$screened & iso$point_mutations %in% FALSE,
    "no point mutations"
  )
  add_flag(iso$partial > 0, "partial elements")

  out <- data.frame(
    Isolate = iso$isolate,
    Flags = flags,
    Status = unname(SCREEN_LABELS[iso$status]),
    `Acquired Genes` = iso$acquired,
    `Point Mutations` = iso$point,
    Virulence = iso$virulence,
    Stress = iso$stress,
    `Drug Classes` = iso$classes,
    Partial = iso$partial,
    Organism = gsub("_", " ", iso$organism),
    `Mutations Screened` = yes_no(iso$point_mutations),
    abritamr = iso$abritamr,
    AMRFinderPlus = iso$amrfinder,
    Database = iso$db_version,
    `Screened At` = substr(iso$screened_at, 1, 19),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  unscreened <- !iso$screened
  counted <- c(
    "Acquired Genes", "Point Mutations", "Virulence", "Stress", "Drug Classes", "Partial"
  )
  for (col in counted) {
    out[[col]][unscreened] <- NA
  }
  out[order(!nzchar(flags), out$Isolate), , drop = FALSE]
}

# One entry per combination of settings isolates were screened with.
settings_list <- function(settings) {
  shiny$div(
    class = "overview-groups",
    lapply(seq_len(nrow(settings)), function(i) {
      db <- settings$db_version[i]
      shiny$div(
        class = "overview-group",
        shiny$div(
          class = "overview-group_head",
          shiny$span(if (is.na(db)) "Database not recorded" else paste("AMRFinderPlus DB", db)),
          shiny$div(
            class = "overview-group_badges",
            if (i == 1L && nrow(settings) > 1) {
              shiny$span(class = "overview-badge is-primary", "Most used")
            }
          )
        ),
        shiny$div(
          class = "overview-group_meta",
          shiny$span(
            shiny$icon("bacterium"),
            if (is.na(settings$organism[i])) {
              "No organism (acquired genes only)"
            } else {
              sprintf(
                "%s, point mutations %s",
                gsub("_", " ", settings$organism[i]),
                if (isTRUE(settings$point_mutations[i])) "on" else "off"
              )
            }
          ),
          if (!is.na(settings$tools[i])) {
            shiny$span(shiny$icon("toolbox"), settings$tools[i])
          },
          shiny$span(shiny$icon("vial"), sprintf("%s isolates", fmt(settings$isolates[i])))
        )
      )
    })
  )
}

# Handles server logic for the checks, screening settings and isolate table.
diagnostics_server <- function(input, output, overview) {
  isolates <- shiny$reactive(isolate_rows(overview()$isolates))

  output$amr_isolates <- renderDT({
    render_info("output$amr_isolates")
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
      return(overview_ui$empty_hint("Nothing to check yet - no isolate has been screened."))
    }
    overview_ui$checks_ui(checks)
  })

  output$settings <- shiny$renderUI({
    render_info("output$settings")
    settings <- overview()$settings
    if (!nrow(settings)) {
      return(overview_ui$empty_hint("No isolate has been screened yet."))
    }
    settings_list(settings)
  })

  output$export_isolates <- shiny$downloadHandler(
    filename = function() "amr_isolate_screening.csv",
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
    # Every AMR screening record of the database, summarised once per change.
    # `amr` because a typing run's screens land there, `isolates` because
    # removal and merges add or drop them, `schema` because a restore can
    # replace them wholesale.
    overview <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "amr", "isolates", "schema")
      guard_db_read("Loading the AMR overview", load_amr_overview(db_path()))
    })

    # Species of the database's scheme, shown with the screening setup.
    species <- shiny$reactive({
      shiny$req(db_path())
      db_events$depend(db_rev, "schema")
      guard_db_read("Loading the scheme", load_db_species(db_path()))
    })

    # Put the loading overlays back while a newly opened database's tables draw.
    shiny$observeEvent(db_path(), overview_ui$reset_table_overlays(session$ns, TABLE_IDS))

    # Why there are no screening results to show, shared by every tab.
    reason <- shiny$reactive(empty_reason(overview()))

    screening_info_server(input, output, overview, species, reason)
    elements_server(input, output, session, overview, reason)
    distribution_server(input, output, overview, reason)
    diagnostics_server(input, output, overview)
  })
}
