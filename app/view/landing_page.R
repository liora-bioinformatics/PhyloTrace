# app/view/landing_page.R
#
# Start screen: introduces PhyloTrace and lets the user open the last used or a
# picked database, or start creating a new one.

box::use(
  htmltools[htmlEscape],
  jsonlite[fromJSON],
  later[later],
  rlang[`%||%`],
  shiny,
  shinyjs[disabled, toggleState, useShinyjs],
)

box::use(
  app / logic / db_compat[check_db_loadable, describe_db],
  app / logic / functions[render_info],
  app / logic / paths[app_local_share_path],
  app / logic / species[GENERIC_IMAGE, image_credit, species_image],
  app / view / about,
  app / view / file_dialog,
)

# Labels of the database the card shows: the one remembered from an earlier
# session, or one the user just picked (or created).
LABEL_LAST_USED <- "Last used"
LABEL_SELECTED <- "Selected"

# Human-readable file size in decimal units.
format_size <- function(bytes) {
  if (is.na(bytes)) {
    return(NA_character_)
  }
  units <- c("Bytes", "KB", "MB", "GB", "TB")
  power <- min(max(floor(log(max(bytes, 1), 1000)), 0), length(units) - 1)
  if (power == 0) {
    return(paste(bytes, "Bytes"))
  }
  paste(format(round(bytes / 1000^power, 2), nsmall = 0), units[power + 1])
}

# Elides the middle of a long path, keeping its start and its end readable.
truncate_middle <- function(text, max_chars = 64L) {
  if (is.na(text) || nchar(text) <= max_chars) {
    return(text)
  }
  keep <- max_chars - 1L
  paste0(
    substr(text, 1L, ceiling(keep / 2)),
    "…",
    substr(text, nchar(text) - floor(keep / 2) + 1L, nchar(text))
  )
}

# One label/value pair of the card's fact list; omitted when the value is unknown.
db_fact <- function(label, value) {
  if (is.null(value) || is.na(value) || !nzchar(value)) {
    return(NULL)
  }
  shiny$div(
    class = "landing-db_fact",
    shiny$tags$dt(label),
    shiny$tags$dd(value)
  )
}

# Text with a line-break opportunity after each separator character in `chars`,
# so a long name without spaces wraps at "_" or "/" rather than mid-word.
breakable <- function(text, chars) {
  parts <- strsplit(text, sprintf("(?<=[%s])", chars), perl = TRUE)[[1]]
  shiny$HTML(paste(htmlEscape(parts), collapse = "<wbr>"))
}

# The folder a database lives in, shortened for display and complete on hover.
db_folder <- function(path) {
  folder <- dirname(path)
  shiny$div(
    class = "landing-db_path",
    title = folder,
    shiny$icon("folder"),
    shiny$span(breakable(truncate_middle(folder, 80L), "/\\\\"))
  )
}

# Card content for a database that can be loaded.
db_entry <- function(label, path) {
  meta <- file.info(path)
  info <- describe_db(path)
  image <- species_image(info$species)

  shiny$div(
    class = "landing-db_entry",
    shiny$div(class = "landing-db_kicker", label),
    shiny$div(
      class = "landing-db_title",
      if (!is.na(image)) {
        credit <- image_credit(info$species)
        # Too small for a caption: the credit is its tooltip, and the About
        # dialog lists every picture's credit.
        shiny$tags$img(
          class = "landing-db_thumb",
          src = image,
          alt = "",
          title = if (identical(image, GENERIC_IMAGE)) {
            "Generic illustration, no picture of this species"
          } else if (!is.null(credit)) {
            sprintf("Image: %s · %s", credit$author, credit$license)
          }
        )
      } else {
        shiny$div(
          class = "landing-db_thumb landing-db_thumb--icon",
          shiny$icon("database")
        )
      },
      shiny$div(
        class = "landing-db_heading",
        shiny$div(
          class = "landing-db_name",
          title = basename(path),
          breakable(basename(path), "_-")
        ),
        if (!is.na(info$species)) {
          shiny$div(class = "landing-db_species", info$species)
        }
      )
    ),
    db_folder(path),
    shiny$tags$dl(
      class = "landing-db_facts",
      db_fact("Scheme", info$scheme),
      db_fact(
        "Isolates",
        if (!is.na(info$isolates)) format(info$isolates, big.mark = ",") else NA
      ),
      db_fact("Size", format_size(meta$size)),
      db_fact("Last changed", format(meta$mtime, "%Y-%m-%d %H:%M"))
    )
  )
}

# Card content for a picked or remembered file that cannot be loaded.
db_entry_invalid <- function(label, path, reason) {
  shiny$div(
    class = "landing-db_entry",
    shiny$div(class = "landing-db_kicker", label),
    if (length(path) && !is.na(path)) {
      shiny$tagList(
        shiny$div(
          class = "landing-db_title",
          shiny$div(
            class = "landing-db_thumb landing-db_thumb--icon landing-db_thumb--warning",
            shiny$icon("database")
          ),
          shiny$div(
            class = "landing-db_heading",
            shiny$div(
              class = "landing-db_name",
              title = basename(path),
              breakable(basename(path), "_-")
            )
          )
        ),
        db_folder(path)
      )
    },
    shiny$div(
      class = "landing-db_alert",
      role = "alert",
      shiny$icon("triangle-exclamation"),
      shiny$span(reason %||% "This file cannot be loaded.")
    )
  )
}

# Card content when no database is remembered or picked.
db_entry_empty <- function() {
  shiny$div(
    class = "landing-db_entry landing-db_entry--empty",
    shiny$div(class = "landing-db_empty-icon", shiny$icon("database")),
    shiny$div(class = "landing-db_empty-title", "No database selected"),
    shiny$p(
      class = "landing-db_hint",
      "Open an existing PhyloTrace database (.db file) or create a new one for your ",
      "species and typing scheme."
    )
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$div(
    class = "landing html-fill-item",
    useShinyjs(),
    shiny$div(class = "landing_backdrop", `aria-hidden` = "true"),
    shiny$div(
      class = "landing_main",
      shiny$tags$section(
        class = "landing_hero",
        shiny$tags$img(
          class = "landing_logo",
          src = "static/images/PhyloTrace_flat_256.png",
          alt = "",
          width = 256,
          height = 256
        ),
        shiny$div(
          class = "landing_intro",
          shiny$tags$h1(class = "landing_title", "PhyloTrace"),
          shiny$p(
            class = "landing_tagline",
            "Decentralised pathogen surveillance — under your control."
          ),
          shiny$p(
            class = "landing_lead",
            "Open a database to continue where you left off, or set up a new one for ",
            "your species and typing scheme."
          ),
          shiny$div(
            class = "landing_actions",
            file_dialog$ui(ns("db_pick"), "Open Database", "folder-open"),
            shiny$actionButton(
              ns("create_new_db"),
              "Create New",
              icon = shiny$icon("plus")
            )
          )
        )
      ),
      shiny$tags$section(
        class = "landing-db pt-delayed-busy",
        `aria-label` = "Database",
        shiny$uiOutput(ns("database_selection"), class = "landing-db_body"),
        shiny$div(
          class = "landing-db_busy",
          shiny$div(class = "spinner-border", role = "status"),
          shiny$span(class = "visually-hidden", "Reading database")
        ),
        disabled(shiny$actionButton(
          ns("load_database"),
          shiny$tagList("Load Database", shiny$icon("arrow-right")),
          class = "btn-primary landing-db_load"
        ))
      )
    ),
    shiny$tags$footer(
      class = "landing_footer",
      shiny$tags$a(
        class = "landing_maker",
        href = "https://liora-bioinformatics.com/phylotrace",
        target = "_blank",
        rel = "noopener noreferrer",
        about$brand_logo()
      ),
      shiny$span(
        class = "landing_legal",
        "Open source under GPLv3 · For research use only. Not a medical device."
      ),
      about$ui(ns("about"), variant = "link")
    )
  )
}

#' @export
server <- function(
  id,
  external_db = shiny$reactive(NULL)
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    about$server("about")

    # Currently selected database location, as c(label, path) or c(label, NA).
    db_location <- shiny$reactiveVal(NULL)

    # Why the currently selected file cannot be loaded (missing, not SQLite, not
    # a PhyloTrace database), or NULL when the selection is fine / absent. Shown
    # in place of the database summary so the user learns why Load is disabled.
    db_invalid_reason <- shiny$reactiveVal(NULL)

    # The file last offered for selection, kept even when it is not loadable
    # (db_location() then holds NA) so the card can still name it.
    db_candidate <- shiny$reactiveVal(NULL)

    # Resolve a candidate path to the c(label, path) pair db_location() expects,
    # applying the same validity check the app runs before opening a database. A
    # missing file or a file that is not a PhyloTrace database resolves to
    # c(label, NA) - which disables Load - and records the reason. This is the
    # single gate that keeps db_path() from ever turning non-NULL for a file
    # every downstream module would then query and crash on.
    resolve_selection <- function(label, path) {
      db_candidate(path)
      chk <- check_db_loadable(path)
      if (isTRUE(chk$ok)) {
        db_invalid_reason(NULL)
        c(label, path)
      } else {
        db_invalid_reason(chk$reason)
        c(label, NA)
      }
    }

    # Bumped synchronously, in the same flush as the click (or the external-db
    # trigger), so main.R can show the loading overlay before load_trigger
    # below changes - db_path() cascades from that into every downstream
    # module's eager (suspendWhenHidden = FALSE) reads, some of which are slow
    # on a large database. Showing the overlay off this earlier signal means
    # it's already on screen once that heavier work starts, instead of the UI
    # sitting merely unresponsive until the whole cascade finishes.
    loading_started <- shiny$reactiveVal(0L)

    # Combined load trigger: incremented by the UI button and by the external
    # db observer so that both paths share one downstream observeEvent.
    load_trigger <- shiny$reactiveVal(NULL)

    # Starts a load: overlay signal now, the actual trigger one tick later.
    fire_load <- function() {
      loading_started(loading_started() + 1L)

      # Deferred one tick so the loading_started bump above reaches the
      # client - and the overlay it triggers actually paints - before
      # load_trigger changes db_path() and everything that cascades from it.
      later(function() {
        shiny$withReactiveDomain(
          session,
          shiny$isolate({
            n <- load_trigger()
            load_trigger(if (is.null(n)) 1L else n + 1L)
          })
        )
      })
    }

    # Loads a database handed over from another tab (the setup's "Load Database").
    shiny$observeEvent(external_db(), {
      db_path <- external_db()
      shiny$req(!is.null(db_path), length(db_path), is.character(db_path))

      loc <- resolve_selection(LABEL_SELECTED, db_path)
      db_location(loc)

      # The request comes from another tab, where the reason recorded on this
      # one would never be seen.
      if (!is.na(loc[2])) {
        fire_load()
      } else {
        shiny$showNotification(
          paste(
            "The database cannot be loaded:",
            db_invalid_reason() %||% basename(db_path)
          ),
          type = "error",
          duration = 10
        )
      }
    })

    # Loads the database shown on the card.
    shiny$observeEvent(input$load_database, {
      loc <- db_location()
      path <- if (is.null(loc)) NA_character_ else loc[2]

      # The button is only enabled for a resolved path; a NA here means a stale
      # click on a disabled control - ignore it, the reason is already shown.
      if (is.na(path)) {
        return()
      }

      # Re-check at click time: the file can be deleted or replaced between the
      # selection rendering and this click. A newly-bad path re-resolves the
      # selection (disabling Load and showing why) instead of firing the load.
      if (!isTRUE(check_db_loadable(path)$ok)) {
        db_location(resolve_selection(loc[1], path))
        return()
      }

      fire_load()
    })

    # The database the user picks in the file dialog.
    picked_db <- file_dialog$server(
      "db_pick",
      mode = "file",
      title = "Choose a database",
      pattern = "\\.db$"
    )

    # Selects the picked database, or else the one remembered in state.json.
    # Falls back to disk (not in-memory) so it picks up whatever main.R wrote
    # in an earlier session.
    shiny$observe({
      loc <- NULL

      location_input <- picked_db()

      if (length(location_input)) {
        loc <- resolve_selection(LABEL_SELECTED, location_input)
      } else {
        state_file <- file.path(app_local_share_path, "state.json")
        disk_stat <- tryCatch(
          if (file.exists(state_file)) fromJSON(state_file) else NULL,
          error = function(e) NULL
        )
        if (
          !is.null(disk_stat) &&
            length(disk_stat$last_db) &&
            file.exists(disk_stat$last_db) &&
            endsWith(disk_stat$last_db, ".db")
        ) {
          # The remembered database is still on disk - but "on disk" is not
          # "loadable": it may have been emptied or replaced. Validate before
          # offering it as "Last used".
          loc <- resolve_selection(LABEL_LAST_USED, disk_stat$last_db)
        } else {
          db_invalid_reason(NULL)
          db_candidate(NULL)
        }
      }

      db_location(loc)
    })

    # Load is only offered for a database that passed the validity check.
    shiny$observe({
      loc <- db_location()
      toggleState("load_database", condition = !is.null(loc) && !is.na(loc[2]))
    })

    # Database card: summary of the selected database, why it cannot be loaded,
    # or an empty state. Plain HTML in one message, so an update swaps in place
    # instead of re-initialising a table widget.
    output$database_selection <- shiny$renderUI({
      render_info("output$database_selection")

      loc <- db_location()
      if (is.null(loc)) {
        return(db_entry_empty())
      }
      if (is.na(loc[2])) {
        return(db_entry_invalid(loc[1], db_candidate(), db_invalid_reason()))
      }
      db_entry(loc[1], loc[2])
    })

    # Return values
    #
    # `db_path` is the *loaded* database, not merely the selected or remembered
    # one. `db_location()` is populated at startup from state.json so the landing
    # page can offer "Last used: …", and it moves as soon as the user picks a
    # file - but neither of those means the database is open. Gating on
    # `load_trigger()` (NULL until Load is pressed) keeps every downstream
    # module from querying - or migrating - a file the user has not asked for
    # yet. Without the gate the Database Browser and
    # Custom Variables panels read the remembered database on the first reactive
    # flush, i.e. before main.R has run its on-load migrations.
    list(
      create_scheme = shiny$reactive(input$create_new_db),
      loading_started = shiny$reactive(loading_started()),
      load_database = shiny$reactive(load_trigger()),
      db_path = shiny$reactive(
        if (is.null(load_trigger())) NULL else db_location()[2]
      )
    )
  })
}
