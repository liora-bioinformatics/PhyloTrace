# path/to/app/view/database_import.R
#
# "Import" interface of the Database menu.

# Merges a peer `.db` into the loaded one. The gate in app/logic/db_compat.R
# must pass before the Merge button unlocks; isolate name collisions are
# resolved per-isolate; the peer's metadata columns are adopted only where the
# user says so. The live database is never mutated in place — the merge runs
# against a copy that is swapped in atomically, leaving a `.bak-*` rollback
# point behind (see app/logic/db_import.R).

box::use(
  bslib[as_fill_carrier, layout_sidebar, sidebar, tooltip],
  fs[path_home],
  openxlsx[getSheetNames],
  shiny[
    actionButton,
    checkboxInput,
    div,
    h5,
    icon,
    modalButton,
    modalDialog,
    moduleServer,
    NS,
    observe,
    observeEvent,
    reactive,
    reactiveVal,
    removeModal,
    renderUI,
    req,
    showModal,
    showNotification,
    span,
    strong,
    tagList,
    tags,
    textInput,
    uiOutput,
    withProgress
  ],
  shinyFiles[parseFilePaths, shinyFileChoose, shinyFilesButton],
  shinyjs[disable, disabled, enable, hidden, runjs, toggle],
  shinyWidgets[pickerInput, pickerOptions],
  stats[setNames],
  utils[head],
  waiter[spin_flower, useWaiter, Waiter],
)

box::use(
  app / logic / custom_fields[CUSTOM_TYPES],
  app / logic / database_functions[metadata_columns],
  app / logic / db_compat[check_import_compatibility],
  app / logic / db_events,
  app / logic / db_export[available_result_tables],
  app /
    logic /
    db_import[
      classify_isolate_collisions,
      default_resolutions,
      import_preview,
      importable_custom_fields,
      list_backups,
      merge_databases,
      prepare_source,
      release_source,
      restore_backup,
      suggest_rename,
      METADATA_RESERVED
    ],
  app /
    logic /
    db_staging[
      delete_imported_set,
      list_imported_sets,
      resolve_profile,
      stage_profile_set,
      taken_isolate_names
    ],
  app / logic / field_labels[field_labels_for],
  app / logic / functions[panel_card, stat_tile, transfer_cards],
  app /
    logic /
    profile_io[
      parse_fasta_headers,
      parse_profile_file,
      read_fasta,
      read_table_any,
      scheme_loci
    ],
  app / logic / pymlst[existing_strains],
)

# Returns default value b if a is NULL
`%||%` <- function(a, b) if (is.null(a)) b else a

# A `.db` carries real sequences and can be merged into the typing database. A
# profile table carries only allele identifiers, so it is staged alongside and
# used for distances (Tree / MST) — never merged in. See db_staging.R.
SOURCE_TYPES <- c(
  "PhyloTrace database (.db)" = "database",
  "Typing results (table)" = "typing"
)

# Helper function to define root directories for file choosers
.roots <- function() c(Home = path_home(), Root = "/")

# Clean leading slashes from file paths outputted by shinyFiles
.clean_path <- function(p) sub("^/{2,}", "/", as.character(p))

.ACTION_CHOICES <- c(
  "Skip" = "skip",
  "Overwrite local" = "overwrite",
  "Import under new name" = "rename"
)

# Helper function to render status badges
.badge <- function(status) {
  span(class = paste0("compat-badge is-", status), status)
}

# Renders HTML template for reading database waiter screen
import_waiter_html <- function() {
  div(
    class = "db-waiter",
    spin_flower(),
    div(class = "db-waiter_text", "Reading database"),
    div(class = "db-waiter_hint", "Hashing alleles and comparing isolates …")
  )
}

# Renders conflict notice for custom variables that cannot be imported
conflict_note <- function(conflicts) {
  # Helper to fetch human-readable custom type label
  label <- function(type) {
    lab <- unname(CUSTOM_TYPES[type])
    if (is.na(lab)) type else lab
  }
  div(
    class = "dest-label is-empty",
    lapply(seq_len(nrow(conflicts)), function(i) {
      div(sprintf(
        "'%s' is %s here but %s in the external file — not imported.",
        conflicts$name[[i]],
        label(conflicts$local_type[[i]]),
        label(conflicts$ext_type[[i]])
      ))
    })
  )
}

# Suggests next available isolate name candidate with _ext suffix
suggest_rename_ext <- function(name, taken) {
  candidate <- paste0(name, "_ext")
  i <- 1L
  while (candidate %in% taken) {
    i <- i + 1L
    candidate <- paste0(name, "_ext", i)
  }
  candidate
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  as_fill_carrier(
    div(
      id = ns("module-container"),
      layout_sidebar(
        padding = 0,
        border = FALSE,
        sidebar = sidebar(
          id = ns("controls_sidebar"),
          position = "right",
          width = 350,
          open = TRUE,
          fillable = TRUE,
          as_fill_carrier(
            div(
              class = "io-control",
              div(
                class = "io-control-group",
                div(class = "control-group-label", "Source"),
                pickerInput(
                  ns("source_type"),
                  label = NULL,
                  choices = SOURCE_TYPES
                )
              ),
              # --- merge a whole PhyloTrace database ------------------------------
              div(
                id = ns("db_group"),
                class = "io-control-group",
                div(class = "control-group-label", "External database"),
                shinyFilesButton(
                  ns("ext_db"),
                  "Select database …",
                  title = "Choose a PhyloTrace database to import",
                  icon = icon("file-import"),
                  buttonType = "default",
                  multiple = FALSE,
                  root = path_home()
                ),
                uiOutput(ns("ext_db_label"), inline = TRUE)
              ),
              # --- stage a profile table ------------------------------------------
              hidden(div(
                id = ns("typing_group"),
                class = "io-control-group",
                div(class = "control-group-label", "Typing results"),
                shinyFilesButton(
                  ns("profile_file"),
                  "Profile table …",
                  title = "Choose a profile table (.tsv / .csv / .xlsx)",
                  icon = icon("table"),
                  buttonType = "default",
                  multiple = FALSE,
                  root = path_home()
                ),
                shinyFilesButton(
                  ns("seq_file"),
                  "Sequences …",
                  title = "Optional: the matching allele FASTA",
                  icon = icon("dna"),
                  buttonType = "default",
                  multiple = FALSE,
                  root = path_home()
                ),
                shinyFilesButton(
                  ns("meta_file"),
                  "Metadata …",
                  title = "Optional: a metadata table for these isolates",
                  icon = icon("table-list"),
                  buttonType = "default",
                  multiple = FALSE,
                  root = path_home()
                )
              )),
              hidden(div(
                id = ns("set_name_group"),
                class = "io-control-group",
                div(class = "control-group-label", "Set name"),
                textInput(
                  ns("set_name"),
                  label = NULL,
                  placeholder = "e.g. partner-lab-2026"
                )
              )),
              div(
                class = "io-control-group",
                div(class = "control-group-label", "Metadata fields"),
                uiOutput(ns("meta_picker_ui"))
              ),
              div(
                class = "io-control-group",
                div(class = "control-group-label", "Custom variables"),
                uiOutput(ns("custom_picker_ui"))
              ),
              div(
                class = "io-control-group",
                div(class = "control-group-label", "Analysis results"),
                uiOutput(ns("results_picker_ui"))
              ),
              div(
                class = "io-control-group",
                div(class = "control-group-label", "Import"),
                uiOutput(ns("action_button_ui"), inline = TRUE)
              ),
              div(
                id = ns("rollback_group"),
                class = "io-control-group",
                div(class = "control-group-label", "Rollback"),
                uiOutput(ns("restore_ui"))
              )
            )
          )
        ),
        as_fill_carrier(
          div(
            class = "db-page_body db-transfer-body",
            useWaiter(),
            uiOutput(ns("body"), fill = TRUE)
          )
        )
      )
    )
  )
}

#' @export
server <- function(
  id,
  db_path = reactive(NULL),
  session_reset = reactive(0L),
  db_rev = db_events$new_bus(),
  # Advances every time this panel's markup is (re)inserted into the page. The
  # Database panel is rebuilt on every database load while this server keeps
  # running, and DOM state applied from here does not survive that rebuild —
  # see main.R's ui_mounted.
  ui_mounted = reactive(0L)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Path chosen by the user, and the hashed staging copy derived from it.
    # `prep` is released whenever the selection changes so a staged temp file
    # never outlives its selection.
    selected <- reactiveVal(NULL)
    prep <- reactiveVal(NULL)
    # Bumped after a successful merge / restore so the local-side reactives
    # (backup list, metadata columns, compatibility) recompute.
    local_token <- reactiveVal(0L)
    imported <- reactiveVal(0L)

    # Updates source file selection and prepares staged database copy
    set_source <- function(path) {
      release_source(prep())
      prep(NULL)
      selected(path)
      if (!is.null(path)) {
        staged <- tryCatch(prepare_source(path), error = function(e) NULL)
        if (is.null(staged)) {
          showNotification(
            "Could not read that database.",
            type = "error",
            duration = 6
          )
        }
        prep(staged)
      }
    }

    # Drop the chosen external database: release its staged copy and put the
    # panel back into its "no source chosen" state.
    #
    # The client-side value has to go too. shinyFiles reports a selection with
    # Shiny.onInputChange, and Shiny drops an input update whose value equals
    # the last one it sent - so with the button still holding the old file,
    # re-choosing that same file would not register as a change and the panel
    # would sit empty. Setting the input to NULL makes the next selection
    # differ; the observer above ignores the NULL itself (ignoreNULL).
    clear_ext_db <- function() {
      set_source(NULL)
      runjs(sprintf(
        "if (window.Shiny) Shiny.setInputValue('%s', null);",
        ns("ext_db")
      ))
    }

    session$onSessionEnded(function() release_source(shiny::isolate(prep())))

    # Define spinner
    db_waiter <- Waiter$new(
      id = ns("module-container"),
      html = div(
        class = "spinner-custom",
        spin_flower(),
        h5("Reading import ...")
      )
    )

    shinyFileChoose(
      input,
      "ext_db",
      roots = .roots(),
      defaultRoot = "Home",
      filetypes = c("db"),
      session = session
    )

    # Handle selection of external database file
    observeEvent(input$ext_db, {
      path <- parseFilePaths(.roots(), input$ext_db)$datapath
      req(length(path), is.character(path), file.exists(path))

      path <- .clean_path(path)

      db_waiter$show() # Show spinner

      set_source(path)
    })

    # The chosen file, named next to the button that chose it — same pattern as
    # the Export panel's dest_label, so the source step reads as complete
    # without a trip to the summary below.
    output$ext_db_label <- renderUI({
      path <- selected()
      if (is.null(path)) {
        return(span(class = "dest-label is-empty", "No file chosen"))
      }
      tooltip(
        span(class = "dest-label", icon("file-import"), basename(path)),
        path
      )
    })

    # =======================================================================
    # Typing-results branch: stage a profile table
    # =======================================================================

    # Reactive flag indicating if typing source mode is active
    typing <- reactive(identical(input$source_type, "typing"))

    # Show only the controls the chosen source needs. shinyjs rather than
    # conditionalPanel: the Database panels are injected into a navset_hidden
    # after the database loads, and Shiny's conditional-panel binding does not
    # take there — the panels would simply stay visible. Same pattern as
    # visualization.R. Also fires on ui_mounted(), because a rebuilt sidebar
    # comes back in its static state — the controls for whichever source is
    # currently selected would otherwise stay hidden.
    observeEvent(
      list(input$source_type, ui_mounted()),
      {
        toggle("db_group", condition = !typing())
        toggle("rollback_group", condition = !typing())
        toggle("typing_group", condition = typing())
        toggle("set_name_group", condition = typing())
      },
      ignoreInit = FALSE
    )

    profile_path <- reactiveVal(NULL)
    seq_path <- reactiveVal(NULL)
    meta_path <- reactiveVal(NULL)

    for (nm in c("profile_file", "seq_file", "meta_file")) {
      shinyFileChoose(
        input,
        nm,
        roots = .roots(),
        defaultRoot = "Home",
        filetypes = if (identical(nm, "seq_file")) {
          c("fasta", "fa", "fna", "fas", "txt")
        } else {
          c("tsv", "csv", "txt", "xlsx")
        },
        session = session
      )
    }

    # Helper function to assign selected file paths to target reactive values
    pick <- function(input_value, target) {
      path <- parseFilePaths(.roots(), input_value)$datapath
      req(length(path), is.character(path), file.exists(path))
      target(.clean_path(path))
    }
    observeEvent(input$profile_file, pick(input$profile_file, profile_path))
    observeEvent(input$seq_file, pick(input$seq_file, seq_path))
    observeEvent(input$meta_file, pick(input$meta_file, meta_path))

    # Parse selected profile file using current database scheme
    parsed <- reactive({
      req(typing())
      p <- profile_path()
      req(!is.null(p))
      local <- db_path()
      req(!is.null(local), !is.na(local))
      parse_profile_file(p, scheme_loci(local))
    })

    # The allele sequences, if supplied. These are what make a foreign profile —
    # whose integers are per-locus allele numbers meaningless to us — linkable.
    supplied_sequences <- reactive({
      f <- seq_path()
      if (is.null(f)) {
        return(NULL)
      }
      fa <- read_fasta(f)
      if (!nrow(fa)) {
        return(NULL)
      }
      hdr <- parse_fasta_headers(fa$header)
      data.frame(
        gene = hdr$gene,
        allele = hdr$allele,
        sequence = fa$sequence,
        stringsAsFactors = FALSE
      )
    })

    # A metadata table for the staged isolates: an explicit file, or the
    # "metadata" sheet of the workbook the profile itself came from.
    supplied_metadata <- reactive({
      f <- meta_path()
      if (!is.null(f)) {
        return(read_table_any(f))
      }

      p <- profile_path()
      if (is.null(p) || !grepl("\\.xlsx$", p, ignore.case = TRUE)) {
        return(NULL)
      }
      sheets <- tryCatch(
        getSheetNames(p),
        error = function(e) character(0)
      )
      if (!"metadata" %in% sheets) {
        return(NULL)
      }
      tryCatch(read_table_any(p, sheet = "metadata"), error = function(e) NULL)
    })

    # Resolves profile allele links against database
    resolved <- reactive({
      req(typing())
      resolve_profile(db_path(), parsed(), supplied_sequences())
    })

    # Checks whether staging typing results is blocked
    typing_blocked <- reactive({
      isTRUE(attr(resolved()$checks, "blocked"))
    })

    # Staged isolates must not collide with a local isolate or another set.
    typing_clashes <- reactive({
      r <- resolved()
      if (!isTRUE(r$linkable)) {
        return(character(0))
      }
      intersect(r$isolates, taken_isolate_names(db_path()))
    })

    # Extracts list of metadata columns provided in typing inputs
    typing_meta_cols <- reactive({
      md <- supplied_metadata()
      if (is.null(md) || !"isolate" %in% names(md)) {
        return(character(0))
      }
      setdiff(names(md), "isolate")
    })

    # Fetches list of currently staged sets
    staged_sets <- reactive({
      local_token()
      db_events$depend(db_rev, "staged")
      list_imported_sets(db_path())
    })

    # -- Compatibility -------------------------------------------------------

    # Evaluates database compatibility between local and external sources
    compat <- reactive({
      local_token()
      db_events$depend(db_rev, "schema", "isolates")
      local <- db_path()
      ext <- selected()
      req(!is.null(local), !is.na(local), !is.null(ext))
      staged <- prep()
      req(!is.null(staged))
      check_import_compatibility(local, staged$path)
    })

    # Returns TRUE if database import compatibility is blocked
    blocked <- reactive(isTRUE(attr(compat(), "blocked")))

    # -- Collisions ----------------------------------------------------------

    # Classifies isolate collision status between local and external databases
    classification <- reactive({
      req(!blocked())
      classify_isolate_collisions(db_path(), prep()$path)
    })

    # Returns names of isolates with name conflicts
    clashes <- reactive({
      cl <- classification()
      cl$isolate[cl$status == "name_clash"]
    })

    # Returns list of existing local isolate names
    local_isolates <- reactive({
      local_token()
      db_events$depend(db_rev, "isolates")
      existing_strains(db_path())
    })

    # Assembled from the per-isolate controls; the isolates the user never sees
    # keep their default (new -> add, identical duplicate -> skip).
    resolutions <- reactive({
      res <- default_resolutions(classification())
      for (i in seq_len(nrow(res))) {
        if (res$ext_isolate[i] %in% clashes()) {
          action <- input[[paste0("action_", i)]] %||% "skip"
          res$action[i] <- action
          if (identical(action, "rename")) {
            nm <- input[[paste0("rename_", i)]] %||% ""
            res$final_isolate[i] <- trimws(nm)
          }
        }
      }
      res
    })

    # Counts number of non-skipped isolate resolutions
    accepted <- reactive(sum(resolutions()$action != "skip"))

    # -- Metadata ------------------------------------------------------------

    # Returns available external metadata columns
    ext_meta_cols <- reactive({
      staged <- prep()
      req(!is.null(staged))
      setdiff(metadata_columns(staged$path), METADATA_RESERVED)
    })

    output$meta_picker_ui <- renderUI({
      cols <- tryCatch(
        if (typing()) typing_meta_cols() else ext_meta_cols(),
        error = function(e) character(0)
      )
      if (!length(cols)) {
        return(div(class = "dest-label is-empty", "No metadata to import"))
      }
      pickerInput(
        ns("meta_cols"),
        label = NULL,
        choices = setNames(cols, field_labels_for(cols)),
        selected = cols,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          title = "Select fields …",
          selectedTextFormat = "count > 3",
          countSelectedText = paste0("{0} / ", length(cols), " fields"),
          liveSearch = TRUE,
          liveSearchPlaceholder = "Search fields ...",
          container = "body"
        )
      )
    })

    # -- Custom variables ----------------------------------------------------

    # What the peer defines, split into what can be merged and what clashes.
    # A `.db` merge only: a profile file has no custom-variable tables.
    custom_split <- reactive({
      # Re-check whenever a local definition changes — its type governs which
      # of the peer's variables clash (see importable_custom_fields()), and that
      # can flip while an external database is already selected.
      db_events$depend(db_rev, "custom_fields")
      staged <- prep()
      req(!is.null(staged), !typing())
      importable_custom_fields(db_path(), staged$path)
    })

    output$custom_picker_ui <- renderUI({
      split <- tryCatch(custom_split(), error = function(e) NULL)
      if (is.null(split) || !length(split$importable)) {
        # A clash is the interesting case: say so rather than reporting nothing
        # to import and leaving the user to wonder where their variable went.
        if (!is.null(split) && nrow(split$conflicts)) {
          return(conflict_note(split$conflicts))
        }
        return(div(
          class = "dest-label is-empty",
          "No custom variables to import"
        ))
      }

      cols <- split$importable
      tagList(
        pickerInput(
          ns("custom_fields"),
          label = NULL,
          choices = setNames(cols, field_labels_for(cols)),
          selected = cols,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Select variables …",
            selectedTextFormat = "count > 3",
            countSelectedText = paste0("{0} / ", length(cols), " variables"),
            liveSearch = TRUE,
            liveSearchPlaceholder = "Search variables ...",
            container = "body"
          )
        ),
        if (nrow(split$conflicts)) conflict_note(split$conflicts)
      )
    })

    # Optional analysis-result tables, offered only for a `.db` merge and only
    # for the tables the external database actually carries. When a checkbox is
    # not rendered, its input is NULL, so the merge simply does not carry it.
    output$results_picker_ui <- renderUI({
      staged <- prep()
      if (typing() || is.null(staged)) {
        return(div(class = "dest-label is-empty", "—"))
      }
      present <- tryCatch(
        available_result_tables(staged$path),
        error = function(e) list(classical = FALSE, amr = FALSE)
      )
      if (!present$classical && !present$amr) {
        return(div(
          class = "dest-label is-empty",
          "No analysis results to import"
        ))
      }
      tagList(
        if (present$classical) {
          checkboxInput(
            ns("include_classical"),
            "Classical MLST results",
            value = TRUE
          )
        },
        if (present$amr) {
          checkboxInput(ns("include_amr"), "AMR results", value = TRUE)
        }
      )
    })

    # -- Action button -------------------------------------------------------

    output$action_button_ui <- renderUI({
      if (typing()) {
        disabled(actionButton(
          ns("stage_btn"),
          "Add typing results",
          class = "btn-success",
          icon = icon("layer-group")
        ))
      } else {
        disabled(actionButton(
          ns("merge_btn"),
          "Merge into database",
          class = "btn-success",
          icon = icon("code-merge"),
          width = "100%"
        ))
      }
    })

    # -- Body ----------------------------------------------------------------

    # The pass/warn/fail grid, shared by both source types. Each check is one
    # item (badge + inline name/detail), and items run two to a row via a CSS
    # multi-column layout — a six-check list reads as three rows, not six.
    check_grid <- function(checks) {
      div(
        class = "compat-check",
        lapply(seq_len(nrow(checks)), function(i) {
          div(
            class = "compat-check_item",
            .badge(checks$status[i]),
            div(
              class = "compat-check_row",
              div(class = "compat-check_name", checks$check[i]),
              div(class = "compat-check_detail", checks$detail[i])
            )
          )
        })
      )
    }

    # The gate and what it lets through are read in order, top to bottom: the
    # checks first, then the tally of what they leave to import. This is a
    # flex column, not a grid: a `layout_column_wrap()` grid gives every row an
    # equal `1fr` share of the fillable height regardless of content, which
    # stretched the short check list and the tall summary to match and left
    # the shorter card padded with dead space. A plain fill carrier instead
    # lets the compatibility card size to its own content and only the
    # summary card grow into whatever height is left.
    transfer_row <- function(checks, summary_title, ...) {
      as_fill_carrier(div(
        class = "transfer-row",
        panel_card("Compatibility", check_grid(checks)),
        panel_card(summary_title, ..., fill = TRUE)
      ))
    }

    # Already-staged sets, with a Remove button each.
    staged_ui <- function() {
      sets <- staged_sets()
      if (!nrow(sets)) {
        return(NULL)
      }
      panel_card(
        "Staged typing results",
        div(
          class = "dest-label is-empty mb-2",
          "These isolates have allele profiles but no sequences, so they are",
          " available in the Tree and MST views only."
        ),
        lapply(seq_len(nrow(sets)), function(i) {
          div(
            class = "clash-row",
            div(
              class = "clash-row_name",
              strong(sets$name[i]),
              span(
                class = "text-muted",
                sprintf(
                  "  %d isolates · %s · %.0f%% shared alleles · %s",
                  sets$n_isolates[i],
                  sets$value_kind[i],
                  100 * sets$shared_allele_rate[i],
                  sets$imported_at[i]
                )
              )
            ),
            div(
              class = "clash-row_action",
              actionButton(
                ns(paste0("drop_", sets$set_id[i])),
                "Remove",
                class = "btn-sm btn-outline-danger"
              )
            )
          )
        })
      )
    }

    output$body <- renderUI({
      on.exit(db_waiter$hide())

      if (typing()) {
        return(typing_body())
      }

      if (is.null(selected())) {
        return(div(
          class = "db-empty-hint",
          "Select a PhyloTrace database to merge into the loaded one."
        ))
      }

      checks <- compat()

      if (blocked()) {
        return(transfer_cards(
          transfer_row(
            checks,
            "Import summary",
            div(
              class = "alert alert-danger mb-0",
              icon("triangle-exclamation"),
              " ",
              strong("This database cannot be merged."),
              " It was not built from the same scheme as the loaded database."
            )
          )
        ))
      }

      cl <- classification()
      # `classification()` is already cached; letting import_preview() recompute
      # it would re-hash every isolate profile on both sides each time a
      # resolution dropdown moves — seconds of lag on a real database.
      p <- import_preview(db_path(), prep()$path, resolutions(), cl)

      transfer_cards(
        transfer_row(
          checks,
          "Import summary",
          div(
            class = "export-stats",
            stat_tile("New isolates", p$n_new_isolates, "vial"),
            stat_tile("Already present", p$n_identical_dupes, "clone"),
            stat_tile(
              "Name conflicts",
              p$n_name_clashes,
              "triangle-exclamation"
            ),
            stat_tile("Novel alleles", p$n_new_alleles, "dna"),
            stat_tile(
              "Shared alleles",
              format(p$n_shared_alleles, big.mark = ","),
              "link"
            )
          ),
          if (p$n_identical_dupes > 0) {
            div(
              class = "dest-label is-empty",
              icon("circle-info"),
              sprintf(
                " %d isolate(s) are already in the database with an identical allele profile and will be skipped.",
                p$n_identical_dupes
              )
            )
          },
          if (length(p$metadata_only_ext)) {
            div(
              class = "alert alert-info py-2 mb-0",
              icon("table-columns"),
              " ",
              strong("New metadata fields: "),
              paste(field_labels_for(p$metadata_only_ext), collapse = ", "),
              " — selected fields are added as new columns."
            )
          }
        ),
        if (length(clashes())) {
          panel_card(
            "Resolve name conflicts",
            div(
              class = "dest-label is-empty mb-2",
              "These isolates already exist locally under the same name but carry a different allele profile."
            ),
            lapply(seq_len(nrow(cl)), function(i) {
              if (!cl$isolate[i] %in% clashes()) {
                return(NULL)
              }
              action <- input[[paste0("action_", i)]] %||% "skip"
              div(
                class = "clash-row",
                div(class = "clash-row_name", cl$isolate[i]),
                div(
                  class = "clash-row_action",
                  pickerInput(
                    ns(paste0("action_", i)),
                    label = NULL,
                    choices = .ACTION_CHOICES,
                    selected = action
                  )
                ),
                if (identical(action, "rename")) {
                  div(
                    class = "clash-row_rename",
                    textInput(
                      ns(paste0("rename_", i)),
                      label = NULL,
                      value = input[[paste0("rename_", i)]] %||%
                        suggest_rename(cl$isolate[i], local_isolates())
                    )
                  )
                }
              )
            })
          )
        }
      )
    })

    # -- Typing-results body -------------------------------------------------

    # Renders the UI body content when typing-results source mode is active
    typing_body <- function() {
      if (is.null(profile_path())) {
        return(transfer_cards(
          div(
            class = "db-empty-hint",
            "Select a profile table (.tsv / .csv / .xlsx) of typing results.",
            tags$br(),
            span(
              class = "small",
              "Allele hashes link straight to this database. Integer allele",
              " numbers from another tool need their allele sequences too."
            )
          ),
          staged_ui()
        ))
      }

      r <- resolved()
      p <- parsed()

      if (typing_blocked()) {
        return(transfer_cards(
          transfer_row(
            r$checks,
            "Staging summary",
            div(
              class = "alert alert-danger mb-0",
              icon("triangle-exclamation"),
              " ",
              strong("These results cannot be linked to this database."),
              " Without a link from each call to a real allele, any distance we",
              " computed would be meaningless."
            )
          ),
          staged_ui()
        ))
      }

      clash <- typing_clashes()

      transfer_cards(
        transfer_row(
          r$checks,
          "Staging summary",
          div(
            class = "export-stats",
            stat_tile("Isolates", length(r$isolates), "vial"),
            stat_tile("Loci", length(p$loci_seen), "table-columns"),
            stat_tile(
              "Allele calls",
              format(nrow(r$long), big.mark = ","),
              "hashtag"
            ),
            stat_tile(
              "Missing calls",
              format(p$n_missing, big.mark = ","),
              "circle-question"
            ),
            stat_tile(
              "Shared alleles",
              sprintf("%.0f%%", 100 * r$shared_allele_rate),
              "link"
            )
          ),
          div(
            class = "dest-label is-empty",
            icon("circle-info"),
            " These isolates are staged beside the database, not merged into it:",
            " a profile table carries no sequences, so they can contribute allele",
            " identity to the Tree and MST but cannot be typed against."
          ),
          if (length(clash)) {
            div(
              class = "alert alert-warning py-2 mb-0",
              icon("triangle-exclamation"),
              " ",
              strong(sprintf(
                "%d isolate name(s) already in use: ",
                length(clash)
              )),
              paste(head(clash, 5), collapse = ", "),
              if (length(clash) > 5) ", …",
              tags$br(),
              "They will be imported with an \"_ext\" suffix."
            )
          }
        ),
        staged_ui()
      )
    }

    # Collisions are renamed rather than skipped: unlike a `.db` merge there is
    # nothing to overwrite — a staged isolate is a separate observation.
    typing_renames <- reactive({
      clash <- typing_clashes()
      if (!length(clash)) {
        return(NULL)
      }
      taken <- taken_isolate_names(db_path())
      out <- character(0)
      for (nm in clash) {
        new <- suggest_rename_ext(nm, c(taken, out))
        out <- c(out, setNames(new, nm))
      }
      out
    })

    # Toggles state (enable/disable) of action buttons based on input validity
    observe({
      if (typing()) {
        ok <- tryCatch(
          !is.null(profile_path()) &&
            !typing_blocked() &&
            length(resolved()$isolates) > 0 &&
            nzchar(trimws(input$set_name %||% "")),
          error = function(e) FALSE
        )
        if (isTRUE(ok)) {
          enable("stage_btn")
        } else {
          disable("stage_btn")
        }
        return()
      }

      ok <- tryCatch(
        !is.null(selected()) && !blocked() && accepted() > 0,
        error = function(e) FALSE
      )
      if (isTRUE(ok)) enable("merge_btn") else disable("merge_btn")
    })

    # Handles staging profile set submission
    observeEvent(input$stage_btn, {
      req(typing(), !typing_blocked())

      md <- supplied_metadata()
      if (!is.null(md) && "isolate" %in% names(md)) {
        keep <- c(
          "isolate",
          intersect(names(md), input$meta_cols %||% character(0))
        )
        md <- md[, keep, drop = FALSE]
      } else {
        md <- NULL
      }

      result <- tryCatch(
        withProgress(
          message = "Adding typing results",
          value = 0.5,
          {
            stage_profile_set(
              db_path = db_path(),
              name = trimws(input$set_name),
              resolved = resolved(),
              metadata = md,
              renames = typing_renames(),
              source_file = basename(profile_path())
            )
          }
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        showNotification(
          tagList(strong("Import failed. "), conditionMessage(result)),
          type = "error",
          duration = NULL
        )
        return()
      }

      profile_path(NULL)
      seq_path(NULL)
      meta_path(NULL)
      local_token(local_token() + 1L)
      # Staging adds a set alongside the local isolates without touching them,
      # so the only reader affected is the Visualization module's staged-set
      # picker, which follows this revision. Deliberately not `imported` - that
      # raises main.R's "reload the database" prompt, which is the right
      # response to a merge or a restore and far too heavy for this.
      db_events$bump(db_rev, "staged")

      showNotification(
        tagList(
          icon("layer-group"),
          " ",
          strong("Typing results added."),
          tags$br(),
          sprintf(
            "%d isolate(s) staged as '%s'. Select the set in Visualization to include them in a Tree or MST.",
            length(resolved()$isolates),
            trimws(input$set_name)
          )
        ),
        type = "message",
        duration = 10
      )
    })

    # One Remove observer per staged set, (re)bound whenever the list changes.
    observeEvent(staged_sets(), {
      for (sid in staged_sets()$set_id) {
        local({
          this <- sid
          observeEvent(
            input[[paste0("drop_", this)]],
            {
              delete_imported_set(db_path(), this)
              local_token(local_token() + 1L)
              db_events$bump(db_rev, "staged")
              showNotification(
                "Staged typing results removed.",
                type = "message",
                duration = 4
              )
            },
            ignoreInit = TRUE,
            once = TRUE
          )
        })
      }
    })

    # -- Merge ---------------------------------------------------------------

    # Displays merge confirmation modal popup
    observeEvent(input$merge_btn, {
      req(accepted() > 0)
      res <- resolutions()
      acc <- res[res$action != "skip", , drop = FALSE]

      showModal(modalDialog(
        title = "Merge database",
        div(
          tags$p(sprintf(
            "%d isolate(s) will be written into %s.",
            nrow(acc),
            basename(db_path())
          )),
          if (any(acc$action == "overwrite")) {
            div(
              class = "alert alert-warning py-2",
              icon("triangle-exclamation"),
              " ",
              sprintf(
                "%d local isolate(s) will be replaced.",
                sum(acc$action == "overwrite")
              )
            )
          },
          tags$p(
            class = "dest-label is-empty",
            "The current database is saved as a timestamped backup first, so this can be rolled back."
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_merge"), "Merge", class = "btn-success")
        ),
        easyClose = TRUE
      ))
    })

    # Executes database merge operation on confirmation
    observeEvent(input$confirm_merge, {
      removeModal()
      local <- db_path()
      staged <- prep()
      req(!is.null(local), !is.null(staged))

      title <- "Merging database"
      db_waiter$show()
      on.exit(db_waiter$hide())

      # Feed progress into the db_waiter spinner instead of a Shiny progress bar.
      step <- function(frac, msg) {
        db_waiter$update(
          html = div(
            class = "spinner-custom",
            spin_flower(),
            h5(title),
            tags$p(sprintf("%d%% — %s", round(frac * 100), msg))
          )
        )
      }

      result <- tryCatch(
        merge_databases(
          local_path = local,
          ext_path = staged$path,
          resolutions = resolutions(),
          metadata_cols = input$meta_cols %||% character(0),
          include_classical = isTRUE(input$include_classical),
          include_amr = isTRUE(input$include_amr),
          custom_fields = input$custom_fields %||% character(0),
          backup = TRUE,
          progress = step,
          # `staged$path` can be a temp copy; the label written into
          # metadata.source has to name the file the user actually picked.
          source_file = selected()
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        showNotification(
          tagList(
            strong("Merge failed. "),
            conditionMessage(result),
            tags$br(),
            "The database was left unchanged."
          ),
          type = "error",
          duration = NULL
        )
        return()
      }

      # Let go of the source before anything recomputes. What the panel was
      # showing - compatibility, collisions, import summary - described the
      # database as it was *before* this merge; every accepted isolate is local
      # now, so recomputing it would re-hash both sides only to arrive at an
      # empty diff, and it would do so while main.R's reload prompt is already
      # up. Clearing first means local_token() below only refreshes the backup
      # list, and the body falls straight through to its empty state.
      clear_ext_db()

      local_token(local_token() + 1L)
      # A merge rewrites the database wholesale, so every domain moves. The
      # `imported` signal stays as well: main.R turns it into the reload prompt,
      # which is what re-runs the hash backfill and releases the modules that
      # hold back automatic refresh while they carry unsaved edits.
      db_events$bump_all(db_rev)
      imported(imported() + 1L)

      showNotification(
        tagList(
          icon("code-merge"),
          " ",
          strong("Database merged."),
          tags$br(),
          sprintf(
            "%d added, %d overwritten, %d renamed, %d skipped; %d novel allele(s).",
            result$added,
            result$overwritten,
            result$renamed,
            result$skipped,
            result$new_alleles
          ),
          tags$br(),
          sprintf("Recorded under Source “%s”.", result$source)
        ),
        type = "message",
        duration = 10
      )
    })

    # -- Rollback ------------------------------------------------------------

    # Fetches available database backups
    backups <- reactive({
      local_token()
      list_backups(db_path())
    })

    output$restore_ui <- renderUI({
      files <- backups()
      if (!length(files)) {
        return(div(class = "dest-label is-empty", "No backups yet"))
      }
      tagList(
        pickerInput(
          ns("backup_pick"),
          label = NULL,
          choices = setNames(files, basename(files))
        ),
        actionButton(
          ns("restore_btn"),
          "Restore",
          class = "btn-outline-danger",
          icon = icon("rotate-left")
        )
      )
    })

    # Displays rollback backup confirmation modal popup
    observeEvent(input$restore_btn, {
      req(input$backup_pick)
      showModal(modalDialog(
        title = "Restore backup",
        tags$p(tagList(
          "Replace the loaded database with ",
          tags$code(basename(input$backup_pick)),
          "?"
        )),
        tags$p(
          class = "dest-label is-empty",
          "The database being replaced is itself backed up first."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_restore"), "Restore", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    # Executes database backup restoration operation on confirmation
    observeEvent(input$confirm_restore, {
      removeModal()

      # restore_backup() is a couple of filesystem ops with nothing to report
      # progress against, so the spinner just names the work rather than
      # stepping through it — same styling as the export/merge waiters.
      db_waiter$show()
      db_waiter$update(
        html = div(
          class = "spinner-custom",
          spin_flower(),
          h5("Restoring backup ...")
        )
      )
      on.exit(db_waiter$hide())

      ok <- tryCatch(
        {
          restore_backup(db_path(), input$backup_pick)
          TRUE
        },
        error = function(e) {
          showNotification(
            tagList(strong("Restore failed. "), conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          FALSE
        }
      )
      if (!ok) {
        return()
      }

      local_token(local_token() + 1L)
      # A restore replaces the file; same reasoning as the merge above.
      db_events$bump_all(db_rev)
      imported(imported() + 1L)

      showNotification(
        tagList(icon("rotate-left"), " ", strong("Backup restored.")),
        type = "message",
        duration = 8
      )
    })

    # Reset module state when the user returns to the landing screen.
    observeEvent(
      session_reset(),
      {
        clear_ext_db()
        profile_path(NULL)
        seq_path(NULL)
        meta_path(NULL)
        local_token(local_token() + 1L)
      },
      ignoreInit = TRUE
    )

    list(imported = imported)
  })
}
