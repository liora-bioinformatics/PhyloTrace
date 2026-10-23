# app/view/database_custom_import.R
#
# "Import from Table" for the Custom Variables panel: reads an isolate-keyed
# table, shows how it matches the loaded database, lets the user decide per
# column what to import and as which type, then applies it in one transaction
# (see app/logic/custom_import.R).

box::use(
  bslib[tooltip],
  fs[path_home],
  rlang[`%||%`],
  shiny,
  shinyFiles[parseFilePaths, shinyFileChoose, shinyFilesButton],
  shinyjs[hidden, html, toggle, toggleState],
  shinyWidgets[pickerInput],
  stats[setNames],
  utils[head],
)

box::use(
  app / logic / custom_fields[CUSTOM_TYPES, list_custom_fields],
  app / logic / custom_import,
  app / logic / db_guard[db_failed, guard_db],
  app / logic / field_labels[field_labels_for],
  app / logic / functions[stat_tile],
)

# Where the wizard's dropdown menus are attached: inside the modal, not <body>.
# Bootstrap's modal focus trap pulls focus back from anything outside it, which
# scrolled the dialog to its first control whenever a menu opened.
.MENU_CONTAINER <- "#shiny-modal"

# Entries the step-1 menus (sheet, key column) show before scrolling, so a wide
# table's column list stays inside the content-sized dialog.
.MENU_ROWS <- 6L

# Isolate names listed inline before the rest is summarised as "and N more".
.LIST_MAX <- 8L

# Isolate names a stat tile's hover list shows before summarising the rest;
# keeps the tooltip shorter than the dialog.
.TIP_MAX <- 15L

# Comma-separated preview of `x`, cut off after .LIST_MAX entries.
.preview_list <- function(x) {
  shown <- paste(head(x, .LIST_MAX), collapse = ", ")
  if (length(x) > .LIST_MAX) {
    shown <- sprintf("%s and %d more", shown, length(x) - .LIST_MAX)
  }
  shown
}

# A stat tile counting `ids`, listing them on hover: at most .TIP_MAX names,
# then how many were left out.
.listed_tile <- function(label, ids, icon_name) {
  tile <- stat_tile(label, length(ids), icon_name)
  if (!length(ids)) {
    return(tile)
  }
  hidden_n <- length(ids) - .TIP_MAX
  tooltip(
    tile,
    shiny$div(
      class = "table-import-tip",
      lapply(head(ids, .TIP_MAX), shiny$div),
      if (hidden_n > 0L) {
        shiny$div(
          class = "table-import-tip_more",
          sprintf("\u2026 and %d more not shown", hidden_n)
        )
      }
    ),
    placement = "bottom",
    options = list(customClass = "table-import-tooltip")
  )
}

# Status pill shared with the database-merge checks (.compat-badge in main.scss).
.badge <- function(status, label) {
  shiny$span(class = paste0("compat-badge is-", status), label)
}

# One info/warning line of the assessment.
.note <- function(icon_name, ..., kind = "info") {
  shiny$div(
    class = paste0("alert alert-", kind, " py-2 mb-2"),
    shiny$icon(icon_name),
    " ",
    ...
  )
}

# What the user may do with a column: merging is only offered into a custom
# variable or editable metadata field whose type every value already fits.
.action_choices <- function(col) {
  mergeable <- col$clash %in% c("custom", "metadata") &&
    isTRUE(col$merge_failures == 0L)
  c(
    "New variable" = "new",
    if (mergeable) c("Fill empty cells" = "fill", "Overwrite values" = "overwrite"),
    "Skip" = "skip"
  )
}

# Starting choice per column: import what is new, fill what matches an
# existing field, skip what is empty or cannot be merged.
.default_choices <- function(cols) {
  mergeable <- cols$clash %in% c("custom", "metadata") &
    !is.na(cols$merge_failures) & cols$merge_failures == 0L
  action <- ifelse(
    cols$n_values == 0L | cols$clash == "label",
    "skip",
    ifelse(cols$clash == "none", "new", ifelse(mergeable, "fill", "skip"))
  )
  data.frame(
    header = cols$header,
    action = action,
    name = ifelse(cols$clash == "none", cols$suggested, paste0(cols$suggested, "_imported")),
    type = cols$guess,
    stringsAsFactors = FALSE
  )
}

# The file chooser itself, kept out of sight in static UI. A shinyFiles button
# that is re-rendered (as the panel's renderUI does when the first variable
# appears) re-opens its dialog on binding, so the visible buttons
# (`open_button()`) only forward their click to this one.
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shinyFilesButton(
    ns("file"),
    "Import from Table",
    title = "Choose a table with one row per isolate (.csv, .tsv, .txt, .xlsx)",
    multiple = FALSE,
    style = "display: none;"
  )
}

#' Visible "Import from Table" Button
#'
#' Opens the file chooser of the import module `id`. May be placed in dynamic
#' UI, as long as only one copy is in the page at a time.
#'
#' @param id The import module's id, as passed to `ui()`.
#' @param ... Further arguments for `shiny::actionButton()`.
#' @return An action button.
#' @export
open_button <- function(id, ...) {
  ns <- shiny$NS(id)
  shiny$actionButton(
    ns("open"),
    "Import from Table",
    icon = shiny$icon("file-import"),
    onclick = sprintf("document.getElementById('%s').click();", ns("file")),
    ...
  )
}

#' @export
server <- function(
  id,
  db_path = shiny$reactive(NULL),
  blocked = shiny$reactive(FALSE),
  on_imported = function(counts) NULL
) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    roots <- c(Home = path_home(), Root = "/")
    shinyFileChoose(
      input,
      "file",
      roots = roots,
      defaultRoot = "Home",
      filetypes = custom_import$IMPORT_EXTENSIONS,
      session = session
    )

    # `read` is what came out of the file, `assessment` its comparison with the
    # database, `choices` the per-column decisions (kept across Back / Next).
    # `gen` numbers each rendering of the plan step: its inputs carry it in
    # their ids, so values left behind by an earlier file can never be read
    # as this one's.
    State <- shiny$reactiveValues(
      path = NULL,
      read = NULL,
      assessment = NULL,
      choices = NULL,
      step = 0L,
      gen = 0L
    )

    # Reads (or re-reads, for another sheet) the file and assesses it, matching
    # isolates on `key` or, by default, the column key_column() picks.
    load_table <- function(path, sheet = NULL, key = NULL) {
      read <- tryCatch(
        custom_import$read_import_table(path, sheet),
        error = function(e) e
      )
      if (inherits(read, "error")) {
        shiny$showNotification(
          paste("The table could not be read:", conditionMessage(read)),
          type = "error",
          duration = 10
        )
        return(FALSE)
      }
      if (ncol(read$data) < 2L || !nrow(read$data)) {
        shiny$showNotification(
          "The table needs an isolate column, at least one more column and one row.",
          type = "error",
          duration = 8
        )
        return(FALSE)
      }
      assessment <- guard_db(
        "Comparing the table with the database",
        custom_import$assess_import(db_path(), read$data, key)
      )
      if (db_failed(assessment)) {
        return(FALSE)
      }
      State$path <- path
      State$read <- read
      State$assessment <- assessment
      State$choices <- .default_choices(assessment$columns)
      TRUE
    }

    # A picked file opens the assessment. Refused while the Values grid holds
    # unsaved edits: the import reloads the panel, which would discard them.
    shiny$observeEvent(input$file, {
      path <- as.character(parseFilePaths(roots, input$file)$datapath)
      shiny$req(length(path) == 1L, file.exists(path))
      if (isTRUE(blocked())) {
        shiny$showNotification(
          "Save or discard the unsaved edits in the Values table before importing.",
          type = "warning",
          duration = 6
        )
        return()
      }
      if (load_table(path)) {
        show_assessment()
      }
    })

    # -- Step 1: assessment --------------------------------------------------

    # Shows how the loaded table's isolates match the database. Sized to its
    # content (.is-check): the columns are reviewed in step 2.
    show_assessment <- function() {
      a <- State$assessment
      r <- State$read
      p <- a$prepared
      State$step <- 1L
      can_continue <- length(a$matched) > 0L && nrow(a$columns) > 0L

      shiny$showModal(shiny$div(
        class = "selection-modal table-import-modal is-check",
        shiny$modalDialog(
          title = NULL,
          shiny$div(
            class = "selection-modal-toolbar table-import-toolbar",
            shiny$div(
              class = "table-import-heading",
              shiny$tags$strong("Import from table"),
              shiny$span(class = "text-muted", basename(State$path)),
              shiny$span(class = "table-import-step", "Step 1 of 2 \u00b7 Check")
            ),
            if (length(r$sheets) > 1L) {
              shiny$div(
                class = "table-import-pick",
                shiny$span(class = "text-muted", "Sheet"),
                pickerInput(
                  ns("sheet"),
                  label = NULL,
                  choices = r$sheets,
                  selected = r$sheet,
                  width = "fit",
                  options = list(container = .MENU_CONTAINER, size = .MENU_ROWS)
                )
              )
            },
            shiny$div(
              class = "table-import-pick",
              shiny$span(class = "text-muted", "Match isolates on"),
              pickerInput(
                ns("key"),
                label = NULL,
                choices = names(r$data),
                selected = a$key,
                width = "fit",
                options = list(
                  container = .MENU_CONTAINER,
                  size = .MENU_ROWS,
                  liveSearch = ncol(r$data) > 10L
                )
              )
            )
          ),
          shiny$h6(class = "table-import-section", "Isolates"),
          shiny$div(
            class = "export-stats",
            stat_tile("Rows read", p$n_rows, "table-list"),
            stat_tile("Matched", length(a$matched), "link"),
            .listed_tile("Not in database", a$unmatched, "circle-question"),
            .listed_tile("Excluded", p$conflicting, "triangle-exclamation"),
            stat_tile("Not in table", a$n_db_untouched, "database")
          ),
          isolate_notes(a, r),
          footer = shiny$tagList(
            shiny$modalButton("Cancel"),
            shiny$actionButton(
              ns("to_plan"),
              "Next",
              icon = shiny$icon("arrow-right"),
              class = "btn-primary",
              disabled = !can_continue
            )
          ),
          easyClose = FALSE
        )
      ))
    }

    # The findings about the key column and the isolate match: what needs the
    # user's attention as alerts, worst first, then the rest as a quiet list.
    isolate_notes <- function(a, r) {
      p <- a$prepared
      absent <- setdiff(a$unmatched, a$case_only)
      missing_tokens <- paste0(
        "\u201c",
        setdiff(custom_import$MISSING_TOKENS, ""),
        "\u201d",
        collapse = ", "
      )
      infos <- list(
        sprintf(
          paste(
            "Isolates are matched on column \u201c%s\u201d. Isolate names are unique",
            "within a database, so every row maps to at most one isolate."
          ),
          a$key
        ),
        sprintf(
          "%d column(s) besides the key will be reviewed in the next step.",
          nrow(a$columns)
        ),
        if (length(absent)) {
          sprintf(
            paste(
              "%d isolate(s) are not in this database and are ignored \u2014 an",
              "import only adds data to existing isolates: %s."
            ),
            length(absent),
            .preview_list(absent)
          )
        },
        if (p$n_blank_keys > 0L) {
          sprintf(
            "%d row(s) have no value in \u201c%s\u201d and are ignored%s.",
            p$n_blank_keys,
            a$key,
            if (length(p$blank_preview)) {
              sprintf(
                " \u2014 e.g. a note or totals row: %s",
                paste0("\u201c", p$blank_preview, "\u201d", collapse = ", ")
              )
            } else {
              ""
            }
          )
        },
        if (p$n_repeated > 0L) {
          sprintf("%d exact duplicate row(s) were counted once.", p$n_repeated)
        },
        if (length(r$renamed)) {
          sprintf(
            "Blank or repeated column headers were renamed: %s.",
            paste(names(r$renamed), collapse = ", ")
          )
        },
        sprintf(
          paste(
            "Empty cells and %s count as missing. Missing values are never",
            "written, so an import never clears a value already in the database."
          ),
          missing_tokens
        )
      )

      shiny$tagList(
        if (!length(a$matched)) {
          .note(
            "circle-xmark",
            "None of the table's isolates are in this database, so nothing can be",
            "imported. Isolate names must match exactly (letter case included).",
            kind = "danger"
          )
        },
        if (length(p$conflicting)) {
          .note(
            "triangle-exclamation",
            shiny$tags$strong(sprintf("%d isolate(s) excluded:", length(p$conflicting))),
            "they are listed more than once with differing values, and which row is",
            "right cannot be decided automatically. Fix the table and re-import",
            sprintf("to include them (%s).", .preview_list(p$conflicting)),
            kind = "warning"
          )
        },
        if (length(a$case_only)) {
          .note(
            "triangle-exclamation",
            sprintf(
              paste(
                "%d isolate(s) match a database isolate only when ignoring letter",
                "case and were not matched: %s."
              ),
              length(a$case_only),
              .preview_list(a$case_only)
            ),
            kind = "warning"
          )
        },
        if (identical(a$key_source, "matches")) {
          .note(
            "triangle-exclamation",
            sprintf(
              paste(
                "No column is headed \u201cisolate\u201d, so isolates are matched on",
                "\u201c%s\u201d, the column holding the most isolate names of this",
                "database. Pick another column above if that is wrong."
              ),
              a$key
            ),
            kind = "warning"
          )
        },
        if (identical(a$key_source, "first")) {
          .note(
            "triangle-exclamation",
            sprintf(
              paste(
                "No column is headed \u201cisolate\u201d and none holds isolate names",
                "of this database, so the left-most column \u201c%s\u201d is used.",
                "Pick another column above if that is wrong."
              ),
              a$key
            ),
            kind = "warning"
          )
        },
        shiny$tags$ul(
          class = "table-import-notes",
          lapply(Filter(Negate(is.null), infos), shiny$tags$li)
        )
      )
    }

    # Re-matches the table on the column the user picked.
    shiny$observeEvent(input$key, {
      a <- State$assessment
      shiny$req(a, identical(State$step, 1L), !identical(input$key, a$key))
      if (load_table(State$path, State$read$sheet, input$key)) {
        show_assessment()
      }
    })

    # Re-reads the workbook when another sheet is picked.
    shiny$observeEvent(input$sheet, {
      r <- State$read
      shiny$req(r, !identical(input$sheet, r$sheet))
      if (load_table(State$path, input$sheet)) {
        show_assessment()
      }
    })

    shiny$observeEvent(input$to_plan, {
      shiny$req(State$assessment)
      show_plan()
    })

    # -- Step 2: plan --------------------------------------------------------

    # Input id of one control of plan row `i` in the current rendering.
    plan_id <- function(what, i) sprintf("%s_%d_%d", what, State$gen, i)

    # Shows one row per column with its action, name and type controls.
    show_plan <- function() {
      a <- State$assessment
      cols <- a$columns
      ch <- State$choices
      defs <- list_custom_fields(db_path())
      State$gen <- shiny$isolate(State$gen) + 1L
      State$step <- 2L

      rows <- lapply(seq_len(nrow(cols)), function(i) {
        col <- cols[i, , drop = FALSE]
        feasible <- strsplit(col$feasible, ",", fixed = TRUE)[[1]]
        fits <- names(CUSTOM_TYPES) %in% feasible
        shiny$div(
          class = "table-import-row",
          shiny$div(
            class = "table-import-cell is-column",
            shiny$tags$strong(col$header),
            shiny$div(
              class = "text-muted small",
              sprintf("%d value(s), %d missing", col$n_values, col$n_missing)
            ),
            shiny$div(class = "text-muted small table-import-sample", col$sample)
          ),
          shiny$div(
            class = "table-import-cell",
            pickerInput(
              ns(plan_id("act", i)),
              label = NULL,
              choices = .action_choices(col),
              selected = ch$action[[i]],
              width = "100%",
              options = list(container = .MENU_CONTAINER)
            )
          ),
          shiny$div(
            class = "table-import-cell",
            shiny$div(
              id = ns(plan_id("name_wrap", i)),
              shiny$textInput(
                ns(plan_id("name", i)),
                label = NULL,
                value = ch$name[[i]],
                width = "100%"
              )
            ),
            if (col$clash %in% c("custom", "metadata")) {
              hidden(shiny$div(
                id = ns(plan_id("target", i)),
                class = "table-import-target",
                shiny$icon("arrow-right"),
                " ",
                col$clash_target,
                shiny$span(
                  class = "text-muted",
                  sprintf(
                    " · %s",
                    tolower(CUSTOM_TYPES[[custom_import$target_type(
                      col$clash,
                      col$clash_target,
                      defs
                    )$type]])
                  )
                )
              ))
            }
          ),
          shiny$div(
            class = "table-import-cell",
            shiny$div(
              id = ns(plan_id("type_wrap", i)),
              # Types some value does not fit stay listed but cannot be picked,
              # so the user sees why "Whole number" is not on offer.
              pickerInput(
                ns(plan_id("type", i)),
                label = NULL,
                choices = setNames(names(CUSTOM_TYPES), unname(CUSTOM_TYPES)),
                selected = ch$type[[i]],
                width = "100%",
                options = list(container = .MENU_CONTAINER),
                choicesOpt = list(
                  disabled = !fits,
                  subtext = ifelse(fits, "", "values don't fit")
                )
              )
            )
          ),
          shiny$div(
            class = "table-import-cell is-status",
            id = ns(plan_id("status", i))
          )
        )
      })

      shiny$showModal(shiny$div(
        class = "selection-modal table-import-modal",
        shiny$modalDialog(
          title = NULL,
          shiny$div(
            class = "selection-modal-toolbar table-import-toolbar",
            shiny$div(
              class = "table-import-heading",
              shiny$tags$strong("Import from table"),
              shiny$span(class = "text-muted", basename(State$path)),
              shiny$span(class = "table-import-step", "Step 2 of 2 · Variables")
            ),
            shiny$uiOutput(ns("plan_summary"), class = "selection-modal-count")
          ),
          shiny$div(
            class = "table-import-plan",
            shiny$div(
              class = "table-import-row is-header",
              shiny$div(class = "table-import-cell", "Column"),
              shiny$div(class = "table-import-cell", "Action"),
              shiny$div(class = "table-import-cell", "Variable name"),
              shiny$div(class = "table-import-cell", "Type"),
              shiny$div(class = "table-import-cell", "Result")
            ),
            rows
          ),
          footer = shiny$tagList(
            shiny$modalButton("Cancel"),
            shiny$actionButton(ns("to_check"), "Back", icon = shiny$icon("arrow-left")),
            shiny$actionButton(
              ns("import"),
              "Import",
              icon = shiny$icon("file-import"),
              class = "btn-success",
              disabled = TRUE
            )
          ),
          easyClose = FALSE
        )
      ))
    }

    # The plan as the controls currently stand; falls back to the stored
    # choice for a control that has not reported its value yet.
    current_choices <- function() {
      ch <- State$choices
      for (i in seq_len(nrow(ch))) {
        ch$action[[i]] <- input[[plan_id("act", i)]] %||% ch$action[[i]]
        ch$name[[i]] <- trimws(input[[plan_id("name", i)]] %||% ch$name[[i]])
        ch$type[[i]] <- input[[plan_id("type", i)]] %||% ch$type[[i]]
      }
      ch
    }

    # The plan of the active columns, with merges pointed at their target.
    plan_of <- function(ch) {
      cols <- State$assessment$columns
      merge <- ch$action %in% c("fill", "overwrite")
      ch$name[merge] <- cols$clash_target[match(ch$header[merge], cols$header)]
      ch[ch$action != "skip", , drop = FALSE]
    }

    # Every control of the plan step, validated against the database. Debounced:
    # validation reads the store, and a name is typed one keystroke at a time.
    plan_state <- shiny$debounce(
      shiny$reactive({
        shiny$req(identical(State$step, 2L), State$assessment)
        ch <- current_choices()
        plan <- plan_of(ch)
        problems <- guard_db(
          "Checking the import",
          custom_import$validate_plan(db_path(), State$assessment$rows, plan)
        )
        if (db_failed(problems)) {
          problems <- setNames("The database could not be read.", plan$header[1])
        }
        list(choices = ch, plan = plan, problems = problems)
      }),
      300
    )

    # Shows each row's outcome, and its name / type controls only where they
    # apply (a merge keeps the target's own name and type).
    shiny$observeEvent(plan_state(), {
      st <- plan_state()
      a <- State$assessment
      cols <- a$columns
      for (i in seq_len(nrow(st$choices))) {
        action <- st$choices$action[[i]]
        toggle(plan_id("name_wrap", i), condition = identical(action, "new"))
        toggle(plan_id("type_wrap", i), condition = identical(action, "new"))
        toggle(plan_id("target", i), condition = action %in% c("fill", "overwrite"))
        status <- row_status(
          cols[i, , drop = FALSE],
          st$choices[i, , drop = FALSE],
          st$problems
        )
        html(plan_id("status", i), as.character(status))
      }
      toggleState("import", condition = nrow(st$plan) > 0L && !length(st$problems))
    })

    # Why a column may be better left out: no values, or a name already taken
    # by a field it cannot be merged into.
    skip_reason <- function(col) {
      target <- sprintf("\u201c%s\u201d", col$clash_target)
      if (col$n_values == 0L) {
        " no values for matched isolates"
      } else if (identical(col$clash, "label")) {
        sprintf(
          paste(
            " reads like the standard field %s \u2014 head the column \u201c%s\u201d",
            "to fill it, or import it as a new variable under another name"
          ),
          sprintf("\u201c%s\u201d", field_labels_for(col$clash_target)),
          col$clash_target
        )
      } else if (identical(col$clash, "readonly")) {
        paste0(" ", target, " is a read-only metadata field \u2014 import as a new variable")
      } else if (isTRUE(col$merge_failures > 0L)) {
        sprintf(
          " %d value(s) do not fit the type of existing %s \u2014 import as a new variable",
          col$merge_failures,
          target
        )
      }
    }

    # The result line of one plan row.
    row_status <- function(col, choice, problems) {
      problem <- problems[col$header]
      if (!is.na(problem)) {
        return(shiny$tagList(.badge("fail", "fix"), " ", problem))
      }
      switch(
        choice$action,
        skip = shiny$tagList(
          .badge("skipped", "skip"),
          skip_reason(col)
        ),
        new = {
          levels <- if (identical(choice$type, "category")) {
            sprintf(" · %d allowed value(s) from the data", col$n_distinct)
          }
          shiny$tagList(
            .badge("pass", "new"),
            sprintf(
              " %d value(s) as %s",
              col$n_values,
              tolower(CUSTOM_TYPES[[choice$type]])
            ),
            levels
          )
        },
        {
          pv <- guard_db(
            "Checking the import",
            custom_import$merge_preview(
              db_path(),
              State$assessment$rows,
              col$header,
              col$clash,
              col$clash_target
            )
          )
          if (db_failed(pv)) {
            return(.badge("fail", "unreadable"))
          }
          overwrite <- identical(choice$action, "overwrite")
          shiny$tagList(
            .badge(if (overwrite && pv$differ > 0L) "warn" else "pass", "merge"),
            sprintf(" %d empty cell(s) filled", pv$fill),
            if (pv$same > 0L) sprintf(", %d already identical", pv$same),
            if (pv$differ > 0L) {
              if (overwrite) {
                shiny$tags$strong(sprintf(", %d different value(s) replaced", pv$differ))
              } else {
                sprintf(", %d different value(s) kept", pv$differ)
              }
            }
          )
        }
      )
    }

    # Totals of the plan, and what still blocks it.
    output$plan_summary <- shiny$renderUI({
      st <- plan_state()
      act <- st$choices$action
      n_new <- sum(act == "new")
      n_merge <- sum(act %in% c("fill", "overwrite"))
      n_skip <- sum(act == "skip")
      shiny$tagList(
        sprintf(
          "%d new variable(s) · %d merge(s) · %d skipped",
          n_new,
          n_merge,
          n_skip
        ),
        if (length(st$problems)) {
          shiny$span(
            class = "text-danger",
            sprintf(" · fix %d column(s) to continue", length(st$problems))
          )
        }
      )
    })

    shiny$observeEvent(input$to_check, {
      State$choices <- current_choices()
      show_assessment()
    })

    # Validates once more against the database as it is now, then writes.
    shiny$observeEvent(input$import, {
      a <- State$assessment
      shiny$req(a, identical(State$step, 2L))
      ch <- current_choices()
      plan <- plan_of(ch)
      shiny$req(nrow(plan) > 0L)

      problems <- guard_db(
        "Checking the import",
        custom_import$validate_plan(db_path(), a$rows, plan)
      )
      if (db_failed(problems)) {
        return()
      }
      if (length(problems)) {
        shiny$showNotification(
          "The database changed meanwhile - check the highlighted columns.",
          type = "warning",
          duration = 6
        )
        return()
      }

      counts <- guard_db(
        "Importing the table",
        custom_import$apply_import(db_path(), a$rows, plan)
      )
      if (db_failed(counts)) {
        return()
      }

      shiny$removeModal()
      State$step <- 0L
      State$assessment <- NULL
      State$read <- NULL
      on_imported(counts)
      shiny$showNotification(
        paste0(
          "Imported: ",
          counts$created,
          " new variable(s) with ",
          counts$values,
          " value(s)",
          if (counts$filled + counts$overwritten > 0L) {
            sprintf(
              "; %d cell(s) filled, %d overwritten in existing fields",
              counts$filled,
              counts$overwritten
            )
          },
          "."
        ),
        type = "message",
        duration = 6
      )
    })

    invisible(NULL)
  })
}
