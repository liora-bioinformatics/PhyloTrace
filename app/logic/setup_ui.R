# app/logic/setup_ui.R
#
# Shared building blocks of the database setup screens: numbered tasks with
# their state, the label of a picked file, headline figures of an imported
# scheme and the list of what an import skipped and why.

box::use(
  bslib,
  shiny,
  utils[head],
)

box::use(
  app / logic / overview_ui[hint_icon],
  app / logic / scheme_sources[safe_url, scheme_source],
)

# Issues listed in full; the rest is summarised in one closing line.
MAX_ISSUES_SHOWN <- 200L

#' Label of a Picked File Selection
#'
#' @param paths Character vector of picked paths, or `NULL`.
#' @param empty Character text shown while nothing is picked.
#'
#' @return A tag naming the selection.
#' @export
picked_label <- function(paths, empty = "Nothing selected") {
  if (!length(paths)) {
    return(shiny$span(class = "setup-picked is-empty", empty))
  }
  name <- if (length(paths) == 1L) basename(paths) else sprintf("%d files", length(paths))
  shiny$span(
    class = "setup-picked",
    title = paste(paths, collapse = "\n"),
    shiny$span(class = "setup-picked_name", name),
    shiny$span(class = "setup-picked_dir", paste("in", dirname(paths[1])))
  )
}

#' Headline Figures
#'
#' @param values Named vector; names are the captions.
#' @param tones Character vector of tones (`""`, `"good"`, `"warn"`), recycled.
#'
#' @return A row of figure tiles.
#' @export
stat_row <- function(values, tones = "") {
  tones <- rep(tones, length.out = length(values))
  shiny$div(
    class = "setup-stats",
    lapply(seq_along(values), function(i) {
      value <- format(values[[i]], big.mark = ",")
      shiny$div(
        class = paste(
          "setup-stat",
          if (nzchar(tones[i])) paste0("is-", tones[i]),
          # A word ("None") instead of a figure is set smaller, so it fits.
          if (!grepl("^[0-9,.-]+$", value)) "is-text"
        ),
        shiny$div(class = "setup-stat_value", value),
        shiny$div(class = "setup-stat_label", names(values)[i])
      )
    })
  )
}

#' Message Box
#'
#' @param text Character message, or tags.
#' @param tone `"info"`, `"good"`, `"warn"` or `"bad"`.
#'
#' @return A tag.
#' @export
setup_note <- function(text, tone = "info") {
  icons <- c(
    info = "circle-info",
    good = "circle-check",
    warn = "triangle-exclamation",
    bad = "circle-xmark"
  )
  shiny$div(class = paste0("setup-note is-", tone), shiny$icon(icons[[tone]]), shiny$div(text))
}

#' List of Import Issues
#'
#' @description What an import left out and why, grouped by reason so a scheme
#'   with hundreds of rejected loci still reads at a glance.
#'
#' @param issues Data frame with `locus` and `problem`.
#'
#' @return A tag, or `NULL` when there is nothing to report.
#' @export
issue_list <- function(issues) {
  if (is.null(issues) || !nrow(issues)) {
    return(NULL)
  }
  groups <- split(issues$locus, issues$problem)
  shown <- 0L
  shiny$tagList(lapply(names(groups), function(problem) {
    loci <- groups[[problem]]
    room <- max(MAX_ISSUES_SHOWN - shown, 0L)
    shown <<- shown + length(loci)
    listed <- head(loci, min(room, 12L))
    more <- length(loci) - length(listed)
    shiny$div(
      class = "setup-issue",
      shiny$div(
        class = "setup-issue_title",
        shiny$tags$strong(format(length(loci), big.mark = ",")),
        " × ",
        problem
      ),
      if (length(listed)) {
        shiny$div(
          class = "setup-issue_loci",
          paste(listed, collapse = ", "),
          if (more > 0L) sprintf(" and %s more", format(more, big.mark = ","))
        )
      }
    )
  }))
}

#' Setup Card
#'
#' @description The card every setup screen is built from: the title and an
#'   optional hint question mark in the header, the content in the body.
#'
#' @param title Character heading.
#' @param ... Content tags.
#' @param hint Optional character text shown on hovering a question mark at the
#'   right end of the header.
#' @param class Optional extra class of the card.
#'
#' @return A `bslib::card()`.
#' @export
setup_card <- function(title, ..., hint = NULL, class = NULL) {
  bslib$card(
    class = paste(c("setup-card", class), collapse = " "),
    bslib$card_header(title, hint_icon(title, hint)),
    bslib$card_body(class = "setup-card_body", fillable = FALSE, ...)
  )
}

#' Section Within a Setup Card
#'
#' @param title Character subheading.
#' @param ... Content tags.
#' @param optional Logical. Mark the section as optional.
#'
#' @return A tag.
#' @export
setup_section <- function(title, ..., optional = FALSE) {
  shiny$div(
    class = "setup-section",
    shiny$div(
      class = "setup-section_title",
      title,
      if (optional) shiny$span(class = "setup-optional", "optional")
    ),
    ...
  )
}

#' Published Schemes List
#'
#' @description Rows of the scheme index as cards: scheme, provider, locus
#'   count, a link to the provider's download page and, when `use_input` is
#'   given, a button reporting the row's number to that Shiny input so the
#'   caller can fill in the scheme's details.
#'
#' @param rows Data frame from `scheme_index$schemes_for_species()`.
#' @param use_input Character namespaced input id, or `NULL` for no button.
#' @param scroll Logical. Bound the list's height and scroll within it, so the
#'   controls below it stay in view however many schemes are listed.
#'
#' @return A tag.
#' @export
scheme_links <- function(rows, use_input = NULL, scroll = FALSE) {
  shiny$div(
    class = paste("setup-schemes", if (scroll) "is-scroll"),
    lapply(seq_len(nrow(rows)), function(i) {
      src <- scheme_source(rows$provider[i])
      provider <- if (is.null(src)) rows$provider[i] else src$label
      shiny$div(
        class = "setup-scheme",
        shiny$div(
          class = "setup-scheme_main",
          shiny$div(
            class = "setup-scheme_head",
            shiny$span(class = "setup-scheme_type", rows$type[i]),
            shiny$span(class = "setup-scheme_name", rows$scheme[i])
          ),
          shiny$div(
            class = "setup-scheme_meta",
            paste(c(
              provider,
              if (!is.na(rows$loci[i])) sprintf("%s loci", format(rows$loci[i], big.mark = ",")),
              if (!is.na(rows$note[i])) rows$note[i]
            ), collapse = " · ")
          )
        ),
        shiny$div(
          class = "setup-scheme_actions",
          if (safe_url(rows$url[i])) {
            shiny$a(
              href = rows$url[i],
              target = "_blank",
              rel = "noopener",
              class = "btn btn-sm btn-outline-secondary",
              title = "Open the provider's download page in your browser",
              shiny$icon("arrow-up-right-from-square"),
              " Download page"
            )
          },
          if (!is.null(use_input)) {
            shiny$tags$button(
              type = "button",
              class = "btn btn-sm btn-outline-primary",
              title = "Fill in the provider, name and web page of this scheme",
              onclick = sprintf(
                "Shiny.setInputValue('%s', {i: %d, t: Date.now()}, {priority: 'event'})",
                use_input,
                i
              ),
              shiny$icon("pen-to-square"),
              " Use details"
            )
          }
        )
      )
    })
  )
}

#' Numbered Setup Task
#'
#' @description One task of a setup step, numbered in the order it is worked
#'   through. Its state (current, done, locked) is applied by `task_states()`.
#'
#' @param id Character namespaced element id.
#' @param number Integer position of the task in its step.
#' @param title Character heading.
#' @param ... Content tags.
#' @param optional Logical. Mark the task as optional.
#' @param locked_hint Character shown while the task is locked.
#' @param hint Optional character text shown on hovering a question mark at the
#'   right end of the header.
#'
#' @return A tag.
#' @export
setup_task <- function(
  id,
  number,
  title,
  ...,
  optional = FALSE,
  locked_hint = "Complete the previous task first.",
  hint = NULL
) {
  bslib$card(
    id = id,
    class = "setup-card setup-task",
    bslib$card_header(
      class = "setup-task_head",
      shiny$span(
        class = "setup-task_number",
        shiny$span(class = "setup-task_digit", number),
        shiny$span(class = "setup-task_check", shiny$icon("check"))
      ),
      shiny$span(class = "setup-task_title", title),
      if (optional) shiny$span(class = "setup-optional", "optional"),
      shiny$uiOutput(paste0(id, "_status"), inline = TRUE, class = "setup-task_status"),
      hint_icon(title, hint)
    ),
    bslib$card_body(
      class = "setup-task_lock",
      shiny$span(shiny$icon("lock"), " ", locked_hint)
    ),
    bslib$card_body(class = "setup-card_body setup-task_body", fillable = FALSE, ...)
  )
}

# Style rules per task state. Rendered as a style output rather than set as
# classes by one-shot messages: an output is replayed when its panel is rebuilt
# (the Database panels are, on every load), a class change is not.
TASK_STATE_CSS <- c(
  current = paste(
    "#%1$s { border-color: var(--bs-primary); }",
    "#%1$s .setup-task_number { background: var(--bs-primary); color: #fff;",
    "border-color: var(--bs-primary); }"
  ),
  done = paste(
    "#%1$s .setup-task_number { background: var(--bs-success); color: #fff;",
    "border-color: var(--bs-success); }",
    "#%1$s .setup-task_digit { display: none; }",
    "#%1$s .setup-task_check { display: inline-flex; }"
  ),
  locked = paste(
    "#%1$s > .setup-task_body { opacity: 0.4; pointer-events: none; user-select: none; }",
    "#%1$s .setup-task_lock { display: block; }",
    "#%1$s .setup-task_title { color: var(--bs-secondary-color); }"
  ),
  open = ""
)

#' Task State Output
#'
#' @description Renders the state of a step's tasks.
#'
#' @param states Function returning a named character vector: namespaced task
#'   ids mapped to `"current"`, `"done"`, `"locked"` or `"open"` (optional and
#'   untouched).
#'
#' @return A `shiny$renderUI()` for a `shiny$uiOutput()` placed in the step.
#' @export
task_states <- function(states) {
  shiny$renderUI({
    all_states <- states()
    # "open" has no rules; formatting its empty template would warn.
    st <- all_states[nzchar(TASK_STATE_CSS[all_states])]
    rules <- vapply(
      names(st),
      function(id) sprintf(TASK_STATE_CSS[[st[[id]]]], id),
      character(1)
    )
    # A locked task's controls are also made inert: dimming only blocks the
    # mouse, and Tab and Enter would still reach them.
    inert <- sprintf(
      paste0(
        "(function () { var b = document.querySelector('#%s > .setup-task_body');",
        " if (b) b.inert = %s; })();"
      ),
      names(all_states),
      ifelse(all_states == "locked", "true", "false")
    )
    shiny$tagList(
      shiny$tags$style(shiny$HTML(paste(rules, collapse = "\n"))),
      shiny$tags$script(shiny$HTML(paste(inert, collapse = "\n")))
    )
  })
}

#' Task Status Line
#'
#' @param text Character short status, or `NULL` for none.
#' @param tone `"good"`, `"warn"`, `"bad"` or `""`.
#'
#' @return A tag, or `NULL`.
#' @export
task_status <- function(text, tone = "") {
  if (is.null(text) || !length(text) || is.na(text) || !nzchar(text)) {
    return(NULL)
  }
  shiny$span(class = paste("setup-task_status-text", if (nzchar(tone)) paste0("is-", tone)), text)
}

#' Coverage List
#'
#' @description What an analysis covers, one line per category, marked as
#'   covered or not, with an optional detail under the label.
#'
#' @param items List of lists with `label`, `on` (logical) and optionally
#'   `detail`.
#'
#' @return A tag.
#' @export
coverage_list <- function(items) {
  shiny$div(
    class = "setup-coverage",
    lapply(items, function(it) {
      shiny$div(
        class = paste("setup-coverage_item", if (isTRUE(it$on)) "is-on" else "is-off"),
        shiny$icon(if (isTRUE(it$on)) "circle-check" else "circle-minus"),
        shiny$div(
          shiny$div(class = "setup-coverage_label", it$label),
          if (!is.null(it$detail)) shiny$div(class = "setup-coverage_detail", it$detail)
        )
      )
    })
  )
}

#' Plan List
#'
#' @description The steps of a procedure, numbered, each with a short detail;
#'   a step marked `done` shows a check instead of its number.
#'
#' @param steps List of lists with `title`, `detail` (text or tags) and
#'   optionally `done`.
#'
#' @return A tag.
#' @export
plan_list <- function(steps) {
  shiny$tags$ol(
    class = "setup-plan",
    lapply(seq_along(steps), function(i) {
      st <- steps[[i]]
      done <- isTRUE(st$done)
      shiny$tags$li(
        class = paste("setup-plan_item", if (done) "is-done"),
        shiny$span(
          class = "setup-plan_number",
          if (done) shiny$icon("check") else i
        ),
        shiny$div(
          shiny$div(class = "setup-plan_title", st$title),
          shiny$div(class = "setup-plan_detail", st$detail)
        )
      )
    })
  )
}
