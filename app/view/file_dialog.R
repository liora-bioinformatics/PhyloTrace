# app/view/file_dialog.R
#
# The app's own file dialog: an overlay that browses the machine PhyloTrace runs
# on to pick files, pick a folder or choose where to save a file. Works the same
# on Linux, macOS, Windows and WSL (see app/logic/file_browser.R), and can open
# over another dialog.

box::use(
  fs[is_absolute_path],
  htmltools[HTML, htmlEscape],
  rlang[`%||%`],
  shiny,
)

box::use(
  app / logic / file_browser,
)

# Icon markup per entry kind, rendered once rather than per row.
ROW_ICONS <- list(
  dir = as.character(shiny$icon("folder")),
  file = as.character(shiny$icon("file")),
  fasta = as.character(shiny$icon("dna")),
  zip = as.character(shiny$icon("file-zipper")),
  table = as.character(shiny$icon("table"))
)

# Picks the icon of each listed entry from its kind and extension.
row_icons <- function(name, is_dir) {
  kind <- ifelse(
    is_dir,
    "dir",
    ifelse(
      grepl("\\.zip$", name, ignore.case = TRUE),
      "zip",
      ifelse(
        grepl("\\.(fasta|fas|fa|fna|ffn|fsa|tfa|seq)(\\.gz)?$", name, ignore.case = TRUE),
        "fasta",
        ifelse(grepl("\\.(csv|tsv|tab|txt|xlsx|xlsm)$", name, ignore.case = TRUE), "table", "file")
      )
    )
  )
  unlist(ROW_ICONS[kind], use.names = FALSE)
}

#' @export
ui <- function(id, label, icon_name = "folder-open", class = NULL, width = NULL) {
  ns <- shiny$NS(id)
  shiny$actionButton(
    ns("open"),
    label,
    icon = shiny$icon(icon_name),
    class = class,
    width = width
  )
}

#' File Dialog Server
#'
#' @param id Module id.
#' @param mode `"file"` to pick files, `"dir"` to pick a folder, `"save"` to
#'   choose a folder and a file name.
#' @param title Character dialog title.
#' @param pattern Regular expression of the files the caller expects, or
#'   `NULL`. Matching files are emphasised; other files stay listed and
#'   pickable, since browsers save some downloads without an extension. In
#'   `"dir"` mode files are listed for orientation only and cannot be picked.
#' @param multiple Logical. Allow several files (`"file"` mode).
#' @param allow_dir Logical. In `"file"` mode, also accept a folder.
#' @param default_name Function returning the proposed file name (`"save"`).
#' @param extension Character extension appended to a saved file name, or a
#'   function returning it (read each time the dialog opens).
#' @param hint Character help line shown under the listing, or `NULL`.
#' @param overwrite Logical. In `"save"` mode, allow replacing an existing
#'   file after a second confirmation; otherwise existing names are refused.
#' @param start Function returning the folder the dialog first opens in, or
#'   `NULL` for the home folder. Later openings return to the last folder.
#' @param open Reactive whose change opens the dialog, for callers that open
#'   it from code instead of (or besides) its own button.
#' @param reset Reactive whose change clears the choice (the result becomes
#'   `NULL`), e.g. the caller's `session_reset`.
#'
#' @return A reactive holding the chosen absolute path(s), `NULL` until the
#'   user confirms a choice. It signals every pick, also of the same path again.
#' @export
server <- function(
  id,
  mode = c("file", "dir", "save"),
  title = "Select",
  pattern = NULL,
  multiple = FALSE,
  allow_dir = FALSE,
  default_name = function() "",
  extension = "db",
  hint = NULL,
  overwrite = FALSE,
  start = NULL,
  open = NULL,
  reset = NULL
) {
  mode <- match.arg(mode)
  ext_of <- if (is.function(extension)) extension else function() extension
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current <- shiny$reactiveVal(file_browser$home_dir())
    # The confirmed choice, with a counter so that picking the same path again
    # still counts as a new pick (a file fixed on disk is read again).
    chosen <- shiny$reactiveVal(list(paths = NULL, n = 0L))
    problem <- shiny$reactiveVal(NULL)
    refresh <- shiny$reactiveVal(0L)
    # Read when the dialog opens: a caller's extension can follow its inputs.
    ext <- shiny$reactiveVal("")
    # Numbers every listing; selections and double-clicks carry the number of
    # the listing they were made in, so one from a folder already left is
    # never applied to the folder now on screen.
    listing_no <- 0L
    # The overlay is added to the page on first use and kept: outside Shiny's
    # single modal slot it can open over another dialog, and another dialog
    # opening does not close it.
    inserted <- FALSE
    opened_once <- FALSE
    # A save target that exists and was confirmed once; confirming the same
    # target again replaces it (another name or folder asks anew).
    replace_pending <- NULL
    last_default <- NULL

    # Entries of the open folder; re-read on navigation and refresh. A folder
    # that cannot be listed yields no entries and a problem, never an error.
    listing <- shiny$reactive({
      refresh()
      listing_no <<- listing_no + 1L
      rows <- tryCatch(
        # Hidden entries are never listed; a hidden folder can still be
        # opened by typing its path. Folder mode lists files too, inert, so a
        # folder of the expected files does not look empty.
        file_browser$list_dir(current(), files = TRUE, pattern = pattern),
        error = function(e) {
          structure(
            file_browser$list_dir(tempfile(), files = FALSE),
            problem = "This folder cannot be listed."
          )
        }
      )
      structure(rows, no = listing_no)
    })

    # Places in the sidebar, read when the dialog opens (drives can come and go).
    place_rows <- shiny$reactiveVal(NULL)

    # The overlay's markup: header, toolbar, listing, name field and buttons.
    dialog_ui <- function() {
      confirm_label <- switch(
        mode,
        file = if (multiple) "Select files" else "Select file",
        dir = "Select folder",
        save = "Save here"
      )
      shiny$div(
        id = ns("dialog"),
        # `modal` for Bootstrap's dialog styling variables; shown and hidden by
        # file-browser.js, not by Bootstrap.
        class = "modal fb-overlay",
        role = "dialog",
        `aria-modal` = "true",
        `aria-label` = title,
        shiny$div(
          class = "modal-dialog modal-xl",
          shiny$div(
            class = "modal-content fb-modal",
            shiny$div(
              class = "modal-header",
              shiny$h4(class = "modal-title", title),
              shiny$tags$button(
                type = "button",
                class = "btn-close fb-close",
                `aria-label` = "Close"
              )
            ),
            shiny$div(
              class = "modal-body",
              shiny$div(
                class = "fb",
                shiny$div(
                  class = "fb-toolbar",
                  shiny$actionButton(
                    ns("up"),
                    NULL,
                    icon = shiny$icon("arrow-up"),
                    title = "Parent folder"
                  ),
                  shiny$div(
                    class = "fb-path allow-free-text",
                    shiny$textInput(
                      ns("path"),
                      NULL,
                      value = current(),
                      width = "100%",
                      updateOn = "blur"
                    )
                  ),
                  shiny$actionButton(
                    ns("reload"),
                    NULL,
                    icon = shiny$icon("rotate"),
                    title = "Refresh"
                  ),
                  if (mode != "file") {
                    shiny$div(
                      class = "fb-newfolder",
                      `data-enter-click` = ns("new_folder"),
                      shiny$textInput(ns("folder_name"), NULL, placeholder = "New folder"),
                      shiny$actionButton(
                        ns("new_folder"),
                        NULL,
                        icon = shiny$icon("folder-plus"),
                        title = "Create folder"
                      )
                    )
                  }
                ),
                shiny$uiOutput(ns("crumbs"), class = "fb-crumbs"),
                shiny$div(
                  class = "fb-main",
                  shiny$uiOutput(ns("places"), class = "fb-places"),
                  # The scroll box is a wrapper of its own: output containers
                  # are laid out as `display: contents` app-wide, which cannot
                  # scroll.
                  shiny$div(class = "fb-list-wrap", shiny$uiOutput(ns("listing")))
                ),
                if (mode == "save") {
                  shiny$div(
                    class = "fb-save",
                    `data-enter-click` = ns("confirm"),
                    shiny$textInput(
                      ns("name"),
                      "File name",
                      value = "",
                      placeholder = "Name of the file"
                    ),
                    shiny$span(class = "fb-ext", shiny$textOutput(ns("ext"), inline = TRUE))
                  )
                },
                if (!is.null(hint)) {
                  shiny$div(class = "fb-hint", shiny$icon("circle-info"), " ", hint)
                }
              )
            ),
            # Problems are shown in the footer, beside the buttons, so a message
            # appearing never changes the height of the dialog.
            shiny$div(
              class = "modal-footer",
              shiny$uiOutput(ns("status"), class = "fb-status-slot"),
              shiny$tags$button(type = "button", class = "btn btn-default fb-close", "Cancel"),
              if (mode == "file" && allow_dir) {
                shiny$actionButton(
                  ns("use_dir"),
                  "Use this folder",
                  icon = shiny$icon("folder-open")
                )
              },
              shiny$actionButton(ns("confirm"), confirm_label, class = "btn-primary")
            )
          )
        )
      )
    }

    # Opens or closes the overlay in the browser (app/js/file-browser.js).
    set_open <- function(show) {
      session$sendCustomMessage(
        "fb-dialog",
        list(id = ns("dialog"), show = show, focus = ns(if (mode == "save") "name" else "path"))
      )
    }

    # Moves the dialog to a folder, reporting a path that is not one.
    navigate <- function(path) {
      # A relative path is taken from the folder on screen, not from the
      # folder the app runs in.
      path <- trimws(path %||% "")
      if (nzchar(path) && !startsWith(path, "~") && !is_absolute_path(path)) {
        path <- file.path(current(), path)
      }
      dir <- file_browser$normalize_dir(path)
      if (is.na(dir)) {
        problem(sprintf("'%s' is not an existing folder that can be opened here.", path))
        return(invisible(FALSE))
      }
      problem(NULL)
      replace_pending <<- NULL
      current(dir)
      shiny$updateTextInput(session, "path", value = dir)
      invisible(TRUE)
    }

    # Opens the dialog: the start folder the first time, afterwards the last
    # folder used in this session.
    show_dialog <- function() {
      if (!opened_once && is.function(start)) {
        first <- file_browser$normalize_dir(start() %||% NA_character_)
        if (!is.na(first)) current(first)
      }
      opened_once <<- TRUE
      if (is.na(file_browser$normalize_dir(current()))) {
        current(file_browser$home_dir())
      }
      place_rows(file_browser$places())
      ext(ext_of())
      if (!inserted) {
        shiny$insertUI("body", where = "beforeEnd", ui = dialog_ui(), immediate = TRUE)
        inserted <<- TRUE
      }
      # The folder is re-read on every opening: files can appear between two
      # visits (a download just finished), and the listing would otherwise be
      # cached for as long as the dialog reopens on the same folder.
      refresh(refresh() + 1L)
      problem(NULL)
      replace_pending <<- NULL
      shiny$updateTextInput(session, "path", value = current())
      # A name typed earlier is kept; a proposal follows the caller's default.
      if (mode == "save") {
        typed <- shiny$isolate(input$name)
        proposal <- default_name()
        if (is.null(typed) || !nzchar(typed) || identical(typed, last_default)) {
          shiny$updateTextInput(session, "name", value = proposal)
        }
        last_default <<- proposal
      }
      set_open(TRUE)
    }

    shiny$observeEvent(input$open, show_dialog())
    if (!is.null(open)) {
      shiny$observeEvent(open(), show_dialog(), ignoreInit = TRUE)
    }
    # Forgets the choice, so the caller starts over without a picked path.
    if (!is.null(reset)) {
      shiny$observeEvent(
        reset(),
        chosen(list(paths = NULL, n = chosen()$n + 1L)),
        ignoreInit = TRUE
      )
    }

    output$ext <- shiny$renderText(paste0(".", ext()))

    # Sidebar of quick-access places.
    output$places <- shiny$renderUI({
      rows <- place_rows()
      shiny$req(rows)
      shiny$tagList(lapply(seq_len(nrow(rows)), function(i) {
        shiny$tags$a(
          href = "#",
          class = paste("fb-nav fb-place", if (identical(rows$path[i], current())) "is-active"),
          `data-input` = ns("nav"),
          `data-kind` = "place",
          `data-i` = i,
          shiny$icon(rows$icon[i]),
          shiny$span(rows$label[i])
        )
      }))
    })

    # Clickable path segments of the open folder.
    output$crumbs <- shiny$renderUI({
      crumbs <- file_browser$path_crumbs(current())
      shiny$tagList(lapply(seq_len(nrow(crumbs)), function(i) {
        shiny$tagList(
          if (i > 1L) shiny$span(class = "fb-crumb-sep", shiny$icon("chevron-right")),
          shiny$tags$a(
            href = "#",
            class = "fb-nav fb-crumb",
            `data-input` = ns("nav"),
            `data-kind` = "crumb",
            `data-i` = i,
            crumbs$label[i]
          )
        )
      }))
    })

    # Folder listing, built as one HTML string: a folder can hold thousands of
    # entries, and every name is escaped before it reaches the markup.
    output$listing <- shiny$renderUI({
      rows <- listing()
      if (!nrow(rows)) {
        return(shiny$div(class = "fb-empty", attr(rows, "problem") %||% "This folder is empty."))
      }
      modified <- format(rows$modified, "%Y-%m-%d %H:%M")
      modified[is.na(modified)] <- ""
      html <- sprintf(
        paste0(
          '<div class="fb-row%s" data-i="%d"><span class="fb-icon">%s</span>',
          '<span class="fb-name">%s</span><span class="fb-size">%s</span>',
          '<span class="fb-date">%s</span></div>'
        ),
        paste0(
          ifelse(rows$is_dir, " is-dir", ifelse(rows$match, " is-match", " is-other")),
          ifelse(!rows$is_dir & mode == "dir", " is-inert", "")
        ),
        seq_len(nrow(rows)),
        row_icons(rows$name, rows$is_dir),
        htmlEscape(rows$name),
        ifelse(rows$is_dir, "", file_browser$format_size(rows$size)),
        modified
      )
      # pt-no-lock: selecting a row reports to the server, and the input shield
      # (busy-shield.js) would otherwise swallow the second click of a
      # double-click while that round trip is in flight.
      shiny$div(
        class = "fb-list pt-no-lock",
        `data-input` = ns("list"),
        `data-no` = attr(rows, "no"),
        `data-multiple` = tolower(as.character(multiple && mode == "file")),
        HTML(paste(html, collapse = ""))
      )
    })

    # Problems and notices for the current folder and selection.
    output$status <- shiny$renderUI({
      notes <- c(
        problem(),
        attr(listing(), "problem"),
        if (isTRUE(attr(listing(), "truncated"))) {
          "Only the first 5,000 entries are shown; type a path to go deeper."
        },
        if (mode != "file" && !file_browser$dir_writable(current())) {
          "You cannot write to this folder."
        }
      )
      if (length(notes)) {
        shiny$div(
          class = "fb-status",
          lapply(notes, function(n) shiny$div(shiny$icon("triangle-exclamation"), " ", n))
        )
      }
    })

    # Entries currently selected in the listing on screen.
    selected <- shiny$reactive({
      sel <- input$list_selected
      rows <- listing()
      idx <- if (identical(as.integer(sel$no), attr(rows, "no"))) as.integer(unlist(sel$i)) else 0L
      idx <- idx[idx >= 1L & idx <= nrow(rows)]
      rows[idx, , drop = FALSE]
    })

    shiny$observeEvent(input$up, navigate(file_browser$parent_dir(current())))
    shiny$observeEvent(input$reload, refresh(refresh() + 1L))
    # A typed path opens a folder; in file mode, a typed file path picks it.
    shiny$observeEvent(input$path, {
      typed <- absolute(input$path)
      if (mode == "file" && file.exists(typed) && !dir.exists(typed)) {
        finish(typed)
      } else if (!identical(input$path, current())) {
        navigate(input$path)
      }
    })

    # A click on a place or a breadcrumb.
    shiny$observeEvent(input$nav, {
      nav <- input$nav
      target <- if (identical(nav$kind, "place")) {
        place_rows()$path[nav$i]
      } else {
        file_browser$path_crumbs(current())$path[nav$i]
      }
      if (length(target) == 1L && !is.na(target)) navigate(target)
    })

    # Double-click: open a folder, or take a file straight away.
    shiny$observeEvent(input$list_open, {
      rows <- listing()
      i <- input$list_open$i
      shiny$req(
        identical(as.integer(input$list_open$no), attr(rows, "no")),
        i >= 1L,
        i <= nrow(rows)
      )
      if (rows$is_dir[i]) {
        navigate(rows$path[i])
      } else if (mode == "file") {
        finish(rows$path[i])
      }
    })

    # A path as typed, absolute: relative to the folder on screen, `~`
    # expanded. Links are not resolved, so a linked file keeps its own name,
    # which is what locus and isolate names are taken from.
    absolute <- function(paths) {
      paths <- path.expand(trimws(paths))
      relative <- !is_absolute_path(paths)
      paths[relative] <- file.path(current(), paths[relative])
      if (file_browser$on_windows()) gsub("\\", "/", paths, fixed = TRUE) else paths
    }

    # Hands a choice back to the caller and closes the dialog.
    accept <- function(paths) {
      chosen(list(paths = paths, n = chosen()$n + 1L))
      set_open(FALSE)
      invisible(TRUE)
    }

    # Checks picked files or folders, then hands them back.
    finish <- function(paths) {
      if (!all(validUTF8(paths))) {
        problem("This name contains characters that cannot be read. Rename the file or folder.")
        return(invisible(FALSE))
      }
      paths <- absolute(paths)
      if (!all(file.exists(paths))) {
        problem("The selection no longer exists. Refresh the folder.")
        return(invisible(FALSE))
      }
      if (!all(file_browser$within_root(paths))) {
        problem("This location cannot be used here.")
        return(invisible(FALSE))
      }
      accept(paths)
    }

    # Creates the named folder inside the open folder and opens it.
    shiny$observeEvent(input$new_folder, {
      created <- tryCatch(
        file_browser$create_folder(current(), trimws(input$folder_name)),
        error = function(e) e
      )
      if (inherits(created, "error")) {
        problem(conditionMessage(created))
      } else {
        shiny$updateTextInput(session, "folder_name", value = "")
        navigate(created)
      }
    })

    shiny$observeEvent(input$use_dir, finish(current()))

    # Checks a save target: a valid name in a writable folder, and an existing
    # file only when replacing is allowed and confirmed a second time.
    save_target <- function() {
      name <- trimws(input$name %||% "")
      issue <- file_browser$name_problem(name)
      target <- file.path(current(), paste0(name, ".", ext()))
      if (!is.na(issue)) {
        problem(issue)
      } else if (!file_browser$dir_writable(current())) {
        problem("You cannot write to this folder. Choose another one.")
      } else if (dir.exists(target)) {
        problem("A folder has this name. Choose another name.")
      } else if (file.exists(target) && !overwrite) {
        problem("A file with this name already exists here. Choose another name.")
      } else if (file.exists(target) && !identical(replace_pending, target)) {
        replace_pending <<- target
        problem(sprintf(
          "'%s' already exists. Click '%s' again to replace it.",
          basename(target),
          "Save here"
        ))
      } else {
        accept(target)
      }
    }

    # Confirms the selection according to the dialog's mode.
    shiny$observeEvent(input$confirm, {
      if (mode == "save") {
        return(save_target())
      }
      sel <- selected()
      if (mode == "dir") {
        dirs <- sel$path[sel$is_dir]
        finish(if (length(dirs)) dirs[1] else current())
        return()
      }
      picks <- sel$path[!sel$is_dir | allow_dir]
      if (!length(picks)) {
        problem("Select a file first (double-click a folder to open it).")
        return()
      }
      finish(if (multiple) picks else picks[1])
    })

    # A reactive, unlike the stored value, signals every pick, also of the
    # same path again.
    shiny$reactive(chosen()$paths)
  })
}
