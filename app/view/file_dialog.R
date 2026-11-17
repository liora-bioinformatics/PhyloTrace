# app/view/file_dialog.R
#
# The app's own file dialog: a modal that browses the machine PhyloTrace runs on
# to pick files, pick a folder or choose where to save a new file. Works the
# same on Linux, macOS, Windows and WSL (see app/logic/file_browser.R).

box::use(
  htmltools[HTML, htmlEscape],
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
#'   choose a folder and a new file name.
#' @param title Character modal title.
#' @param pattern Regular expression of the files the caller expects, or
#'   `NULL`. Matching files are emphasised; other files stay listed and
#'   pickable, since browsers save some downloads without an extension.
#' @param multiple Logical. Allow several files (`"file"` mode).
#' @param allow_dir Logical. In `"file"` mode, also accept a folder.
#' @param default_name Function returning the proposed file name (`"save"`).
#' @param extension Character extension appended to a saved file name.
#' @param hint Character help line shown under the listing, or `NULL`.
#'
#' @return A reactive holding the chosen absolute path(s), `NULL` until the
#'   user confirms a choice.
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
  hint = NULL
) {
  mode <- match.arg(mode)
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current <- shiny$reactiveVal(file_browser$home_dir())
    chosen <- shiny$reactiveVal(NULL)
    problem <- shiny$reactiveVal(NULL)
    refresh <- shiny$reactiveVal(0L)

    # Entries of the open folder; re-read on navigation and refresh.
    listing <- shiny$reactive({
      refresh()
      file_browser$list_dir(
        current(),
        files = mode != "dir",
        pattern = pattern,
        hidden = isTRUE(input$hidden)
      )
    })

    # Places in the sidebar, read when the dialog opens (drives can come and go).
    place_rows <- shiny$reactiveVal(NULL)

    # Moves the dialog to a folder, reporting a path that is not one.
    navigate <- function(path) {
      dir <- file_browser$normalize_dir(path)
      if (is.na(dir)) {
        problem(sprintf("'%s' is not an existing folder.", path))
        return(invisible(FALSE))
      }
      problem(NULL)
      current(dir)
      shiny$updateTextInput(session, "path", value = dir)
      invisible(TRUE)
    }

    # Opens the modal at the last folder used in this session.
    shiny$observeEvent(input$open, {
      place_rows(file_browser$places())
      if (is.na(file_browser$normalize_dir(current()))) {
        current(file_browser$home_dir())
      }
      # The folder is re-read on every opening: files can appear between two
      # visits (a download just finished), and the listing would otherwise be
      # cached for as long as the dialog reopens on the same folder.
      refresh(refresh() + 1L)
      problem(NULL)
      confirm_label <- switch(
        mode,
        file = if (multiple) "Select files" else "Select file",
        dir = "Select folder",
        save = "Save here"
      )
      shiny$showModal(shiny$div(
        class = "fb-modal",
        shiny$modalDialog(
          title = title,
          size = "xl",
          easyClose = TRUE,
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
                  shiny$textInput(ns("folder_name"), NULL, placeholder = "New folder"),
                  shiny$actionButton(
                    ns("new_folder"),
                    NULL,
                    icon = shiny$icon("folder-plus"),
                    title = "Create folder"
                  )
                )
              },
              shiny$checkboxInput(ns("hidden"), "Hidden files", value = FALSE)
            ),
            shiny$uiOutput(ns("crumbs"), class = "fb-crumbs"),
            shiny$div(
              class = "fb-main",
              shiny$uiOutput(ns("places"), class = "fb-places"),
              # The scroll box is a wrapper of its own: output containers are
              # laid out as `display: contents` app-wide, which cannot scroll.
              shiny$div(class = "fb-list-wrap", shiny$uiOutput(ns("listing")))
            ),
            if (mode == "save") {
              shiny$div(
                class = "fb-save",
                shiny$textInput(
                  ns("name"),
                  "File name",
                  value = default_name(),
                  placeholder = "Name of the new file"
                ),
                shiny$span(class = "fb-ext", paste0(".", extension))
              )
            },
            if (!is.null(hint)) shiny$div(class = "fb-hint", shiny$icon("circle-info"), " ", hint),
            shiny$uiOutput(ns("status"))
          ),
          footer = shiny$tagList(
            shiny$modalButton("Cancel"),
            if (mode == "file" && allow_dir) {
              shiny$actionButton(ns("use_dir"), "Use this folder", icon = shiny$icon("folder-open"))
            },
            shiny$actionButton(ns("confirm"), confirm_label, class = "btn-primary")
          )
        )
      ))
    })

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
        return(shiny$div(class = "fb-empty", "This folder is empty."))
      }
      modified <- format(rows$modified, "%Y-%m-%d %H:%M")
      modified[is.na(modified)] <- ""
      html <- sprintf(
        paste0(
          '<div class="fb-row%s" data-i="%d"><span class="fb-icon">%s</span>',
          '<span class="fb-name">%s</span><span class="fb-size">%s</span>',
          '<span class="fb-date">%s</span></div>'
        ),
        ifelse(rows$is_dir, " is-dir", ifelse(rows$match, " is-match", " is-other")),
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
        `data-multiple` = tolower(as.character(multiple && mode == "file")),
        HTML(paste(html, collapse = ""))
      )
    })

    # Problems and notices for the current folder and selection.
    output$status <- shiny$renderUI({
      notes <- c(
        problem(),
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

    # Entries currently selected in the listing.
    selected <- shiny$reactive({
      idx <- input$list_selected
      rows <- listing()
      idx <- idx[idx >= 1L & idx <= nrow(rows)]
      rows[idx, , drop = FALSE]
    })

    shiny$observeEvent(input$up, navigate(file_browser$parent_dir(current())))
    shiny$observeEvent(input$reload, refresh(refresh() + 1L))
    shiny$observeEvent(input$path, {
      if (!identical(input$path, current())) navigate(input$path)
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
      shiny$req(i >= 1L, i <= nrow(rows))
      if (rows$is_dir[i]) {
        navigate(rows$path[i])
      } else if (mode == "file") {
        finish(rows$path[i])
      }
    })

    # Hands a choice back to the caller and closes the dialog.
    finish <- function(paths) {
      chosen(normalizePath(paths, winslash = "/", mustWork = FALSE))
      shiny$removeModal()
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

    # Confirms the selection according to the dialog's mode.
    shiny$observeEvent(input$confirm, {
      sel <- selected()
      if (mode == "dir") {
        dirs <- sel$path[sel$is_dir]
        finish(if (length(dirs)) dirs[1] else current())
        return()
      }
      if (mode == "file") {
        picks <- sel$path[!sel$is_dir | allow_dir]
        if (!length(picks)) {
          problem("Select a file first (double-click a folder to open it).")
          return()
        }
        finish(if (multiple) picks else picks[1])
        return()
      }
      name <- trimws(input$name)
      issue <- file_browser$name_problem(name)
      target <- file.path(current(), paste0(name, ".", extension))
      if (!is.na(issue)) {
        problem(issue)
      } else if (!file_browser$dir_writable(current())) {
        problem("You cannot write to this folder. Choose another one.")
      } else if (file.exists(target)) {
        problem("A file with this name already exists here. Choose another name.")
      } else {
        finish(target)
      }
    })

    chosen
  })
}
