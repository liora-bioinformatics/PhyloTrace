shinyDirChoose_mod <- function(
  input,
  id,
  updateFreq = 0,
  session = getSession(),
  defaultPath = "",
  defaultRoot = NULL,
  allowDirCreate = TRUE,
  ...
) {
  # Access internal functions from shinyFiles
  dirGet <- shinyFiles:::dirGetter(...)
  fileGet <- shinyFiles:::fileGetter(...)
  dirCreate <- shinyFiles:::dirCreator(...)

  currentDir <- list()
  currentFiles <- NULL
  lastDirCreate <- NULL
  clientId <- session$ns(id)

  sendDirectoryData <- function(message) {
    req(input[[id]])
    tree <- input[[paste0(id, "-modal")]]
    createDir <- input[[paste0(id, "-newDir")]]

    if (!identical(createDir, lastDirCreate)) {
      if (allowDirCreate) {
        dirCreate(createDir$name, createDir$path, createDir$root)
        lastDirCreate <<- createDir
      } else {
        shiny::showNotification(
          shiny::p("Creating directories has been disabled."),
          type = "error"
        )
        lastDirCreate <<- createDir
      }
    }

    exist <- TRUE
    if (is_not(tree)) {
      # REPLACED .is_not(tree) with is_not(tree)
      dir <- list(
        tree = list(name = defaultPath, expanded = TRUE),
        root = defaultRoot
      )
      files <- list(dir = NA, root = tree$selectedRoot)
    } else {
      dir <- list(tree = tree$tree, root = tree$selectedRoot)
      files <- list(dir = unlist(tree$contentPath), root = tree$selectedRoot)
      passedPath <- list(list(...)$roots[tree$selectedRoot])
      exist <- dir.exists(do.call(path, c(passedPath, files$dir[-1])))
    }

    newDir <- do.call(dirGet, dir)

    if (is_not(files$dir)) {
      newDir$content <- NA
      newDir$contentPath <- NA
      newDir$writable <- FALSE
    } else {
      newDir$contentPath <- as.list(files$dir)
      files$dir <- paste0(files$dir, collapse = "/")

      # content <- do.call(fileGet, files)
      # newDir$content <- content$files
      newDir$content <- NULL # Skip loading files

      # newDir$writable <- content$writable
    }

    newDir$exist <- exist
    newDir$root <- files$root
    currentDir <<- newDir
    session$sendCustomMessage(message, list(id = clientId, dir = newDir))

    if (updateFreq > 0) {
      invalidateLater(updateFreq, session)
    }
  }

  observe({
    sendDirectoryData("shinyDirectories")
  })

  observeEvent(input[[paste0(id, "-refresh")]], {
    if (!is.null(input[[paste0(id, "-refresh")]])) {
      sendDirectoryData("shinyDirectories-refresh")
    }
  })
}
