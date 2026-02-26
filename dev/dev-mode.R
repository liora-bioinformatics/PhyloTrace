startShiny <- function() {
  paths <- c(
    "/usr/bin/brave-browser"
  )

  options(
    browser = paths[which(file.exists(paths))]
  )

  shiny::runApp("App.R", launch.browser = T)
}
