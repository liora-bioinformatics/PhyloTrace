startShiny <- function() {
  options(
    browser = "/usr/bin/brave-browser"
  )
  shiny::runApp("App.R", launch.browser = T)
}
