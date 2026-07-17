throw_msg <- function(message) {
  message(
    format(Sys.time(), digits = 3L),
    " | ",
    "----- ",
    message
  )
}

startShiny <- function() {
  throw_msg("Running Dev Launch Protocol...")

  # Set browser
  paths <- c(
    "/usr/bin/brave-browser"
  )
  valid_browser <- paths[which(file.exists(paths))]
  options(
    browser = valid_browser
  )
  throw_msg(paste("Browser paths:", valid_browser))

  # Build app.min.css
  rhino::build_sass()

  # Restore packages from lockfile
  status <- renv::status()
  if (isFALSE(status)) {
    renv::restore()
  }

  throw_msg("All is initiated. Launching ...")

  # Launch app
  shiny::runApp(launch.browser = TRUE)
}
