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

  # Set fresh (not just relying on .Rprofile) so a browser is always
  # configured even in a session started before that default was set up.
  if (!nzchar(getOption("browser", ""))) {
    options(browser = "xdg-open")
  }

  # Build app.min.css
  rhino::build_sass()

  throw_msg("All is initiated. Launching ...")

  # Launch app
  shiny::runApp(launch.browser = TRUE)
}
