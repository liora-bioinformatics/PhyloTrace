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
  # Overridden even when a *function* is already there: RStudio and Positron
  # both preset `browser` to a closure that opens their own integrated Viewer
  # instead, and this dev launch wants the real external browser regardless -
  # see the matching (fuller) comment in .Rprofile.
  browser_opt <- getOption("browser", "")
  if (!is.character(browser_opt) || !nzchar(browser_opt)) {
    options(browser = "xdg-open")
  }

  # Build app.min.css
  rhino::build_sass()

  throw_msg("All is initiated. Launching ...")

  # Launch app
  shiny::runApp(launch.browser = TRUE)
}
