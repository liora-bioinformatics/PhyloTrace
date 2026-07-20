if (file.exists("dev/startShiny.R")) {
  source("dev/startShiny.R")
}

# Open the URL with the desktop's configured default browser, unless one is
# already configured (e.g. R_BROWSER=wslview on WSL, set by run_phylotrace.sh).
# Set here (not just inside startShiny()) so it also applies when the app is
# launched some other way (e.g. RStudio's "Run App" button), which would
# otherwise hit shiny::runApp()'s default, unset `browser` option and fail.
if (!nzchar(getOption("browser", ""))) {
  options(browser = "xdg-open")
}

# Allow absolute module imports (relative to the app root).
options(box.path = getwd())

# box.lsp languageserver external hook
if (nzchar(system.file(package = "box.lsp"))) {
  options(
    languageserver.parser_hooks = list(
      "box::use" = box.lsp::box_use_parser
    )
  )
}
