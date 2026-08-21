if (file.exists("dev/startShiny.R")) {
  source("dev/startShiny.R")
}

# Open the URL with the desktop's configured default browser, unless one is
# already deliberately configured (e.g. R_BROWSER=wslview on WSL, set by
# run_phylotrace.sh — R seeds `options("browser")` from that env var before
# .Rprofile ever runs). Overridden even when a *function* is already there:
# RStudio and Positron both preset `browser` to a closure that opens their own
# integrated Viewer instead, and this app wants the real external browser
# regardless — hence overriding on anything that isn't already a real,
# non-empty string, not just on empty/unset. (A bare `!nzchar(closure)` would
# error instead of just being FALSE, so the character check has to come
# first.)
# Set here (not just inside startShiny()) so it also applies when the app is
# launched some other way (e.g. RStudio's "Run App" button), which would
# otherwise hit shiny::runApp()'s default, unset `browser` option and fail.
browser_opt <- getOption("browser", "")
if (!is.character(browser_opt) || !nzchar(browser_opt)) {
  options(browser = "xdg-open")
}
rm(browser_opt)

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
