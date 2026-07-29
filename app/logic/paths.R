# app/logic/paths.R
#
# Global system path definitions and state initialization.
# Resolves application configuration directories and loads persistent application state.

box::use(
  fs[path_home, dir_ls],
  jsonlite[fromJSON],
)

#' Base directory path for local application data storage (`~/.local/share/phylotrace`).
#' @export
app_local_share_path <- file.path(
  path_home(),
  ".local",
  "share",
  "phylotrace"
)

if (!dir.exists(file.path(app_local_share_path))) {
  dir.create(file.path(app_local_share_path), recursive = TRUE)
}

# Check for state.json in the local share directory and load JSON content if present
check_status_available <- function(local_share) {
  state_file <- file.path(local_share, "state.json")
  if (state_file %in% dir_ls(file.path(local_share))) {
    return(fromJSON(state_file))
  }

  return(NULL)
}

#' Active application state object loaded from state.json, or NULL if uninitialized.
#' @export
stat_json <- check_status_available(app_local_share_path)
