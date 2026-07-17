# app/logic/paths.R

box::use(
  fs[path_home, dir_ls],
  jsonlite[fromJSON],
)

#' @export
app_local_share_path <- file.path(
  path_home(),
  ".local",
  "share",
  "phylotrace"
)

if (!dir.exists(file.path(app_local_share_path))) {
  dir.create(file.path(app_local_share_path))
}

check_status_available <- function(local_share) {
  if (
    file.path(local_share, "state.json") %in%
      dir_ls(file.path(local_share))
  ) {
    return(
      fromJSON(
        file.path(
          local_share,
          "state.json"
        )
      )
    )
  }

  return(NULL)
}

#' @export
stat_json <- check_status_available(app_local_share_path)
