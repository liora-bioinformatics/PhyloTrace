component_table <- function(export = FALSE, output = getwd()) {
  # Get required pkgs
  if (!require("renv")) {
    install.packages("renv")
  } else {
    renv::deactivate()
  }

  # Read lockfile
  lock <- renv::lockfile_read()

  # Initiate package metadata data frame
  pkg_df <- data.frame(
    Package = character(),
    Version = character(),
    Source = character(),
    Type = character(),
    Title = character(),
    License = character(),
    URL = character(),
    Author = character(),
    Maintainer = character(),
    Repository = character(),
    row.names = NULL
  )

  # Fill package metadata data frame
  for (pkg in seq_along(names(lock$Packages))) {
    
    get_meta <- function(meta) {
      meta_info <- unlist(lock$Packages[[pkg]][meta])
      if (is.null(meta_info)) {
        meta_info <- ""
      }
      return(meta_info)
    }

    Package <- 
    Version <- 
    Source <- 
    Type <- 
    Title <- 
    License <- 
    URL <- get_meta("URL")
    Author <- get_meta("Author")
    Maintainer <- get_meta("Maintainer")
    Repository <- get_meta("Repository")

    new_entry <- data.frame(
      get_meta("Package"),
      get_meta("Version"),
      get_meta("Source"),
      get_meta("Type"),
      get_meta("Title"),
      get_meta("License"),
      URL,
      Author,
      Maintainer,
      Repository
    )

    pkg_df <- rbind(pkg_df, new_entry, make.row.names = F)
  }

  if (export) {
    write.csv(pkg_df, file.path(output, "r_packages.csv"), col.names = FALSE)
  }

  return(pkg_df)
}

#component_table(export = TRUE)