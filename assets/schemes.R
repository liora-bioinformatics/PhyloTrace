pubmlst_schemes <- read.csv("./assets/pubmlst_schemes.csv")[, -1] |>
  dplyr::mutate(
    species = paste0(trimws(species), "_PM"),
    database = "PubMLST"
  ) |>
  dplyr::arrange(species)

cgmlstorg_schemes <- read.csv("./assets/cgmlst_schemes.csv")[, -1] |>
  dplyr::mutate(
    species = paste0(trimws(species), "_CM"),
    database = "cgMLST.org"
  ) |>
  dplyr::arrange(species)

schemes <- dplyr::arrange(
  dplyr::add_row(pubmlst_schemes, cgmlstorg_schemes),
  species
)

amrfinder_species <- read.csv("./assets/amrfinder_species.csv")[, -1]
