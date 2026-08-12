# app/logic/schemes.R

box::use(
  utils[read.csv],
)

#' Static cgMLST.org Scheme Metadata
#'
#' @description Pre-loaded data frame mapping species names to their corresponding
#'   cgMLST.org schema abbreviations and URL endpoints.
#'
#' @return A `data.frame` containing species and abbreviation mapping rules.
#' @export
# box::file() resolves against this module's own directory, so the table also
# loads when the working directory is not the app root (testthat runs in
# tests/testthat).
cgmlst_org_schemes <- read.csv(box::file("data", "cgmlst_schemes.csv"))
