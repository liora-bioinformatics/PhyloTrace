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
cgmlst_org_schemes <- read.csv("app/logic/data/cgmlst_schemes.csv")
