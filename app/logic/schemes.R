# app/logic/schemes.R

box::use(
  utils[read.csv],
)

#' @export
cgmlst_org_schemes <- read.csv("app/logic/data/cgmlst_schemes.csv")
