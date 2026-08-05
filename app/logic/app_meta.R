# app/logic/app_meta.R
#
# Identity of the running application: the single place its version is written
# down, so the badge in the navigation bar and the version stamped into every
# typing provenance row can never drift apart.

#' PhyloTrace Version
#'
#' @description Version of the running application, as shown in the navigation
#'   bar and recorded on every isolate typed by it.
#'
#' @return Character string, without a leading "v".
#' @export
APP_VERSION <- "1.6.1"
