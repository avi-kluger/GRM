#' Helper imports for namespace
#' @name pkg_helpers
#' @importFrom rlang .data
#' @importFrom grDevices png dev.off
#' @importFrom graphics abline
#' @importFrom utils head
NULL

# Declare global variables to silence notes
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("theta", "information", "item"))
}