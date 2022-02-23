#' Lauch Shiny App to Performs Small Area Estimation
#'
#' The command launches a Shiny application that assists the user from the data loading step to the export of the outputs. See the vignette for further details.
#'
#'
#'
#'@return
#'No value returned.
#'
#'
#'@examples
#'
#'library(tipsae)
#'
#' # Starting the Shiny application
#'if(interactive()){
#'  runShiny_tipsae()
#'}
#'
#'
#'
#' @export
runShiny_tipsae <- function() {
  appDir <- system.file("shiny_tipsae", package = "tipsae")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tipsae`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
