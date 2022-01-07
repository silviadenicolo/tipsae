#' @export
runShiny_tipsae <- function() {
  appDir <- system.file("shiny_tipsae", package = "tipsae")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tipsae`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
