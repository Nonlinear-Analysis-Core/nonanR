#' This function allows you to run a shiny app that will do all the things for you
#'
#'
#'
#'
#'
#' @export
runGUI <- function() {
  appDir <- system.file("NONANapp", "NONANr_app.R", package = "NONANr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `NONANr`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}