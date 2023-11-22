#' This function allows you to run a shiny app that will do all the things for you
#' 
#' 
#' Once the app is running, you can select the analysis type that you want to use from the top tab menu and then select your data frame and the column you want to analyse using the options on the left hand side. 
#' The analysis methods have suggested inputs for you to use however, it is strongly recommended that you use parameters specific for your analysis. 
#'
#'
#'
#' @examples
#' # Create some mock data (or load in your own) to use in the app.
#' left_dat = data.frame("stride_number" = 1:1000, "stride_time_left" = rnorm(1000, 1.2, 0.03))
#' right_dat = data.frame("stride_number" = 1:1000, "stride_time_right" = rnorm(1000, 1.3, 0.05))
#' 
#' runGUI()
#'
#' @export
runGUI <- function() {
  appDir <- system.file("NONANapp", "NONANr_app.R", package = "NONANr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `NONANr`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}