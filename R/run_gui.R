#' A point-and-click Shiny app to assist in nonlinear analysis
#' 
#' @details
#' While there are no explicit function arguments, the app does require the user to have a data frame in the global environment. 
#' 
#' Once the app is running, you can select the analysis type that you wish to use from the top tab menu and then select 
#' your data frame and the column you want to analyse using the dropdown menu on the left hand side. 
#' The analysis methods have suggested inputs for you to use however, it is strongly recommended that you use parameters specific for your analysis. 
#'
#' @import nonanR
#' @import ggplot2
#' @rawNamespace import(plotly, except = last_plot)
#' @import shiny
#' @import shinythemes
#' @import markdown
#' @importFrom dplyr row_number
#' 
#' @examples
#' # Create some mock data (or load in your own) to use in the app.
#' example_dat = data.frame("Index" = 1:1000, 
#'                          "amplitude_1" = fgn_sim(n = 1000, H = 0.8, mean = 1.2, std = 0.03), 
#'                          "amplitude_2" = fgn_sim(n = 1000, H = 0.7, mean = 1.25, std = 0.04))
#' 
#' 
#' \dontrun{ 
#' run_gui()
#' }
#'
#' @export
run_gui <- function() {
  appDir <- system.file("NONANapp", "NONANr_app.R", package = "nonanR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `nonanR`.", call. = FALSE)
  }
  
  #suppressWarnings(
   #suppressMessages(
     shiny::runApp(appDir, display.mode = "normal")
   #)
  #)
  
}