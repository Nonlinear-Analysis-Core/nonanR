#' IAAFFT Surrogates Plot
#' 
#' Plot method for Pseudoperiodic surrogates.
#' 
#' @param x Original time series
#' @param surrogate Pseudoperiodic surrogate
#'
#' @examples
#' # Generate example time series data
#' x <- sin(seq(0, 2 * pi, length.out = 500))
#' 
#' # Pseudoperiodic surrogate
#' PseudoPeriodic_out  <- Surr_PseudoPeriodic(x, tau, dim, rho)
#' 
#' # Plot
#' plot_Surr_PseudoPeriodic(x, PseudoPeriodic_out$ys)
#' 
#' @export
plot_Surr_PseudoPeriodic <- function(x, surrogate) {
  require(ggplot2)
  require(reshape2)
  # Adjust the length of the original series to match the surrogate series
  min_length <- min(length(x), length(surrogate))
  x <- x[1:min_length]
  surrogate <- surrogate[1:min_length]
  
  index <- 1:min_length
  data <- data.frame(index = index, original = x, surrogate = surrogate)
  data_long <- melt(data, id = "index")
  
  ggplot(data_long, aes(x = index, y = value, color = variable)) +
    geom_line() +
    labs(title = "Original and Pseudoperiodic Surrogate Time Series", x = "Time", y = "Amplitude") +
    theme_minimal() +
    theme(legend.position = "top")
}
