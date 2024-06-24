#' Pseudoperiodic surrogate Plot
#' 
#' Plot method for Pseudoperiodic surrogates.
#' 
#' @param x Original time series
#' @param surrogate Pseudoperiodic surrogate
#'
#' @examples
#' # Generate example time series data
#' x = sin(seq(0, 2 * pi, length.out = 500))
#' 
#' # Set parameters
#' tau = 1
#' dim = 2
#' rho = 0.1
#' 
#' # Generate the Pseudo Periodic Surrogate
#' PseudoPeriodic_out = Surr_PseudoPeriodic(y, tau, dim, rho)
#' 
#' # Plot
#' plot_Surr_PseudoPeriodic(x, PseudoPeriodic_out)
#' 
#' @export
plot_Surr_PseudoPeriodic <- function(x, PseudoPeriodic_out) {
  
  # Adjust the length of the original series to match the surrogate series
  surrogate <- PseudoPeriodic_out$ys
  min_length <- min(length(x), length(surrogate))
  x <- x[1:min_length]
  surrogate <- surrogate[1:min_length]
  
  index <- 1:min_length
  data <- data.frame(index = index, original = x, surrogate = surrogate)
  data_long <- melt(data, id = "index")
  
  ggplot(data_long, aes(x = index, y = value, color = variable)) +
    geom_line() +
    labs(title = "Original and Pseudoperiodic Surrogate Time Series", x = "Time", y = "Amplitude") +
    theme_nonan() +
    theme(legend.position = "top") +
    scale_color_manual(values = c("black", "#C8102E"))
}
