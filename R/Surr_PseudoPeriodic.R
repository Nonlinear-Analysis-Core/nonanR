#' Pseudo-Periodic Surrogate Time Series Generation
#'
#' Generate a surrogate time series for a periodic signal that destroys the dynamics between cycles and preserves dynamics within cycles.

#'
#' @param y Numeric vector representing the original time series.
#' @param tau Integer representing the time lag for phase space reconstruction.
#' @param dim Integer representing the embedding dimension for phase space reconstruction.
#' @param rho Numeric value representing the noise radius.
#' 
#' @return A list containing:
#'   \item{ys}Numeric vector representing the surrogate time series.
#'   \item{yi}Integer vector representing the selected indexes for the surrogate from the original time series.
#' 
#' @details This function produces one pseudo-periodic surrogate time series, which is appropriate for removing long-term correlations in periodic time series. This is useful for testing the presence of chaos or various nonlinear analysis methods. There may be an optimal value of rho that can be found using a different function or specified manually. If rho is too low (e.g., <0.01), the function may not be able to find a neighbor.
#' 
#' 
#' @references Small, M., Yu, D., & Harrison, R. G. (2001). Surrogate Test for Pseudoperiodic Time Series Data. Physical Review Letters, 87(18). https://doi.org/10.1063/1.1487534
#' 
#' @examples
#' # Generate a sine wave
#' y = sin(seq(0, 2 * pi, length.out = 500))
#' 
#' # Set parameters
#' tau = 1
#' dim = 2
#' rho = 0.1
#' 
#' # Generate the Pseudo Periodic Surrogate
#' result = Surr_PseudoPeriodic(y, tau, dim, rho)
#' ys = result$ys
#' yi = result$yi
#' 
#' @export
Surr_PseudoPeriodic <- function(y, tau, dim, rho) {
  if (!is.numeric(y)) {
    stop("Input time series y must be numeric.")
  }
  
  # Phase space reconstruction
  N <- length(y)
  Y <- matrix(0, nrow = N - (dim - 1) * tau, ncol = dim)
  for (i in 1:dim) {
    Y[, i] <- y[seq(1 + (i - 1) * tau, N - (dim - i) * tau)]
  }
  
  # Seeding and initial points
  lenY <- nrow(Y)
  xi <- sample(1:lenY, 1)
  ys <- numeric(lenY)
  ys[1] <- y[xi]
  yi <- integer(lenY)
  yi[1] <- xi
  
  M <- lenY - 2
  
  # Construct the surrogate
  for (i in 2:lenY) {
    # Calculates the distance from the previous point to all other points.
    # This is the probability calculation in Small, 2001. Points that are 
    # close neighbors will end up with a higher value.
    prob <- exp(-sqrt(rowSums((Y[1:M, ] - matrix(Y[xi, ], nrow = M, ncol = dim, byrow = TRUE))^2)) / rho)
    # A self-match will be exp(0)=1, which can be large compared to the
    # other values. It could be removed. Adding in this line appears to
    # produce decent surrogates but makes the optimization method
    # un-applicable.
    #     prob(xi)=0
    # Cummulative sum of the probability
    sum3 <- cumsum(prob)
    # A random number is chosen between 0 and the cummulative probability.
    # Where it goes above the cumsum that is chosen as the next point, +2.
    # Most of the values in prob have a very small value, the close
    # neighbors are the spikes.
    xi_n <- integer(0)
    ind <- 0
    while (length(xi_n) == 0 || xi_n > lenY || xi_n == xi) {
      a <- runif(1)
      xi_n <- which(sum3 < (a * sum3[length(sum3)]))
      if (length(xi_n) > 0) {
        xi_n <- xi_n[length(xi_n)] + 2
      }
      ind <- ind + 1
      if (ind == 100) {
        stop("A new value of xi could not be found, check that rho is not too low")
      }
    }
    xi <- xi_n
    # Add the new point to the surrogate time series.
    ys[i] <- y[xi]
    yi[i] <- xi
  }
  
  list(ys = ys, yi = yi)
}