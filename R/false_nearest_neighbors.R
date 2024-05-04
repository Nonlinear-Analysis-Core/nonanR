#' False Nearest Neighbors
#'
#' Computes the false nearest neighbors for a given time series.
#'
#' @param data Time series data (numeric vector)
#' @param maxDim Maximum embedding dimension to consider
#' @param delay Time delay for the embedding
#' @param rtol Relative tolerance for the neighbor distance (often, 10)
#' @param atol Absolute tolerance for the neighbor distance (often, between 10 and 20, typically 15)
#' @param fnn_tol Proportion of false neighbors below which to choose dim
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{fnn}{Percentage of false nearest neighbors for each dimension}
#'   \item{dim}{Embedding dimension at which the percentage of FNN drops below fnn_tol}
#' }
#'
#' @importFrom RANN nn2
#'
#' @examples
#' # Generate example time series data
#' data <- rnorm(1000)
#'
#' # Compute false nearest neighbors
#' result <- false_nearest_neighbors(data, maxDim = 10, delay = 1, rtol = 10, atol = 15, fnn_tol = 0.01)
#'
#' # Print the results
#' print(result$fnn)
#' print(result$dim)
#'
#' @export
false_nearest_neighbors <- function(data, maxDim, delay, rtol, atol, fnn_tol) {
  # Initialize variables
  N <- length(data)
  fnn <- numeric(maxDim)
  
  # Compute false nearest neighbors for each dimension
  for (dim in 1:maxDim) {
    # Create the delay vectors
    Y <- matrix(0, nrow = N - (dim - 1) * delay, ncol = dim)
    for (i in 1:dim) {
      Y[, i] <- data[(1:(N - (dim - 1) * delay)) + (i - 1) * delay]
    }
    
    # Find the nearest neighbors for each point
    idx <- RANN::nn2(Y, Y, k = 2)$nn.idx
    
    # Compute the distances to the nearest neighbors
    D <- sqrt(rowSums((Y - Y[idx[, 2], ])^2))
    
    # Compute the distances in the next higher dimension
    if (dim < maxDim) {
      Y_next <- matrix(0, nrow = N - dim * delay, ncol = dim + 1)
      for (i in 1:(dim + 1)) {
        Y_next[, i] <- data[(1:(N - dim * delay)) + (i - 1) * delay]
      }
      
      # Compute the distances in the next higher dimension
      D_next <- numeric(nrow(Y_next))
      for (i in 1:nrow(Y_next)) {
        if (idx[i, 2] <= nrow(Y_next)) {
          D_next[i] <- sqrt(sum((Y_next[i, ] - Y_next[idx[i, 2], ])^2))
        } else {
          D_next[i] <- D[i]
        }
      }
    } else {
      D_next <- D
    }
    
    # Compute the false nearest neighbors
    fnn[dim] <- sum((D_next > rtol * D[1:length(D_next)]) | (D_next > atol)) / length(D_next)
  }
  
  # Find the embedding dimension at which the percentage of false neighbors drops below fnn_tol
  dim <- which(fnn < fnn_tol)[1]
  if (is.na(dim)) {
    dim <- maxDim
  }
  
  return(list(fnn = fnn, dim = dim))
}