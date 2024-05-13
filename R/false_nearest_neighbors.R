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
#' @details
#' Finding the embedding dimensions is typically the second step in the phase space reconstruction. This can be done using the False Nearest Neighbors which minimizes the number of false neighbors as the attractor (input time series) is unfolded at increasingly higher and higher dimensions. In the package, FNN follows the calculation of average mutual information (AMI), where the output tau serves as an input into this function. 
#' 
#' The False Nearest Neighbors algorithm calculates the distance between a vector and its nearest neighbor in an m-dimensional space, repeating the process for each subsequent dimension by incrementing m by 1. True neighbors exhibit minimal change in distance between dimensions d and d+1. The algorithm then computes the ratio of the difference in distances between vectors at dimensions d and d+1 to the distance at dimension d. If this ratio exceeds a predefined threshold, `rtol`, the vector is determined to be a false neighbor. As the dimension increases, the percentage of false neighbors should decrease as the dynamics of the attractor unfold. The first dimension where the percentage drops to 0 (or very close to it) is chosen as the optimal embedding dimension for the data.
#' 
#' The calculation for False Nearest Neighbors can be represented by the following equation:
#' 
#' \eqn{\frac{||\hat{V}(t) - \hat{V}^{NN}t||^2 - || V(t) - V^{NN}(t)||^2} {||V(t) - V^{NN}(t)||} >R_{tol}}
#' 
#' Where \eqn{||V(t) – V(t)^{NN}||} is the distance between two vectors, and \eqn{||\hat{V}(t) – \hat{V}(t)^{NN}||} is the distance between the vector and its neighbor in d+1 dimensional space. If the vectors truly are neighbors, the results of \eqn{||\hat{V}(t) - \hat{V}^{NN}t||^2 - || V(t) - V^{NN}(t)||^2} should be constant. If a false neighbor is detected and is greater than \eqn{R_{tol} = 15} then the vector is determined to be a false neighbor.
#' 
#'
#' @examples
#' # Generate example time series data
#' x = rnorm(1000)
#' 
#' # Function paramters
#' maxDim = 10
#' delay = 1 # You can get this value like: ami_out$tau[1,1]
#' rtol = 10
#' atol = 15
#' fnn_tol = 0.01
#'
#' # Compute false nearest neighbors
#' fnn_out = false_nearest_neighbors(x, maxDim = maxDim, delay = delay, rtol = rtol, atol = atol, fnn_tol = fnn_tol)
#'
#' @references
#' Kennel, M. B., & Abarbanel, H. D. I. (2002). False neighbors and false strands: A reliable minimum embedding dimension algorithm. Phys. Rev. E, 66(2), 026209. https://doi.org/10.1103/PhysRevE.66.026209
#' 
#' Raffalt, P. C., Senderling, B., & Stergiou, N. (2020). Filtering affects the calculation of the largest Lyapunov exponent. Computers in Biology and Medicine, 122, 103786. https://doi.org/10.1016/j.compbiomed.2020.103786
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