#' False Nearest Neighbors Plot
#' 
#' Plot method for false nearest neighbors.
#' 
#' @param x The list object returned \code{fnn} in this package.
#' 
#' @examples
#' 
#' # When performing phase space reconstruction there are two crucial steps. 
#' # - The first step is running the ami function. 
#' 
#' #' # Create a time series
#' x = fgn_sim(n = 1000, H = 0.9)
#' 
#' # Specify the parameters for ami
#' y = x
#' L = 50
#' bins = 30
#' 
#' ami_out = ami(x, y, L, bins)
#' 
#' # - The second step is running the fnn function
#' # Specify the parameters for fnn
#' tau = 3 # You can get this value like: ami_out$tau[1,1]
#' mmax = 12
#' rtol = 15
#' atol = 2
#' 
#' fnn_out = fnn(x = x, tau = tau, mmax = mmax, rtol = rtol, atol = atol)
#' 
#' # Plot fnn_out
#' plot_fnn(fnn_out)
#' 
#' ## Second example
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
#' @export
plot_fnn = function(x){
  
  dat = as.data.frame(x[1]) # dE data frame
  
  #emb_dim = as.numeric(x[2]) # embedding dimension
  emb_dim = cbind.data.frame(as.numeric(x[2]), dat[as.numeric(x[2]),])
  
  ggplot(dat, aes(x = 1:nrow(dat), y = dat[,1] * 100)) + 
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = emb_dim, aes(x = emb_dim[1,1], y = emb_dim[1,2]), size = 4, color = "#C8102E") + 
    scale_x_continuous(breaks = pretty_breaks(n = nrow(dat) + 1)) +
    geom_label(label = paste0("dim = ", emb_dim[1,1],  "\n", "%FNN = ", round(emb_dim[1,2], 3)), 
               x = nrow(dat) - 1, 
               y = 30, 
               label.size = NA, 
               fill = "lightgray") +
    labs(title = "False Nearest Neighbors", 
         x = "Lag", 
         y = "% False Neighbors") + 
    theme_nonan()
}
