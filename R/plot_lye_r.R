#' Lyapunov Exponent - Rosenstein Plot
#' 
#' Plot method for Lyapunov Exponent - Rosenstein. 
#' 
#' 
#' @param x The list object returned \code{LyE_R} in this package.
#' 
#' @examples
#' 
#' # Create a time series
#' x = rnorm(1000)
#' tau = 3 # You can get this value like: ami_out$tau[1,1]
#' dim = 4 # You can get this value like: fnn_out$dim
#' fs = 60
#'
#' lye_out = lye_r(x = x, tau = tau, dim = dim, fs = fs)
#' 
#' # Plot ami_out
#' plot_lye_r(lye_out)
#' 
#' @export
plot_lye_r = function(x){
  
  dat = as.data.frame(x[2]) # out data frame
  
  
  ggplot(dat, aes(x = dat[,1], y = dat[,3])) + 
    geom_line(linewidth = 1) +
    geom_label(label = "Automatic slope calculations are coming soon",
               x = nrow(dat)/2,
               y = mean(dat[,3]),
               label.size = NA,
               fill = "lightgray") +
    labs(title = "Lyapunov Exponent - Rosenstein", 
         x = "Matched Pair", 
         y = "Average Line Divergence") +
    theme_minimal()
}