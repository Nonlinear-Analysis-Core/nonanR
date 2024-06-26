#' Average Mutual Information Plot
#' 
#' Plot method for average mutual information. 
#' 
#' @param x The list object returned \code{ami} in this package.
#' 
#' @import ggplot2
#' @importFrom nonanR theme_nonan
#' 
#' @examples
#' 
#' # Create a time series
#' x = fgn_sim(n = 1000, H = 0.9)
#' y = x
#' L = 50
#' bins = 30
#' 
#' ami_out = ami(x, y, L, bins)
#' 
#' # Plot ami_out
#' plot_ami(ami_out)
#' 
#' @export
plot_ami = function(x){
  
  dat = as.data.frame(x[2]) # ami data frame
  
  tau = as.data.frame(x[1]) # tau data frame
  min_tau = tau[1,] # Get the first tau value which is the one most commonly used
  
  
  ggplot(dat, aes(x = dat[,1], y = dat[,2])) + 
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_point(data = min_tau, aes(x = min_tau[,1], y = min_tau[,2]), size = 4, color = "#C8102E") + 
    geom_label(label = paste0("Tau = ", round(min_tau[,2], 3),  "\n", "Lag = ", min_tau[,1]), 
                              x = ceiling(3/4 * nrow(dat)), 
                              y = dat[1,2]/2,#min_tau[,2], 
               label.size = NA, 
               fill = "lightgray") +
    labs(title = "AMI vs Lag", 
         x = "Lag", 
         y = "AMI") +
    theme_nonan()
}
