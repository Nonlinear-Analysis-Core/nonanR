#' Hurst Exponent Posterior Distribution Plot
#' 
#' Plot the posterior distribution of the Hurst exponents sampled from bayesH
#' 
#' 
#' @param x The vector returned \code{bayesH} in this package.
#' 
#' @import ggplot2
#' @importFrom nonanR theme_nonan
#' 
#' @examples
#' x = fgn_sim(n = 128, H = 0.9)
#' 
#' h.pdf = bayesH(x = x, n = 200) 
#' 
#' # Plot h.pdf
#' plot_bayesh(h.pdf)
#' 
#' @export
#' 
plot_bayesh = function(x){
  
  # Calculate statistics required for plotting
  h.range = max(x) - min(x)
  h.med = median(x)
  h.mean = mean(x)

  # Calculate horizontal displacement of labels
  values = c(h.med, h.mean)
  if (h.med > h.mean | h.med == h.mean) {
    hjust = c(-0.15, 1.2)
  } else if (h.med < h.mean) {
    hjust = c(1.2, -0.15)
  }
  xpos = c(h.med, h.mean)
  ypos = c(-Inf, -Inf)
  label = c("Median", "Mean")
  dist.df = data.frame("values" = values, "xpos" = xpos, "ypos" = ypos, "label" = label)
  
  ggplot() +
    geom_density(aes(x), linewidth = 1, alpha = 0.6, fill = 'lightgray') +
    geom_vline(xintercept = values, linewidth = 1, color = c('#C8102E', '#10C732')) +
    geom_label(data = dist.df, aes(label = label),
               x = xpos, y = ypos,
               parse = F, vjust = "bottom", hjust = hjust,
               label.size = NA, fill = "lightgray") +
    labs(title = "Posterior Distribution of Hurst Exponents",
         x = "Hurst Exponent",
         y = "Density") +
    theme_nonan()
  
}
