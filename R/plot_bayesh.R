#' Hurst Exponent Posterior Distribution Plot
#' 
#' Plot the posterior distribution of the Hurst exponents sampled from bayesH
#' 
#' 
#' @param x The vector returned \code{bayesH} in this package.
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
  
  h.range = max(x) - min(x)
  h.med = median(x)
  h.mean = mean(x)
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
    # geom_label() +
    labs(title = "Posterior Distribution of Hurst Exponents",
         x = "Hurst Exponent",
         y = "Density") +
    theme_nonan()
  
  # ggplot(dat, aes(x = dat[,1], y = dat[,2])) +
  #   geom_line(linewidth = 1) +
  #   geom_segment(data = reg.dat, aes(x = x_start, y = y_start, 
  #                                    xend = x_end, yend = y_end),
  #                
  #                linewidth = 0.6, color="#C8102E") + 
  #   geom_label(label = plot.text, 
  #              x = Inf, y = -Inf, 
  #              parse = F, vjust = "bottom", hjust = "inward",
  #              label.size = NA, fill = "lightgray") +
  #   labs(title = "Log of Divergence", 
  #        x = "ln(Divergence)", 
  #        y = "Time (s)") +
  #   theme_nonan()
  
}
