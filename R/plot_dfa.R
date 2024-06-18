#' Detrended Fluctuation Analysis Plot
#' 
#' Plot method for monofractal detrended fluctuation analysis
#' 
#' 
#' 
#' @param x The list object returned \code{dfa} in this package.
#' 
#' @examples
# Generate example time series data
#' x = fgn_sim(n = 1000, H = 0.9)
#' 
#' # Specify inputs for dfa()
#' order = 1
#' verbose = 1
#' scales = logscale(scale_min = 16, scale_max = 512, scale_ratio = 2)
#' scale_ratio = 2
#' 
#' # Run DFA
#' dfa_out = dfa(x, order, verbose, scales, scale_ratio)
#' 
#' # Plot dfa_out
#' plot_dfa(dfa_out)
#' 
#' @export
#' 
plot_dfa = function(x){

  dat = do.call(cbind.data.frame, x)
  m = lm(dat[,2] ~ dat[,1])
  r2 = format(summary(m)$r.square,4, digits = 3)
  
  plot.text = paste0("Alpha = ", round(dat[,3][1], 3), "\n", "R^2 = ", (r2), sep = "")
  
  ggplot(dat, aes(x = dat[,1], y = dat[,2])) + 
    geom_smooth(formula = y ~ x, method = lm, se = FALSE, color="#C8102E") + 
    geom_point(size = 3) + 
    geom_label(label = plot.text, 
               x = Inf, y = -Inf, 
               parse = F, vjust = "bottom", hjust = "inward",
               label.size = NA, fill = "lightgray") +
    labs(title = "Detrended Fluctuation Analysis", 
         x = "Scale (log)", 
         y = "Fluctuation (log)") +
    theme_nonan()
}


