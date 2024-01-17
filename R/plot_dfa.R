#' Detrended Fluctuation Analysis Plot
#' 
#' Plot method for monofractal detrended fluctuation analysis
#' 
#' 
#' 
#' @param x The list object returned \code{dfa} in this package.
#' 
#' @examples
#' 
#' # Create a time series and perform DFA
#' x = rnorm(1000)
#' order = 1
#' verbose = 1
#' scales = c(16,32,64,128,256,512)
#' scale_ratio = 2
#' 
#' dfa_out = dfa(x, order, verbose, scales, scale_ratio)
#' 
#' # Plot dfa_out
#' plot_dfa(dfa_out)
#' 
#' @export
#' 
plot_dfa = function(x){
  library(ggplot2)
  
  dat = do.call(cbind.data.frame, x)
  m = lm(x$log_rms ~ x$log_scales)
  r2 = format(summary(m)$r.square,4, digits = 3)
  plot.text = paste0("Alpha = ", round(dat$alpha[1], 3), "\n", "R^2 = ", (r2))
  x_pos = max(dat$log_scales)-1
  y_pos = dat$log_rms[2]:dat$log_rms[1] 
  
  ggplot(dat, aes(x = log_scales, y = log_rms)) + 
    geom_smooth(formula = y ~ x, method=lm, se = FALSE, color="red") + 
    geom_point(size = 3) + 
    annotate(geom = "label", x = x_pos, y = y_pos, label = plot.text, label.size = NA, fill = "lightgray") +
    labs(title = "Detrended Fluctuation Analysis", 
         x = "Scale (log)", 
         y = "Fluctuation (log)") +
    NONANr::theme_nonan()
}


