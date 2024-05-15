#' IAAFT Surrogates Plot
#' 
#' Plot method for IAAFT surrogates.
#' 
#' @param x Original time series
#' @param surrogates IAAFT surrogates
#'
#' @examples
#' 
#' # Original time series
#' x = rnorm(1000)
#' 
#' # IAAFT surrogates
#' iaaft_out = nonanR::iaafft(x, N = 19)
#' # Plot
#' plot_iaaft(x, iaaft_out)
#' 
#' @export
plot_iaaft = function(x, surrogates){
  
  colnames(surrogates) = paste0("surrogate_", 1:ncol(surrogates))
  
  original_ts = x
  index = 1:1000
  dat  = data.frame(index, original_ts, surrogates)
  data_long <- melt(dat, id = "index") 
  
  ggplot() +  
    geom_line(data_long, mapping = aes(x = index, y = value, color = variable)) +
    facet_wrap(~variable) + 
    labs(title = "IAAFT Simulated Time Series", x = "", y = "") +
    theme_nonan() + 
    theme(
      axis.text.x = element_blank(), 
      legend.position = "none"
    )
  
}
