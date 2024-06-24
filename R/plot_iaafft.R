#' IAAFFT Surrogates Plot
#' 
#' Plot method for IAAFFT surrogates.
#' 
#' @param x Original time series
#' @param surrogates IAAFFT surrogates
#'
#' @import ggplot2
#' @importFrom nonanR theme_nonan
#' @importFrom reshape2 melt
#'
#' @examples
#' # Generate example time series data
#' x = rnorm(1000)
#' 
#' # IAAFFT surrogates
#' iaafft_out = iaafft(x, N = 19)
#' 
#' # Plot
#' plot_iaafft(x, iaafft_out)
#' 
#' @export
plot_iaafft = function(x, surrogates){
  
  value <- variable <- NULL
  colnames(surrogates) = paste0("surrogate_", 1:ncol(surrogates))
  
  original_ts = x
  index = 1:1000
  dat  = data.frame(index, original_ts, surrogates)
  data_long <- melt(dat, id = "index") 
  
  ggplot() +  
    geom_line(data_long, mapping = aes(x = index, y = value, color = variable)) +
    facet_wrap(~variable) + 
    labs(title = "IAAFFT Simulated Time Series", x = "", y = "") +
    theme_nonan() + 
    theme(
      axis.text.x = element_blank(), 
      legend.position = "none"
    )
  
}
