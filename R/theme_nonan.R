#' This function allows you to use the NONAN theme for ggplot2 graphics
#' 
#'
#' @examples
#' 
#' library(ggplot2)
#' library(NONANr)
#' 
#' # Extract data
#' your_column <- NONANr::healthy_young$`Contact RT`
#' # Find the differences between consecutive elements
#' differences <- diff(your_column)
#'  
#' # Find the indices where the difference is 1000 and the previous value was 0
#' indices <- which(differences == 1000)
#'  
#' stride_time = diff(indices)/ 200 # 200 is the sampling frequency of the data
#'  
#' # Combine into a data frame for ggplot
#' dat = data.frame("stride_num" = 1:length(stride_time), "stride_time" = stride_time)
#'  
#' # Plot the data and add nonan_theme()
#' ggplot(dat, ggplot2::aes(x = stride_num, y = stride_time)) + 
#'  geom_line(color = "black", linewidth = 1) + 
#'  theme_nonan()
#'   
#'   
#' # Alternatively you could provide color values for the different features of the graph
#' ggplot(dat, ggplot2::aes(x = stride_num, y = stride_time)) + 
#'  geom_line(color = "black", linewidth = 1) + 
#'  theme_nonan(background = "#ffffff", text = "#000000", axes = "#D9D9D9")
#'
#' @export
theme_nonan <- function(background = "#646666", text = "#D9D9D9", axes = "#8A8A8A"){
  library(ggplot2)
  
  theme(
    # Text
    text = element_text(colour = text), 
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(colour = text),
    
    # Axes
    axis.ticks = element_line(colour = axes, linewidth = 1),
    panel.grid.major = element_line(axes, linewidth = 1),
    panel.grid.minor = element_line(axes, linewidth = 0.1),
    
    # Background
    plot.background = element_rect(fill = background),
    panel.background = element_rect(fill = background),
    legend.background = element_rect(fill = background),
    legend.key = element_rect(fill = background, color = NA))
}
