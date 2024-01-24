#' This function allows you to use the NONAN theme for ggplot2 graphics
#' 
#'
#' @examples
#' 
#' library(ggplot2)
#' library(NONANr)
#' 
#' # Extract data
#' your_column <- healthy_young$contact_lt
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
#'  theme_nonan(background = "#BCBBBA", main_text = "#D71920", axis_text = "#681820", axes = "#474648")
#'
#' @export
theme_nonan <- function(background = "#ffffff", main_text = "#000000", axis_text = "#444444", axes = "#D9D9D9"){

  theme(
    # Text
    text = element_text(colour = main_text, family = "Arial"), 
    plot.title = element_text(size = 13, margin = margin(0, 0, 0.1, 0, "cm")),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(colour = axis_text),
    axis.title.x = element_text(margin = margin(0.1, 0, 0, 0, "cm")),
    axis.title.y = element_text(margin = margin(0, 0.1, 0, 0, "cm")),
    
    # Axes
    axis.ticks = element_line(colour = axes, linewidth = 0.7),
    panel.grid.major = element_line(axes, linewidth = 0.7),
    panel.grid.minor = element_line(axes, linewidth = 0.1),
    
    # Background
    plot.background = element_rect(fill = background),
    panel.background = element_rect(fill = background),
    legend.background = element_rect(fill = background),
    legend.key = element_rect(fill = background, color = NA))
}
