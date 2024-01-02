#' This function allows you to use the NONAN theme for ggplot2 graphics
#' 
#'
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mtcars, aes(x = mpg, y = disp, color = cyl)) + 
#'   geom_point(aes(colour = factor(cyl))) + 
#'   theme_nonan()
#'   
#'   
#' # Alternatively you could provide color values for the different features of the graph
#' ggplot(mtcars, aes(x = mpg, y = disp, color = cyl)) + 
#'   geom_point(aes(colour = factor(cyl))) + 
#'   theme_nonan(background = "#000000", text = "#ffffff", axes = "#888888")
#'   
#'
#' @export
theme_nonan <- function(background = "#646666", text = "#D9D9D9", axes = "#8A8A8A"){
  
  theme(
    # Text
    text = element_text(family = "Verdana", colour = text), 
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
