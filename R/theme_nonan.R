#' Theme to create NONAN style plots
#' 
#' This function allows you to use the NONAN theme for ggplot2 graphics
#' 
#' @param background Color for the background of the plot
#' @param main_text Color of the text for the main title and axis titles
#' @param axis_text Color of the axis tick labels
#' @param axes Color for the axis lines
#' 
#'
#' @examples
#' 
#' library(ggplot2)
#'  
#' iris_dat = iris
#'  
#' # Plot the data and add nonan_theme()
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#' geom_point() +
#' labs(title = "Sepal Length vs Sepal Width",
#'      x = "Sepal Length",
#'      y = "Sepal Width") +
#'  theme_nonan()
#'   
#'   
#' # Alternatively you can provide explicit color values for the different features of the graph
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#' geom_point() +
#' labs(title = "Sepal Length vs Sepal Width",
#'      x = "Sepal Length",
#'      y = "Sepal Width") +
#'  theme_nonan(background = "grey15", 
#'              main_text = "goldenrod", 
#'              axis_text = "maroon", 
#'              axes = "#ffffff")
#'
#' @export
theme_nonan <- function(background = "#ffffff", main_text = "#000000", axis_text = "#444444", axes = "#D9D9D9"){

  theme(
    # Text
    text = element_text(colour = main_text), 
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
