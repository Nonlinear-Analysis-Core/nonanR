#' Plot RQA Recurrence Matrix
#' 
#' This function allows you to use the NONAN theme for ggplot2 graphics
#' 
#' @param recurrence_matrix The output from the \code{rqa} function
#' 
#'
#' @examples
#' 
#' library(NONANr)
#' 
#' # Create a sample time series
#' x = fgn_sim(n = 100, H = 0.8)
#' 
#' # Compute RQA
#' x.recpt = rqa(x, x, 1, 1, 0, 1, 2, 2, 0, .0001, 0, 1)
#' 
#' # Return recurrence plot
#' # You can either enter the list returned by rqa() or specify the recurrence plot matrix; the function works both ways.
#' plot_rqa(x.recpt) # --> using the list
#' plot_rqa(x.recpt$rp) # --> specify the rp matrix in the list
#' 
#' @export
plot_rqa = function(recurrence_matrix){
  
  if (is.list(recurrence_matrix)) {
    
    A = recurrence_matrix$rp
  } else {
    A = recurrence_matrix
  }
  
  diag(A) = 1
  colnames(A) <- 1:ncol(A)
  rownames(A) <- 1:nrow(A)
  
  longData = reshape2::melt(A)
  longData = longData[longData$value!=0,]
  longData$value = as.factor(longData$value)
  
  ggplot(longData, aes(x = Var2, y = Var1)) + 
    geom_tile(aes(fill=value), colour = "black", fill = "black") + 
    theme(aspect.ratio = 1) +
    labs(title = "Reccurence Quantification Matrix") +
    theme_nonan()
  
}

