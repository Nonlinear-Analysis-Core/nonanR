#' Log Divergence Plot
#' 
#' Plot method for log of divergence.
#' 
#' 
#' 
#' @param x The list object returned \code{lye_rosenstein} in this package.
#' 
#' @examples
#' # Generate example time series data
#' fs = 100
#' t = seq(0, 10, 1/fs)
#' ts = sin(2*pi*10*t) + 2*cos(2*pi*5*t)
#' 
#' # Calculate mean frequency
#' mean_frequency = meanfreq(signal = ts, samp_rate = fs)
#' 
#' # Run AMI
#' ami_out = ami(ts, ts, 50, 30)
#' 
#' # Other inputs for FNN
#' maxDim = 10
#' tau = ami_out$tau[1,1] # Optimal time delay estimated by AMI
#' rtol = 10
#' atol = 15
#' fnn_tol = 0.01
#' 
#' # Compute false nearest neighbors
#' fnn_out = false_nearest_neighbors(ts, maxDim = maxDim, delay = tau, rtol = rtol, 
#'                                  atol = atol, fnn_tol = fnn_tol)
#' dim = fnn_out$dim # Optimal embedding dimension estimated by FNN
#' 
#' # Phase space reconstruction
#' psr_length = length(ts) - tau*(dim-1)
#' start = 1
#' stop = psr_length
#' X = matrix(nrow = psr_length, ncol = dim)
#' for (i in 1:dim) {
#'   X[,i] = ts[start:stop]
#'   start = start + tau
#'   stop = stop + tau
#' }
#' 
#' # Estimate the Largest Lyapunov Exponent
#' lye_out = lye_rosenstein(X = X, samp_rate = fs, mean_freq = mean_frequency, 
#'                         nsteps = 500, regpoints = 10:500)
#' 
#' # Plot lye_out
#' plot_lye(lye_out)
#' 
#' @export
#' 
plot_lye = function(x){
  
  x_end <- x_start <- y_end <- y_start <- NULL
  dat = do.call(cbind.data.frame, list(x$time_steps, x$mean_distances))
  
  m = lm(dat[x$reg_points,2] ~ dat[x$reg_points,1])
  n = length(x$reg_points)
  
  plot.text = paste0("lambda == ", round(x$lye[2], digits = 5))
  
  reg.dat = data.frame("x_start" = dat[x$reg_points[1],1], "y_start" = predict(m)[1],
                       "x_end" = dat[x$reg_points[n],1], "y_end" = predict(m)[n])
  
  ggplot(dat, aes(x = dat[,1], y = dat[,2])) +
    geom_line(linewidth = 1) +
    geom_segment(data = reg.dat, aes(x = x_start, y = y_start, 
                                     xend = x_end, yend = y_end),
                 linewidth = 0.6, color="#C8102E") + 
    geom_label(label = plot.text, 
               x = Inf, y = -Inf, 
               parse = T, vjust = "bottom", hjust = "inward",
               label.size = NA, fill = "lightgray") +
    labs(title = "Log of Divergence", 
         x = "ln(Divergence)", 
         y = "Time (s)") +
    theme_nonan()
  # geom_line(linewidth = 1)
  # geom_point(size = 2)
  
}


