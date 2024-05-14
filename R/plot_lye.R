#' Log Divergence Plot
#' 
#' Plot method for log of divergence.
#' 
#' 
#' 
#' @param x The list object returned \code{lye_rosenstein} in this package.
#' 
#' @examples
#' fs = 100
#' t = seq(0, 10, 1/fs)
#' ts = sin(2*pi*10*t) + 2*cos(2*pi*5*t)
#' 
#' mean_frequency = meanfreq(signal = ts, samp_rate = fs)
#' 
#' mmax = 12
#' rtol = 15
#' atol = 2
#' 
#' time_delay = ami(ts, ts, 50, 30)
#' tau = time_delay$tau[1,1] # Optimal time delay estimated by AMI
#' 
#' embed = fnn(x = ts, tau = tau, mmax = mmax, rtol = rtol, atol = atol)
#' dim = embed$dim # Optimal embedding dimension estimated by FNN
#' 
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
#' lye_out = lye_rosenstein(X = X, samp_rate = fs, mean_freq = mean_frequency, 
#'                          nsteps = 500, regpoints = 10:500)
#' 
#' 
#' # Plot lye_out
#' plot_lye(lye_out)
#' 
#' @export
#' 
plot_lye = function(x){
  
  dat = do.call(cbind.data.frame, list(x$time_steps, x$mean_distances))
  
  m = lm(dat[x$reg_points,2] ~ dat[x$reg_points,1])
  n = length(x$reg_points)
  
  plot.text = paste0("\U03BB = ", x$lye[2])
  
  ggplot(dat, aes(x = dat[,1], y = dat[,2])) +
    geom_line(linewidth = 1) +
    geom_segment(aes(x = dat[x$reg_points[1],1], y = predict(m)[1], 
                     xend = dat[x$reg_points[n],1], yend = predict(m)[n]),
                 linewidth = 0.6, color="#C8102E") + 
    geom_label(label = plot.text, 
               x = Inf, y = -Inf, 
               parse = F, vjust = "bottom", hjust = "inward",
               label.size = NA, fill = "lightgray") +
    labs(title = "Log of Divergence", 
         x = "ln(Divergence)", 
         y = "Time (s)") +
    theme_nonan()
  # geom_line(linewidth = 1)
  # geom_point(size = 2)
  
}


