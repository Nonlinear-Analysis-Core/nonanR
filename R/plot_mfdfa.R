#' Multifractal Detrended Fluctuation Plot
#'
#'
#' Method for plotting various forms of the multifractal spectrum
#' 
#' 
#' @param mf An object containing elements related to the mutlifractal 
#' spectrum derived from Multifractal Detrended Fluctuation Analysis
#' @param do.surrogate Logical indicating whether surrogation should be 
#' performed on the time series
#' @param nsurrogates Integer indicating the number of surrogates to be 
#' constructed. Default is 19 for 95% confidence limit. A larger number of 
#' surrogates are more precise but increase computational time.
#' @param return.ci Logical indicating if confidence intervals derived from 
#' surrogate analysis should be returned. 
#' 
#' @examples
#' # White Noise
#' white_noise_ts = fgn_sim(n = 1000, H = 0.5)
#'  
#' q = -5:5
#' order = 1
#' scales = c(16,32,64,128,256,512)
#' scale_ratio = 2
#'
#' mfdfa_white_out <- mfdfa(
#'     x = white_noise_ts, 
#'     q = q, 
#'     order = order, 
#'     scales = scales, 
#'     scale_ratio = scale_ratio)
#'     
#' # Plot White Noise
#' plot_mfdfa(mfdfa_white_out, do.surrogate = TRUE)
#'  
#' # Pink Noise
#' pink_noise_ts <- fgn_sim(n = 1000, H = 0.9)
#' 
#' mfdfa_pink_out <- mfdfa(
#'     x = pink_noise_ts, 
#'     q = q, 
#'     order = order, 
#'     scales = scales, 
#'     scale_ratio = scale_ratio) 
#'     
#' plot_mfdfa(mfdfa_pink_out, do.surrogate = TRUE)
#' 
#' @export
plot_mfdfa = function(mf, do.surrogate,  nsurrogates = 19, return.ci = FALSE){
  # Listing these here as NULL removes a CRAN warning about "Undefined global functions or variables"
  index <- value <- Hq <- CI_low <- CI_high <- tau <- h <- Dh <- h_surr_ci_mean <- dh_surr_ci_mean <- h_CI_low <- h_CI_high <- dh_CI_low <- dh_CI_high <- NULL
  
  if (length(mf) != 11){
    cat('This does not appear to be a multifractal spectrum object.\n')
    return(NULL)
  }else{
    
    
    # do better color coding using a ramp function
    # require(colorRamps)
    # cols = rev(colorRamps::blue2red(length(mf$q)))
    
    # Plot q-order fluctuation function
    log_fq <- as.data.frame(mf$log_fq)
    log_fq$index <- as.numeric(mf$log_scale)
    log_fq_long <- tidyr::pivot_longer(log_fq, cols = -index, names_to = "column", values_to = "value") # go to long format
    
    q = ggplot(data = log_fq_long, aes(x = index, y = value, color = as.factor(column))) +
      geom_smooth(formula = y ~ x, method = lm, se = FALSE, linewidth = 0.5) +
      geom_point() +  # Add points to the plot
      scale_x_continuous(breaks = log_fq$index) +  # Show all numbers on the x-axis
      labs(x = "logScale", y = "logF(q)") +
      guides(color = "none") +
      theme_nonan()
    
    # matplot(mf$log_scale, mf$log_fq,type = 'p',
    #         xlab = expression('logScale'), 
    #         ylab = expression('logF(q)'), 
    #         col = cols, tck = .03, pch = 16, cex = .75)
    # 
    # # compute regressions and add regression lines to plot
    # # regs = lm(log10(mf$fq) ~ log10(mf$scales))
    # regs = lm(mf$log_fq ~ mf$log_scale)
    # for (i in 1:length(mf$q)){
    #   abline(regs[[1]][,i], col = cols[i])
    # }
    
    
    # create surrogates and run multifractal analysis on each surrogate
    if (do.surrogate){
      x.surr = iaafft(mf$x, nsurrogates)
      x.surr.mfdfa = apply(x.surr, MARGIN = 2, FUN = function(x){
        out = mfdfa(x = x, q = mf$q, order = mf$order, scales = mf$scales,
                    scale_ratio = mf$scale_ratio)
      })
      
      # compute CI for each value of H(q)
      Hq.surr = lapply(x.surr.mfdfa, FUN =function(x){
        out = x$Hq
      })
      Hq.surr = Reduce(cbind, Hq.surr)
      
      Hq.ci = matrix(NA, nrow(Hq.surr), 2)
      for (i in 1:nrow(Hq.surr)){
        Hq.ci[i,] = as.vector(quantile(Hq.surr[i, ], c(0.025, 0.975)))
      }
      
      # compute CI for each value of tau(q)
      tau.surr = lapply(x.surr.mfdfa, FUN =function(x){
        out = x$Tau
      })
      
      tau.surr = Reduce(cbind, tau.surr)
      tau.ci = matrix(NA, nrow(tau.surr),ncol = 2)
      for (i in 1:nrow(tau.surr)){
        tau.ci[i,] = as.vector(quantile(tau.surr[i,], c(0.025, 0.975)))
      }
      
      # Compute CI for Holder exponent 
      h.surr = lapply(x.surr.mfdfa, FUN = function(x){
        out = x$h
      })
      h.surr = Reduce(cbind, h.surr)
      h.ci = matrix(NA, nrow(h.surr), ncol = 2)
      for (i in 1:nrow(h.surr)){
        h.ci[i, ] = as.vector(quantile(h.surr[i,], c(0.025, 0.975)))
      }
      
      # also get point estimate for h
      h.surr.ci.mean = rowMeans(h.surr)
      
      # compute CI for Holder dimension
      Dh.surr = lapply(x.surr.mfdfa, FUN = function(x){
        out = x$Dh
      })
      Dh.surr = Reduce(cbind, Dh.surr)
      Dh.ci = matrix(NA, nrow(Dh.surr), ncol = 2)
      for (i in 1:nrow(Dh.surr)){
        Dh.ci[i, ] = as.vector(quantile(Dh.surr[i, ], c(0.025, 0.975)))
      }
      
      # also get point estimates for Dh
      Dh.surr.ci.mean = rowMeans(Dh.surr)
      
      # plot H(q) with confidence intervals as a line
      
      df2 = data.frame("Hq" = mf$Hq, "q" = mf$q, "CI_low" = Hq.ci[,1], "CI_high" = Hq.ci[,2])
      int = df2[df2$q == 2,]
      
      hq = ggplot(data = df2, aes(x = q, y = Hq, color = as.factor(q))) + 
        geom_vline(xintercept = int$q, color = "#C8102E", linewidth = 1) + 
        geom_hline(yintercept = int$Hq, color = "#C8102E", linewidth = 1) + 
        geom_point(size = 3) + 
        geom_line(aes(x = q, y = CI_low, group = 1), color = "black", linetype = "dashed", linewidth = 0.6) + 
        geom_line(aes(x = q, y = CI_high, group = 1), color = "black", linetype = "dashed", linewidth = 0.6) + 
        labs(x = "q", y = "H(q)") + 
        guides(color = "none") +
        theme_nonan()

      # hq.ymax = max(max(Hq.ci[,2]), max(mf$Hq))
      # hq.ymin = min(min(Hq.ci[,1]), min(mf$Hq))
      # plot(mf$q, mf$Hq, pch = 16,xlab = 'q', ylab = 'H(q)', col = cols,
      #      tck = .03, ylim = c(hq.ymin, hq.ymax))
      # 
      # lines(mf$q, Hq.ci[,1], col = 'black', lty = 2)
      # lines(mf$q, Hq.ci[,2], col = 'black', lty = 2)
      # abline(v = 2, col = 'red',lwd = 2)
      # abline(h = mf$Hq[mf$q==2], col = 'red', lwd = 2)
      
      
      
      # plot the mass exponent as a function of q
      df3 = data.frame("tau" = mf$Tau, "q" = mf$q, "CI_low" = tau.ci[,1], "CI_high" = tau.ci[,2])
      
      tauq = ggplot(data = df3, aes(x = q, y = tau, color = as.factor(q))) + 
        geom_point(size = 3) + 
        geom_line(aes(x = q, y = CI_low, group = 1), color = "black", linetype = "dashed", linewidth = 0.6) + 
        geom_line(aes(x = q, y = CI_high, group = 1), color = "black", linetype = "dashed", linewidth = 0.6) + 
        labs(x = "q", y = expression(tau(q))) + 
        guides(color = "none") +
        theme_nonan()
      
      # tau.ymax = max(max(tau.ci[,2]), max(mf$Tau))
      # tau.ymin = min(min(tau.ci[,1]), min(mf$Tau))
      # plot(mf$q, mf$Tau, pch = 16, xlab = 'q', ylab = expression(tau(q)),
      #      col = cols, tck = .03, ylim = c(tau.ymin, tau.ymax))
      # lines(mf$q, tau.ci[,1], col = 'black', lty = 2)
      # lines(mf$q, tau.ci[,2], col = 'black', lty = 2)
      
      # plot the Legendre transformed multifractal spectrum
      df4 = data.frame("h" = mf$h, "Dh" = mf$Dh, "h_CI_low" = h.ci[,1], "h_CI_high" = h.ci[,2], 
                       "dh_CI_low" = Dh.ci[,1], "dh_CI_high" = Dh.ci[,2],
                       "h_surr_ci_mean" = h.surr.ci.mean, "dh_surr_ci_mean" = Dh.surr.ci.mean, 
                       "index" = 1:nrow(mf$Dh))
      
      dh = ggplot(df4, aes(x = h, y = Dh, color = as.factor(index))) + 
        geom_point(size = 3) + 
        geom_point(aes(x = h_surr_ci_mean, dh_surr_ci_mean), size = 1.5) + 
        geom_segment(aes(x = h_CI_low, y = dh_surr_ci_mean, 
                         xend = h_CI_high, yend = dh_surr_ci_mean), linewidth = 0.6) + 
        geom_segment(aes(x = h_surr_ci_mean, y = dh_CI_low, 
                         xend = h_surr_ci_mean, yend = dh_CI_high), linewidth = 0.6) + 
        labs(x = "h", y = "D(h)") + 
        guides(color = "none") +
        theme_nonan()
      
      plot_grid(q, hq, tauq, dh, nrow = 2)
      
      
      # hmin = min(min(h.ci[,1]), min(mf$h))
      # hmax = max(max(h.ci[,2]), max(mf$h))
      # Dhmin = min(min(Dh.ci[,1]), min(mf$Dh))
      # Dhmax = max(max(Dh.ci[,2]), max(mf$Dh))
      # plot(mf$h, mf$Dh, pch = 16, xlab = 'h', ylab = 'D(h)', 
      #      col = cols, tck = .03, xlim = c(hmin, hmax), ylim = c(Dhmin, Dhmax))
      
      # add confidence intervals for dh/Dh pairs
      # points(h.surr.ci.mean, Dh.surr.ci.mean, pch = 16, cex = 0.5, 
      #        col = cols)
      # suppressWarnings(
      #   arrows(x0 = h.ci[,1], y0 = Dh.surr.ci.mean,
      #          x1 = h.ci[,2], y1 = Dh.surr.ci.mean,
      #          code = 3, angle = 90, length = 0.05,
      #          col = cols)
      # )
      # suppressWarnings(
      #   arrows(x0 = h.surr.ci.mean, y0 = Dh.ci[,1],
      #          x1 = h.surr.ci.mean, y1 = Dh.ci[,2],
      #          code = 3, angle = 90, length = 0.05, 
      #          col = cols)
      # )
    }else{
      
      df2 = data.frame("Hq" = mf$Hq, "q" = mf$q)
      int = df2[df2$q == 2,]
      
      hq = ggplot(data = df2, aes(x = q, y = Hq, color = as.factor(q))) + 
        geom_vline(xintercept = int$q, color = "#C8102E", linewidth = 1) + 
        geom_hline(yintercept = int$Hq, color = "#C8102E", linewidth = 1) + 
        geom_point(size = 3) + 
        labs(x = "q", y = "H(q)") + 
        guides(color = "none") +
        theme_nonan()
      
      # plot(mf$q, mf$Hq, pch = 16,xlab = 'q', ylab = 'H(q)', col = cols,
      #      tck = .03)
      # draw cross-hairs to to indicate the standard Hurst exponent derived from
      # monofractal detrended fluctuation anaysis
      # abline(v = 2, col = 'red',lwd = 2)
      # abline(h = mf$Hq[mf$q==2], col = 'red', lwd = 2)
      
      # plot the mass exponent as a function of q
      df3 = data.frame("tau" = mf$Tau, "q" = mf$q)
      
      tauq = ggplot(data = df3, aes(x = q, y = tau, color = as.factor(q))) + 
        geom_point(size = 3) + 
        labs(x = "q", y = expression(tau(q))) + 
        guides(color = "none") +
        theme_nonan()
      
      # plot(mf$q, mf$tq, pch = 16, xlab = 'q', ylab = expression(tau(q)),
      #      col = cols, tck = .03)
      
      # plot the Legendre transformed multifractal spectrum
      df4 = data.frame("h" = mf$h, "Dh" = mf$Dh, "index" = 1:nrow(mf$Dh))
      
      dh = ggplot(df4, aes(x = h, y = Dh, color = as.factor(index))) + 
        geom_point(size = 3) + 
        labs(x = "h", y = "D(h)") + 
        guides(color = "none") +
        theme_nonan()
      
      plot_grid(q, hq, tauq, dh, nrow = 2)

      # plot(mf$h, mf$Dh, pch = 16, xlab = 'h', ylab = 'D(h)', 
      #      col = cols, tck = .03)
      
    }

  }
  
}

