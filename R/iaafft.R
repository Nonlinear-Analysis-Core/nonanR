#' Iterated Amplitude Adjusted Fast Fourier Transform
#' 
#' @param signal is a real valued time series 
#' @param N is the number of desired surrogates. Default is 9
#' 
#' @details
#' Iterated amplitude adjusted Fourier Transform (IAAFFT) is an extension of the amplitude adjusted Fourier algorithm. IAAFFT is used with surrogates allows the autocorrelation and power spectrum to be preserved from the original time series, along with the distribution. 
#' 
#' IAAFFT is most commonly used in surrogate testing of data. Surrogate testing is a useful approach to identify evidence of nonlinearity by excluding a linear origin as a null hypothesis. This is done using a discriminating statistic (i.e. the result of some nonlinear function with a single output) like LyE or an entropy function. If the results from the discriminating are consistent, the null hypothesis is true however, if the original result is different from the surrogates, the null hypothesis should be rejected. 
#' 
#' The use of multiple discriminating statistics is encouraged and can help to eliminate any spurious results. Some caution should be taken if using IAAFFT on shorter time series as it may not contain enough information to truly randomize. 
#' 
#' @references Schreiber, T., & Schmitz, A. (1996). Improved surrogate data for nonlinearity tests. Physical Review Letters, 77(4), 635.
#' 
#' @export
iaafft <- function(signal, N=9){
  # this function generates surrogates using the iterated amplitude
  # adjusted fourier transform discussed in Ihlen & Vereijken, 2010
  # and Schreiber & Schmitz, 1996 and gazillion other papers.
  
  
  mx = 1000
  x = signal
  ln = length(x)
  amp = Mod(fft(x))
  sgates = matrix(rep(0,ln*N),nrow = ln)
  for (n in 1:N){
    s = sample(ln)
    sgates[,n]<-x[s]
  }
  
  tmp <- sort(x,index.return = TRUE)
  x <- tmp$x
  ind <- tmp$ix
  
  for ( n in 1:N){
    phase_x <- Arg(fft(sgates[,n]))
    nn =1
    conv = 0
    ind_prev=ind
    while ( nn <= mx && conv == 0 ){
      sgates[,n] <- amp*exp(phase_x*1i)
      sgates[,n] <- Re(fft(sgates[,n],inverse = TRUE))
      sgates_sort_ind <- sort(sgates[,n],index.return = TRUE)
      sgates_sort_new <- sort(sgates_sort_ind$ix,index.return=TRUE)
      sgates[,n] <- x[sgates_sort_new$ix]
      ind_new <- sgates_sort_new$ix
      if (all(ind_new == ind_prev)){
        conv <-1
      }else{
        ind_prev <- ind_new
        nn = nn+1;
      }
      phase_x <- Arg(fft(sgates[,n]))
    }
  }
  sgates <- data.frame(Re(sgates))
  return(sgates)
}

