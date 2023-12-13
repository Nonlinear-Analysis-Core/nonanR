

fgn_sim <- function(n = 100, H = 0.6){   
  mean = 0
  std = 1
  ans = NA
  # Generate Sequence:
  #z = rnorm(2*n)
  z = as.matrix(data.table::fread("C:/Users/jsommerfeld/Desktop/NONANr/src/fng_simTestTS.csv"))
  zr = z[1:n]
  zi = z[c((n+1):(2*n))]
  zic = -zi
  zi[1] = 0
  zr[1] = zr[1]*sqrt(2)
  zi[n] = 0
  zr[n] = zr[n]*sqrt(2)
  
  zr = c(zr[c(1:n)], zr[c((n-1):2)]) # -- here
  zi = c(zi[c(1:n)], zic[c((n-1):2)])
  z = complex(real = zr,imaginary = zi)
  
  # .gkFGN0:
  k = 0:(n-1)
  gammak = (abs(k-1)**(2*H)-2*abs(k)**(2*H)+abs(k+1)**(2*H))/2 # ** means squared
  # 
  ind = c(0:(n - 2), (n - 1), (n - 2):1)
  #shifted_gammak = gammak[ind+1]
  gkFGN0 = fft(c(gammak[ind+1]), inverse = TRUE)
  gksqrt = Re(gkFGN0)
  if (all(gksqrt > 0)) {
    gksqrt = sqrt(gksqrt)
    z = z*gksqrt
    z = fft(z, inverse = TRUE)
    z1 = 0.5*(n-1)**(-0.5)*z
    z = Re(z[c(1:n)])
    } else {
    #   gksqrt = 0*gksqrt
    #   stop("Re(gk)-vector not positive")
  }
  # 
  # # Standardize:
  # # (z-mean(z))/sqrt(var(z))
  ans = std*drop(z) + mean
  return(gkFGN0)
}


# This runs and internally loads a file that will always have the same original starting values as the cpp function
set.seed()
sim = fgn_sim(n = 100, H = 0.6)

set.seed(123456)
test = fgn_test(n = 100, H = 0.6)

plot(gkFGN0,type="l",col="red")
lines(test,col="blue")

plot.ts(sim, main= "sim")
plot.ts(test, main = "test")

