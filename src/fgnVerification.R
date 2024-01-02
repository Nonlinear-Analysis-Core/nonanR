

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
    z = 0.5*(n-1)**(-0.5)*z
    z = Re(z[c(1:n)])
    } else {
    #   gksqrt = 0*gksqrt
    #   stop("Re(gk)-vector not positive")
  }
  # 
  # # Standardize:
  # # (z-mean(z))/sqrt(var(z))
  ans = std*drop(z) + mean
  return(z)
}


# This runs and internally loads a file that will always have the same original starting values as the cpp function
set.seed()
sim = fgn_sim(n = 100, H = 0.6)

set.seed(123456)
test = fgn_test(n = 1000, H = 0.8)

plot(sim,type="l",col="red")
lines(test,col="blue")

plot.ts(sim, main= "sim")
plot.ts(test, main = "test")



order = 1
verbose = 1
scales <- c(16,32,64,128,256,512)
scale_ratio = 2


temp = NULL
results = NULL
for (i in 1:1000) { 
  for(j in 1:9){
    
    hurst = j/10
    
    test = fgn_test(n = 1000, H = hurst)
    nonan_dfa = NONANr::dfa(test, order, verbose, scales, scale_ratio)
    fr_dfa = fractalRegression::dfa(test, order, verbose, scales, scale_ratio)

    temp[[j]] = data.frame("sim" = i, "hurst"= hurst, "nonan_alpha" = nonan_dfa$alpha, "fr_alpha" = fr_dfa$alpha)
    }
    
  results[[i]] = Reduce(rbind, temp)
}

results = Reduce(rbind,results)

library(ggplot2)

ggplot(results, aes(x=hurst, y=nonan_alpha, fill=hurst, group = hurst)) +
  geom_point(aes(color = hurst), show.legend = F) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9), minor_breaks = NULL) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 9), minor_breaks = NULL) +
  scale_color_viridis_b() + 
  labs(title = "fgn_sim alpha vs expected alpha",
       subtitle = "As quantified by NONANr::dfa()",
       x = "Expected Alpha", 
       y = "Calcualted Alpha") + 
  NONANr::theme_nonan()


ggplot(results, aes(x=fr_alpha, y=nonan_alpha, fill=hurst, group = hurst)) +
  geom_point(aes(color = hurst), show.legend = F) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 9), minor_breaks = NULL) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 9), minor_breaks = NULL) +
  scale_color_viridis_b() + 
  labs(title = "Comparison of alpha values calculated with DFA",
       x = "fractalRegression::dfa()", 
       y = "NONANr::dfa()") + 
  NONANr::theme_nonan()




