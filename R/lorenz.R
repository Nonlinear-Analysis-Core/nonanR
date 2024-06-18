#' Simulate Lorenz system
#' 
#' This function allows you to simulate the Lorenz system and generate synthesized Lorenz attractor
#' 
#' @param sigma A control parameter for the Lorenz system
#' @param rho A control parameter for the Lorenz system
#' @param beta A control parameter for the Lorenz system
#' @param x0 A double that indicates the initial state of x component of the Lorenz system
#' @param y0 A double that indicates the initial state of y component of the Lorenz system
#' @param z0 A double that indicates the initial state of z component of the Lorenz system
#' @param duration The duration to simulate the Lorenz system
#' @param fs The sampling rate to simulate the Lorenz system
#'
#' @returns The output of the algorithm is a data frame that contains the synthesized Lorenz attractor. The first column is the time vector. The second column is the x component of the Lorenz attractor. The third column is the y component of the Lorenz attractor. The fourth column is the z component of the Lorenz attractor.
#'
#' @details The most commonly investigated set of control parameters are sigma = 10, rho = 28, beta = 8/3. The most commonly investigated set of initial condition is x0 = 0, y0 = -0.01, z0 = 9.
#'
#' This function requires "deSolve" library.
#'
#' @examples
#' # Simulate Lorenz system
#' lorenz.df = lorenz(sigma = 10, rho = 28, beta = 8/3,
#'                    x0 = 0, y0 = -0.01, z0 = 9,
#'                    duration = 50, fs = 100)
#'                    
#'  # Plot Lorenz attractor
#'  plotly::plot_ly(data = lorenz.df, 
#'                x = ~x0, 
#'                y = ~y0,
#'                z = ~z0,
#'                color = ~time,
#'                type = "scatter3d",
#'                mode = "lines") %>%
#'  layout(title = "Lorenz Attractor")               
#'  
#' # Phase space reconstruction of Lorenz system
#' x = lorenz.df$x0
#' y = lorenz.df$x0
#' L = 50
#' bins = 0 # If you do not want to specify a bin number, you can set it to 0.
#' ami_out = ami(x, y, L, bins) # Optimal time delay is ami_out$tau[1,1]
#' tau = ami_out$tau[1,1]
#' 
#' # Compute RQA
#' x.recpt = rqa(x, x, 3, tau, 0, 1, 2, 2, 0, .0001, 0, 1)
#' 
#' # Return recurrence plot
#' plot_rqa(x.recpt)
#' 
#' @references
#' - Sprott, J. C. (2003). Chaos and time-series analysis. Oxford university press.
#' 
#' @export
lorenz = function(sigma = 10, rho = 28, beta = 8/3,
                  x0 = 0, y0 = -0.01, z0 = 9,
                  duration = 50, fs = 100){
  
  lorenz.ode <- function (t, X, pars) {
    
    ### Extract the State Variables
    x = X["x0"]
    y = X["y0"]
    z = X["z0"]
    
    ### Extract Parameters
    sigma = pars["sigma"]
    rho = pars["rho"]
    beta = pars["beta"]
    
    ### Code the Model Equations
    dxdt = sigma*(y-x)
    dydt = x*(rho-z) - y
    dzdt = x*y - beta*z
    
    ### Combine Results into a Single Vector and Return Result
    list(c(dxdt, dydt, dzdt))
    
  }
  
  pars = c(sigma = sigma, rho = rho, beta = beta)
  x.not = c(x0 = x0, y0 = y0, z0 = z0)
  times = seq(from = 0, to = duration, by = 1/fs)
  
  lorenz.dat <- deSolve::ode(y = x.not, func = lorenz.ode, times = times, parms = pars)
  lorenz.df = as.data.frame(lorenz.dat)
  
  return(lorenz.df)
  
}