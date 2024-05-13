% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lorenz.R
\name{lorenz}
\alias{lorenz}
\title{Simulate Lorenz system}
\usage{
lorenz(
  sigma = 10,
  rho = 28,
  beta = 8/3,
  x0 = 0,
  y0 = -0.01,
  z0 = 9,
  duration = 50,
  fs = 100
)
}
\arguments{
\item{sigma}{A control parameter for Lorenz system}

\item{rho}{A control parameter for Lorenz system}

\item{beta}{A control parameter for Lorenz system}

\item{x0}{A double that indicates the initial state of x component of Lorenz system}

\item{y0}{A double that indicates the initial state of y component of Lorenz system}

\item{z0}{A double that indicates the initial state of z component of Lorenz system}

\item{duration}{The duration to simulate Lorenz system}

\item{fs}{The sampling rate to simulate Lorenz system}
}
\value{
The output of the algorithm is a data frame that contains the synthesized Lorenz attractor. The first column is the time vector. The second column is the x component of the Lorenz attractor. The third column is the y component of the Lorenz attractor. The fourth column is the z component of the Lorenz attractor.
}
\description{
This function allows you to simulate Lorenz system and generate synthesized Lorenz attractor
}
\details{
The most commonly investigated set of control parameters are sigma = 10, rho = 28, beta = 8/3. The most commonly investigated set of initial condition is x0 = 0, y0 = -0.01, z0 = 9.

This function requires "deSolve" library.
}
\examples{

library(nonanR)

# Simulate Lorenz system
lorenz.df = lorenz(sigma = 10, rho = 28, beta = 8/3,
                   x0 = 0, y0 = -0.01, z0 = 9,
                   duration = 50, fs = 100)
                   
x = lorenz.df$x0
y = lorenz.df$x0
L = 50
bins = 0 # If you do not want to specify a bin number, you can set it to 0.
ami_out = ami(x, y, L, bins) # Optimal time delay is ami_out$tau[1,1]
tau = ami_out[1,1]

# Compute RQA
x.recpt = rqa(x, x, 3, tau, 0, 1, 2, 2, 0, .0001, 0, 1)

# Return recurrence plot
plot_rqa(x.recpt)


}