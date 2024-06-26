% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{meanfreq}
\alias{meanfreq}
\title{Mean Frequency Estimation}
\usage{
meanfreq(signal, samp_rate)
}
\arguments{
\item{signal}{- A single column time series.}

\item{samp_rate}{- A double indicating the sampling rate of the time series.}
}
\value{
The output of the algorithm is a list that includes:
\itemize{
 \item \code{mean_frequency} The mean frequency of the time series.
}
}
\description{
Calculate mean frequency of a time series.
}
\details{
Mean frequency is needed for the Lyapunov exponent estimation.
}
\examples{

fs = 100
t = seq(0, 3, 1/fs)
x = sin(2*pi*10*t) + 2*cos(2*pi*5*t)

mean_frequency = meanfreq(signal = x, samp_rate = fs)

}
