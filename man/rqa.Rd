% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rqa}
\alias{rqa}
\title{Recurrence Quantification Analysis}
\usage{
rqa(
  ts1,
  ts2,
  embed = 1L,
  delay = 1L,
  normalize = 1L,
  rescale = 1L,
  mindiagline = 2L,
  minvertline = 2L,
  t_win = 0L,
  radius = 1e-04,
  whiteline = 0L,
  recpt = 0L
)
}
\arguments{
\item{ts1}{A numerical time series}

\item{ts2}{A numerical time series}

\item{embed}{The embedding dimension of the time series}

\item{delay}{The optimal time delay (lag)}

\item{normalize}{Should the time series be normalized? (0 = no, 1 = unit interval, 2 = z-score)}

\item{rescale}{Should the distance matrix be rescaled? (0 = no, 1 = max norm, 2 = min norm)}

\item{mindiagline}{The smallest number of diagonal points to be considered a line}

\item{minvertline}{smallest number of vertical points to be considered a line}

\item{t_win}{Theiler window}

\item{radius}{Minimum distance within which points are considered recurrent}

\item{whiteline}{not implemented}

\item{recpt}{Should recurrence plot be returned? (Not recommended for long series)}
}
\value{
The output of the algorithm is a list that includes:
\itemize{
 \item \code{rr}: (Recurrence rate), the overall percentage of recurrent points
 \item \code{det}: (determinism), the percentage of recurrent points that fall on a line
 \item \code{div}: (divergence), inverse of determinism i.e. 1/det
 \item \code{nrline}: (number of lines), total number of lines in the upper triangle
 \item \code{ratio}: (ratio), percent determinism/percent recurrence i.e det/rr
 \item \code{maxline}: (longest line), the number points in the longest diagonal line
 \item \code{meanline}: (average line), average length of diagonal lines
 \item \code{lam}: (laminarity), perecentage of points that fall on vertical lines
 \item \code{tt}: (trapping time), average length of vertical lines
 \item \code{vmax}: (longest vertical line), the number of points in the longest vertical line
 \item \code{entropy}: (Shannon entropy), based on distribution of line lengths
 \item \code{rentropy}: (relative entropy), Shannon entropy normalized by number of lines 
}
Optionally, you can return the recurrence matrix but caution should be taken when returning this especially with larger time series as it can take some time.
}
\description{
This function performs recurrence quantification analysis.
}
\details{
This function performs recurrence quantification analysis (RQA) and its bivariate extension, cross recurrence quantification analysis (CRQA) on time series data that have (potentially) been embedded in higher dimension than the originating series. A common approach for univariate series involves several steps: First, identify the optimal time delay as either the first zero crossing of the autocorrelation function or the first minimum of the average mutual information function. Second, the time series is unfolded into embed dimensions by creating time-delayed copies of the original series. One method for determining the number of dimensions is by the method of False Nearest Neighbors. Third, a distance matrix is computed among the embedded points of the series. A recurrence plot is constructed by passing the distance matrix through a heavyside function: distances less than or equal to the chosen radius are marked as 1 (recurrent); distances falling outside the radius are marked as 0 (not recurrent).

After constructing the recurrence plot, a number of measures are computed to characterize recurrent structure in the time series. These measures and their interpretation are well documented in the literature. We provide simple definitions for each recurrence metric below. In addition, we provide references to standard readings including a very readable introduction to RQA (i.e., Webber & Zbilut, 2005; Marwan et al., 2007).
}
\examples{
# Create a sample time series
x = fgn_sim(n = 100, H = 0.8)

L = 50 # Lag - usually the same as the sampling rate
bins = 30 # If you do not want to specify a bin number, you can set it to 0.
ami_out = ami(x, x, L, bins)

# Function paramters
maxDim = 10
delay = ami_out$tau[1,1]
rtol = 10
atol = 15
fnn_tol = 0.01
# Compute false nearest neighbors
fnn_out = false_nearest_neighbors(x, maxDim = maxDim, delay = delay, rtol = rtol, 
                                 atol = atol, fnn_tol = fnn_tol)


# Compute RQA
embed = fnn_out$dim
delay = ami_out$tau[1,1]
normalize = 0
rescale = 1
mindiagline = 2
minvertline = 2
t_win = 0
radius = 0.0001
whiteline = 0
recpt = 1

x.recpt = rqa(x, x, embed, delay, normalize, rescale, mindiagline, 
              minvertline, t_win, radius, whiteline, recpt)

# Return recurrence plot
plot_rqa(x.recpt$rp)


}
\references{
- Webber, C. L., & Zbilut, J. P. (2005). Recurrence quantification analysis of nonlinear dynamical time series. In S. Riley and G. C. Van Orden (eds). Tutorials in contemporary nonlinear methods for the behavioral sciences.

- Marwan, N., Romano, M. C. Theil, M., & Kurths, J. (2007). Recurrence plots for the analysis of complex systems. Physics Reports, 438, 237-329.
}
