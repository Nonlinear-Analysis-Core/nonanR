#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

arma::uvec get_nearest_neighbours(const arma::mat& X, double mu, int time_steps); // Function to find all nearest neighbors

arma::vec mean_log_distance(const arma::uvec& step_time, const arma::uvec& nn, const arma::mat& X); // Function to compute the average divergence at each time step

//' Lyapunov Rosenstein Method
//'
//' Calculate Lyapunov exponent of a time series.
//' 
//' @param X - A single column time series.
//' @param samp_rate - A double indicating the sampling rate of the time series.
//' @param mean_freq - A double indicating the mean frequency of the time series.
//' @param nsteps - Number of time steps to consider for calculating the Lyapunov exponent.
//' @param regpoints - Number of points to include in fitting a line on the divergence plot.
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{lye} The Lyapunov exponent of the time series.
//'  \item \code{time_steps} A vector of the indices of the time steps used to calculate the Lyapunov exponent.
//'  \item \code{mean_distances} A vector of mean log distances used to calculate the Lyapunov exponent.
//' } 
//' @import Rcpp
//' @export
//'
//' @details Lyapunov exponent is the rate of divergence between two neighboring points of a trajectory in a state space.
//' 
//' 
//' @examples
//'
//' fs = 100
//' t = seq(0, 10, 1/fs)
//' ts = sin(2*pi*10*t) + 2*cos(2*pi*5*t)
//' 
//' mean_frequency = meanfreq(signal = ts, samp_rate = fs)
//' 
//' mmax = 12
//' rtol = 15
//' atol = 2
//' 
//' time_delay = ami(ts, ts, 50, 30)
//' tau = time_delay$tau[1,1] # Optimal time delay estimated by AMI
//' 
//' embed = fnn(x = ts, tau = tau, mmax = mmax, rtol = rtol, atol = atol)
//' dim = embed$dim # Optimal embedding dimension estimated by FNN
//' 
//' psr_length = length(ts) - tau*(dim-1)
//' start = 1
//' stop = psr_length
//' X = matrix(nrow = psr_length, ncol = dim)
//' for (i in 1:dim) {
//'   X[,i] = ts[start:stop]
//'   start = start + tau
//'   stop = stop + tau
//' }
//'
//' lye_out = lye_rosenstein(X = X, samp_rate = fs, mean_freq = mean_frequency, 
//'                          nsteps = 500, regpoints = 10:500)
//' 
// [[Rcpp::export]]
List lye_rosenstein(const arma::mat& X, double samp_rate, double mean_freq, int nsteps, const arma::uvec& regpoints) {
  arma::uvec neighbors = get_nearest_neighbours(X, mean_freq, nsteps);
  arma::uvec step_times = arma::linspace<arma::uvec>(0, nsteps, nsteps + 1);
  arma::vec mean_distances = mean_log_distance(step_times, neighbors, X);
  arma::vec time_steps = arma::conv_to<arma::vec>::from(step_times) * (1.0 / samp_rate);
  
  // Linear regression
  arma::vec x = time_steps(regpoints - 1); 
  arma::vec y = mean_distances(regpoints - 1);
  arma::vec lye(2);
  bool success = arma::solve(lye, arma::join_rows(arma::ones(x.n_elem), x), y);
  
  return List::create(Named("lye") = lye,
                      Named("time_steps") = time_steps,
                      Named("mean_distances") = mean_distances);
}


// Function to find all nearest neighbors
arma::uvec get_nearest_neighbours(const arma::mat& X, double mu, int time_steps) {
  int n = X.n_rows;
  arma::uvec nearest_neighbors(n);
  
  for (int i = 0; i < n; ++i) {
    arma::vec d = arma::zeros<arma::vec>(n - time_steps);
    
    for (int j = 0; j < n-time_steps; ++j) {
      d(j) = arma::norm(X.row(i) - X.row(j), 2);
      
      if (d(j) == 0 || std::abs(i - j) < mu) {
        d(j) = arma::datum::inf;
      } 
      
    } 
    
    nearest_neighbors(i) = d.index_min();
  } 
  return nearest_neighbors;
} 

// Function to compute the average divergence at each time step
arma::vec mean_log_distance(const arma::uvec& step_time, const arma::uvec& nn, const arma::mat& X) {
  int m = step_time.n_elem;
  arma::vec logd = arma::zeros<arma::vec>(m);
  
  for (int i = 0; i < m; ++i) {
    int st = step_time(i);
    arma::vec d = arma::zeros<arma::vec>(X.n_rows - st);
    
    for (int j = 0; j < X.n_rows - st; ++j) {
      d(j) = arma::norm(X.row(nn(j) + st) - X.row(j + st), 2);
    } 
    logd(i) = arma::mean(arma::log(d + 1e-10)); // Add a small number to avoid log(0)
  }
  return logd;
} 