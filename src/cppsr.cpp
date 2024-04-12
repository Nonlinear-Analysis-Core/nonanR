#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat phaseSpaceReconstruction(const arma::vec& x, int tau, int m);
  
//' Constant Embedding Parameters and Principal Component Analysis-based Phase Space Reconstruction
 //'
 //' Reconstruct attractor based on constant embedding parameters and principle component analysis.
 //' 
 //' @param x - A single column time series.
 //' @param m - An integer indicating the embedding dimension of the time series.
 //' @returns The output of the algorithm is a list that includes:
 //' \itemize{
 //'  \item \code{Yprime} The reconstructed attractor of the time series.
 //' } 
 //' @import Rcpp
 //' @export
 //'
 //' @details Constant embedding parameters and principal component analysis-based phase space reconstruction (CPPSR) is a specific version of phase space reconstruction that is applicable on low-dimensional systems (dimension < 3).
 //' 
 //' @examples
 //'
 //' x = sin(2*pi*10) + 2*cos(2*pi*5)
 //'
 //' Yprime = cppsr(x = x, m = 2)
 //' 
arma::mat phaseSpaceReconstruction(const arma::vec& x, int tau, int m) {
  int n = x.n_elem - (m - 1) * tau; // Calculate the number of rows in the reconstructed matrix
  arma::mat Y(n, m);
  
  for (int i = 0; i < m; ++i) {
    Y.col(i) = x.subvec(i * tau, i * tau + n - 1);
  }
  
  return Y;
}

// [[Rcpp::export]]
arma::mat cppsr(const arma::vec& x, int m) {
  // Assuming tau = 1 for these calculations
  arma::mat Y = phaseSpaceReconstruction(x, 1, m);
  
  // Perform PCA on the reconstructed phase space matrix
  // Directly using arma::princomp as it performs PCA
  arma::mat coefficients;
  arma::mat Yprime;
  arma::vec latent;
  arma::princomp(coefficients, Yprime, latent, Y);
  
  // Return the principal component scores, which are the PCA-transformed data
  return Yprime;
}
