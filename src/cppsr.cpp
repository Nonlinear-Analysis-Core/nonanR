#include <RcppArmadillo.h>
// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// Function to perform phase space reconstruction
// This is a direct translation of MATLAB's phase space reconstruction approach
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
