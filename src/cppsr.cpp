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
//' @param x A single column time series
//' @param m An integer indicating the embedding dimension of the time series
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{Yprime} The reconstructed attractor of the time series.
//' } 
//' @import Rcpp
//' @export
//'
//' @details Constant embedding parameters and Principal component analysis-based Phase Space Reconstruction (CPPSR) is a specific version of phase space reconstruction that is applicable to low-dimensional systems (dimension < 3).
//' 
//' 
//' @references 
//' Li, D., Cao, M., Manoach, E. & Ragulskis, M. A novel embedding method for characterization of low-dimensional nonlinear dynamical systems. Nonlinear Dyn 104, 125–148 (2021). https://doi.org/10.1007/s11071-021-06229-1
//' 
//' @examples
//' # Generate example time series data and inputs for FNN
//' t = seq(0, 1, 0.01)
//' x = sin(2*pi*10*t) + 2*cos(2*pi*5*t)
//' maxDim = 10
//' delay = 1 # tau for CPPSR is fixed to tau = 1
//' rtol = 10
//' atol = 15
//' fnn_tol = 0.01
//' 
//' # Run FNN
//' fnn_out = false_nearest_neighbors(x, maxDim = maxDim, delay = delay, rtol = rtol, 
//'                                   atol = atol, fnn_tol = fnn_tol)
//'
//' # Run CPPSR
//' y_cppsr = cppsr(x = x, m = fnn_out$dim)
//' 
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
 
 arma::mat phaseSpaceReconstruction(const arma::vec& x, int tau, int m) {
   int n = x.n_elem - (m - 1) * tau; // Calculate the number of rows in the reconstructed matrix
   arma::mat Y(n, m);
   
   for (int i = 0; i < m; ++i) {
     Y.col(i) = x.subvec(i * tau, i * tau + n - 1);
   }
   
   return Y;
 }