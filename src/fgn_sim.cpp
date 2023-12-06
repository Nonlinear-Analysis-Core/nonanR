#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// Function to generate Fractional Gaussian Noise
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::vec fgn_sim(int n = 1000, double H = 0.7) {
  // Settings
  double mean = 0;
  double std = 1;
  arma::vec ans(n); // Resulting FGN
  
  // Generate Sequence
  arma::vec z = arma::randn(2 * n); // Generate normally distributed random numbers
  arma::vec zr = z.subvec(0, n - 1);
  arma::vec zi = z.subvec(n, 2 * n - 1);
  arma::vec zic = -zi;
  zi(0) = 0;
  zr(0) = zr(0) * std::sqrt(2);
  zi(n - 1) = 0;
  zr(n - 1) = zr(n - 1) * std::sqrt(2);
  
  arma::vec zr_concat = join_cols(zr.subvec(0, n - 1), flipud(zr.subvec(n - 2, 0)));
  arma::vec zi_concat = join_cols(zi.subvec(0, n - 1), flipud(zic.subvec(n - 2, 0)));
  
  arma::cx_vec z_complex = arma::cx_vec(zr_concat, zi_concat);
  
  // Calculate parameters for FGN
  arma::vec k = arma::regspace(0, n - 1);
  arma::vec gammak = (arma::pow(abs(k - 1), 2 * H) - 2 * arma::pow(abs(k), 2 * H) + arma::pow(abs(k + 1), 2 * H)) / 2;
  
  arma::uvec ind = join_cols(arma::regspace<uvec>(0, n - 2), join_cols(n - 1, arma::regspace<uvec>(n - 2, 1)));
  arma::vec shifted_gammak = gammak.elem(ind);
  
  arma::cx_vec gkFGN0 = arma::ifft(shifted_gammak);
  arma::vec gksqrt = real(gkFGN0);
  
  // Check if all elements in gksqrt are positive
  if (all(gksqrt > 0)) {
    gksqrt = sqrt(gksqrt);
    z_complex = z_complex*gksqrt; //.head(n) %= gksqrt;
    z_complex = ifft(z_complex);
    z_complex *= 0.5 * sqrt(n - 1);
    ans = real(z_complex.head(n));
  } else {
    gksqrt.zeros();
    stop("Re(gk)-vector not positive");
  }
  
  // Standardize the result
  ans = std * ans + mean;
  
  return ans; // Return the generated FGN
}





/*** R
timesTwo(42)
*/
