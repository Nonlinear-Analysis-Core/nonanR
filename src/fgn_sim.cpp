#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Function to generate Fractional Gaussian Noise

// [[Rcpp::export]]
arma::vec fgn_sim(int n = 1000, double H = 0.7) {
  // Settings
  double mean = 0;
  double std = 1;
  //arma::vec ans(n); // Resulting FGN
  arma::vec ans = arma::randn(10); // create a fake output just for testing
  
  // Generate Sequence
  arma::vec z = arma::randn(2 * n); // Generate normally distributed random numbers
  arma::vec zr = z.subvec(0, n - 1); // minus one becuause zero based - first half of `z`
  arma::vec zi = z.subvec(n, 2 * n - 1); // plus one because zero based - second half of `z`
  arma::vec zic = -zi; // make it negative
  zi[0] = 0; // Make the first number of `zi` equal 0.
  zr[0] = zr[0] * std::sqrt(2); // take the first element and multiply that by the sqrt of 2 
  zi[zi.size() - 1] = 0; // Make the last number of `zi` equal 0.
  zr[zr.size() - 1] = zr[zr.size() - 1] * std::sqrt(2); // take the last element and multiply that by the sqrt of 2 
  
  arma::vec zr_concat = join_cols(zr.subvec(0, n - 1), flipud(zr.subvec(n - 2, 0))); // take the full zr time series and join it with a flipped copy of the time series of the two points less
  arma::vec zi_concat = join_cols(zi.subvec(0, n - 1), flipud(zic.subvec(n - 2, 0))); // do the same thing here. 
  
  arma::cx_vec z_complex = arma::cx_vec(zr_concat, zi_concat); // Create a vector of real and imaginary numbers
  
  // Calculate parameters for FGN
  arma::vec k = arma::regspace(0, n - 1); // 1000 points long
  arma::vec gammak = (arma::pow(abs(k - 1), 2 * H) - 2 * arma::pow(abs(k), 2 * H) + arma::pow(abs(k + 1), 2 * H)) / 2; // This seems to check out
  
  // define the elements to join. These should create a pyramid with 999 being the point in the middle of the vector.
  arma::umat A = arma::regspace<arma::uvec>(0, n - 1);
  arma::umat B = arma::regspace<arma::uvec>(n - 2, 1);
  
  //arma::uvec ind = arma::join_cols(A, B); // join the columns A and B to create one long vector
  arma::uvec ind = arma::join_cols(arma::regspace<arma::uvec>(0, n - 1), arma::regspace<arma::uvec>(n - 2, 1)); // join the columns A and B to create one long vector
  
  
  arma::vec shifted_gammak = gammak.elem(ind); // Takes the elements of gammak at index `ind`
  
  arma::cx_colvec complex_gammak = arma::conv_to<arma::cx_vec>::from(shifted_gammak); // convert to a complex vector
  
  
  arma::cx_mat gkFGN0 = arma::ifft(complex_gammak);
  
  //arma::vec gksqrt = arma::real(gkFGN0);

  // Check if all elements in gksqrt are positive
  // if (all(gksqrt > 0)) {
  //    gksqrt = sqrt(gksqrt);
  //    z_complex = z_complex*gksqrt; //.head(n) %= gksqrt;
  //    z_complex = ifft(z_complex);
  //    z_complex *= 0.5 * sqrt(n - 1);
  //    ans = real(z_complex.head(n));
  //  } else {
  //    gksqrt.zeros();
  //    stop("Re(gk)-vector not positive");
  //  }
  //
  // // Standardize the result
  ans = std * ans + mean;
  
  return(ans); // Return the generated FGN
}


