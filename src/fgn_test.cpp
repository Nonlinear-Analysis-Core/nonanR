#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Function to generate Fractional Gaussian Noise
 
 // [[Rcpp::export]]
 arma::cx_vec fgn_test(int n = 1000, double H = 0.7) {
   // Settings
   double mean = 0;
   double std = 1;
   arma::vec ans(n); // Resulting FGN
   //arma::vec ans = arma::randn(n); // create a fake output just for testing
   
   // Generate Sequence
   arma::vec z = arma::randn(2 * n); // Generate normally distributed random numbers
   arma::vec zr = z.subvec(0, n - 1); // minus one because zero based - first half of `z`
   arma::vec zi = z.subvec(n, 2 * n - 1); // plus one because zero based - second half of `z`
   arma::vec zic = -zi; // flip the signs
   zi[0] = 0; // Make the first number of `zi` equal 0.
   zr[0] = zr[0] * std::sqrt(2); // take the first element and multiply that by the sqrt of 2 
   zi[zi.size() - 1] = 0; // Make the last number of `zi` equal 0.
   zr[zr.size() - 1] = zr[zr.size() - 1] * std::sqrt(2); // take the last element and multiply that by the sqrt of 2 
   
   // Take the zr vector, reverse it, take the first 100 points of it, and the first 98 of the reversed vector and join them together.
   arma::vec zr_subvec = zr.subvec(0, n - 1); // first part of zr_concat    
   arma::vec flipped_zr = arma::flipud(zr); // second part of zr_concat
   flipped_zr = flipped_zr.subvec(0, flipped_zr.n_elem - 3); // Remove the last elements - second part of zr_concat
   arma::vec zr_concat = arma::join_cols(zr_subvec, flipped_zr); // join the two vectors
   
   
   // Do the same thing here
   arma::vec zi_subvec = zi.subvec(0, n - 1); // first part of zi_concat    
   arma::vec flipped_zic = arma::flipud(zic); // second part of zi_concat
   flipped_zic = flipped_zic.subvec(0, flipped_zic.n_elem - 3); // Remove the last elements - second part of zi_concat
   arma::vec zi_concat = arma::join_cols(zi_subvec, flipped_zic); // join the two vectors
   
   arma::cx_vec z_complex = arma::cx_vec(zr_concat, zi_concat); // Create a vector of real and imaginary numbers
   
   // Calculate parameters for FGN
   arma::vec k = arma::regspace(0, n - 1); // 1000 points long
   arma::vec gammak = (arma::pow(abs(k - 1), 2 * H) - 2 * arma::pow(abs(k), 2 * H) + arma::pow(abs(k + 1), 2 * H)) / 2; // This seems to check out
   
   
   // define the elements to join. These should create a pyramid with 999 being the point in the middle of the vector.
   arma::uvec A = arma::regspace<arma::uvec>(0, n - 1);
   arma::uvec B = arma::regspace<arma::uvec>(n - 2, 1);
   arma::uvec ind = arma::join_cols(A, B); // join the columns A and B to create one long vector
   
   arma::vec shifted_gammak = gammak.elem(ind); // Takes the elements of gammak at index `ind`
   
   arma::cx_colvec complex_gammak = arma::conv_to<arma::cx_vec>::from(shifted_gammak); // convert to a complex vector
   arma::cx_mat gkFGN0 = arma::ifft(complex_gammak);
   
   arma::vec gksqrt = arma::real(gkFGN0);
   
   arma::vec temp;
   
   // Check if all elements in gksqrt are positive
   if (all(gksqrt > 0)) {
     gksqrt = arma::sqrt(gksqrt);
     z_complex = z_complex % arma::conv_to<arma::cx_vec>::from(gksqrt);
     
     z_complex = arma::ifft(z_complex);
     
     double one = std::pow(n - 1, -0.5);
     double two = one * 0.5;
     // Everything seems to be fine until here
     
     z_complex = z_complex * two;
     arma::vec temp = arma::real(z_complex);
     ans = temp.head(n);
   } else {
     //    gksqrt.zeros();
     //    stop("Re(gk)-vector not positive");
   }
   //
   // Standardize the result
   ans = std * ans + mean;
   
   return(temp); // Return the generated FGN
 }
 
 
 