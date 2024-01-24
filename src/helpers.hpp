#include <Rcpp.h>
using namespace Rcpp;

// FOR DFA
//  Polynomial detrending
// [[Rcpp::export]]
arma::vec poly_residuals(arma::vec yr, int m);  

// Linear regression
// [[Rcpp::export]]
arma::vec lm_c(arma::vec xs, arma::vec yr); 

// Create a sequence of unsigned integers
// [[Rcpp::export]]
arma::uvec seq_int(arma::uword length);


// FOR RQA
// function for creating embedding matrix (phase-space reconstruction)
// // [[Rcpp::export]]
// arma::mat psr(arma::vec& x, int dim, int tau);
// 
// // function to compute distance matrix
// // [[Rcpp::export]]
// void dist_mat(arma::mat& x1, arma::mat& x2, arma::mat& dist);
// 
// // function for creating the recurrence plot
// // [[Rcpp::export]]
// void rp(arma::mat& x, double radius);
// 
// // function for computing the variance metrics of recurrence
// // [[Rcpp::export]]
// List line_stats(arma::mat& rp, int mindiagline, int minvertline, int t_win);
// 
// // extract diagonal line lengths
// // [[Rcpp::export]]
// arma::vec diagonal_lines(arma::mat& rp, int mindiagline);
// 
// // extract vertical line lengths
// // [[Rcpp::export]]
// arma::vec vertical_lines(arma::mat& rp, int minvertline);


