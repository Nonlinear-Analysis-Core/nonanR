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


