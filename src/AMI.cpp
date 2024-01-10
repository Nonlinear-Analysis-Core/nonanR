#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Average Mutual Information
//' 
//' Calculate the average mutual information of a time series.
//' 
//' @param x - a single column time series
//' @param L - the maximum lag of the time series. This is usually the same as the sampling frequency.
//' @import Rcpp
//' 
//' @details AMI is part of the phase space reconstruction step that is needed for some nonlinear analysis methods. 
//' 
//' @examples 
//' 
//' x = rnorm(1000)
//' L = 100
//' 
//' ami_out = AMI(x, L)
//' 
//' @references Something should go here
//' 
// [[Rcpp::export]]
List AMI(arma::colvec x, int L){
  
  //arma::colvec data = x;
  //int L = L;
  int N = x.n_elem;
  int bins = arma::ceil(arma::range(x) / (3.49 * arma::stddev(x) * arma::pow(N, -1/3))); // Scott 1979
  double epsilon = 1e-10;
  
  // data = data - min(data); % make all data points positive
  arma::vec data_pos = x - arma::min(x);
  
  // y = 1+ floor(data/(max(data)/(bins-epsilon))); % scaling the data
  arma::vec y = 1 + arma::floor(data_pos / arma::max(data_pos) / bins - epsilon);
  
  //ami=zeros(L,1); % preallocate vector
  arma::colvec ami = arma::zeros(L); // double check the shape of this vector
  int overlap = N - L;
  int increment = 1 / overlap;
  arma::colvec one = arma::ones(overlap); // create a column vector with all elements being one
  
  //pA = sparse(y(1:overlap),one,increment); // create a sparse matrix
  arma::sp_mat pA = arma::spones(y.subvec(0, overlap - 1), one);
  
  //a=unique(y(1:overlap)); % get unique values from y
  arma::vec a = arma::unique(y.subvec(0, overlap - 1));
  
  // for i=1:length(a)
    // pA2(i,1)=sum(y(1:overlap)==a(i))*increment; % Get the sum of the values in y which equals whatever value is in a(i) and multiply by the increment
  // end

  
  //return(data);
  return List::create(Named("data") = data_pos, Named("y") = y, Named("bins") = bins);
  
}


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
