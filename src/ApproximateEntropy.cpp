#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' Approximate Entropy
//' 
//' Calculate the approximate entropy of a time series.
//' 
//' @param x is the time series to analyse
//' @param dim is the embedding dimension of the time series
//' @param R is the radius in which to search for matches
//' 
//' @returns The output of the algorithm is a single integer that reflects the entropy of the time series in bits.
//' 
//' @import Rcpp
//' @useDynLib nonanR
//' @export
//' 
//' @details Here are some additional details about approximate entropy.
//' 
//' @examples
//' 
//' x = rnorm(1000)
//' dim = 8
//' R = 0.2
//' 
//' AE = ApproximateEntropy(x, dim, R)
// [[Rcpp::export]]
double ApproximateEntropy(arma::colvec x, int dim, double R) {
 
 double AE = 0.0;
  
  double r = R * stddev(x);
  //arma::vec dataVec = arma::colvec(x);
  double N = x.size();
  arma::vec2 phim = arma::mat(2,1);
  
  for (int i = 0; i < 2; ++i) {
    
    int m = dim + i;
    int val = N - m + 1; // calculate once in the hopes that it speeds things up
    int inval = 1.0/val;
    arma::vec phi = arma::zeros(N - m + 1);
    arma::mat dataMat = arma::zeros(m, N - m + 1);
    
    for (int j = 0; j < m; ++j){ 
      
      dataMat.row(j) = x.subvec(j, N - m + j).t();
    }
    for (int k = 0; k < N - m + 1; ++k) {
      
      arma::mat tempMat = abs(dataMat.each_col() - dataMat.col(k));
      arma::umat AorB = any(tempMat > r, 0);
      phi[k] = sum(conv_to <colvec>::from((AorB == 0))) / val; // sum(AorB) / (N - m + 1)
    }
    
    phim[i] = sum(log(phi)) / val;
    
  }
  
  AE = phim[0] - phim[1];
  return(AE);
  
}
  



  /*** R
  #source("C:/Users/jsommerfeld/Desktop/Rcode/ApEn.R")
   
  #y = rnorm(1000)
  
  #dim = nonlinearTseries::estimateEmbeddingDim(y, do.plot = FALSE)
  
  
  #rbenchmark::benchmark("R" = Ent_Ap(y, dim, 0.2), 
   #                    "C++" = ApproximateEntropy(y, dim, 0.2), replications = 1)
  
  */
