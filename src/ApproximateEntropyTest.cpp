#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
double ApproximateEntropyTest(arma::colvec x, int dim, double R) {
  
  double AE = 0.0;
  
  double r = R * arma::stddev(x);
  //arma::vec dataVec = arma::colvec(x);
  double N = x.size();
  arma::vec2 phim = arma::mat(2,1);
  //arma::mat tempMat(10, 10000, fill::zeros); 
  
  for (int i = 0; i < 2; i++) {
    
    int m = dim + i;
    int val = N - m + 1; // calculate once in the hopes that it speeds things up
    int inval = 1/val;
    arma::vec phi = arma::zeros(val);
    arma::mat dataMat = arma::zeros(m, val);

    
    for (int j = 0; j < m; j++){ 
      
      dataMat.row(j) = x.subvec(j, N - m + j).t();
    
    } 
    for (int k = 0; k < val; k++) {
      
      arma::mat tempMat = abs(dataMat.each_col() - dataMat.col(k));
      //Rcpp::NumericVector a(tempMat.size());
      tempMat = tempMat - r;
      tempMat.elem(find(tempMat > 0)).ones(); //arma::umat AorB = any(tempMat > r, 0);
      //test = tempMat;
      phi[k] = accu(tempMat) / val; //phi[k] = sum(conv_to <colvec>::from((AorB == 0))) / (N - m + 1); // sum(AorB) / (N - m + 1)
      
    } 
    
    phim[i] = sum(log(phi)) / val;
    
  } 
  
  AE = phim[0] - phim[1];
  return(AE);
  
} 