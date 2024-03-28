#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

double sdp(arma::vec x); // Function for calculating the population standard deviation
arma::mat psr(arma::vec x, int m, int tao, int npoints); // Phase space reconstruction
  
//' False Nearest Neighbor
//'
//' Calculate the average mutual information of a time series.
//'
//' @param x - a single column time series
//' @param tau - the first local minimum from the data. This is a value returned from the \code{ami} function
//' @param mmax - The maximum embedding dimension. Common practice is to set this to 12.
//' @param rtol - The near tolerance. Common practice is to set this to 15. 
//' @param atol - The far tolerance. Used to accommodate for the fact that all neighbors may be far away to begin with. Common practice is to set this to 2.
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{dE} A column vector of the percentages of false neighbors at the embedding dimensions up to \code{mmax}
//'  \item \code{dim} An integer reflecting the embedding dimension at which there is the lowest number of false neighbors
//' } 
//' @import Rcpp
//' @export
//'
//' @details AMI is part of the phase space reconstruction step that is needed for some nonlinear analysis methods.
//' 
//' 
//' @examples
//'
//' x = rnorm(1000)
//' tau = 3 # You can get this value like: ami_out$tau[1,1]
//' mmax = 12
//' rtol = 15
//' atol = 2
//' 
//' fnn_out = fnn(x = x, tau = tau, mmax = mmax, rtol = rtol, atol = atol)
//'
//'
// [[Rcpp::export]]
List fnn(arma::vec x, int tau, int mmax, double rtol, double atol){
  
  double Ra = sdp(x);
  arma::vec fn(mmax+1);
  fn.zeros();
  double D = 0.0;
  double R = 0.0;
  
  for (int m = 1; m < mmax + 1; ++m){
    // perform phase space reconstruction
    int M = x.n_elem - m*tau;
    arma::mat Y = psr(x,m,tau,M);
    for (int n = 0; n < M; ++n){
      
      arma::colvec ones(M);
      ones.ones();
      // create a matrix, y0(M,m), where each row is equal to 
      // row n of of the psr, Y
      arma::mat y0 = ones*Y.row(n);
      
      // substract y0 from Y to obtain the distance of row n from 
      // all other rows
      arma::vec distance = arma::sqrt(arma::sum(arma::pow(Y-y0,2),1));
      
      //TODO: MAYBE USE A FIND FUNCTION INSTEAD OF SORT
      //      NOTE: I TRIED SOMETHING LIKE THIS IN THE DEV VERSION BUT
      //      IT WAS SLOWER THAN THIS ONE.
      // arma::vec neardis = sort(distance);
      arma::uvec nearpos = sort_index(distance);
      
      // only examine the rth nearest neighbor where r = 1 (i.e. Kennel et al.,1992)
      unsigned int ix1 = n + (m)*tau;
      unsigned int ix2 = nearpos(1) + (m)*tau;
      D = std::abs(x(ix1) - x(ix2));
      R = std::sqrt(std::pow(D,2) + std::pow(distance(nearpos(1)),2));
      
      // comapre to tolerances to determine if the a false neighbor
      if (D/distance(nearpos(1)) > rtol || R/Ra > atol){
        
        fn(m) += 1;
      }
    }
  }
  
  // discard first element in fn because it is just a zero
  arma::vec result = fn.subvec(1,mmax);
  
  
  arma::vec dE = (result/result(0))*100;
  int dim = dE.index_min() + 1; // add one because it is zero based

  return List::create(Named("dE") = dE, Named("dim") = dim);
}


// population standard deviation
double sdp(arma::vec x){
  
  return std::sqrt(arma::accu(arma::pow(x-arma::mean(x),2))/x.n_elem);
}


// do phase space reconstruction
arma::mat psr(arma::vec x, int m, int tau, int npoints){
  // int N = x.n_elem;
  // int M = x.n_elem - m*tao;
  int M = npoints;
  arma::mat Y(M,m);
  Y.zeros();
  
  for ( int i = 0; i < m; i++){
    int start = i*tau;
    // Rcout << "start is " << start << "\n";
    int stop = M-1 + (i*tau);
    // Rcout << "stop is " << stop << "\n";
    
    Y.col(i) = x.subvec(start,stop);
  }
  
  return Y;
  
}
