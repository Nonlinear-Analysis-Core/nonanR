#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' Sample Entropy
//' 
//' Calculate the sample entropy of a time series.
//' 
//' @param x A single column time series
//' @param m The length of the vectors to be compared for matches
//' @param R The radius for accepting matches
//' 
//' @returns The output of the algorithm is a single integer that reflects the entropy of the time series in bits.
//' 
//' @import Rcpp
//' @export
//'
//' @details Sample entropy can be used to quantify the complexity of a time series. A higher sample entropy value is associated with a more complex signal (more random). On the other hand a time series with a lower sample entropy value could be considered more predictable and repetitive.
//' 
//' @examples 
//' 
//' x = rnorm(1000)
//' m = 2
//' R = 0.2
//' 
//' SE = Ent_Samp(x, m, R)
//' 
//' @references
//' Richman, J.S., Moorman, J.R., 2000. Physiological time-series analysis using approximate entropy and sample entropy. Am. J. Physiol. Heart Circ. Physiol. 278. https://doi.org/10.1152/ajpheart.2000.278.6.H2039
// [[Rcpp::export]]
double Ent_Samp(arma::colvec x, int m, double R) {
  
  
  double SE = 0.0;
  double Bmr, Amr;
  
  double r = R * stddev(x);
  double N = x.size();
  
  arma::mat dij = arma::mat(m + 1, N - m);
  
  arma::colvec d, d1; // Initialize column vectors. Unsure if it is actually needed
  
  // Initialize row vectors
  arma::rowvec dj = arma::rowvec(N - m);
  arma::rowvec dj1 = arma::rowvec(N - m);
  arma::rowvec Bm = arma::rowvec(N - m);
  arma::rowvec Am = arma::rowvec(N - m);
  
  for (int i = 0; i < N - m; i++) {
    for (int k = 0; k < m + 1; k++) { dij.row(k) = abs(x.subvec(k, N - m + k - 1) - x(i + k)).t(); }
    dj = max(dij.rows(0, m - 1), 0);
    dj1 = max(dij, 0);
    d = dj(find(dj <= r));
    d1 = dj1(find(dj1 <= r));
    Bm(i) = d.n_elem - 1;
    Am(i) = d1.n_elem - 1;
  }
  Bm /= (double)(N - m);
  Am /= (double)(N - m);
  
  Bmr = sum(Bm) / (N - m);
  Amr = sum(Am) / (N - m);
   
  SE = -log(Amr / Bmr);
  

  return(SE);
  
}

