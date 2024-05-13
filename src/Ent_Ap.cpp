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
//' @details
//' Approximate entropy is a measure of a systems regularity or predictability. First introduced in 1991 by Steven Pincus as a way to quantify the predictability of heart rate variability, it has since been used in a wider physiology research and even extended to economics. Approximate entropy is aims to overcome the effects of noise in the time series.
//' 
//' The steps in the algorithm are generally rather simple. The time series is divided up into vectors of length \eqn{m}. Each vector is then compared to the original time series as well as all other vectors with matches to similar ones counted. In this function Euclidean distance is used to match vector within the tolerance specified by \eqn{r}. The log average of the counts is taken and the process is repeated for vectors of length \eqn{m+1}. The final step is subtracting the conditional probability of \eqn{m + 1} from that of \eqn{m}. 
//' 
//' Approximate entropy is measured in bits and is bound between 0 bits (entirely predictable) to 2 bits (completely random). Approximate entropy has been found to be inconsistent in its results, very data hungry and also contains a bias towards more predictability as a result of the self-match.
//' 
//' @references
//' Pincus, S. M. (1991). Approximate entropy as a measure of system complexity. Proceedings of the National Academy of Sciences, 88(6), 2297–2301. https://doi.org/10.1073/pnas.88.6.2297
//' 
//' Yentes, J. M., & Raffalt, P. C. (2021). Entropy Analysis in Gait Research: Methodological Considerations and Recommendations. Annals of Biomedical Engineering, 49(3), 979–990. https://doi.org/10.1007/s10439-020-02616-8
//' 
//' @examples
//' 
//' x = rnorm(1000)
//' dim = 8
//' R = 0.2
//' 
//' AE = Ent_Ap(x, dim, R)
// [[Rcpp::export]]
double Ent_Ap(arma::colvec x, int dim, double R) {
 
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

