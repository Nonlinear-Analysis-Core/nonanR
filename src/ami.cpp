#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' Average Mutual Information
//'
//' Calculate the average mutual information of a time series.
//'
//' @param x - a single column time series
//' @param y - a single column time series. This can be the same as x
//' @param L - the maximum lag of the time series. This is usually the same as the sampling frequency.
//' @param bins - the number of histogram bins to split the data into. You can specify 0 and the algorithm will bin the data for you. 
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{tau} A data frame of the local minima of the AMI values and the corresponding lag
//'  \item \code{ami} A data frame of all the AMI values at each lag
//' } 
//' @import Rcpp
//' @export
//'
//' @details Average Mutual Information (AMI) is an integral part of state space reconstruction, used to find the optimal time delay (tau) of a system. Tau is needed in algorithms such as Recurrence Quantification Analysis (RQA) and Lyapunov Exponents (LyE). AMI measures the probability that some information in one time series is shared with the same series delayed by one time step. Typically, this step precedes the False Nearest Neighbor (FNN) analysis. For practical implementation, NONAN prefers a histogram-based approach due to its relative speed compared to kernel density methods. Important consideration should be given to factors such as the length of the time series, the presence of artifacts in the data, additional noise, and the sampling rate when using this function.
//' The formula for AMI is as follows: 
//' 
//' \eqn{I(k) = \sum_{t = 1}^{n} P(x_t, x_{t+k}) log_2 [\frac{P(x_t, x_{t+k})}{P(x_t)P(x_{t+k})} ]}
//' 
//' Where \eqn{P(x_t)} is the probability density function of the original time series, \eqn{P(x_{t+k})} is the probability density function of the time series shifted by a time delay \eqn{t}, and \eqn{P(x_t, x_{t+k})} is the joint probability density function of the original time series and the time series shifted by \eqn{t}. 
//' 
//' @references 
//' Scott, D. W. (1979). On optimal and data-based histograms. Biometrika, 66(3), 605–610. https://doi.org/10.1093/biomet/66.3.605
//' 
//' Raffalt, P. C., Senderling, B., & Stergiou, N. (2020). Filtering affects the calculation of the largest Lyapunov exponent. Computers in Biology and Medicine, 122, 103786. https://doi.org/10.1016/j.compbiomed.2020.103786
//' 
//' @examples
//' x = rnorm(1000)
//' y = x
//' L = 50
//' bins = 30 # If you do not want to specify a bin number, you can set it to 0.
//'
//' ami_out = ami(x, y, L, bins)
//'
// [[Rcpp::export]]
List ami(arma::colvec x, arma::colvec y, int L, int bins) {
  
  vec data;
  data = conv_to<vec>::from(x.col(0));
  
  int n = data.n_elem;
  double eps = datum::eps;
  
  if (bins == 0) {
    bins = (int)((data.max() - data.min()) / (3.49 * stddev(data) * pow(double(n), (-1.0 / 3.0))));
  }
  
  data = data - min(data);	// make all data points positive
  data = data / (max(data) / (bins + 1 - eps)); // scaling the data
  ivec data2 = conv_to<ivec>::from(data);			// Note: Compared to MATLAB implementation 
  // this does a floor and a conversion to int.
  // This floor is equivalent as these are pos values.
  vec v = zeros(L);
  int overlap = n - L;
  
  double increment = 1.0 / (double)overlap;
  vec one = ones(overlap);
  
  ivec centers = regspace<ivec>(0, 1, max(data2));	// Centers for our histogram.
  vec pA = conv_to<vec>::from(hist(data2.subvec(0, overlap - 1), centers)) * increment;
  vec pB;
  //umat pAB2;
  //mat AB = zeros(pA.n_elem, 2);
  //AB.col(0) = pA;
  
  
  for (int lag = 0; lag < L; lag++) {
    sp_mat pAB(max(data2) + 1, max(data2) + 1);
    
    pB = conv_to<vec>::from(hist(data2.subvec(0 + lag, overlap - 1 + lag), centers)) * increment;
    
    //hist(AB, centers, 0);
    //pAB2 = hist(AB, centers, 0);
    //AB.col(1) = pB;
    
    for (int i = 0; i < overlap; i++) {
      pAB(data2(i), data2(i + lag)) += increment;
    }
    uvec A = regspace<uvec>(0, 1, pAB.n_nonzero - 1);
    uvec B = regspace<uvec>(0, 1, pAB.n_nonzero - 1);
    vec AB = zeros(pAB.n_nonzero);
    sp_mat::iterator start = pAB.begin();
    sp_mat::iterator end = pAB.end();
    int idx = 0;
    for (sp_mat::iterator it = start; it != end; ++it) {
      A[idx] = it.row();
      B[idx] = it.col();
      AB[idx] = *it;
      idx++;
    }
    v[lag] = sum(AB % log2(AB / (pA(A) % pB(B))));
  }
  
  // Finding number of dips in data.
  mat tau = zeros(L, 2);
  int tauIdx = 0;
  for (int i = 1; i < L - 1; i++) {
    if (v(i - 1) >= v(i) && v(i) <= v(i + 1)) {
      tau(tauIdx, 0) = i;
      tau(tauIdx, 1) = v(i);
      tauIdx++;
    }
  }
  
  // Finding .2 value from initial AMI at L = 0.
  uvec ind = find(v <= 0.2 * v(0), 0, "first");
  if (!ind.is_empty()) {
    tau(tauIdx, 0) = ind[0];
    tau(tauIdx, 1) = v(ind[0]);
  }
  
  // Processing for return values.
  mat retTau = nonzeros(tau);
  retTau = reshape(retTau, retTau.n_rows / 2, 2); // Puts lag vals and AMI side-by-side
  arma::vec lags = linspace(0, L, L);
  mat retV = join_rows(lags, v);
  
  // plhs[0] = armaCreateMxMatrix(retTau.n_rows, retTau.n_cols, mxDOUBLE_CLASS);
  // plhs[1] = armaCreateMxMatrix(retV.n_rows, retV.n_cols, mxDOUBLE_CLASS);
  // 
  // armaSetData(plhs[0], retTau);
  // armaSetData(plhs[1], retV);
  
  return List::create(Named("tau") = retTau, Named("ami") = retV);
}

