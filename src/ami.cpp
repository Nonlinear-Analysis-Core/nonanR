#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' Average Mutual Information
//'
//' Calculate the average mutual information of a time series.
//'
//' @param x - a single column time series
//' @param L - the maximum lag of the time series. This is usually the same as the sampling frequency.
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{tau} A data frame of the local minima of the AMI values and the corresponding lag
//'  \item \code{ami} A data frame of all the AMI values at each lag
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
//' L = 100
//'
//' ami_out = AMI(x, L)
//'
//'
// [[Rcpp::export]]
List ami(arma::colvec x, arma::colvec y, int L, int bins) {
  // Mat<double> x = armaGetPr(prhs[0]);		// time series
  // int L = armaGetScalar<int>(prhs[1]);		// maximal lag
  // int bins = armaGetScalar<int>(prhs[2]);		// number of bins
  
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
  arma::vec lags = linspace(0, L, 50);
  mat retV = join_rows(lags, v);
  
  // plhs[0] = armaCreateMxMatrix(retTau.n_rows, retTau.n_cols, mxDOUBLE_CLASS);
  // plhs[1] = armaCreateMxMatrix(retV.n_rows, retV.n_cols, mxDOUBLE_CLASS);
  // 
  // armaSetData(plhs[0], retTau);
  // armaSetData(plhs[1], retV);
  
  return List::create(Named("tau") = retTau, Named("ami") = retV);
}

