#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' Lyapunov Rosenstein Method
//'
//' Calculate the average mutual information of a time series.
//' 
//'
//' @param x - A single column time series.
//' @param tau - The first local minimum from the data. This is a value returned from the \code{ami} function.
//' @param dim - An integer reflecting the embedding dimension at which there is the lowest number of false neighbors. This is a value returned from the \code{fnn} function
//' @param fs - The sampling frequency that the data was collected at. 
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{M} The number of matched pairs
//'  \item \code{out} A matrix of all the matched pairs and the average line divergence from which the slope is calculated. The matched pairs are columns 1 and 2. The average line divergence is column 3.
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
//' dim = 4 # You can get this value like: fnn_out$dim
//' fs = 60
//'
//' lye_out = LyE_R(x = x, tau = tau, dim = dim, fs = fs)

// [[Rcpp::export]]
List lye_r(arma::vec x, int tau, int dim, int fs) {
  
  //Mat<double> x = armaGetPr(prhs[0]);
  //int fs = armaGetScalar<int>(prhs[1]);						// Sampling frequency
  //int tau = armaGetScalar<int>(prhs[2]);						// time lag (Tau)
  //int dim = armaGetScalar<int>(prhs[3]);						// embedding dimension
  
  int M;
  
  Mat<double> Y;
  
  if (x.n_cols > 1) {
    M = x.n_elem;
    Y = x;
  }
  else {
    int N = x.n_elem;
    M = N - (dim - 1) * tau;
    
    Y = zeros<mat>(M, dim);
    for (int i = 0; i < dim; i++) {
      Y.col(i) = x.rows(i * tau, i * tau + M - 1);
    }
  }
  
  // Find nearest neighbors
  
  mat ind2 = zeros<mat>(M, 1);
  
  for (int i = 0; i < M; i++) {
    // Find nearest neighbor.
    mat yInit = repmat(Y.row(i), M, 1);
    mat yDiff = square(yInit - Y.rows(0, M - 1));
    mat yDisti = sqrt(sum(yDiff, 1));
    
    // Exclude points too close based on dominant frequency.
    // TODO: The number of points generated needs to be figure out
    ivec rangeExclude = regspace<ivec>(i - (int)round(tau * 0.8), i + (int)round(tau * 0.8));
    rangeExclude = rangeExclude.elem(find(rangeExclude >= 0 && rangeExclude < M));
    //yDisti(find(rangeExclude)).print();
    yDisti.submat(rangeExclude.min(), 0, rangeExclude.max(), 0).fill(10000.0);
    ind2.row(i) = yDisti.index_min();
  }
  // Calculate Distances between matched pairs.
  
  mat dm = zeros<mat>(M - 1, M - 1);
  for (int i = 0; i < ind2.n_elem - 1; i++) {
    
    // The data can only be propogated so far from the matched pair.
    int endITL = M - ind2(i);
    if (M - ind2(i) > (M - i)) {
      endITL = M - i;
    }
    
    // Finds the distance between the matched pairs and their propogated
    // points to the end of the useable data.
    
    //mat test = dm(span(0, endITL), i);
    //mat test1 = Y(span(i + 1, endITL + i - 1), span(0, Y.n_cols - 1));
    //mat test2 = Y(span(ind2(i) + 1, endITL + ind2(i) - 1), span(0, Y.n_cols - 1));
    
    if (endITL >= 2) {
      dm(span(0, endITL - 2), i) =
        sqrt(
          sum(
            square(
              Y(span(i + 1, endITL + i - 1), span(0, Y.n_cols - 1)) -
                Y(span(ind2(i) + 1, endITL + ind2(i) - 1), span(0, Y.n_cols - 1))
            )
      , 1)
        );
    }
  }
  
  mat out = zeros<mat>(M, 3);
  out.col(0) = regspace(1, 1, M);
  out.col(1) = ind2;
  
  // Calculates the average line divergence.
  
  int r = dm.n_rows;
  mat avgLinDiv = zeros<mat>(r, 1);
  
  for (int i = 0; i < r; i++) {
    rowvec distanceM = dm.row(i);
    if (sum(distanceM) != 0) {
      avgLinDiv(i) = mean(log(distanceM(find(distanceM > 0))));
      out(i, 2) = avgLinDiv(i);
    }
  }
  
  avgLinDiv = nonzeros(avgLinDiv);
  
  // Find the long slope of the data
  
  
  // Find the short slope of the data
  
  
  //compute slope of the line
  //arma::colvec lye_long = lm_c(log_scale,log_rms);
  
  // Set variables for output.
  return List::create(Named("M") = M, Named("out") = out);
  
  
} 

  
