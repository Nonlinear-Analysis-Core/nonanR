#include <RcppArmadillo.h>
#include "helpers.hpp"
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' Detrended Fluctuation Analysis
//' 
//' Something a little more can go here
//' 
//' @param x A real values vector (i.e., time series data) to be analyzed.
//' @param order An integer indicating the polynomial order used for detrending the local windows (e.g., 1 = linear, 2 = quadratic, etc.).
//' @param verbose A boolean that when = 1 indicates that the flucuation function inlcuding the log of all included scales as well as the log Rms should be returned as well as the alpha or when = 0 only the estimated scaling exponent alpha will be returned.
//' @param scales An integer valued vector indicating the scales one wishes to resolve in the analysis.
//' @param scale_ratio A scaling factor by which to create successive window sizes from 'sc_min' to 'sc_max.
//' @import Rcpp
//' @export
//' 
//' @details DFA is useful in the analysis of many things but also it has a lot of requirements that should be met before using it.
//' 
//' @examples
//' 
//' x = rnorm(1000)
//' order = 1
//' verbose = 1
//' scales = c(16,32,64,128,256,512)
//' scale_ratio = 2
//' 
//' dfa_out = dfa(x, order, verbose, scales, scale_ratio)
//' 
//' @references
//' - Eke, A., Herman, P., Kocsis, L., & Kozak, L. R. (2002). Fractal characterization of complexity in temporal physiological signals. Physiological measurement, 23(1), R1-R38.
//' 
//' - Gulich, D., & Zunino, L. (2014). A criterion for the determination of optimal scaling ranges in DFA and MF-DFA. Physica A: Statistical Mechanics and its Applications, 397, 17-30.
//' 
//' - Kantelhardt, J. W., Koscielny-Bunde, E., Rego, H. H., Havlin, S., & Bunde, A. (2001). Detecting long-range correlations with detrended fluctuation analysis. Physica A: Statistical Mechanics and its Applications, 295(3-4), 441-454.
//' 
//' - Kelty-Stephen, D. G., Stirling, L. A., & Lipsitz, L. A. (2016). Multifractal temporal correlations in circle-tracing behaviors are associated with the executive function of rule-switching assessed by the Trail Making Test. Psychological assessment, 28(2), 171-180.
//'  
//' - Peng C-K, Buldyrev SV, Havlin S, Simons M, Stanley HE, and Goldberger AL (1994), Mosaic organization of DNA nucleotides, Physical Review E, 49, 1685-1689. 
//' 
//' - Peng C-K, Havlin S, Stanley HE, and Goldberger AL (1995), Quantification of scaling exponents and crossover phenomena in nonstationary heartbeat time series, Chaos, 5, 82-87.
//' 
//' - Perakakis, P., Taylor, M., Martinez-Nieto, E., Revithi, I., & Vila, J. (2009). Breathing frequency bias in fractal analysis of heart rate variability. Biological psychology, 82(1), 82-88.
// [[Rcpp::export]]
List dfa(arma::vec x, int order, arma::uword verbose, 
         arma::uvec scales, double scale_ratio = 2){
  
  double len = x.n_elem;
  arma::uword number_of_scales = scales.n_elem;
  arma::vec resid(number_of_scales);
  arma::vec X = cumsum(x-mean(x));
  
  // do the detrending and return the RMSE for each of the ith scales
  arma::vec RMS(number_of_scales);
  
  for (arma::uword i = 0; i < number_of_scales; ++i){
    arma::uword window = scales[i];
    arma::uword count = 0;
    arma::uvec indx = seq_int(window);
    // indx = indx-1;
    arma::uword number_of_blocks = floor(len/window);
    for ( arma::uword j = 0; j < number_of_blocks; ++j){
      RMS(i) = RMS(i) + arma::accu(arma::pow(poly_residuals(X.rows(indx), 
                                   order),2));
      count = count + 1;
      indx = indx + window;
    }
    
    RMS(i) = sqrt(RMS(i)/(count*window));
    
  }
  
  //take the logm of scales and RMS
  arma::vec log_scale(number_of_scales);
  arma::vec log_rms(number_of_scales);
  if (abs(scale_ratio-2)<.00001){
    log_scale = arma::log2(arma::conv_to<arma::vec>::from(scales));
    log_rms = arma::log2(RMS);
  }else{
    log_scale = log(arma::conv_to<arma::vec>::from(scales))/log(scale_ratio);
    log_rms = log(RMS)/log(scale_ratio);
  }
  
  //compute scaling coefficient
  arma::colvec alpha = lm_c(log_scale,log_rms);
  
  //create a list of output and return it
  if(verbose == 0){
    return List::create(Named("alpha") = alpha(1));
  }else{
    return List::create(Named("log_scales") = log_scale, Named("log_rms")=log_rms, Named("alpha") = alpha(1));
  }
}


