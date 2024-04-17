#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

arma::vec computePSD(const arma::vec& signal, double samp_rate);

//' Mean Frequency Estimation
//'
//' Calculate mean frequency of a time series.
//' 
//' @param signal - A single column time series.
//' @param samp_rate- A double indicating the sampling rate of the time series.
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{mean_frequency} The mean frequency of the time series.
//' } 
//' @import Rcpp
//' @export
//'
//' @details Mean frequency is needed for the Lyapunov exponent estimation.
//' 
//' 
//' @examples
//'
//' fs = 100
//' t = seq(0, 3, 1/fs)
//' x = sin(2*pi*10*t) + 2*cos(2*pi*5*t)
//'
//' mean_frequency = meanfreq(signal = x, samp_rate = fs)
//' 
// [[Rcpp::export]]
double meanfreq(const arma::vec& signal, double samp_rate) {
  arma::vec psd = computePSD(signal, samp_rate);
  arma::vec freqs = arma::linspace<arma::vec>(0, samp_rate / 2, psd.n_elem);
  
  double total_power = arma::accu(psd);
  double weighted_sum = arma::dot(freqs, psd);
  
  double mean_frequency = weighted_sum / total_power;
  
  return weighted_sum / total_power;
}

// Helper function to compute the power spectral density (PSD) using FFT
arma::vec computePSD(const arma::vec& signal, double samp_rate) {
  arma::cx_vec fft_result = arma::fft(signal);
  arma::vec psd = arma::abs(fft_result % arma::conj(fft_result)) / signal.n_elem;
  psd = psd.head(psd.n_elem / 2); // Keep only the positive frequencies
  psd *= 2; // Compensate for the removed negative frequencies
  psd(0) /= 2; // DC component should not be doubled
  if (psd.n_elem % 2 == 0) { // Nyquist frequency should not be doubled
    psd(psd.n_elem - 1) /= 2;
  }
  psd /= samp_rate; // Scale by the sampling rate
  return psd;
} 