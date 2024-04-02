#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

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

// Function to calculate mean frequency from a signal
// [[Rcpp::export]]
double meanfreq(const arma::vec& signal, double samp_rate) {
  arma::vec psd = computePSD(signal, samp_rate);
  arma::vec freqs = arma::linspace<arma::vec>(0, samp_rate / 2, psd.n_elem);
  
  double total_power = arma::accu(psd);
  double weighted_sum = arma::dot(freqs, psd);
  
  return weighted_sum / total_power;
}
