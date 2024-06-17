#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

int binToDec(arma::ivec binSeq, unsigned int seqLength);

//' Symbolic Entropy 
//' 
//' Calculate the symbolic entropy of a time series.
//' 
//' @param x A single column time series
//' @param thresholdVal The threshold to decide the symbol (0 or 1) of each value. Typically, 0, mean, or median of the time series is chosen.
//' @param seqLength The length of the vectors to be compared for matches
//' 
//' @returns The output of the algorithm is a single value that reflects the entropy of the time series in bits.
//' 
//' @import Rcpp
//' @export
//' 
//' @details Like all entropy functions, this one also quantifies the amount of complexity (or uncertainty/unpredictability) in the signal. Higher values indicates that the time series is less predictable (or more random) and the temporal patterns in the time series are less repetitive. Lower values indicates that the time series is more predictable and the temporal patterns in the time series are more repetitive. Symbolic entropy is rather robust to the length of the time series, making it more suitable for short time series compared to other entropy functions.
//' 
//' @examples 
//' 
//' x = rnorm(1000)
//' thresholdVal = 2
//' seqLength = 0.2
//' 
//' SymE = Ent_Sym(x, thresholdVal, seqLength)
//' 
//' @references
//' Aziz, W., Arif, M. Complexity analysis of stride interval time series by threshold dependent symbolic entropy. Eur J Appl Physiol 98, 30–40 (2006). https://doi.org/10.1007/s00421-006-0226-5
//' 
// [[Rcpp::export]]
double Ent_Sym(arma::vec x, double thresholdVal, unsigned int seqLength) {
   
  unsigned int dataSize = x.n_elem;
   
  arma::ivec binData(dataSize, arma::fill::zeros);
  for (unsigned int i = 0; i < dataSize; ++i){
    if (x(i) > thresholdVal){
      binData(i) = 1;
    }
  }
   
  int numSeqs = floor(dataSize / 3);
   
  arma::vec decimals(numSeqs);
  arma::ivec sequence(seqLength);
   
  for (unsigned int i = 0; i < numSeqs; i++){
    sequence = binData.subvec(i * 3, (i * 3) + 2);
    decimals(i) = binToDec(sequence, seqLength);
  }
   
  double possibleSeqs = pow(2, seqLength);
  arma::vec observedSeqs = arma::unique(decimals);
  int numObservedSeqs = observedSeqs.n_elem;
   
  arma::uvec counts = arma::histc(decimals,arma::linspace(-.5,8.5,10));
  arma::vec prob = arma::conv_to<arma::vec>::from(counts)/sum(counts);
  arma::vec pp = prob(arma::find(prob > 0));
   
  double entropy = -(sum(pp % log2(pp)));
  double correctedEntropy = entropy + (numObservedSeqs-1)/(2*possibleSeqs*log(2));
  double maxCorrectedEntropy = -log2(1/possibleSeqs) + ((possibleSeqs - 1)/(2*possibleSeqs*log(2)));
  double normalizedCorrectedEntropy = correctedEntropy / maxCorrectedEntropy;
   
  return normalizedCorrectedEntropy;
}
 
int binToDec(arma::ivec binSeq, unsigned int seqLength){
  arma::ivec binary(seqLength);
  arma::ivec powers(seqLength);
   
  for (unsigned int i = 0; i < seqLength; ++i){
     powers(i) = pow(2,i);
  }
  powers = arma::flipud(powers);
   
  for (unsigned int j = 0; j < seqLength; ++j){
    binary(j) = binSeq(j) * powers(j);
  }
  return sum(binary);
}