// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ApproximateEntropy
arma::mat ApproximateEntropy(arma::colvec x, int dim, double R);
RcppExport SEXP _NONANr_ApproximateEntropy(SEXP xSEXP, SEXP dimSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    rcpp_result_gen = Rcpp::wrap(ApproximateEntropy(x, dim, R));
    return rcpp_result_gen;
END_RCPP
}
// SampleEntropy
arma::mat SampleEntropy(arma::colvec x, int m, double R);
RcppExport SEXP _NONANr_SampleEntropy(SEXP xSEXP, SEXP mSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    rcpp_result_gen = Rcpp::wrap(SampleEntropy(x, m, R));
    return rcpp_result_gen;
END_RCPP
}
// SymbolicEntropy
double SymbolicEntropy(arma::vec x, double thresholdVal, unsigned int seqLength);
RcppExport SEXP _NONANr_SymbolicEntropy(SEXP xSEXP, SEXP thresholdValSEXP, SEXP seqLengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type thresholdVal(thresholdValSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seqLength(seqLengthSEXP);
    rcpp_result_gen = Rcpp::wrap(SymbolicEntropy(x, thresholdVal, seqLength));
    return rcpp_result_gen;
END_RCPP
}
// dfa
List dfa(arma::vec x, int order, arma::uword verbose, arma::uvec scales, double scale_ratio);
RcppExport SEXP _NONANr_dfa(SEXP xSEXP, SEXP orderSEXP, SEXP verboseSEXP, SEXP scalesSEXP, SEXP scale_ratioSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type scales(scalesSEXP);
    Rcpp::traits::input_parameter< double >::type scale_ratio(scale_ratioSEXP);
    rcpp_result_gen = Rcpp::wrap(dfa(x, order, verbose, scales, scale_ratio));
    return rcpp_result_gen;
END_RCPP
}
// fgn_sim
arma::vec fgn_sim(int n, double H);
RcppExport SEXP _NONANr_fgn_sim(SEXP nSEXP, SEXP HSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type H(HSEXP);
    rcpp_result_gen = Rcpp::wrap(fgn_sim(n, H));
    return rcpp_result_gen;
END_RCPP
}
// fgn_test
arma::vec fgn_test(int n, double H);
RcppExport SEXP _NONANr_fgn_test(SEXP nSEXP, SEXP HSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type H(HSEXP);
    rcpp_result_gen = Rcpp::wrap(fgn_test(n, H));
    return rcpp_result_gen;
END_RCPP
}
// poly_residuals
arma::vec poly_residuals(arma::vec yr, int m);
RcppExport SEXP _NONANr_poly_residuals(SEXP yrSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type yr(yrSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(poly_residuals(yr, m));
    return rcpp_result_gen;
END_RCPP
}
// lm_c
arma::vec lm_c(arma::vec xs, arma::vec yr);
RcppExport SEXP _NONANr_lm_c(SEXP xsSEXP, SEXP yrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type xs(xsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type yr(yrSEXP);
    rcpp_result_gen = Rcpp::wrap(lm_c(xs, yr));
    return rcpp_result_gen;
END_RCPP
}
// seq_int
arma::uvec seq_int(arma::uword length);
RcppExport SEXP _NONANr_seq_int(SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::uword >::type length(lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(seq_int(length));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_NONANr_ApproximateEntropy", (DL_FUNC) &_NONANr_ApproximateEntropy, 3},
    {"_NONANr_SampleEntropy", (DL_FUNC) &_NONANr_SampleEntropy, 3},
    {"_NONANr_SymbolicEntropy", (DL_FUNC) &_NONANr_SymbolicEntropy, 3},
    {"_NONANr_dfa", (DL_FUNC) &_NONANr_dfa, 5},
    {"_NONANr_fgn_sim", (DL_FUNC) &_NONANr_fgn_sim, 2},
    {"_NONANr_fgn_test", (DL_FUNC) &_NONANr_fgn_test, 2},
    {"_NONANr_poly_residuals", (DL_FUNC) &_NONANr_poly_residuals, 2},
    {"_NONANr_lm_c", (DL_FUNC) &_NONANr_lm_c, 2},
    {"_NONANr_seq_int", (DL_FUNC) &_NONANr_seq_int, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_NONANr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
