// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Ent_Ap
double Ent_Ap(arma::colvec x, int dim, double R);
RcppExport SEXP _nonanR_Ent_Ap(SEXP xSEXP, SEXP dimSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    rcpp_result_gen = Rcpp::wrap(Ent_Ap(x, dim, R));
    return rcpp_result_gen;
END_RCPP
}
// Ent_Samp
double Ent_Samp(arma::colvec x, int m, double R);
RcppExport SEXP _nonanR_Ent_Samp(SEXP xSEXP, SEXP mSEXP, SEXP RSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type R(RSEXP);
    rcpp_result_gen = Rcpp::wrap(Ent_Samp(x, m, R));
    return rcpp_result_gen;
END_RCPP
}
// Ent_Sym
double Ent_Sym(arma::vec x, double thresholdVal, unsigned int seqLength);
RcppExport SEXP _nonanR_Ent_Sym(SEXP xSEXP, SEXP thresholdValSEXP, SEXP seqLengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type thresholdVal(thresholdValSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type seqLength(seqLengthSEXP);
    rcpp_result_gen = Rcpp::wrap(Ent_Sym(x, thresholdVal, seqLength));
    return rcpp_result_gen;
END_RCPP
}
// ami
List ami(arma::colvec x, arma::colvec y, int L, int bins);
RcppExport SEXP _nonanR_ami(SEXP xSEXP, SEXP ySEXP, SEXP LSEXP, SEXP binsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::colvec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type bins(binsSEXP);
    rcpp_result_gen = Rcpp::wrap(ami(x, y, L, bins));
    return rcpp_result_gen;
END_RCPP
}
// bayesH
arma::vec bayesH(arma::vec x, unsigned int n);
RcppExport SEXP _nonanR_bayesH(SEXP xSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(bayesH(x, n));
    return rcpp_result_gen;
END_RCPP
}
// cppsr
arma::mat cppsr(const arma::vec& x, int m);
RcppExport SEXP _nonanR_cppsr(SEXP xSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(cppsr(x, m));
    return rcpp_result_gen;
END_RCPP
}
// dfa
List dfa(arma::vec x, int order, arma::uword verbose, arma::uvec scales, double scale_ratio);
RcppExport SEXP _nonanR_dfa(SEXP xSEXP, SEXP orderSEXP, SEXP verboseSEXP, SEXP scalesSEXP, SEXP scale_ratioSEXP) {
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
arma::vec fgn_sim(int n, double H, double mean, double std);
RcppExport SEXP _nonanR_fgn_sim(SEXP nSEXP, SEXP HSEXP, SEXP meanSEXP, SEXP stdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type H(HSEXP);
    Rcpp::traits::input_parameter< double >::type mean(meanSEXP);
    Rcpp::traits::input_parameter< double >::type std(stdSEXP);
    rcpp_result_gen = Rcpp::wrap(fgn_sim(n, H, mean, std));
    return rcpp_result_gen;
END_RCPP
}
// poly_residuals
arma::vec poly_residuals(arma::vec yr, int m);
RcppExport SEXP _nonanR_poly_residuals(SEXP yrSEXP, SEXP mSEXP) {
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
RcppExport SEXP _nonanR_lm_c(SEXP xsSEXP, SEXP yrSEXP) {
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
RcppExport SEXP _nonanR_seq_int(SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::uword >::type length(lengthSEXP);
    rcpp_result_gen = Rcpp::wrap(seq_int(length));
    return rcpp_result_gen;
END_RCPP
}
// lye_rosenstein
List lye_rosenstein(const arma::mat& X, double samp_rate, double mean_freq, int nsteps, const arma::uvec& regpoints);
RcppExport SEXP _nonanR_lye_rosenstein(SEXP XSEXP, SEXP samp_rateSEXP, SEXP mean_freqSEXP, SEXP nstepsSEXP, SEXP regpointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< double >::type samp_rate(samp_rateSEXP);
    Rcpp::traits::input_parameter< double >::type mean_freq(mean_freqSEXP);
    Rcpp::traits::input_parameter< int >::type nsteps(nstepsSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type regpoints(regpointsSEXP);
    rcpp_result_gen = Rcpp::wrap(lye_rosenstein(X, samp_rate, mean_freq, nsteps, regpoints));
    return rcpp_result_gen;
END_RCPP
}
// meanfreq
double meanfreq(const arma::vec& signal, double samp_rate);
RcppExport SEXP _nonanR_meanfreq(SEXP signalSEXP, SEXP samp_rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type signal(signalSEXP);
    Rcpp::traits::input_parameter< double >::type samp_rate(samp_rateSEXP);
    rcpp_result_gen = Rcpp::wrap(meanfreq(signal, samp_rate));
    return rcpp_result_gen;
END_RCPP
}
// mfdfa
List mfdfa(arma::vec x, arma::vec q, int order, arma::uvec scales, double scale_ratio);
RcppExport SEXP _nonanR_mfdfa(SEXP xSEXP, SEXP qSEXP, SEXP orderSEXP, SEXP scalesSEXP, SEXP scale_ratioSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type order(orderSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type scales(scalesSEXP);
    Rcpp::traits::input_parameter< double >::type scale_ratio(scale_ratioSEXP);
    rcpp_result_gen = Rcpp::wrap(mfdfa(x, q, order, scales, scale_ratio));
    return rcpp_result_gen;
END_RCPP
}
// rqa
List rqa(arma::vec ts1, arma::vec ts2, unsigned int embed, unsigned int delay, int normalize, int rescale, int mindiagline, int minvertline, int t_win, double radius, int whiteline, int recpt);
RcppExport SEXP _nonanR_rqa(SEXP ts1SEXP, SEXP ts2SEXP, SEXP embedSEXP, SEXP delaySEXP, SEXP normalizeSEXP, SEXP rescaleSEXP, SEXP mindiaglineSEXP, SEXP minvertlineSEXP, SEXP t_winSEXP, SEXP radiusSEXP, SEXP whitelineSEXP, SEXP recptSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type ts1(ts1SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type ts2(ts2SEXP);
    Rcpp::traits::input_parameter< unsigned int >::type embed(embedSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type delay(delaySEXP);
    Rcpp::traits::input_parameter< int >::type normalize(normalizeSEXP);
    Rcpp::traits::input_parameter< int >::type rescale(rescaleSEXP);
    Rcpp::traits::input_parameter< int >::type mindiagline(mindiaglineSEXP);
    Rcpp::traits::input_parameter< int >::type minvertline(minvertlineSEXP);
    Rcpp::traits::input_parameter< int >::type t_win(t_winSEXP);
    Rcpp::traits::input_parameter< double >::type radius(radiusSEXP);
    Rcpp::traits::input_parameter< int >::type whiteline(whitelineSEXP);
    Rcpp::traits::input_parameter< int >::type recpt(recptSEXP);
    rcpp_result_gen = Rcpp::wrap(rqa(ts1, ts2, embed, delay, normalize, rescale, mindiagline, minvertline, t_win, radius, whiteline, recpt));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nonanR_Ent_Ap", (DL_FUNC) &_nonanR_Ent_Ap, 3},
    {"_nonanR_Ent_Samp", (DL_FUNC) &_nonanR_Ent_Samp, 3},
    {"_nonanR_Ent_Sym", (DL_FUNC) &_nonanR_Ent_Sym, 3},
    {"_nonanR_ami", (DL_FUNC) &_nonanR_ami, 4},
    {"_nonanR_bayesH", (DL_FUNC) &_nonanR_bayesH, 2},
    {"_nonanR_cppsr", (DL_FUNC) &_nonanR_cppsr, 2},
    {"_nonanR_dfa", (DL_FUNC) &_nonanR_dfa, 5},
    {"_nonanR_fgn_sim", (DL_FUNC) &_nonanR_fgn_sim, 4},
    {"_nonanR_poly_residuals", (DL_FUNC) &_nonanR_poly_residuals, 2},
    {"_nonanR_lm_c", (DL_FUNC) &_nonanR_lm_c, 2},
    {"_nonanR_seq_int", (DL_FUNC) &_nonanR_seq_int, 1},
    {"_nonanR_lye_rosenstein", (DL_FUNC) &_nonanR_lye_rosenstein, 5},
    {"_nonanR_meanfreq", (DL_FUNC) &_nonanR_meanfreq, 2},
    {"_nonanR_mfdfa", (DL_FUNC) &_nonanR_mfdfa, 5},
    {"_nonanR_rqa", (DL_FUNC) &_nonanR_rqa, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_nonanR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
