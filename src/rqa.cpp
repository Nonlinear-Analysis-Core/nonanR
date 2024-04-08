#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

arma::mat psr(arma::vec& x, int dim, int tau); // function for creating embedding matrix (phase-space reconstruction)

void dist_mat(arma::mat& x1, arma::mat& x2, arma::mat& dist); // function to compute distance matrix

void rp(arma::mat& x, double radius); // function for creating the recurrence plot

List line_stats(arma::mat& rp, int mindiagline, int minvertline, int t_win); // function for computing the variance metrics of recurrence

arma::vec diagonal_lines(arma::mat& rp, int mindiagline); // extract diagonal line lengths

arma::vec vertical_lines(arma::mat& rp, int minvertline); // extract vertical line lengths

const double tol = 0.0001;

//' Recurrence Quantification Analysis
//' 
//' This function performs recurrence quantification analysis.
//' 
//' @param ts1 A numerical time series
//' @param ts2 A numerical time series
//' @param embed The embedding dimension of the time series
//' @param delay The optimal time delay (lag)
//' @param normalize Should the time series be normalized? (0 = no, 1 = unit interval, 2 = z-score)
//' @param rescale Should the distance matrix be rescaled? (0 = no, 1 = max norm, 2 = min norm)
//' @param mindiagline The smallest number of diagonal points to be considered a line
//' @param minvertline smallest number of vertical points to be considered a line
//' @param t_win Theiler window
//' @param radius Minimum distance within which points are considered recurrent
//' @param whiteline not implemented
//' @param recpt Should recurrence plot be returned? (Not recommended for long series)
//' 
//' @returns The output of the algorithm is a list that includes:
//' \itemize{
//'  \item \code{rr}: (Recurrence rate), the overall percentage of recurrent points
//'  \item \code{det}: (determinism), the percentage of recurrent points that fall on a line
//'  \item \code{div}: (divergence), inverse of determinism i.e. 1/det
//'  \item \code{nrline}: (number of lines), total number of lines in the upper triangle
//'  \item \code{ratio}: (ratio), percent determinism/percent recurrence i.e det/rr
//'  \item \code{maxline}: (longest line), the number points in the longest diagonal line
//'  \item \code{meanline}: (average line), average length of diagonal lines
//'  \item \code{lam}: (laminarity), perecentage of points that fall on vertical lines
//'  \item \code{tt}: (trapping time), average length of vertical lines
//'  \item \code{vmax}: (longest vertical line), the number of points in the longest vertical line
//'  \item \code{entropy}: (Shannon entropy), based on distribution of line lengths
//'  \item \code{rentropy}: (relative entropy), Shannon entropy normalized by number of lines 
//' }
//' Optionally, you can return the recurrence matrix but caution should be taken when returning this especially with larger time series as it can take some time.
//' 
//' @import Rcpp
//' @export
//' 
//' @details This function performs recurrence quantification analysis (RQA) and its bivariate extension, cross recurrence quantification analysis (CRQA) on time series data that have (potentially) been embedded in higher dimension than the originating series. A common approach for univariate series involves several steps: First, identify the optimal time delay as either the first zero crossing of the autocorrelation function or the first minimum of the average mutual information function. Second, the time series is unfolded into embed dimensions by creating time-delayed copies of the original series. One method for determining the number of dimensions is by the method of False Nearest Neighbors. Third, a distance matrix is computed among the embedded points of the series. A recurrence plot is constructed by passing the distance matrix through a heavyside function: distances less than or equal to the chosen radius are marked as 1 (recurrent); distances falling outside the radius are marked as 0 (not recurrent).
//' 
//' After constructing the recurrence plot, a number of measures are computed to characterize recurrent structure in the time series. These measures and their interpretation are well documented in the literature. We provide simple definitions for each recurrence metric below. In addition, we provide references to standard readings including a very readable introduction to RQA (i.e., Webber & Zbilut, 2005; Marwan et al., 2007).
//' 
//' @examples
//' # Create a sample time series
//' x = fgn_sim(n = 100, H = 0.8)
//' 
//' # Compute RQA
//' x.recpt = rqa(x, x, 1, 1, 0, 1, 2, 2, 0, .0001, 0, 1)
//' 
//' # Return recurrence plot
//' plot_rqa(x.recpt$rp)
//' 
//' 
//' @references 
//' - Webber, C. L., & Zbilut, J. P. (2005). Recurrence quantification analysis of nonlinear dynamical time series. In S. Riley and G. C. Van Orden (eds). Tutorials in contemporary nonlinear methods for the behavioral sciences.
//' 
//' - Marwan, N., Romano, M. C. Theil, M., & Kurths, J. (2007). Recurrence plots for the analysis of complex systems. Physics Reports, 438, 237-329.
// [[Rcpp::export]]
List rqa(arma::vec ts1, arma::vec ts2, unsigned int embed = 1,
         unsigned int delay = 1, int normalize=1,
         int rescale = 1, int mindiagline = 2, 
         int minvertline = 2, int t_win = 0, double radius=0.0001, 
         int whiteline = 0, int recpt = 0){
  
  // TODO:  Make sure time series are long enough
  // if(ts1.n_elem < embed*delay){
  //     stop("Phase-space (embed*delay) longer than ts1");
  //     }
  // if(ts2.n_elem < embed*delay){
  //     stop("Phase-space (embed*delay) longer than ts2");
  //     }
  
  // normalize 
  if (normalize > 0){
    switch(normalize){
    // normalize to unit interval
    case 1: ts1 = (ts1 - min(ts1))/max(ts1);
      ts2 = (ts2 - min(ts2))/max(ts2);
      // normlaize to a z-score
    case 2: ts1 = (ts1 - mean(ts1))/stddev(ts1);
      ts2 = (ts2 - mean(ts2))/stddev(ts2);
    }
  }
  
  // embed time series according to chosen dimension and time delay
  arma::mat psr_ts1 = psr(ts1, embed, delay);
  arma::mat psr_ts2 = psr(ts2, embed, delay);
  arma::mat dm(ts1.n_elem, ts1.n_elem);
  // dm.zeros();
  dist_mat(psr_ts1, psr_ts2, dm);
  // arma::mat dm_rescale(dm.n_rows, dm.n_cols);
  
  double rescaledist;
  
  // find indices of the distance matrix that fall within the prescriped radius
  if (rescale > 0){
    switch(rescale){
    
    // rescale distance matrix to mean distance
    case 1: rescaledist = mean(mean(dm));
      dm = (dm/rescaledist)/100;
      
      // rescale distance matrix to max distance
    case 2: rescaledist = max(max(dm));
      dm = (dm/rescaledist)/100;
    }
  } else{
    dm = dm.t();
  }
  
  // create the recurrence plot matrix
  // arma::mat recurrence_plot = rp(dm, radius).t();
  // List output = line_stats(recurrence_plot, mindiagline, minvertline, t_win);
  rp(dm, radius);
  // Rcout << "The sum is " << arma::accu(dm) << "\n";
  List output = line_stats(dm, mindiagline, minvertline, t_win);
  
  
  // comute recurrence matrix
  if (recpt > 0){
    return List::create(Rcpp::Named("rp") = dm, 
                        Rcpp::Named("rqa") = output);
  }else{
    return List::create(Rcpp::Named("rqa") = output);
  }
  
  
}


arma::mat psr(arma::vec& x, int dim, int tau){
  unsigned int embed = dim;
  unsigned int delay = tau;
  
  // find size of psr matrix and allocate memory
  arma::mat space(x.n_elem - (embed-1)*delay, embed);
  space.zeros();
  
  // insert first column 
  space.col(0) = x(arma::span(0, x.n_elem - embed*delay + delay -1));
  
  // main loop that does the embedding
  unsigned int start = 0;
  unsigned int stop = x.n_elem - embed*delay + delay - 1;
  for (unsigned int i = 1; i < embed; ++i){
    start += delay;
    stop  += delay;
    space.col(i) = x(arma::span(start,stop));
  }
  return space;
  
}

// TODO: This function could be additional optomized
//      Currently  it is aobut 2-3 time slower than rdist in the fields package

void dist_mat(arma::mat& x1, arma::mat& x2, arma::mat& dist){
  // TODO: distance matrix and recurrence plot could be combined in one for loop
  //      CAN'T BE DONE:  ENTIRE DISTANCE MATRIX MUST BE KNOWN BEFORE RESCALING, 
  //      IMPLING THAT RECURRENCE PLOT MUST BE CALCULATED AFTER THE FAC
  // TODO: add in check for matrix and throw error
  // TODO: also check that matrices have same dimension
  
  //NumericVector tempx1(x1.cols());
  // arma::mat dist(x1.n_rows, x2.n_rows);
  
  for ( unsigned int i = 0; i < x1.n_rows; ++i ){
    
    for (unsigned int j = 0; j < x1.n_rows; ++j){
      
      // dist(i,j) = sqrt(arma::accu(pow(x2.row(j) - x1.row(i),2)));
      dist(i,j) = sqrt(arma::accu(
        (x2.row(j) - x1.row(i))%(x2.row(j) - x1.row(i))
      ));
    }
    
  }
  //dist.t();
  
  // return(dist);
}

void rp(arma::mat& x, double radius){
  
  // arma::mat recurrence_plot = as<arma::mat>(x);
  // find indices outside of specified range and flag as 999
  arma::uvec index1 = find(x > radius);
  // x.elem( index1 ).fill(999);
  
  // find all values inside the radius and replace with 1
  // arma::uvec index2 = find(x <= radius);
  // x.elem( index2 ).fill(1);
  x.elem( find(x <= radius) ).fill(1);
  // replace all 999s with 0s
  x.elem( index1 ).fill(0);
  
  // return x;
}



List line_stats(arma::mat & rp, int mindiagline, int minvertline, int t_win){
  
  double rr = 0.0, det = 0.0, maxline = 0.0, meanline = 0.0, entropy = 0.0;
  double lam = 0.0, tt = 0.0, vmax = 0.0, rentropy = 0.0;
  arma::uword nrline;
  nrline = 0;
  double rp_sum = arma::accu(rp);
  
  // TODO: DONE: I need to make theiler window adjustments here
  //      In addition, I will follow Marwan and blank account recurrence on the
  //      diagonal. i.e. zero out the line of incidence before computing stats
  // TODO: For cross-recurrence, I don't wan to blank out LOI. This can be
  //      controlled by the user.
  // BUG: theiler window is not being applied to the lower diagonal.
  // BUG: passing by reference is allowing the RP itself be altered. Not 
  //      necessarily a bug because it is what was analyzed.
  
  if (t_win > 0){
    for (int i = -t_win; i < t_win; ++i){
      rp.diag(i).zeros();
    }
    // for (int i = 0; i < t_win; ++i){
    //     rp.diag(-i).zeros();
    // }
  }else{
    rp.diag().zeros();
  }
  
  // calculate percent recurrence
  // double nrecurrence = arma::accu(rp);
  // if (arma::accu(rp) > 0){
  if(rp_sum > tol){
    // rr = 100*nrecurrence/(rp.n_rows*rp.n_rows);
    // rr = 100*arma::accu(rp)/(rp.n_rows*rp.n_rows);
    rr = 100*rp_sum/(rp.n_rows*rp.n_rows);
  }
  
  // create initialize vector to store lengths
  // int ndiags = x.rows();
  arma::vec dlengths = diagonal_lines(rp, mindiagline);
  arma::vec vlengths = vertical_lines(rp, minvertline);
  
  
  // TODO: make decisions about upper, lower, both triangles. 
  //      For now, I will do both. Doing upper would accelerate
  //      computation time.
  arma::vec diagonal_lines = dlengths(find(dlengths > tol));
  arma::vec vert_lines = vlengths(find(vlengths > tol));
  
  // estiamte number of lines parameter
  nrline = diagonal_lines.n_elem;
  // arma::vec u = (unique(diagonal_lines));
  // double n_unique = u.n_elem;
  
  
  
  // average length of lines
  if(diagonal_lines.n_elem > 0){
    meanline = mean(diagonal_lines);
    
    // maximum line length
    maxline = diagonal_lines.max();
    
    // perent determinism.  divide length by 29
    det = (arma::accu(diagonal_lines)/arma::accu(rp))*100;
    
    //TODO: entropy and normalized entropy.  These functions are not yet completely 
    //      equivalen those found in the CRQA package by Moreno.  In fact, I'm not sure
    //      that the calculations in Morenoa are correct outside of categorical recurrence
    //      NOTE: Entropy value seem to match Marwan.
    
    // calculate entropy and relative entropy
    arma::vec counts = arma::conv_to<arma::vec>::from(hist(diagonal_lines,arma::linspace(1,rp.n_rows,rp.n_rows)));
    arma::vec prob = counts/sum(counts);
    arma::vec p = prob(find(prob > 0));
    entropy = -arma::accu(p % arma::log(p));
    
    
    // relative entropy following form Moreno and Dale
    double nbins = p.n_elem;
    double denom = -1*std::log(1/nbins);
    rentropy = entropy/denom;
  }
  
  if(vert_lines.n_elem > 0){
    lam = (sum(vert_lines)/arma::accu(rp))*100;
    tt = mean(vert_lines);
    vmax = max(vert_lines);
  }
  
  // TODO: ADD IN OTHER MEASURES FROM MARWAN E.G., T1, T2, RTE, CLUST, TRANS
  return List::create(Named("rr") = rr,
                      Named("det") = det,
                      Named("div") = 1/maxline,
                      Named("nrline") = nrline,
                      Named("ratio") = det/rr,
                      Named("maxline") = maxline,
                      Named("meanline") = meanline,
                      Named("lam") = lam,
                      Named("tt") = tt,
                      Named("vmax") = vmax,
                      Named("entropy") = entropy,
                      Named("rentropy") = rentropy);
}


arma::vec diagonal_lines(arma::mat& rp, int mindiagline){
  
  arma::vec lengths(1);
  lengths(0) = 0;
  
  // start interation with first diagonal
  // extract diagonals one by and and find line lengths
  int start = rp.n_rows;
  int stop = rp.n_rows;
  start *= -1;
  start += 1;
  
  for (int i = start; i < stop; ++i){
    
    //  check if vertical line contains any recurrent points
    // if not, skip to the next line and avoid those computations
    if (arma::accu(rp.diag(i)) > 0.0001){
      
      // zero pad to capture patterns that start and end with 1
      arma::vec pad1(10);
      arma::vec d = arma::join_cols(arma::join_cols(pad1, rp.diag(i)),pad1);
      
      // find all zeros
      arma::uvec inds = find(d < 1);
      arma::vec k = arma::conv_to<arma::vec>::from(inds);
      
      // take the first difference of the indices of zeros so that 
      // difference between them reveals the length of interceding 
      // stretches of 1s
      arma::vec diffs = arma::diff(k)-1;
      
      // get all lines longer than minvertline
      arma::vec diaglines = diffs.elem(find(diffs >= mindiagline));
      
      
      // grow vector of lines lengths on every loop
      lengths = arma::join_vert(lengths, diaglines);
      
    }
    
  }
  
  return lengths;
}


arma::vec vertical_lines(arma::mat& rp, int minvertline){
  
  arma::vec lengths(1);
  lengths(0) = 0;
  // start interation with first column
  // extract columns one by and and compute
  int n_cols = rp.n_cols;
  for (int i = 0; i < n_cols; ++i){
    
    //  check if vertical line contains any recurrent points
    // if not, skip to the next line and avoid those computations
    if (arma::accu(rp.col(i)) > 0.0001){
      // zero pad to capture patterns that start and end with 1
      // TODO: TRY TO ELIMINATE SOME UNWANTED STEPS HERE
      arma::vec pad1(10);
      // arma::vec pad2(10);
      // arma::vec d2 = rp.col(i);
      arma::vec d3 = arma::join_cols(pad1, rp.col(i));
      // arma::vec d = arma::join_cols(d3, pad2);
      arma::vec d = arma::join_cols(d3,pad1);
      
      // find all zeros
      arma::uvec inds = find(d < 1);
      
      arma::vec k = arma::conv_to<arma::vec>::from(inds);
      
      // take the first difference of the indices of zeros so that 
      // difference between them reveals the length of interceding 
      // stretches of 1s
      arma::vec diffs = arma::diff(k)-1;
      
      // get all lines longer than minvertline
      arma::vec vertlines = diffs.elem(find(diffs >= minvertline));
      
      
      // grow vector of lines lengths on every loop
      lengths = arma::join_vert(lengths, vertlines);
      
    }
    
  }

  return lengths;
}
