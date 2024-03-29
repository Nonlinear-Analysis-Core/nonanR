#define ARMA_NO_DEBUG //decomment to speed up code once tested
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

arma::mat psr(arma::vec& x, int dim, int tau); // Phase space reconstruction

void dist_mat(arma::mat& x1, arma::mat& x2, arma::mat& dist); // Distance matrix

void rp1(arma::mat& x, double radius); // Recurrence plot - Radius

void rp2(arma::mat& x, double& recurrence, double radius0, double radius1); // Recurrence plot - Recurrence

List line_stats(arma::mat& rp, int mindiag, int minvert); // Variance metrics of recurrence

double rr(arma::mat& rp); // Recurrence rate

arma::vec diagonal_lines(arma::mat& rp, int mindiag); // Diagonal line lengths

arma::vec vertical_lines(arma::mat& rp, int minvert); // Vertical line lengths

const double tol = 0.0001; // Set default values for some parameters

//' Fast Recurrence Quantification Analysis
//' 
//' This function perform recurrence quantification analysis faster.
//' 
//' @param data A numerical time series
//' @param dim The embedding dimension of the time series
//' @param tau The optimal time delay (lag)
//' @param normalize Should the time series be normalized? (0 = no, 1 = unit interval, 2 = z-score)
//' @param rescale Should the distance matrix be rescaled? (0 = no, 1 = mean norm, 2 = max norm)
//' @param method Which criteria will be used to perform the analysis? (1 = radius, 2 = reccurence) 
//' @param threshold A value that specifies the criteria (If method = 1, radius to compute recurrence plot, If method = 2, desired recurrence from computed recurrence plot)
//' @param mindiag The smallest number of diagonal points to be considered a line
//' @param minvert The smallest number of vertical points to be considered a line
//' 
//' @returns The output of the algorithm is a list that includes : 
//' 
//' RQA : 
//' \itemize{
 //'  \item \code{rec}: (Recurrence rate), the overall percentage of recurrent points
 //'  \item \code{det}: (determinism), the percentage of recurrent points that fall on a line
 //'  \item \code{div}: (divergence), inverse of determinism i.e. 1/det
 //'  \item \code{nrline}: (number of lines), total number of lines in the upper triangle
 //'  \item \code{ratio}: (ratio), percent determinism/percent recurrence i.e det/rr
 //'  \item \code{dmax}: (longest line), the number points in the longest diagonal line
 //'  \item \code{dmean}: (average line), average length of diagonal lines
 //'  \item \code{lam}: (laminarity), perecentage of points that fall on vertical lines
 //'  \item \code{vmean}: (trapping time), average length of vertical lines
 //'  \item \code{vmax}: (longest vertical line), the number of points in the longest vertical line
 //'  \item \code{entropy}: (Shannon entropy), based on distribution of line lengths
 //'  \item \code{rentropy}: (relative entropy), Shannon entropy normalized by number of lines 
//' }
//' 
//' RP : Recurrence plot
//' 
//' Radius : The radius used to generate the recurrence plot
//' 
 //' @import Rcpp
 //' @export
 //' 
 //' @details This function performs recurrence quantification analysis (RQA) on time series data that have (potentially) been embedded in higher dimension than the originating series. A common approach for univariate series involves several steps: First, identify the optimal time delay as either the first zero crossing of the autocorrelation function or the first minimum of the average mutual information function. Second, the time series is unfolded into embed dimensions by creating time-delayed copies of the original series. One method for determining the number of dimensions is by the method of False Nearest Neighbors. Third, a distance matrix is computed among the embedded points of the series. A recurrence plot is constructed by passing the distance matrix through a heavyside function: distances less than or equal to the chosen radius are marked as 1 (recurrent); distances falling outside the radius are marked as 0 (not recurrent).
 //' 
 //' After constructing the recurrence plot, a number of measures are computed to characterize recurrent structure in the time series. These measures and their interpretation are well documented in the literature. We provide simple definitions for each recurrence metric below. In addition, we provide references to standard readings including a very readable introduction to RQA (i.e., Webber & Zbilut, 2005; Marwan et al., 2007).
 //' 
 //' @examples
 //' # Create a sample time series
 //' x = fgn_sim(n = 100, H = 0.8)
 //' 
 //' # Compute RQA
 //' x.rqa = rqa_nonan(x, 1, 1, 0, 0, 2, 5, 2, 2)
 //' 
 //' @references 
 //' - Webber, C. L., & Zbilut, J. P. (2005). Recurrence quantification analysis of nonlinear dynamical time series. In S. Riley and G. C. Van Orden (eds). Tutorials in contemporary nonlinear methods for the behavioral sciences.
 //' 
 //' - Marwan, N., Romano, M. C. Theil, M., & Kurths, J. (2007). Recurrence plots for the analysis of complex systems. Physics Reports, 438, 237-329.


// [[Rcpp::export]]
List rqa_nonan(arma::vec data, 
               unsigned int dim = 1, unsigned int tau = 1,
               int normalize = 1, int rescale = 1,
               int method = 1, double threshold = 0.0001,
               int mindiag = 2, int minvert = 2){
  
  // Normalize
  if (normalize > 0){
    // Normalize to unit interval
    if (normalize == 1){
      data = (data - min(data))/(max(data)-min(data));
    }
    // Normlaize to a z-score
    if (normalize == 2){
      data = (data - mean(data))/stddev(data);
    }
  }
  
  // Embed time series according to chosen dimension and time delay
  arma::mat psr_data = psr(data, dim, tau);
  arma::mat dm(data.n_elem, data.n_elem);
  
  // Phase space reconstruction
  dist_mat(psr_data, psr_data, dm);
  
  double rescaledist;
  
  // Find indices of the distance matrix that fall within the prescriped radius
  if (rescale > 0){
    // rescale distance matrix to mean distance
    if (rescale == 1){
      rescaledist = mean(mean(dm));
      dm = (dm/rescaledist)/100;
    }
    // rescale distance matrix to max distance
    if (rescale == 2){
      rescaledist = max(max(dm));
      dm = (dm/rescaledist)/100;
    }
  }
  
  // Create the recurrence plot matrix
  if (method == 1){
    rp1(dm, threshold);
  }
  if (method == 2){
    double radius0 = 0.01;
    double radius1 = 0.5;
    rp2(dm, threshold, radius0, radius1);
    
  }
  
  double radius = threshold;
  
  List output = line_stats(dm, mindiag, minvert);
  
  return List::create(Rcpp::Named("RQA") = output,
                      Rcpp::Named("RP") = dm,
                      Rcpp::Named("Radius")=threshold);
  
}

/* Phase space reconstruction */
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

/* Distance matrix */
void dist_mat(arma::mat& x1, arma::mat& x2, arma::mat& dist){
  
  // Compute distance between each element (only for the lower half)
  for (unsigned int i = 1; i < x1.n_rows; ++i){
    for (unsigned int j = 0; j < i; ++j){
      dist(i,j) = sqrt(arma::accu(
        (x2.row(j) - x1.row(i))%(x2.row(j) - x1.row(i))
      ));
    }
  }
  dist.t();
  
}

/* Recurrence plot - Radius*/
void rp1(arma::mat& x, double radius){
  
  // Find indices outside of specified range and flag as 999
  arma::uvec index1 = find(x > radius);
  // Find all values inside the radius and replace with 1s
  x.elem( find(x <= radius) ).fill(1);
  // Replace all 999s with 0s
  x.elem( index1 ).fill(0);
  
  // Replace upper half and main diagonal with 0s
  for (unsigned int i = 0; i < x.n_rows; ++i){
    for (unsigned int j = i+1; j < x.n_cols; ++j){
      x(i,j) = 0;
    }
  }
  
}

/* Recurrence plot - Recurrence */
void rp2(arma::mat& x, double& recurrence, double radius0, double radius1){
  
  // Test recurrence rate with preset starting radius
  arma::mat xx = x;
  rp1(xx, radius0);
  double rec = rr(xx);
  unsigned int count = 1;
  
  // Adjust the lower bound of range for radius
  while (rec == 0 || rec > recurrence) {
    
    Rcout << "Specifying range of radius: ";
    Rcout << count << std::endl;
    
    if (rec == 0) {
      radius0 = radius0*2;
    } else if (rec > recurrence) {
      radius0 = radius0/1.5;
    }
    
    try {
      if (count>20) {
        throw 505;
      }
    }
    catch(...) {
      Rcerr << "Radius not found" <<std::endl;
      break; 
    }
    
    arma::mat xx = x;
    rp1(xx, radius0);
    rec = rr(xx);
    count = count+1;
    
  }
  
  xx = x;
  rp1(xx, radius1);
  rec = rr(xx);
  
  // Adjust the upper bound of range for radius
  while (rec < recurrence) {
    
    radius1 = radius1*2;
    
    xx = x;
    rp1(xx, radius1);
    rec = rr(xx);
    
  }
  
  // Search for radius with target recurrence rate
  unsigned int iter = 50; // Set iterations to search
  unsigned int ind;
  
  // Create empty vectors to track change of lower and upper bound and radius
  arma::vec lb(iter+1);
  arma::vec hb(iter+1);
  arma::vec mid(iter);
  arma::vec rad(iter);
  
  // Start searching with adjusted lower and upper bound
  lb[0] = radius0;
  hb[0] = radius1;
  
  // Iterate search
  for (unsigned int i = 0; i < iter; ++i){
    
    Rcout << "Iterated : ";
    Rcout << i;
    Rcout << ", Boundary : [";
    Rcout << lb[i];
    Rcout << " , ";
    Rcout << hb[i];
    Rcout << "]" << std::endl;
    
    mid[i] = (lb[i] + hb[i])/2;
    rad[i] = mid[i];
    
    xx = x;
    rp1(xx, rad[i]);
    rec = rr(xx);
    
    Rcout << "Current Recurrence : ";
    Rcout << rec << std::endl;
    
    // Replace upper or lower bound with the mid point of the range
    if (rec < recurrence) {
      hb[i+1] = hb[i];
      lb[i+1] = mid[i];
    } else {
      lb[i+1] = lb[i];
      hb[i+1] = mid[i];
    }
    
    if (abs(rec - recurrence) < 0.005) {
      ind = i;
      break;
    }
    
  }
  
  rp1(x, rad[ind]);
  recurrence = rad[ind];
  
}



/* Recurrence Rate */
double rr(arma::mat& rp){
  
  double rr = 0.0;
  double rp_sum = arma::accu(rp);
  rr = 100*(2*rp_sum-rp.n_rows)/(rp.n_rows*rp.n_rows);
  
  return rr;
  
}

List line_stats(arma::mat & rp, int mindiag, int minvert){
  
  double rec = 0.0, det = 0.0, dmax = 0.0, dmean = 0.0, entropy = 0.0;
  double lam = 0.0, vmax = 0.0, vmean = 0.0, rentropy = 0.0;
  arma::uword nrline;
  nrline = 0;
  double rp_sum = arma::accu(rp);
  
  // calculate percent recurrence
  if(rp_sum > tol){
    rec = rr(rp);
  }
  
  // create initialize vector to store lengths
  arma::vec dlengths = diagonal_lines(rp, mindiag);
  arma::vec vlengths = vertical_lines(rp, minvert);
  
  
  // TODO: make decisions about upper, lower, both triangles. 
  //      For now, I will do both. Doing upper would accelerate
  //      computation time.
  arma::vec diagonal_lines = dlengths(find(dlengths > tol));
  arma::vec vertical_lines = vlengths(find(vlengths > tol));
  
  // estiamte number of lines parameter
  nrline = diagonal_lines.n_elem*2;
  
  // average length of lines
  if(diagonal_lines.n_elem > 0){
    dmean = mean(diagonal_lines);
    
    // maximum line length
    dmax = diagonal_lines.max();
    
    // percent determinism.  divide length by 29
    det = ((arma::accu(diagonal_lines))/(rp_sum-rp.n_rows))*100;
    
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
  
  if(vertical_lines.n_elem > 0){
    lam = (sum(vertical_lines)/(2*rp_sum-rp.n_rows))*100;
    vmean = mean(vertical_lines);
    vmax = max(vertical_lines);
  }
  
  return List::create(Named("rec") = rec,
                      Named("det") = det,
                      Named("div") = 1/dmax,
                      Named("nrline") = nrline,
                      Named("ratio") = det/rec,
                      Named("dmax") = dmax,
                      Named("dmean") = dmean,
                      Named("lam") = lam,
                      Named("vmean") = vmean,
                      Named("vmax") = vmax,
                      Named("entropy") = entropy,
                      Named("rentropy") = rentropy);
  
}

/* Diagonal line lengths */
arma::vec diagonal_lines(arma::mat& rp, int mindiag){
  
  arma::vec lengths(1);
  lengths(0) = 0;
  
  // start interation with first diagonal
  // extract diagonals one by and and find line lengths
  int start = rp.n_rows;
  int stop = rp.n_rows;
  start *= -1;
  start += 1;
  
  for (int i = start; i < 0; ++i){
    
    //  check if diagonal line contains any recurrent points
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
      
      // get all lines longer than mindiag
      arma::vec diaglines = diffs.elem(find(diffs >= mindiag));
      
      
      // grow vector of lines lengths on every loop
      lengths = arma::join_vert(lengths, diaglines);
      
    }
    
  }
  
  return lengths;
}

/* Vertical line lengths */
arma::vec vertical_lines(arma::mat& rp, int minvert){
  
  arma::vec lengths(1);
  lengths(0) = 0;
  
  // start interation with first column
  // extract columns one by and and compute
  int n_cols = rp.n_cols;
  for (int i = 0; i < n_cols; ++i){
    
    //  check if vertical line contains any recurrent points
    // if not, skip to the next line and avoid those computations
    if (arma::accu(rp.col(i)) > 0.0001 || arma::accu(rp.row(i)) > 0.0001){
      
      // zero pad to capture patterns that start and end with 1
      // TODO: TRY TO ELIMINATE SOME UNWANTED STEPS HERE
      arma::vec v1 = rp.row(i).t();
      arma::vec v2 = rp.col(i);
      arma::vec v = join_cols(v1.head(i), v2.tail(n_cols-i));
      arma::vec pad1(10);
      arma::vec d = arma::join_cols(arma::join_cols(pad1, v),pad1);
      
      // find all zeros
      arma::uvec inds = find(d < 1);
      
      arma::vec k = arma::conv_to<arma::vec>::from(inds);
      
      // take the first difference of the indices of zeros so that
      // difference between them reveals the length of interceding
      // stretches of 1s
      arma::vec diffs = arma::diff(k)-1;
      
      // get all lines longer than mindiag
      arma::vec vertlines = diffs.elem(find(diffs >= minvert));
      
      
      // grow vector of lines lengths on every loop
      lengths = arma::join_vert(lengths, vertlines);
      
    }
    
  }
  
  return lengths;
}