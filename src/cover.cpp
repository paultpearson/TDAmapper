#include <Rcpp.h>
using namespace Rcpp;

// Given a logical vector, returns an integer vector (1-based) of the positions in the vector which are true
IntegerVector which_true( LogicalVector x) {
  int nx = x.size();
  std::vector<int> y;
  y.reserve(nx);
  for(int i = 0; i < nx; i++) { if (x[i]) y.push_back(i+1); }
  return wrap(y);
}

// [[Rcpp::export]]
List constructLevelSets(const NumericMatrix& filter_values, const IntegerMatrix& index_set, const NumericVector& interval_length, const NumericVector& step_size, const NumericVector& filter_min) {
  const int n = index_set.nrow();
  const int d = index_set.ncol();
  
  NumericMatrix ls_bnds = NumericMatrix(2, d);
  List level_sets = List(n);
  LogicalVector level_set_test = LogicalVector(filter_values.nrow(), true);
  for (int i = 0; i < n; ++i){
    IntegerVector ls_idx  = index_set(i, _) - 1;
    NumericVector level_set_min = filter_min + (as<NumericVector>(ls_idx) * step_size);
    ls_bnds(0, _) = level_set_min;
    ls_bnds(1, _) = level_set_min + interval_length;
    
    // Reset to all true prior to doing logical range checks
    std::fill(level_set_test.begin(), level_set_test.end(), true);
    for (int d_i = 0; d_i < d; ++d_i){
      level_set_test = level_set_test & ((filter_values.column(d_i) >= ls_bnds(0, d_i)) & (filter_values.column(d_i) <= ls_bnds(1, d_i)));
    }
    // Don't explicitly need to save the bounds, but they may useful later
    level_sets[i] = List::create(_["points_in_level_set"] = which_true(level_set_test)); //, _["bounds"] = ls_bnds);
  }
  return(level_sets);
}


/*** R
# load("test.rdata")
# fv <- as.matrix(test$filter_values)
# is <- as.matrix(test$index_set)
# il <- as.numeric(test$interval_length)
# ss <- as.numeric(test$step_size)
# fmin <- as.numeric(test$filter_min)
# constructLevelSets(filter_values = fv, index_set = is, interval_length = il, step_size = ss, filter_min = fmin)
*/
