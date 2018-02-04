#include <Rcpp.h>
using namespace Rcpp;

#define INDEX_TF(N,to,from) (N)*(to) - (to)*(to+1)/2 + (from) - (to) - (1)

// Given a 'dist' object (vector) and an integer vector of indices (1-based), create a new dist object 
// represented by the subset of 'dist' indexed by 'idx'
// [[Rcpp::export]]
NumericVector dist_subset(const NumericVector& dist, IntegerVector idx){
  const int n = dist.attr("Size");
  const int cl_n = idx.length();
  NumericVector new_dist = NumericVector((cl_n * (cl_n - 1))/2);
  int ii = 0; 
  for (IntegerVector::iterator i = idx.begin(); i != idx.end(); ++i){
    for (IntegerVector::iterator j = i; j != idx.end(); ++j){
      if (*i == *j) { continue; }
      const int ij_idx = INDEX_TF(n, (*i < *j ? *i : *j) - 1, (*i < *j ? *j : *i) - 1); 
      new_dist[ii++] = dist[ij_idx];
    }
  }
  new_dist.attr("Size") = cl_n;
  new_dist.attr("class") = "dist";
  return(new_dist);
}
