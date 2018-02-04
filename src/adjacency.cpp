#include <Rcpp.h>
using namespace Rcpp;


// Fast partial-sort/binary-search check to see if the intersection between two given vectors has non-zero length
// Loosely based off of bugged version found here: https://stackoverflow.com/questions/21359432/a-c-version-of-the-in-operator-in-r
bool any_is_in(const IntegerVector& x, const IntegerVector& y){
  std::vector<int> y_sort(y.size());
  std::partial_sort_copy (y.begin(), y.end(), y_sort.begin(), y_sort.end()); // partial-sorted elements of y copied to y_sort
  const int nx = x.size();
  for (int i = 0; i < nx; ++i) {  
    if (std::binary_search(y_sort.begin(), y_sort.end(), x[i])) {
      return(true); // end the search 
    }
  }
  return(false);
}

// adjacencyCpp: Creates an adjacency matrix forming edges between nodes w/ non-empty intersections
// Parameters: 
//  ls_pairs := Integer Matrix of level index pairs to consider
//  nodes := List of nodes (each element of which is a vector containing the point indices contained in the node)
//  node_map := List where each index corresponds to the ordered level set flat indices, and each element the indices of the nodes in that level set 
// [[Rcpp::export]]
IntegerMatrix adjacencyCpp(const IntegerMatrix& ls_pairs, const List& nodes, const List& ls_node_map) {
  int n = nodes.length();
  IntegerMatrix adj_mat(n, n);  
  for (int i = 0; i < ls_pairs.nrow(); ++i){
    
    // Get the current pair of level sets to compare; skip if either are empty
    const int ls_1 = ls_pairs(i, 0), ls_2 = ls_pairs(i, 1);
    if ( Rf_isNull(ls_node_map.at(ls_1 - 1)) || Rf_isNull(ls_node_map.at(ls_2 - 1))){
      continue;
    }
    const IntegerVector& nodes1 = ls_node_map.at(ls_1 - 1);
    const IntegerVector& nodes2 = ls_node_map.at(ls_2 - 1);
    
    // Compare the nodes within each level set
    for (IntegerVector::const_iterator n1 = nodes1.begin(); n1 != nodes1.end(); ++n1){
      for (IntegerVector::const_iterator n2 = nodes2.begin(); n2 != nodes2.end(); ++n2){
        // Retrieve node indices 
        const IntegerVector& n1_idx = nodes[*n1 - 1];
        const IntegerVector& n2_idx = nodes[*n2 - 1];
        // Add edge between the two if they share a data point
        bool intersect_check = any_is_in(n1_idx, n2_idx);
        if (intersect_check){
          adj_mat(*n1 - 1, *n2 - 1) = 1;
          adj_mat(*n2 - 1, *n1 - 1) = 1;
        }
      }
    }
  }
  return adj_mat;
}

/*** R
# load("test.rdata")
# adjacencyCpp(ls_pairs = test$ls_pairs, nodes = test$nodes, ls_node_map = test$ls_node_map)
*/
