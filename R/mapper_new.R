#' mapper_new function
#'
#' This function uses a filter function f: X -> R on a data set X that has n rows (observations) and k columns (variables).
#'
#' @param X Either a data matrix or an object of type 'dist'.
#' @param filter_values An n x m matrix, n-length vector real numbers, or an m-length list each contain n real numbers.
#' @param num_intervals Either a positive integer or a vector of m positive integers specifying the number of intervals to cover the filter values with. 
#' @param percent_overlap Either a number or m-length vector of values between 0 and 100 specifying how much adjacent intervals should overlap.
#' @param num_bins_when_clustering A positive integer that controls whether points in the same level set end up in the same cluster.
#' 
#' @return An object of class \code{TDAmapper} which is a list of items named \code{adjacency} (adjacency matrix for the edges), \code{num_vertices} (integer number of vertices), \code{level_of_vertex} (vector with \code{level_of_vertex[i]} = index of the level set for vertex i), \code{points_in_vertex} (list with \code{points_in_vertex[[i]]} = vector of indices of points in vertex i), \code{points_in_level} (list with \code{points_in_level[[i]]} = vector of indices of points in level set i, and \code{vertices_in_level} (list with \code{vertices_in_level[[i]]} = vector of indices of vertices in level set i.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}, Matt Piekenbrock, \email{piekenbrock.5@@wright.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#'
#' @examples
#' m1 <- mapper_new(
#'        dist_object = dist(data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )),
#'        filter_values = 2*cos(0.5*(1:100)),
#'        num_intervals = 10,
#'        percent_overlap = 50,
#'        num_bins_when_clustering = 10)
#' \dontrun{
#' #install.packages("igraph") 
#' library(igraph)
#' g1 <- graph.adjacency(m1$adjacency, mode="undirected")
#' plot(g1, layout = layout.auto(g1) )
#' }
#' @export

mapper_new <- function(X, filter_values, num_intervals, percent_overlap, num_bins_when_clustering, 
                       return_reference = FALSE, ...) {
  
  ### Check to see if 'dist_object' was passed in using old API call. 
  extra <- list(...)
  args <- c("dist_object", "distance_matrix")
  m <- pmatch(names(extra), args)
  if(any(is.na(m))) { stop("Unknown parameter: ", paste(names(extra)[is.na(m)], collapse = ", ")) } 
  names(extra) <- args[m]
  
  if(!is.null(extra$dist_object)) {
    warning("mapper accepts dist objects through the 'X' parameter. Setting X equal to the value of the argument passed via 'dist_object.'")
    X <- extra$dist_object
  }
  if(!is.null(extra$distance_matrix)) {
    warning("mapper accepts dist objects through the 'X' parameter. Setting X equal to the value of the argument passed via 'distance_matrix'")
    X <- extra$distance_matrix
  }
  
  ## SETUP 
  # Constructs a MapperRef instance, a mutable R5 class for generating the Mapper construction
  m <- mapper_ref$new(X = X)
    
  ## BEGIN COVER
  # The level set flat index (lsfi) is implicitly represented by the position of the level set in the 
  # level sets list (cover$level_sets). The name of each level set corresponds with its level set multi index (lsmi), 
  # and thus can be used alternatively as a key into the level sets map. 
  # (The looping construct and point queries have been optimized in 'setCover')
  if (is.list(filter_values)){ filter_values <- do.call(cbind, filter_values) }
  m$setCover(fv = filter_values, k = num_intervals, g = percent_overlap/100)
  ## END COVER
    
  ## BEGIN CLUSTER
  # Through the reference class, other clustering algorithms can be used. At this high-level API call, make suitable assumptions that 
  # single-linkage w/ bin-based cutting heuristic is preferred. 
  m$setClusteringAlgorithm(cl = "single")
  ## END CLUSTER
    
  ## BEGIN VERTEX CONSTRUCTION
  # Actually performs the clustering and vertex creation. Note that if a 'dist' object was passed in via X, then it 
  # will be subset based on the point indices within each level set. If X is a point cloud, then the (euclidean) dist 
  # object is computed locally within each level set. If the number of intervals is high enough and the data set is 
  # sufficiently large, this should result in large improvements in efficiency. 
  m$computeNodes(num_bins_when_clustering = num_bins_when_clustering)
  ## END VERTEX CONSTRUCTION
  
  ## BEGIN SIMPLICIAL COMPLEX
  # At this high-level interface call, only the 1-skeleton (graph) is computed within 'G.'
  m$computeEdges()
  ## END SIMPLICIAL COMPLEX
  
  ## Convert to 'TDAmapper' object or, if the reference class is wanted, return that
  mapperoutput <- if (return_reference) m else m$exportTDAmapper()
  return(mapperoutput)
} # end mapper function

