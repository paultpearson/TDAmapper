#' mapperVertices function
#'
#' The input to this function is a TDAmapper class object and the output
#' is a data frame of vertices that can be used as input to the networkD3 
#' plot utility.
#'
#' @param m An object of class TDAmapper that is the output of the mapper 
#' function.
#'
#' @return A data frame describing the vertices in the graph of the mapper 
#' output and the point labels that will be displayed when the mouse 
#' hovers over a vertex in the graph.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapperEdges}}
#' @keywords mapperVertices
#'
#' @examples
#' \dontrun{
#' X <- data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )
#' f <- X$y
#' m1 <- mapper(
#'        distance_matrix = dist(X),
#'        filter_values = f,
#'        num_intervals = 10,
#'        percent_overlap = 50,
#'        num_bins_when_clustering = 10)
#'        
#' pt_labels <- 1:length(f)
#' vertices <- mapperVertices(m1, pt_labels)
#' edges <- mapperEdges(m1)
#' 
#' # interactive plot
#' forceNetwork(Nodes = nodes, Links = links, 
#'             Source = "Linksource", Target = "Linktarget",
#'             Value = "Linkvalue", NodeID = "Nodename",
#'             Group = "Nodegroup", opacity = 0.8, 
#'             linkDistance = 10, charge = -400)
#' }
#' @export
#'


mapperVertices <- function(m, pt_labels) {
    
    # Hovering over vertices gives the point labels:
    # convert the list of vectors of point indices to a list of vectors of labels
    labels_in_vertex <- lapply( m$points_in_vertex, FUN=function(v){ pt_labels[v] } )
    nodename <- sapply( sapply(labels_in_vertex, as.character), paste0, collapse=", ")
    nodename <- paste0("V", 1:m$num_vertices, ": ", nodename )
    
    # Hovering over vertices gives the point indices:
    # list the points in each vertex
    # nodename <- sapply( sapply(m$points_in_vertex, as.character), paste0, collapse=", ")
    # concatenate the vertex number with the labels for the points in each vertex
    #nodename <- paste0("V", 1:m$num_vertices, ": ", nodename )
    
    nodegroup <- m$level_of_vertex
    nodesize <- sapply(m$points_in_vertex, length)
    
    return(data.frame( Nodename=nodename, 
                       Nodegroup=nodegroup, 
                       Nodesize=nodesize ))
    
}
