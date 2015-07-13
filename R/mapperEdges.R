#' mapperEdges function
#'
#' The input to this function is a TDAmapper class object and the output
#' is a data frame of edges that can be used as input to the networkD3 
#' plot utility.
#'
#' @param m An object of class TDAmapper that is the output of the mapper function.
#'
#' @return A data frame describing the edges in the graph of the mapper output.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapperVertices}}
#' @keywords mapperEdges
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

mapperEdges <- function(m) {
    linksource <- c()
    linktarget <- c()
    linkvalue <- c()
    k <- 1
    for (i in 2:m$num_vertices) {
        for (j in 1:(i-1)) {
            if (m$adjacency[i,j] == 1) {
                linksource[k] <- i-1
                linktarget[k] <- j-1
                linkvalue[k] <- 2
                k <- k+1
            }
        }
    }
    return( data.frame( Linksource=linksource,
                        Linktarget=linktarget, 
                        Linkvalue=linkvalue ) )
    
}
