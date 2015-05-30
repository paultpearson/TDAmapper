#' Plot an object of class TDAmapper using igraph
#'
#' Plot method defined for objects of class "TDAmapper" using auto layout for igraph.
#'
#' @param x An object of class "TDAmapper".
#' @param ... Optional arguments to the igraph plot() function.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapper1D}}, \code{\link{mapper2D}}
#' @keywords plot.TDAmapper
#'
#' @return plot of the mapper object using igraph
#'
#' @import igraph
#'
#' @examples
#' \dontrun{
#' plot(x, ...)
#' }
#'
#' @export
#'
plot.TDAmapper <- function (x, ...){
  g <- igraph::graph.adjacency(x$adjacency, mode="undirected")
  plot(g, layout = layout.auto(g), ... )
}
