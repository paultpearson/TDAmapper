#' Print an object of class TDAmapper
#'
#' Print method defined for objects of class "TDAmapper".
#'
#' @param x An object of class "TDAmapper".
#' @param ... Optional arguments.
#'
#' @author Paul Pearson, \email{pearsonp@@hope.edu}
#' @references \url{https://github.com/paultpearson/TDAmapper}
#' @seealso \code{\link{mapper1D}}, \code{\link{mapper2D}}
#' @keywords print.TDAmapper
#'
#' @return Print the number of vertices in the mapper object
#'
#' @examples
#' \dontrun{
#' print(x, ...)
#' }
#'
#' @export
#'
print.TDAmapper <- function(x, ...) {
	cat("Call:\n", x$call, "\n")
	cat("Number of vertices: ", x$num_vertices, "\n")
	cat("Index of the level set of vertex i: ", x$level_of_vertex[1:10], "\n")
	cat("Indices for points in vertex i: ", x$points_in_vertex[[1:3]], "\n")
	cat("Indices for points in level set i: ", x$points_in_level[[1:3]], "\n")
	cat("Indices for vertices in level set i: ", x$vertices_in_level[[1:3]], "\n")
}
