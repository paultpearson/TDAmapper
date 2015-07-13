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
