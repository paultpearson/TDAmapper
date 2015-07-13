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
