# TDAmapper: Topological Data Analysis using Mapper


## Description

An R package for using discrete Morse theory to analyze a data set using the Mapper algorithm described in:

> G. Singh, F. Memoli, G. Carlsson (2007).  Topological Methods for the Analysis of High Dimensional Data Sets and 3D Object Recognition, Point Based Graphics 2007, Prague, September 2007.


## Installation

To install the latest version of this R package directly from github:

    install.packages("devtools")
    library(devtools)
    devtools::install_github("paultpearson/TDAmapper")
    library(TDAmapper)

To install from Github you might need: 

- **Windows:** Rtools (http://cran.r-project.org/bin/windows/Rtools/)
- **OS X:** xcode (from the app store)
- **Linux:** apt-get install r-base-dev (or similar).

	
## Example 1: using mapper1D to identify an oval as S^1

    # The fastcluster package is not necessary.  By loading the
	# fastcluster package, the fastcluster::hclust() function 
	# automatically replaces the slower stats::hclust() function
	# whenever hclust() is called.
    install.packages("fastcluster") 
    require(fastcluster) 

	# The igraph package is necessary to view simplicial complexes
    # (undirected graph) resulting from mapper1D().
    install.packages("igraph") 
    library(igraph)
    
    # sample points from an oval
    set.seed("1")
    t <- runif(100, min=0, max=6.3) # theta
    X <- data.frame( x = 2*cos(t), y = sin(t) )
    d <- dist(X)
    plot(X[,1], X[,2])

    filter <- X[,2] # height projection
    num_intervals <- 10
    percent_overlap <- 50
    num_bins_when_clustering <- 10

    map1 <- mapper1D(d, filter, num_intervals, percent_overlap, num_bins_when_clustering)

    map1$num_vertices

    g <- graph.adjacency(map1$adjacency, mode="undirected")
    plot(g, layout = layout.auto(g) )


## Example 2: using mapper2D to identify an oval as S^1

    # sample points from an oval
    set.seed("2")
    t <- runif(200, min=0, max=6.3) # theta
    X <- data.frame( x = 2*cos(t), y = sin(t) )
    d <- dist(X)
    plot(X[,1], X[,2])

    #filter1 <- X[,1] # projection onto x
    #filter2 <- X[,2] # projection onto y
    filter <- list( X[,1], X[,2] )
    num_intervals <- c(5, 5)
    percent_overlap <- 50
    num_bins_when_clustering <- 10

    map2 <- mapper2D(d, filter, num_intervals, percent_overlap, num_bins_when_clustering)

    map2$num_vertices

    g2 <- graph.adjacency(map2$adjacency, mode="undirected")
    plot(g2, layout = layout.auto(g2) )


## Example 3: using mapper1D to identify two independent spirals as two line segments

    # sample points from two intertwined spirals
    #set.seed("1")
    t <- runif(100, min=1, max=6.3) # theta
    X <- data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )
    d <- dist(X)
    plot(X[,1], X[,2])
        
    filter <- X[,2] # height projection
    num_intervals <- 10
    percent_overlap <- 50
    num_bins_when_clustering <- 10

    map3 <- mapper1D(d, filter, num_intervals, percent_overlap, num_bins_when_clustering)
        
    g3 <- graph.adjacency(map3$adjacency, mode="undirected")
    plot(g3, layout = layout.auto(g3) )
