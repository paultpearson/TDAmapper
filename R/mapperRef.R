#' Reference Class (R5) implementation of Mapper for the TDAmapper package in R
#' Composes a set of classes to perform Mapper efficiently.
#' Author: Matt Piekenbrock 

## Create the reference class, along with required field types
mapper_ref <- setRefClass("MapperRef", fields = list(X="ANY", cover="ANY", clustering_algorithm="ANY", G="ANY", config="list"))

## The cover stores the filter values
mapper_ref$methods(setCover = function(fv, k, g, measure = "euclidean"){
  if (is.null(dim(fv)) && is.numeric(fv)){ fv <- matrix(fv, nrow = length(fv), ncol = 1) }
  if (is.null(dim(fv)) || (!"matrix" %in% class(fv))) { stop("Filter values must be numeric and in matrix form") }
  cover <<- cover_ref$new(filter_values = fv, k, g, measure = measure)
})

## Changes the clustering algorithm used by mapper. 
## Must accept a 'dist' object and return a static or 'flat' clustering result
mapper_ref$methods(setClusteringAlgorithm = function(cl = c("single", "ward.D", "ward.D2", "complete", "average", "mcquitty", "median", "centroid")){
  
  ## Default to single linkage clustering with heuristic histogram-like cut rule
  if (missing(cl) || cl == "single"){
    clustering_algorithm <<- function(dist_x, num_bins_when_clustering = 10){
      hcl <- hclust(dist_x, method = "single")
      cut_height <- cluster_cutoff_at_first_empty_bin(heights = hcl$height, diam = max(dist_x), num_bins_when_clustering = num_bins_when_clustering)
      as.vector(cutree(hcl, h = cut_height))
    } 
  } 
  ## If other linkage criterion is provided, use that
  else if (class(cl) == "character"){
    hclust_opts <- c("single", "ward.D", "ward.D2", "complete", "average", "mcquitty", "median", "centroid")
    if (!cl %in% hclust_opts){ stop(sprint("Unknown linkage method passed. Please use one of (%s). See ?hclust for details.", paste0(hclust_opts, collapse = ", "))) }
    clustering_algorithm <<- function(dist_x, num_bins_when_clustering = 10){
      hcl <- hclust(dist_x, method = cl)
      cut_height <- cluster_cutoff_at_first_empty_bin(heights = hcl$height, diam = max(dist_x), num_bins_when_clustering = num_bins_when_clustering)
      as.vector(cutree(hcl, h = cut_height))
    }
  }  
  ## Allow the user to use their own clustering algorithm if they desire. Must accept a dist object and return a flat clustering result. 
  else if (class(f) == "function"){
    clustering_algorithm <<- f
  } else {
    stop("'cl' must be either a linkage criterion (character) or a clustering algorithm (function)")
  }
})

## The cover stores the filter values
mapper_ref$methods(computeNodes = function(...){
  if ("uninitializedField" %in% class(cover)){ stop("Cover must be constructed prior to computing nodes.") }
  
  ## Compute the interpoint distances for each subset 
  #  LS_dist <- lapply(m$cover$level_sets, function(ls){ dist(X[ls$points_in_level_set,], method = "euclidean") })
  LS_dist <- lapply(cover$level_sets, function(ls){
    if ("dist" %in% class(X)){
      dist_subset(dist = X, idx = ls$points_in_level_set)
    } else { dist(X[ls$points_in_level_set,], method = "euclidean") }
  })
  
  ## Initialize the graph as an empty list 
  G <<- list()
  
  ## Iterate through the 'dist' objects stored for each level set, perform the clustering
  cl_res <- lapply(LS_dist, function(ls_dist) { 
    if (length(ls_dist) > 0) clustering_algorithm(ls_dist, ...)
  })
  
  ## Precompute useful variables to know
  n_nodes <- sum(sapply(cl_res, function(cl) length(unique(cl))))
  node_idx <- unlist(mapply(function(cl, ls_i) if (length(cl) > 0) paste0(ls_i, ".", unique(cl)), cl_res, 1:length(cl_res)))
  n_lvlsets <- length(cover$level_sets)
  
  ## Agglomerate the nodes into a list. This matches up the original indexes of the filter values with the 
  ## the clustering results, such that each node stores the original filter index values as well as the 
  ## creating a correspondence between the node and it's corresponding level set flat index (lsfi)
  ## TODO: Cleanup and either vectorize or send down to C++
  G$nodes <<- vector(mode = "list", length = n_nodes)
  n_i <- 1L
  for (lsfi in 1:n_lvlsets){
    cl_i <- cl_res[[lsfi]]
    ## Extract the node point indices for each cluster 
    node_pt_idx <- lapply(unique(cl_i), function(cl_idx) cover$level_sets[[lsfi]]$points_in_level_set[which(cl_i == cl_idx)])
    for (node in node_pt_idx){
      attr(node, "level_set") <- lsfi
      G$nodes[[n_i]] <<- node
      n_i <- n_i + 1L
    }
  }
})

mapper_ref$methods(computeEdges = function(){
  if ("uninitializedField" %in% class(G)){ stop("Graph with nodes must be constructed before computing edges.") }
  
  ## Retrieve the level set flat indices (LSFI) for each corresponding node
  node_lsfi <- sapply(G$nodes, function(node) attr(node, "level_set")) # which level set (by value) each node (by index) is in 
  
  ## Create map from the level set flat index (by index) to the node indices the level set stores
  ## Note in this map empty level sets are NULL
  ls_node_map <- lapply(seq(length(cover$level_sets)), function(lvl_set_idx) {
    node_indices <- which(node_lsfi == lvl_set_idx)
    if (length(node_indices) == 0){ return(NULL) } else { return(node_indices) }
  }) 
  
  ## Retrieve the valid level set index pairs to compare. In the worst case, with no cover-specific optimization 
  ## or 1-skeleton assumption, this may just be all pairwise combinations of LSFI's for the full simplicial complex 
  ls_to_compare <- cover$valid_pairs()
  
  ## Let Rcpp handle the O(n^2) non-empty intersection checks
  G$adjacency <<- adjacencyCpp(ls_pairs = ls_to_compare, nodes = G$nodes, ls_node_map = ls_node_map);
})


mapper_ref$methods(plotNetwork = function(){
  agg_pt_fv <- sapply(G$nodes, function(n_idx){ apply(matrix(cover$filter_values[n_idx,], nrow = length(n_idx)), 1, mean)})
  agg_node_fv <- sapply(agg_pt_fv, mean)
  exp_factor <- log(sapply(G$nodes, function(n_idx){ length(n_idx) }))
  rbw_pal <- rev(rainbow(100, start = 0, end = 4/6))
  binned_idx <- cut(agg_node_fv, breaks = 100, labels = F)
  base_net <- network::network.initialize(nrow(G$adjacency), directed = F)
  plot(network::network.adjacency(x = G$adjacency, g = base_net), 
       vertex.col = rbw_pal[binned_idx], vertex.cex = exp_factor, 
       label = 1:length(G$nodes), displaylabels = TRUE)
})

mapper_ref$methods(computeGain = function(){
  if ("uninitializedField" %in% class(filter_values)){ stop("Filter values must be set prior to constructing a valid cover.") }
  
})

## S3-like print override 
mapper_ref$methods(show = function(){
  if ("dist" %in% class(X)){ n <- attr(X, "Size") } else { n <- nrow(X) }
  if (!is.null(config$call)) 
    cat("\nCall:\n", deparse(config$call), "\n\n", sep = "")
  message <- sprintf("Mapper construction for %d objects", n)
  if (!"uninitializedField" %in% class(cover)){ message <- append(message, cover$summary()) }
  if (!"uninitializedField" %in% class(clustering_algorithm)){ message <- append(message, cover$summary()) }
  writeLines(message)
})



## Exports the internal mapper core structures to a TDAmapper output 
mapper_ref$methods(exportTDAmapper = function(){
  level_of_vertex <- sapply(G$nodes, function(ni) attr(ni, "level_set"))
  structure(list( adjacency = G$adjacency, 
                  num_vertices = length(G$nodes), 
                  level_of_vertex = level_of_vertex, 
                  points_in_vertex = lapply(G$nodes, as.vector), 
                  points_in_level = unname(lapply(cover$level_sets, function(ls) ls$points_in_level_set)), 
                  vertices_in_level = lapply(1:length(cover$level_sets), function(ls_idx) { 
                    tmp <- which(level_of_vertex == ls_idx)
                    if (length(tmp) == 0){ return(-1) } 
                    else return(tmp)
                  })), 
                  class = "TDAmapper")
})

mapper_ref$methods(getLevel = function(){
  
})
mapper_ref$methods(computeGraph = function(){
  
})