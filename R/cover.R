#' Reference Class (R5) implementation of Mapper for the TDAmapper package in R
#' Cover reference class
#' filter_values := Filter values 
#' num_intervals := vector of number of bins to cover the Z with (per dimension)
#' percent_overlap := vector of overlap percentages
#' level_sets := list of level_sets (including min/max bounds)
cover_ref <- setRefClass("Cover", fields = c("filter_values", "num_intervals", "percent_overlap", "index_set", "level_sets", "metric", "type"))

## Cover initialization
cover_ref$methods(initialize = function(filter_values, k, g, measure = "euclidean", type = "rectangular"){
  if (is.null(dim(filter_values))) { filter_values <<- array(filter_values, dim = c(length(filter_values), 1)) }
  else { filter_values <<- filter_values }
  metric <<- measure
  type <<- type
  
  ## TODO: move k and g to other instantiations 
  num_intervals <<- k; percent_overlap <<- g
  constructCover()
})

cover_ref$methods(summary = function(){
  type_pretty <- paste0(toupper(substr(type, start = 1, stop = 1)), tolower(substr(type, start = 2, stop = nchar(type))))
  sprintf("%s cover: number intervals = %d, overlap = %.2f", type_pretty, num_intervals, percent_overlap)
})

cover_ref$methods(show = function(){
  message <- c(sprintf("Open cover for %d objects (d = %d)", nrow(filter_values), ncol(filter_values)))
  message <- append(message, sprintf("Parameters: number intervals = %d, overlap = %.2f", num_intervals, percent_overlap))
  writeLines(message)
})

## Set overlap/gain threshold
cover_ref$methods(setOverlap = function(g){
  dim_Z <- ncol(filter_values)
  if (length(g) != dim_Z && length(g) != 1 || any(g > 0.50)){ stop("The gain 'g' must be a single scalar or a vector of scalars with length equal to the dimensionality of the filter space.") }
  if (length(g) == 1 && dim_Z > 1){ g <- rep(g, dim_Z) } ## create a vector 
  percent_overlap <<- g
})

## Set resolution 
cover_ref$methods(setResolution = function(k){
  dim_Z <- ncol(filter_values)
  if (length(k) != dim_Z && length(k) != 1){ stop("The resolution 'k' must be a single scalar or a vector of scalars with length equal to the dimensionality of the filter space.") }
  if (length(k) == 1 && dim_Z > 1){ k <- rep(k, dim_Z) } ## create a vector 
  num_intervals <<- k
})

## Constructs a base cover where the Lebesgue covering dimension = 0 (i.e. a valid cover, but where each set is disjoint)
## TODO: Add support for other types of covering methods 
cover_ref$methods(constructBaseCover = function(cover_type = c("rectangular")){
  if ("uninitializedField" %in% class(percent_overlap)){ stop("'percent_overlap' must be set prior to constructing a valid cover.") }
  if ("uninitializedField" %in% class(num_intervals)){ stop("'num_intervals' must be set prior to constructing a valid cover.") }
  
  ## Setup 
  filter_dim <- ncol(filter_values) ## filter dimensionality
  indices <- lapply(k, function(k_l) 1:k_l) ## per-dimension possible indexes
  index_set <<- structure(eval(as.call(append(quote(expand.grid), indices))), names = paste0("d", 1:filter_dim)) ## full indexing set 
  
  ## Find which level set each point lies within under the case the given filter space is
  ## zero-dimensional w.r.t. its Lebesgue covering dimension
  points_in_level_set <- sapply(1:filter_dim, function(i) { 
    x <- filter_values[, i]
    findInterval(x = x, seq(min(x), max(x), length.out = num_intervals[[i]]+1), all.inside = TRUE)
  })
  
  ## Create the level sets
  level_sets <<- structure(points_in_level_set, names = index_set)
  
  ## Store all the information as a list in the cover field 
  # cover <- list(type = cover_type, index_set = index_set, points_in_level_set = points_in_level_set)
  # print.mapper_cover <- function(){ print("something")}
})

cover_ref$methods(constructCover = function(){
  if ("uninitializedField" %in% class(percent_overlap)){ stop("'percent_overlap' must be set prior to constructing a valid cover.") }
  if ("uninitializedField" %in% class(num_intervals)){ stop("'num_intervals' must be set prior to constructing a valid cover.") }
  
  ## Setup unexpanded and full indexing set (the full indexing set is the cartesian product of each unexpanded index set)  
  filter_dim <- ncol(filter_values) ## filter dimensionality
  indices <- lapply(num_intervals, function(k_l) 1:k_l) ## per-dimension possible indexes
  index_set <<- structure(eval(as.call(append(quote(expand.grid), indices))), names = paste0("d", 1:filter_dim)) ## full indexing set 
  
  ## Get filter min and max ranges 
  filter_rng <- apply(filter_values, 2, range)
  { filter_min <- filter_rng[1,]; filter_max <- filter_rng[2,] }
  
  ## Using the current gain, setup the cover. This computes the min and max bounds of each level set. 
  interval_length <- (filter_max - filter_min)/(num_intervals - percent_overlap * (num_intervals - 1)) ## interval length 
  step_size <- interval_length * (1 - percent_overlap) ## step size 
  index_set_str <- apply(index_set, 1, function(alpha) paste0(as.character(alpha), collapse = ",")) ## level set index tuple names
  
  ## Construct the level sets
  # level_sets <<- structure(vector(mode = "list", length = nrow(index_set)), names = index_set_str) ## level sets 
  # for (i in 1:nrow(index_set)){
  #   ls_idx <- index_set[i,]
  #   ls_bnds <- rbind(filter_min + (ls_idx-1) * step_size, (filter_min + (ls_idx-1) * step_size) + interval_length)
  #   rownames(ls_bnds) <- c("min", "max") ## For clarity 
  #   level_sets[[i]]$bounds <<- ls_bnds
  #   
  #   ## For the given level set, query which filter values lie within it, per dimension
  #   ls_queries <- sapply(1:filter_dim, function(d_i) filter_values[, d_i] >= ls_bnds[1, d_i] & filter_values[, d_i] <= ls_bnds[2, d_i])
  #   level_sets[[i]]$points_in_level_set <<- which(apply(ls_queries, 1, all))
  # }
  level_sets <<- constructLevelSets(filter_values, as.matrix(index_set), as.numeric(interval_length),  as.numeric(step_size), as.numeric(filter_min))
  
  # .GlobalEnv[["test"]] <- list(filter_values = filter_values, index_set = index_set, interval_length = interval_length, step_size = step_size, filter_min = filter_min)
  # test <- constructLevelSets(filter_values = filter_values, index_set = index_set, interval_length = interval_length, step_size = step_size, filter_min = filter_min)
})

## Which level set (flat indices) are valid to compare against? When the overlap is <= 50%, it's required that the
## level sets are checked only against immediate neighbors of that set (when the index is at maximum one
## index away). If the overlap is > 50%, then check every combination. 
cover_ref$methods(valid_pairs = function(){
  all_ls_pairs <- t(combn(1:length(level_sets), 2))
  idx_set_pairs <- as.matrix(index_set[all_ls_pairs[, 1],] - index_set[all_ls_pairs[, 2],])
  if (all(percent_overlap <= 0.50)){
    valid_ls_pairs <- as.vector(abs(apply(idx_set_pairs, 1, max))) <= 1
    return(all_ls_pairs[valid_ls_pairs,])
  } else {
    ## TODO: Extend mapper to compute more than the 1-skeleton efficiently 
    return(all_ls_pairs)
  }
})

## Function to plot the filter space, including the level set bounds 
cover_ref$methods(plotFilterSpace = function(show_ls_bounds = TRUE, ...){
  filter_dim <- ncol(filter_values)
  if (filter_dim > 2){ stop("Cannot plot a filter space with more than 3 dimensions") }
  if (filter_dim == 1){
    filter_sz <- nrow(m$filter_values)
    alpha_scale <- ifelse(filter_sz > 1000, 0.01, 1 - (log(filter_sz)-1)/(log(1000)-1))
    plot(cbind(m$filter_values, rep(0L, length(m$filter_values))), ylim = c(-1, 1), pch = "|", cex = 5, 
         col = adjustcolor("black", alpha.f = alpha_scale),
         ylab = "", xlab = "Filter Values")
    for (ls in cover$LS){
      lines(x = c(ls[1, 1], ls[2, 1]), y = c(-0.5, -0.5), ...)
      lines(x = c(ls[2, 1], ls[2, 1]), y = c(-0.5, 0.5), ...)
      lines(x = c(ls[2, 1], ls[1, 1]), y = c(0.5, 0.5), ...)
      lines(x = c(ls[1, 1], ls[1, 1]), y = c(0.5, -0.5), ...)
    }
  } else if (filter_dim == 2){
    plot(m$filter_values)
    for (ls in cover$LS){
      lines(x = c(ls[1, 1], ls[2, 1]), y = c(ls[1, 2], ls[1, 2]), ...)
      lines(x = c(ls[2, 1], ls[2, 1]), y = c(ls[1, 2], ls[2, 2]), ...)
      lines(x = c(ls[2, 1], ls[1, 1]), y = c(ls[2, 2], ls[2, 2]), ...)
      lines(x = c(ls[1, 1], ls[1, 1]), y = c(ls[2, 2], ls[1, 2]), ...)
    }
  }
})

