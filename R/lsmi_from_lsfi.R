# We want to flatten out our indexing so that we have just one for loop.
# For instance, if num_intervals = c(3,2), then we want a function that
# will associate a flat index with a multi-index (i,j) as follows: 
# 1 --> (1,1),  4 --> (1,2)
# 2 --> (2,1),  5 --> (2,2)
# 3 --> (3,1),  6 --> (3,2)
# and generalize well to arbitrary length multi-indices.  Basically, we
# want to generalize this using a function instead of a list:
#    multi_index <- list( x=rep(1:3, times=2),
#                         y=rep(1:2, each =3) )
#    print(multi_index)


# function from the level set flat index (lsfi) to the level set multi-index (lsmi)
lsmi_from_lsfi <- function( lsfi, num_intervals ) {
    # inputs:
    # lsfi = an integer in the range 1:prod(v)
    # num_intervals = c(i1,i1,...) a vector of numbers of intervals
    # output:
    # f+1 = a vector of multiindices with length filter_output_dim
    j <- c(1,num_intervals) # put 1 in front to make indexing easier in the product prod(j[1:k])
    f <- c()
    for (k in 1:length(num_intervals)) {
        # use lsfi-1 to shift from 1-based indexing to 0-based indexing
        f[k] <- floor( (lsfi-1) / prod(j[1:k])) %% num_intervals[k]
    }
    #print(f+1)
    # lsmi = f+1 = level set multi index
    return(f+1) # shift from 0-based indexing back to 1-based indexing
}
