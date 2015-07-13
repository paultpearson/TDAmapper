# the inverse function
lsfi_from_lsmi <- function( lsmi, num_intervals ) {
    lsfi <- lsmi[1]
    if (length(num_intervals) > 1) {
        for (i in 2:length(num_intervals)) {
            lsfi <- lsfi + prod(num_intervals[1:(i-1)]) * (lsmi[i]-1)
        }
    }
    return(lsfi)
}
