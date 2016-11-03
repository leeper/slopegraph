# function for eliminating overlaps
bump_overlaps <- function(yvec, h = strheight('M'), w = strwidth('M')){
    
    #return(yvec)
    
    upper <- yvec + (0.5 * h)
    lower <- yvec - (0.5 * h)
    
    # overlap 1st label below
    overlap1 <- c(lower[1:(length(yvec)-1)] - upper[2:(length(yvec))], 0)
    # overlap 2nd label below
    overlap2 <- c(rep(0,2), lower[1:(length(yvec)-2)] - upper[3:(length(yvec))])
    # overlap 3rd label below
    overlap3 <- c(rep(0,3), lower[1:(length(yvec)-3)] - upper[4:(length(yvec))])
    
    ifelse(overlap1 < 0, 
           ifelse(is.na(yvec), 0, yvec) - ifelse(is.na(overlap1), 0, overlap1/2L), 
           yvec)
    
}
    