# function for eliminating overlaps
bump_overlaps <- function(yvec, decimals = 0L, h = strheight('M')){
    
    return(yvec)
    
    y_original <- yvec
    yvec <- round(yvec, decimals)
    ord <- order(yvec, decreasing = TRUE)
    yvec <- yvec[ord]
    
    upper <- yvec + (0.5 * h) # upper limit of text
    lower <- yvec - (0.5 * h) # lower limit of text
    
    is_overlapped <- function(low, upp, height = h) {
        ifelse(upp != (low + height), low - upp, 0)
    }
    ifna0 <- function(x) {
        ifelse(is.na(x), 0, x)
    }
    bump_up_and_down <- function(v, w, by = 1 * h, off = 1) {
        out <- v
        w <- w < 0
        out[w] <- out[w] + by
        nextrow <- c(rep(FALSE, off), head(w, length(w) - off))
        out[nextrow] <- out[nextrow] - by
        return(out)
    }
    
    # overlap 3rd label below
    overlap3 <- c(is_overlapped(lower[1:(length(yvec)-3)], upper[4:(length(yvec))]), rep(0,3))
    yvec <- bump_up_and_down(yvec, overlap3, by = 0.5 * h, off = 3)
    
    # overlap 2nd label below
    upper <- yvec + (0.5 * h) # upper limit of text
    lower <- yvec - (0.5 * h) # lower limit of text
    overlap2 <- c(is_overlapped(lower[1:(length(yvec)-2)], upper[3:(length(yvec))]), rep(0,2))
    yvec <- bump_up_and_down(yvec, overlap2, by = 0.5 * h, off = 2)
    
    # overlap 1st label below
    upper <- yvec + (0.5 * h) # upper limit of text
    lower <- yvec - (0.5 * h) # lower limit of text
    overlap1 <- c(is_overlapped(lower[1:(length(yvec)-1)], upper[2:(length(yvec))]), rep(0,1))
    yvec <- bump_up_and_down(yvec, overlap1, by = 0.5 * h, off = 1)
    
    out <- yvec[order(ord)]
    #print(rbind(y_original, out))
    return(out)
}
    