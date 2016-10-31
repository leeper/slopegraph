# function to create segment-by-values matrix
segmentize <- function(dat) {
    # create structure that is row, x1, x2, y1, y2
    # for every pair of contiguous points
    
    if (ncol(dat) == 2) {
        pairsmat <- matrix(1:2, nrow = 1L)
    } else {
        pairsmat <- embed(seq_len(ncol(dat)), 2)[,2:1]
    }
    # output
    out <- matrix(NA_real_, nrow = nrow(dat) * nrow(pairsmat), ncol = 5L)
    k <- 1L
    for (i in seq_len(nrow(dat))) {
        for (j in seq_len(nrow(pairsmat))) {
            out[k,] <- c(i, 
                         pairsmat[j,1], 
                         pairsmat[j,2], 
                         dat[i, pairsmat[j,1]], 
                         dat[i, pairsmat[j,2]])
            k <- k + 1L
        }
    }
    # return, dropping missing values
    na.omit(out)
}

# function for finding consecutive indices
# from: http://stackoverflow.com/a/16118320/2338862
seqle <- function(x, incr=1) { 
    if(!is.numeric(x)) x <- as.numeric(x) 
    n <- length(x)  
    y <- x[-1L] != x[-n] + incr 
    i <- c(which(y|is.na(y)),n) 
    list(lengths = diff(c(0L,i)),
         values = x[head(c(0L,i)+1L,-1L)]) 
} 
    
# function for eliminating overlaps
overlaps <- function(coldf, cat='rownames'){
    # conditionally remove exactly duplicated values
    if (any(duplicated(coldf[,1]))) {
        u <- unique(coldf[,1])
        out <- cbind.data.frame(t(sapply(u, function(i) {
            c(paste(rownames(coldf)[coldf[,1]==i],collapse='\n'),i)
        })))
        rownames(out) <- out[,1]
        out[,1] <- NULL
        names(out) <- names(coldf)
        out[,1] <- as.numeric(as.character(out[,1]))
        coldf <- out[order(out[,1]),,drop=FALSE]
    }
    # function to fix overlaps
    overlaps <- which(abs(diff(coldf[,1]))<(binval*h))
    if (length(overlaps)) {
        runs <- seqle(overlaps) # use seqle function
        overlaps2 <- mapply(function(i,j) seq(i,length.out=j+1), runs$values, runs$lengths)
        oldlabs <- coldf[-unique(c(overlaps,overlaps+1)),,drop=FALSE]
        newlabs <- data.frame(sapply(overlaps2, function(i) mean(coldf[i,1])))
        names(newlabs) <- names(coldf)
        if (cat == 'rownames') {
            rownames(newlabs) <- 
                sapply(overlaps2, function(i) {
                    paste(rownames(coldf)[rev(i)],collapse='\n')
                })
        } else if(cat == 'values') {
            rownames(oldlabs) <- sprintf(paste0('%.',decimals,'f'),oldlabs[,1])
            rownames(newlabs) <-
                sapply(overlaps2, function(i) {
                    paste(sprintf(paste0('%.',decimals,'f'),coldf[rev(i),1]),collapse='\n')
                })
        }
        return(rbind(oldlabs,newlabs))
    } else {
        if (cat == 'values') {
            rownames(coldf) <- coldf[,1]
        }
        return(coldf)
    }
}
    