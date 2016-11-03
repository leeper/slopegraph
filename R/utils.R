# function for finding consecutive indices
# from: http://stackoverflow.com/a/16118320/2338862
seqle <- function(x, incr = 1) { 
    if(!is.numeric(x)) x <- as.numeric(x) 
    n <- length(x)  
    y <- x[-1L] != x[-n] + incr 
    i <- c(which(y|is.na(y)),n) 
    list(lengths = diff(c(0L,i)),
         values = x[head(c(0L,i)+1L,-1L)]) 
} 

# function for eliminating overlaps
overlaps <- function(coldf, binval, h = strheight('m'), w = strwidth('m'), cat='rownames'){
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
        return(rbind(oldlabs,newlabs))
    } else {
        if (cat == 'values') {
            rownames(coldf) <- coldf[,1]
        }
        return(coldf)
    }
}
    