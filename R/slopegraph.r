slopegraph <-
function(
df,
xlim = c(.5,ncol(df)+.5),
ylim = c(min(df,na.rm=TRUE)-diff(range(df,na.rm=TRUE))/100,max(df,na.rm=TRUE)+diff(range(df,na.rm=TRUE))/100),
main = NULL,
bty = 'n',
xaxt = 'n',
yaxt = 'n',
xlab = '',
ylab = '',
add.before = NULL, # an expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph)
add.after = NULL, # an expression to add something after adding the plot content
labels = names(df),
labpos.left = 2,
labpos.right = 4,
decimals = NULL,
binval = 1.5, # threshold at which to force binning of labels and values (multiplier of the height of an "m")
col.lines = par('fg'),
col.lab = par('fg'),
col.num = par('fg'),
col.xaxt = par('fg'),
offset.x = .1, # small offset for `segments`
offset.lab = .1,
cex.lab = 1,
cex.num = 1,
font.lab = 1,
font.num = 1,
lty = par("lty"),
lwd = par("lwd"),
mai = NULL,
...)
{
    if(ncol(df) < 2)
        stop('`df` must have at least two columns')
    # draw margins
    if(is.null(mai))
        par(mai=c(1, 0, if(is.null(main)) 0 else 1, 0))
    else
        par(mai=mai)
    
    plot(NA, y=NULL, xlim=xlim, ylim=ylim, main=main,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if(!is.null(add.before))
        eval(add.before)
    
    # calculate decimals from data
    if(is.null(decimals)){
        decimals <- 
        max(sapply(as.vector(sapply(df, as.character)),function(i) {
            a <- strsplit(i, '.', fixed=TRUE)[[1]][2]
            if(!is.na(a))
                nchar(a)
            else
                0
        }), na.rm=TRUE)
    }
    
    # x-axis
    axis(1, 1:ncol(df), labels=labels, col=col.xaxt, col.ticks=col.xaxt)
    
    # height and width of 'm' on plotting device
    h <- strheight('m')
    w <- strwidth('m')
    
    # function for finding consecutive indices
    # from: http://stackoverflow.com/a/16118320/2338862
    seqle <- function(x,incr=1) { 
        if(!is.numeric(x)) x <- as.numeric(x) 
        n <- length(x)  
        y <- x[-1L] != x[-n] + incr 
        i <- c(which(y|is.na(y)),n) 
        list(lengths = diff(c(0L,i)),
             values = x[head(c(0L,i)+1L,-1L)]) 
    } 
    
    overlaps <- function(coldf, cat='rownames'){
        # conditionally remove exactly duplicated values
        if(any(duplicated(coldf[,1]))){
            u <- unique(coldf[,1])
            out <- cbind.data.frame(t(sapply(u, function(i)
                c(paste(rownames(coldf)[coldf[,1]==i],collapse='\n'),i))))
            rownames(out) <- out[,1]
            out[,1] <- NULL
            names(out) <- names(coldf)
            out[,1] <- as.numeric(as.character(out[,1]))
            coldf <- out[order(out[,1]),,drop=FALSE]
        }
        # function to fix overlaps
        overlaps <- which(abs(diff(coldf[,1]))<(binval*h))
        if(length(overlaps)){
            runs <- seqle(overlaps) # use seqle function
            overlaps2 <- mapply(function(i,j) seq(i,length.out=j+1), runs$values, runs$lengths)
            oldlabs <- coldf[-unique(c(overlaps,overlaps+1)),,drop=FALSE]
            newlabs <- data.frame(sapply(overlaps2, function(i) mean(coldf[i,1])))
            names(newlabs) <- names(coldf)
            if(cat=='rownames'){
                rownames(newlabs) <- 
                    sapply(overlaps2, function(i) paste(rownames(coldf)[rev(i)],collapse='\n'))
            } else if(cat=='values'){
                rownames(oldlabs) <- sprintf(paste('%.',decimals,'f',sep=''),oldlabs[,1])
                rownames(newlabs) <-
                    sapply(overlaps2, function(i)
                        paste(sprintf(paste('%.',decimals,'f',sep=''),coldf[rev(i),1]),collapse='\n'))
            }
            return(rbind(oldlabs,newlabs))
        } else
            return(coldf)
    }
    
    # left-side labels
    l <- overlaps(df[order(df[,1]),1,drop=FALSE])
    text(1-offset.lab, l[,1],
         col=col.lab, rownames(l), pos=labpos.left, cex=cex.lab, font=font.lab)
    
    # right-side labels
    r <- overlaps(df[order(df[,ncol(df)]),ncol(df),drop=FALSE])
    text(ncol(df)+offset.lab, r[,1], 
         col=col.lab, rownames(r), pos=labpos.right, cex=cex.lab, font=font.lab)
    
    # numeric value labels
    valslist <- lapply(seq_along(df), function(i) overlaps(df[order(df[,i]),i,drop=FALSE], cat='values'))
    for(i in 1:length(valslist)){
        text(rep(i,nrow(valslist[[i]])), valslist[[i]][,1], rownames(valslist[[i]]),
            col=col.num, cex=cex.num, font=font.num)
    }
    
    # draw lines
    col.lines <- rep(col.lines, length.out=nrow(df))
    lty <- rep(lty, length.out=nrow(df))
    lwd <- rep(lwd, length.out=nrow(df))
    for(i in 1:nrow(df)){
        mapply(function(x1,y1,x2,y2,...){
            ysloped <- (y2-y1)*offset.x
            segments(x1+offset.x, if(y1==y2) y1 else (y1+ysloped),
                     x2-offset.x, if(y1==y2) y2 else (y2-ysloped),
                     col=col.lines[i],
                     lty=lty[i],
                     lwd=lwd[i]
                    )},
               1:(length(df[i,])-1), # x1-positions
               df[i,][-length(df[i,])], # y1-positions
               2:(length(df[i,])), # x2-positions
               df[i,][-1] # y2-positions
               )
    }
    
    # optional expression
    if(!is.null(add.after))
        eval(add.after)
    
    # return invisibly
    invisible(NULL)
}
