# R Function to Draw Edward Tufte-style Slopeplots

# TODO:
## deal with overlapping numeric values: if more than one label falls at a position, offset based on rank() of the subsequent column values for those rows
## overlapping rownames (same algorithm as above, but hard code for 1st and 2nd value)


slopegraph <-
function(
df,
xlim = c(.5,ncol(df)+.5),
ylim = c(min(df)-diff(range(df))/100,max(df)+diff(range(df))/100),
main = NULL,
bty = 'n',
yaxt = 'n',
xaxt = 'n',
xlab = '',
ylab = '',
add.before = NULL, # an expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph)
add.after = NULL, # an expression to add something after adding the plot content
labels = names(df),
labpos.left = 2,
labpos.right = 4,
col.lines = par('fg'),
col.lab = par('fg'),
col.num = par('fg'),
col.xaxt = par('fg'),
offset.x = .1, # THIS DOESN'T SEEM TO WORK???
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
    overlaps <- function(coldf){
        # function to fix overlaps
        overlaps <- which(abs(diff(coldf[,1]))<(1.5*h))
        runs <- seqle(overlaps) # use seqle function
        overlaps2 <- mapply(function(i,j) seq(i,length.out=j+1), runs$values, runs$lengths)
        
        newlabs <- data.frame(sapply(overlaps2, function(i) mean(coldf[i,1])))
        names(newlabs) <- names(coldf)
        rownames(newlabs) <- 
            sapply(overlaps2, function(i) paste(rownames(coldf)[i],collapse='\n'))
        return(rbind(coldf[-unique(c(overlaps,overlaps+1)),,drop=FALSE],newlabs))
    }
    
    # left-side labels
    l <- overlaps(df[,1,drop=FALSE])
    text(1-offset.lab, l[,1],
         col=col.lab, rownames(l), pos=labpos.left, cex=cex.lab, font=font.lab)
    
    # right-side labels
    r <- overlaps(df[,ncol(df),drop=FALSE])
    text(ncol(df)+offset.lab, r[,1], 
         col=col.lab, rownames(r), pos=labpos.right, cex=cex.lab, font=font.lab)
    
    # numeric value labels
    # deal with duplicate value labels (i.e., not double printing anything)
    df2 <- do.call(cbind,lapply(df, function(y) {y[duplicated(y)] <- ''; y}))
    # print them
    apply(cbind(df,df2),1, function(y){
        text(1:ncol(df), as.numeric(y[1:ncol(df)]), y[(ncol(df)+1):(2*ncol(df))],
             col=col.num, cex=cex.num, font=font.num)
    })
    
    # draw lines
    offset.x <- .1 # small offset for `segments`
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


# EXAMPLES

## Tufte's Cancer Graph (to the correct scale)
cancer <- read.csv('tufte-cancer-survival-data.csv')
rownames(cancer) <- cancer[,1]
cancer <- cancer [,-1]
pdf('tufte-cancer-survival-plot.pdf',height=16, width=12, family='Palatino')
slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5), labels=c('5 Year','10 Year','15 Year','20 Year'))
dev.off()


## Tufte's GNP Graph
gnp <- read.csv('tufte-gnp-data.csv')
gnp[,3] <- NULL
pdf('tufte-gnp-plot.pdf',height=12, width=8, family='Palatino')
slopegraph(gnp, col.line='gray', labels=c('1970','1979'), 
    main='Current Receipts of Goverment as a Percentage of Gross Domestic Product')
dev.off()
