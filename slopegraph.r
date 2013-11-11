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
add.exp = NULL, # an expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph)
labels = names(df),
labpos.left = 2,
labpos.right = 4,
col.lines = par('fg'),
col.lab = par('fg'),
col.num = par('fg'),
col.xaxt = par('fg'),
offset.x = .1, # THIS DOESN'T SEEM TO WORK
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
    if(!is.null(add.exp))
        eval(add.exp)
    
    # x-axis
    axis(1, 1:ncol(df), labels=labels, col=col.xaxt, col.ticks=col.xaxt)
    
    # left-side labels
    l <- df[,1] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    leftlabs <- lapply(split(rownames(df),l), paste, collapse=', ')
    text(1-offset.lab, as.numeric(names(leftlabs)),
         col=col.lab, leftlabs, pos=labpos.left, cex=cex.lab, font=font.lab)
    
    # right-side labels
    r <- df[,ncol(df)] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    rightlabs <- lapply(split(rownames(df),r), paste, collapse=',')
    text(ncol(df)+offset.lab, as.numeric(names(rightlabs)), 
         col=col.lab, rightlabs, pos=labpos.right, cex=cex.lab, font=font.lab)
    
    # numeric value labels
    # deal with duplicate value labels (i.e., not double printing anything)
    df2 <- do.call(cbind,lapply(df, function(y) {y[duplicated(y)] <- ''; y}))
    # print them
    apply(cbind(df,df2),1, function(y)
        text(1:ncol(df), as.numeric(y[1:ncol(df)]), y[(ncol(df)+1):(2*ncol(df))],
             col=col.num, cex=cex.num, font=font.num))
    
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
    
    # return invisibly
    invisible(NULL)
}


# EXAMPLE

## Tufte's original graph (to the correct scale)
cancer <- read.csv('tufte-cancer-survival-data.csv')
rownames(cancer) <- cancer[,1]
cancer <- cancer [,-1]
pdf('tufte-cancer-survival-plot.pdf',height=16, width=12)
slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5), labels=c('5 Year','10 Year','15 Year','20 Year'))
dev.off()

