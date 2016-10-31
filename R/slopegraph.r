#' @rdname slopegraph
#' @aliases slopegraph-package
#' @title Create Slopegraph from a data frame
#' @description Convert an R data frame (containing a panel dataset, where rows are observations and columns are time periods) into an Edward Tufte-inspired Slopegraph.
#' @param df An observation-by-period data.frame, with at least two columns. Missing values are allowed.
#' @param xlim The x-axis limits of the plot. Automatically calculated by default.
#' @param ylim The y-axis limits of the plot. Automatically calculated by default.
#' @param main The main title of the plot. Default is \code{NULL} (none). See \code{? par}.
#' @param bty The box type for the plot. Default is \code{'n'} (none). See \code{? par}.
#' @param xaxt The x-axis type for the plot. Default is \code{'n'} (none). See \code{? par}.
#' @param yaxt The y-axis type for the plot. Default is \code{'n'} (none). See \code{? par}.
#' @param xlab The x-axis label for the plot. Default is \code{''} (none). See \code{? par}.
#' @param ylab The y-axis label for the plot. Default is \code{''} (none). See \code{? par}.
#' @param panel.first An expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph). Default is \code{NULL}.
#' @param panel.last An expression to add something after adding the plot content. Default is \code{NULL}.
#' @param labels The labels to use for the slopegraph periods. Default is \code{names(df)}.
#' @param labpos.left The \code{pos} (positioning) parameter for the leftside observation labels. Default is \code{2}. See \code{? par}.
#' @param labpos.right The \code{pos} (positioning) parameter for the rightside observation labels. Default is \code{2}. See \code{? par}.
#' @param decimals The number of decimals to display for values in the plot. Default is \code{NULL} (none).
#' @param binval Threshold at which to force binning of labels and values (multiplier of the height of an "m"). Default is \code{1.5}.
#' @param col.lines A vector of colors for the slopegraph lines. Default is \code{par('fg')}.
#' @param col.lab A vector of colors for the observation labels. Default is \code{par('fg')}.
#' @param col.num A vector of colors for the number values. Default is \code{par('fg')}.
#' @param col.xaxt A character string containing the x-axis color. Default is \code{par('fg')}.
#' @param offset.x A small offset for \code{segments}, to be used when positioning the numeric values. Default is \code{.1}.
#' @param offset.lab A small offset for the observation labels. Default is \code{.1}.
#' @param cex.lab Default is \code{1}. See \code{? par}.
#' @param cex.num Default is \code{1}. See \code{? par}.
#' @param family The font family to use in the plots. Set to \dQuote{serif} by default.
#' @param font.lab Default is \code{1}. See \code{? text}.
#' @param font.num Default is \code{1}. See \code{? text}.
#' @param lty A vector of line type values for the slopegraph lines. Default is \code{par("lty")}. See \code{? par}.
#' @param lwd A vector of line width values for the slopegraph lines. Default is \code{par("lwd")}. See \code{? par}.
#' @param mai A margin specification. Default is \code{NULL}. See \code{? par}.
#' @param \ldots Additional arguments to \code{plot}.
#' @return A matrix of , invisibly.
#' @examples
#' ## Tufte's Cancer Graph (to the correct scale)
#' data(cancer)
#' slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5),
#'            labels=c('5 Year','10 Year','15 Year','20 Year'), binval=2.5)
#' 
#' ## Tufte's GDP Graph
#' data(gdp)
#' slopegraph(gdp, col.line='gray', labels=c('1970','1979'), 
#'     main='Current Receipts of Goverment as a Percentage of Gross Domestic Product',
#'     binval=3.75)
#' 
#' ## Ranking of U.S. State populations
#' data(states)
#' slopegraph(states, col.line='black', ylim = c(38,0),
#'            main='Relative Rank of U.S. State Populations, 1790-1870')
#' @references
#' \url{http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk}
#' 
#' Tufte, Edward. \emph{The Visual Display of Quantitative Information}. Graphics Press.
#' 
#' Tufte, Edward. \emph{Beautiful Evidence}. Graphics Press.
#' 
#' @seealso \code{\link{cancer}}, \code{\link{gdp}}, \code{\link{states}}
#' @author Thomas J. Leeper
#' @import graphics
#' @importFrom utils head
#' @importFrom stats embed na.omit
#' @export
slopegraph <- function(
    df,
    xlim = c(.5,ncol(df)+.5),
    ylim = c(min(df,na.rm=TRUE)-diff(range(df,na.rm=TRUE))/100,max(df,na.rm=TRUE)+diff(range(df,na.rm=TRUE))/100),
    main = NULL,
    bty = 'n',
    xaxt = 'n',
    yaxt = 'n',
    xlab = '',
    ylab = '',
    panel.first = NULL,
    panel.last = NULL,
    labels = names(df),
    labpos.left = 2,
    labpos.right = 4,
    decimals = NULL,
    binval = 1.5,
    col.lines = par('fg'),
    col.lab = par('fg'),
    col.num = par('fg'),
    col.xaxt = par('fg'),
    offset.x = .1,
    offset.lab = .1,
    cex.lab = 1,
    cex.num = 1,
    family = "serif",
    font.lab = 1,
    font.num = 1,
    lty = par("lty"),
    lwd = par("lwd"),
    mai = NULL,
    ...)
{
    # check data
    if (ncol(df) < 2) {
        stop('`df` must have at least two columns')
    }
    # create structure that is row, x1, x2, y1, y2
    # for every pair of contiguous points
    segmentize <- function(dat) {
        # segment-by-values matrix
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
    to_draw <- segmentize(as.matrix(df))
    
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
    
    
    # PLOTTING
    
    # draw margins
    if (is.null(mai)) {
        par(mai=c(1, 0, if(is.null(main)) 0 else 1, 0))
    } else {
        par(mai=mai)
    }
    plot(NA, xlim=xlim, ylim=ylim, main=main, family=family,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if (!is.null(panel.first)) {
        eval(panel.first)
    }
    # calculate decimals from data
    if (is.null(decimals)) {
        decimals <- 
        max(sapply(as.vector(sapply(df, as.character)), function(i) {
            a <- strsplit(i, '.', fixed = TRUE)[[1]][2]
            if (!is.na(a)) { nchar(a) } else { 0 }
        }), na.rm = TRUE)
    }
    
    # x-axis
    axis(1, 1:ncol(df), labels=labels, col=col.xaxt, col.ticks=col.xaxt, family=family)
    
    # height and width of 'm' on plotting device
    h <- strheight('m')
    w <- strwidth('m')
    
    # left-side labels
    leftlabs <- df[!is.na(df[,1]),1, drop = FALSE]
    text(1-offset.lab, leftlabs[,1],
         col=col.lab, rownames(leftlabs), pos=labpos.left, 
         cex=cex.lab, font=font.lab, family=family)
    
    # right-side labels
    rightlabs <- df[!is.na(df[,ncol(df)]),ncol(df), drop = FALSE]
    text(ncol(df)+offset.lab, rightlabs[,1], 
         col=col.lab, rownames(rightlabs), pos=labpos.right, 
         cex=cex.lab, font=font.lab, family=family)
    
    # draw numeric value labels
    # valslist <- lapply(seq_along(df), function(i) overlaps(df[order(df[!is.na(df[,i]),i]),i,drop=FALSE], cat='values'))
    apply(to_draw, 1, function(rowdata){
            i <- rowdata[1]
            x1 <- rowdata[2]
            x2 <- rowdata[3]
            y1 <- rowdata[4]
            y2 <- rowdata[5]
            ysloped <- (y2-y1)*offset.x
            text(x1, y1, y1, col = col.num, cex = cex.num, font = font.num, family = family)
            text(x2, y2, y2, col = col.num, cex = cex.num, font = font.num, family = family)
    })
    
    # draw lines
    if (length(col.lines) == 1L) {
        col.lines <- rep(col.lines, length.out = nrow(df))
    }
    lty <- rep(lty, length.out = nrow(df))
    lwd <- rep(lwd, length.out = nrow(df))
    apply(to_draw, 1, function(rowdata){
            i <- rowdata[1]
            x1 <- rowdata[2]
            x2 <- rowdata[3]
            y1 <- rowdata[4]
            y2 <- rowdata[5]
            ysloped <- (y2-y1)*offset.x
            segments(x1+offset.x, if(y1==y2) y1 else (y1+ysloped),
                     x2-offset.x, if(y1==y2) y2 else (y2-ysloped),
                     col = col.lines[i],
                     lty = lty[i],
                     lwd = lwd[i])
    })
    
    # optional expression
    if (!is.null(panel.last)) {
        eval(panel.last)
    }
    # return `to_draw` invisibly
    invisible(structure(to_draw, dimnames = list(seq_len(nrow(to_draw)), c("row", "x1", "x2", "y1", "y2"))))
}
