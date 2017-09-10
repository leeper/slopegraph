#' @rdname slopegraph
#' @aliases slopegraph-package
#' @title Create Slopegraph from a data frame
#' @description Convert an R data frame (containing a panel dataset, where rows are observations and columns are time periods) into an Edward Tufte-inspired Slopegraph.
#' @param data An observation-by-period data.frame, with at least two columns. Missing values are allowed.
#' @param main The main title of the plot. Default is \code{NULL} (none). See \code{\link[graphics]{par}}.
#' @param xlab The x-axis label for the plot. Default is \code{''} (none). See \code{\link[graphics]{par}}.
#' @param ylab The y-axis label for the plot. Default is \code{''} (none). See \code{\link[graphics]{par}}.
#' @param xlabels The labels to use for the slopegraph periods. Default is \code{names(data)}.
#' @param xlim The x-axis limits of the plot. Automatically calculated by default.
#' @param ylim The y-axis limits of the plot. Automatically calculated by default.
#' @param bty The box type for the plot. Default is \code{'n'} (none). See \code{\link[graphics]{par}}.
#' @param xaxt The x-axis type for the plot. Default is \code{'n'} (none). See \code{\link[graphics]{par}}.
#' @param yaxt The y-axis type for the plot. Default is \code{'n'} (none). See \code{\link[graphics]{par}}.
#' @param panel.first An expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph). Default is \code{NULL}.
#' @param panel.last An expression to add something after adding the plot content. Default is \code{NULL}.
#' @param labpos.left The \code{pos} (positioning) parameter for the leftside observation labels. Default is \code{2}. See \code{\link[graphics]{par}}. If \code{NULL}, labels are not drawn.
#' @param labpos.right The \code{pos} (positioning) parameter for the rightside observation labels. Default is \code{2}. See \code{\link[graphics]{par}}. If \code{NULL}, labels are not drawn.
#' @param leftlabels The parameter for the rightside observation labels. Default is using row indexes.
#' @param rightlabels The parameter for the rightside observation labels. Default is using row indexes.
#' @param decimals The number of decimals to display for values in the plot. Default is \code{0} (none).
#' @param col.lines A vector of colors for the slopegraph lines. Default is \code{par('fg')}.
#' @param col.lab A vector of colors for the observation labels. Default is \code{par('fg')}.
#' @param col.num A vector of colors for the number values. Default is \code{par('fg')}. If \code{NULL}, labels are not drawn.
#' @param col.xaxt A character string containing the x-axis color. Default is \code{par('fg')}.
#' @param offset.x A small offset for \code{segments}, to be used when positioning the numeric values. Default is \code{NULL} (set automatically based on the data.
#' @param offset.lab A small offset for the observation labels. Default is \code{.1}.
#' @param cex.lab A numeric value indicating the size of row labels. Default is \code{1}. See \code{\link[graphics]{par}}.
#' @param cex.num A numeric value indicating the size of numeric labels. Default is \code{1}. See \code{\link[graphics]{par}}.
#' @param family The font family to use in the plots. Set to \dQuote{serif} by default.
#' @param font.lab Default is \code{1}. See \code{? text}.
#' @param font.num Default is \code{1}. See \code{? text}.
#' @param lty A vector of line type values for the slopegraph lines. Default is \code{par("lty")}. See \code{\link[graphics]{par}}.
#' @param lwd A vector of line width values for the slopegraph lines. Default is \code{par("lwd")}. See \code{\link[graphics]{par}}.
#' @param mai A margin specification. Default is \code{NULL}. See \code{\link[graphics]{par}}.
#' @param na.span A logical indicating whether line segments should span periods with missing values. The default is \code{FALSE}, such that some segments are not drawn.
#' @param \ldots Additional arguments to \code{plot}.
#' @return A five-variable data frame, where each row contains: the row number from \code{data}, \samp{x1}, \samp{x2}, \samp{y1}, and \samp{y2} coordinates for each drawn segment, invisibly.
#' @examples
#' ## Tufte's Cancer Graph (to the correct scale)
#' data(cancer)
#' slopegraph(cancer, col.lines = 'gray', xlim = c(-.5,5.5),
#'            xlabels = c('5 Year','10 Year','15 Year','20 Year'))
#' 
#' ## Tufte's GDP Graph
#' data(gdp)
#' slopegraph(gdp, col.line='gray', xlabels = c('1970','1979'), 
#'     main = 'Current Receipts of Goverment as a Percentage of Gross Domestic Product')
#' 
#' ## Ranking of U.S. State populations
#' data(states)
#' slopegraph(states, col.line='black', ylim = c(38,0),
#'            main = 'Relative Rank of U.S. State Populations, 1790-1870')
#' 
#' @references
#' \url{http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk}
#' 
#' Tufte, Edward. \emph{The Visual Display of Quantitative Information}. Graphics Press.
#' 
#' Tufte, Edward. \emph{Beautiful Evidence}. Graphics Press.
#' 
#' @seealso \code{\link{cancer}}, \code{\link{gdp}}, \code{\link{states}}
#' For a ggplot2 version, use \code{\link{ggslopegraph}}.
#' @author Thomas J. Leeper
#' @import graphics
#' @importFrom stats reshape
#' @importFrom utils head
#' @export
slopegraph <- function(
    data,
    main = NULL,
    xlab = '',
    ylab = '',
    xlabels = names(data),
    xlim = c(.5,ncol(data)+.5),
    ylim = c(min(data,na.rm=TRUE)-diff(range(data,na.rm=TRUE))/100,max(data,na.rm=TRUE)+diff(range(data,na.rm=TRUE))/100),
    bty = 'n',
    xaxt = 'n',
    yaxt = 'n',
    panel.first = NULL,
    panel.last = NULL,
    labpos.left = 2,
    labpos.right = 4,
    leftlabels = NULL,
    rightlabels = NULL,
    decimals = 0L,
    col.lines = par('fg'),
    col.lab = col.lines,
    col.num = col.lines,
    col.xaxt = par('fg'),
    offset.x = NULL,
    offset.lab = .1,
    cex.lab = 1,
    cex.num = 1,
    family = "serif",
    font.lab = 1,
    font.num = 1,
    lty = par("lty"),
    lwd = par("lwd"),
    mai = NULL,
    na.span = FALSE,
    ...)
{
    # check decimal formatting
    fmt <- paste0("%0.", decimals, "f")
    # check data
    if (ncol(data) < 2) {
        stop('`data` must have at least two columns')
    }
    data[] <- lapply(data, round, decimals)
    # segmentize
    to_draw <- segmentize(as.matrix(data))
    # reshape for printing numeric value labels
    long <- reshape(data, direction = "long", varying = names(data), v.names = "value", sep = "")
    
    # draw margins
    if (is.null(mai)) {
        op <- par(mai=c(1, 0, if(is.null(main)) 0 else 1, 0))
        on.exit(par(op))
    } else {
        op <- par(mai=mai)
        on.exit(par(op))
    }
    plot(NA, xlim=xlim, ylim=ylim, main=main, family=family,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if (!is.null(panel.first)) {
        eval(panel.first)
    }
    
    # expand formatting arguments
    if (length(col.lab) == 1L) {
        col.lab <- rep(col.lab, length.out = nrow(data))
    }
    if (length(col.num) == 1L) {
        col.num <- rep(col.num, length.out = nrow(data))
    }
    if (length(col.lines) == 1L) {
        col.lines <- rep(col.lines, length.out = nrow(data))
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, nrow(data))
    }
    if (length(lty) == 1) {
        lty <- rep(lty, nrow(data))
    }
    col.num <- col.num[long[["id"]]]
    
    # x-axis
    axis(1, 1:ncol(data), labels = xlabels, col = col.xaxt, col.ticks = col.xaxt, family = family)
    
    # left-side labels
    if (!is.null(labpos.left)) {
        if (!is.null(leftlabels)) {
            leftlabs <- data[!is.na(data[,1]),1, drop = FALSE]
        } else {
            leftlabs <- leftlabels
        }
        text(1-offset.lab, bump_overlaps(leftlabs[,1], decimals = decimals),
             col=col.lab[which(!is.na(data[,1]))], rownames(leftlabs), pos=labpos.left, 
             cex=cex.lab, font=font.lab, family=family)
    }
    # right-side labels
    if (!is.null(labpos.right)) {
        if (!is.null(rightlabels)) {
            rightlabs <- data[!is.na(data[,ncol(data)]), ncol(data), drop = FALSE]
        } else {
            rightlabs <- rightlabels
        }
        text(ncol(data)+offset.lab, bump_overlaps(rightlabs[,1], decimals = decimals), 
             col=col.lab[which(!is.na(data[,ncol(data)]))], rownames(rightlabs), pos=labpos.right, 
             cex=cex.lab, font=font.lab, family=family)
    }
    
    if (is.null(offset.x)) {
        offset.x <- (max(strwidth(sprintf(fmt, long[["value"]]))) + 0.02)/2L
    }
    to_draw2 <- to_draw[!duplicated(to_draw),]
    apply(to_draw2, 1, function(rowdata){
            i <- rowdata[1]
            x1 <- rowdata[2]
            x2 <- rowdata[3]
            y1 <- rowdata[4]
            y2 <- rowdata[5]
            # draw lines
            ysloped <- (y2-y1)*offset.x
            segments(x1+offset.x, if(y1==y2) y1 else (y1+ysloped),
                     x2-offset.x, if(y1==y2) y2 else (y2-ysloped),
                     col = col.lines[i],
                     lty = lty[i],
                     lwd = lwd[i])
    })
    # numeric value labels 
    if (!is.null(col.num)) {
        text(long[["time"]], bump_overlaps(long[["value"]]), sprintf(fmt, long[["value"]]), 
             col = col.num, cex = cex.num, font = font.num, family = family)
    }
    # optional expression
    if (!is.null(panel.last)) {
        eval(panel.last)
    }
    # return `to_draw` invisibly
    invisible(to_draw)
}
