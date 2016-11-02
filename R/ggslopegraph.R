#' @rdname ggslopegraph
#' @title Create Slopegraph from a data frame using ggplot2
#' @description Convert an R data frame (containing a panel dataset, where rows are observations and columns are time periods) into an Edward Tufte-inspired Slopegraph using ggplot2
#' @param data An observation-by-period data.frame, with at least two columns. Missing values are allowed.
#' @param title A character string specifying a title. Passed to \code{\link[ggplot2]{ggtitle}}.
#' @param xlim A two-element numeric vector specifying the y-axis limits.
#' @param ylim A two-element numeric vector specifying the y-axis limits.
#' @param xlab A character string specifying an x-axis label. Passed to \code{\link[ggplot2]{scale_x_continuous}}.
#' @param ylab A character string specifying an y-axis label. Passed to \code{\link[ggplot2]{scale_y_continuous}}, or \code{\link[ggplot2]{scale_y_reverse}} if \code{yrev = TRUE}.
#' @param labels The labels to use for the slopegraph periods. Default is \code{names(df)}.#' @param xlab A character string specifying an x-axis label. Passed to \code{\link[ggplot2]{scale_x_continuous}}.
#' @param xbreaks Passed to \code{breaks} in \code{\link[ggplot2]{scale_x_continuous}}.
#' @param ybreaks Passed to \code{breaks} in \code{\link[ggplot2]{scale_y_continuous}}.
#' @param yrev A logical indicating whether to use \code{\link[ggplot2]{scale_y_reverse}} rather than the default \code{\link[ggplot2]{scale_y_continuous}}.
#' @param decimals The number of decimals to display for values in the plot. Default is \code{NULL} (none).
#' @param col.lines A vector of colors for the slopegraph lines. Default is \code{par('fg')}.
#' @param col.lab A vector of colors for the observation labels. Default is \code{par('fg')}.
#' @param col.num A vector of colors for the number values. Default is \code{par('fg')}.
#' @param cex.lab A numeric value indicating the size of row labels. Default is \code{3}. See \code{\link[ggplot2]{geom_text}}.
#' @param cex.num A numeric value indicating the size of numeric labels. Default is \code{3}. See \code{\link[ggplot2]{geom_text}}.
#' @param lwd A vector of line width values for the slopegraph lines.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @examples
#' ## Ranking of U.S. State populations
#' data(states)
#' ggslopegraph(states, title = 'Relative Rank of U.S. State Populations, 1790-1870', yrev = TRUE)
#' 
#' cls <- rep("black", nrow(states))
#' cls[rownames(states) == "South Carolina"] <- "red"
#' cls[rownames(states) == "Tennessee"] <- "blue"
#' ggslopegraph(states, title = 'Relative Rank of U.S. State Populations, 1790-1870', yrev = TRUE, col.lines = cls, col.lab = cls)
#' @seealso For a base graphics version, use \code{\link{slopegraph}}.
#' @import ggplot2
#' @export
ggslopegraph <- 
function(data, 
         title = as.character(substitute(data)), 
         xlab = "", 
         ylab = "", 
         xlim = c(-1L,ncol(data)+2L), 
         ylim = range(data, na.rm = TRUE), 
         xbreaks = NULL,
         ybreaks = NULL,
         yrev = ylim[1] > ylim[2], 
         decimals = NULL,
         col.lines = "black",
         col.lab = "black",
         col.num = "black",
         lwd = 0.5,
         cex.lab = 3L,
         cex.num = 3L)
{
    # check data
    if (ncol(data) < 2) {
        stop("'data' must have at least two columns")
    }
    to_draw <- segmentize(as.matrix(data))
    colnames(to_draw) <- c("row", "x1", "x2", "y1", "y2")
    to_draw <- as.data.frame(to_draw)
    
    xoffset <- 0.1
    yoffset <- 0
    
    # check decimal formatting
    fmt <- if (is.null(decimals)) "%0.0f" else paste0("%0.", decimals, "f")
    # expand formatting arguments
    if (length(col.lines) == 1) {
        col.lines <- rep(col.lines, nrow(data))
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, nrow(data))
    }
    if (length(col.num) == 1) {
        col.num <- rep(col.num, nrow(data))
    }
    if (length(col.lab) == 1) {
        col.lab <- rep(col.lab, nrow(data))
    }
    col.num <- col.num[to_draw[["row"]]]
    col.lines <- col.lines[to_draw[["row"]]]
    lwd <- lwd[to_draw[["row"]]]
    
    # draw
    g <- ggplot() + 
        # segments
        geom_segment(aes(x = x1 + xoffset, y = y1 + yoffset, xend = x2 - xoffset, yend = y2 - yoffset), 
                     col = col.lines,
                     data = to_draw, inherit.aes = FALSE) + guides(fill = FALSE) + 
        # x1 labels 
        geom_text(aes(x = x1, y = y1, label = sprintf(fmt, y1)), color = col.num, 
                  data = to_draw, inherit.aes = FALSE,
                  size = cex.num, hjust = 0.5) +
        # x2 labels 
        geom_text(aes(x = x2, y = y2, label = sprintf(fmt, y2)), color = col.num, 
                  data = to_draw, inherit.aes = FALSE,
                  size = cex.num, hjust = 0.5) +
        # x-axis labels
        scale_x_continuous(name = xlab, breaks = xbreaks, 
                           labels = names(data), limits = xlim) +
        # title
        ggtitle(title) +
        if (isTRUE(yrev)) {
            scale_y_reverse(name = ylab, breaks = ybreaks, labels = NULL, limits = rev(ylim))
        } else {
            scale_y_continuous(name = ylab, breaks = ybreaks, labels = NULL, limits = ylim)
        }
        
        leftlabs <- data[!is.na(data[,1]), 1, drop = FALSE]
        which_right <- data[!is.na(data[,ncol(data)]), ncol(data), drop = FALSE]
        g <- g + # left-side row labels
                 geom_text(aes(x = 0.8, y = leftlabs[,1], 
                               label = rownames(leftlabs)), 
                           color = col.lab[!is.na(data[,1])],
                           data = NULL, inherit.aes = FALSE, size = cex.lab, hjust = 1L) +
                 # right-side row labels
                 geom_text(aes(x = ncol(data) + 0.2, y = which_right[,1], 
                               label = rownames(which_right)), 
                           color = col.lab[!is.na(data[,ncol(data)])],
                           data = NULL, inherit.aes = FALSE, size = cex.lab, hjust = 0L)
            
    return(g + theme(legend.position="none") + guides(fill = FALSE))
}
