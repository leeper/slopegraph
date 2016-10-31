#' @rdname ggslopegraph
#' @title Create Slopegraph from a data frame using ggplot2
#' @description Convert an R data frame (containing a panel dataset, where rows are observations and columns are time periods) into an Edward Tufte-inspired Slopegraph using ggplot2
#' @param data An observation-by-period data.frame, with at least two columns. Missing values are allowed.
#' @param title A character string specifying a title. Passed to \code{\link[ggplot2]{ggtitle}}.
#' @param xlab A character string specifying an x-axis label. Passed to \code{\link[ggplot2]{scale_x_continuous}}.
#' @param ylab A character string specifying an y-axis label. Passed to \code{\link[ggplot2]{scale_y_continuous}}.
#' @param yrev A logical indicating whether to use \code{\link[ggplot2]{scale_y_reverse}} rather than the default \code{\link[ggplot2]{scale_y_continuous}}.
#' @param size A numeric value indicating the size of numeric labels.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @examples
#' ## Ranking of U.S. State populations
#' data(states)
#' ggslopegraph(states, title = 'Relative Rank of U.S. State Populations, 1790-1870') + scale_y_reverse()
#' @seealso For a base graphics version, use \code{\link{slopegraph}}.
#' @import ggplot2
#' @export
ggslopegraph <- function(data, title = as.character(substitute(data)), xlab = "", ylab = "", yrev = FALSE, size = 3) {
    # check data
    if (ncol(data) < 2) {
        stop("'data' must have at least two columns")
    }
    to_draw <- segmentize(as.matrix(data))
    colnames(to_draw) <- c("row", "x1", "x2", "y1", "y2")
    to_draw <- as.data.frame(to_draw)
    
    g <- ggplot(to_draw) + 
           scale_x_continuous(name = xlab, breaks = seq_len(ncol(data)), 
                              labels = names(data), limits = c(0L,ncol(data))) +
           ggtitle(title)
    if (isTRUE(yrev)) {
        g <- g + scale_y_reverse(name = ylab, labels = NULL, limits = range(c(to_draw[["y1"]], to_draw[["y2"]])))
    } else {
        g <- g + scale_y_continuous(name = ylab, labels = NULL, limits = range(c(to_draw[["y1"]], to_draw[["y2"]])))
    }
    
    for (i in seq_len(nrow(data))) {
        # add left-side row labels
        if (!is.na(data[i, 1])) {
            g <- g + annotate("text", x = 0.75, y = data[i, 1], label = rownames(data)[i], hjust = 1L)
        }
        # add left-side row labels
        if (!is.na(data[i, ncol(data)])) {
            g <- g + annotate("text", x = ncol(data)+0.25, y = data[i, ncol(data)], label = rownames(data)[i], hjust = 0L)
        }
    }
    for (i in seq_len(nrow(to_draw))) {
        # add segments
        g <- g + annotate("segment", 
                          x = to_draw[i, "x1"] + 0.1, y = to_draw[i, "y1"], 
                          xend = to_draw[i, "x2"] - 0.1, yend = to_draw[i, "y2"])
        # add value labels
        g <- g + annotate("text", x = to_draw[i, "x1"], y = to_draw[i, "y1"], 
                          label = as.character(to_draw[i, "y1"]), hjust = 0.5, size = size) +
                 annotate("text", x = to_draw[i, "x2"], y = to_draw[i, "y2"], 
                          label = as.character(to_draw[i, "y2"]), hjust = 0.5, size = size)
    }
    
    return(g)
}
