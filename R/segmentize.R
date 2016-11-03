#' @title Segmentize an observation-by-period data frame
#' @description Convert an observation-by-period data frame into a data frame of line segment coordinates, where each row represents a line segment connecting two time points from the original data.
#' @param data A data frame containing observation-by-period data, where the only columns represent sequentially ordered time period values for each observation. Some values can be missing.
#' @param na.span A logical indicating whether line segments should span periods with missing values. The default is \code{FALSE}, such that some segments are not drawn.
#' @param na.omit A logical indicating whether to drop missing observations from the resulting data frame
#' @return A five-variable data frame containing: the row from the original data frame, and x1, x2, y1, y2 positions for each segment.
#' @examples
#' data(gdp)
#' head(segmentize(gdp))
#' @importFrom stats embed na.omit setNames
#' @export
segmentize <- function(data, na.span = FALSE, na.omit = TRUE) {
    
    # `pairsmat`: matrix of pairs of adjacent columns in each row
    if (ncol(data) == 2) {
        pairsmat <- matrix(1:2, nrow = 1L)
    } else {
        pairsmat <- embed(seq_len(ncol(data)), 2)[,2:1]
    }
    # output
    out <- matrix(NA_real_, nrow = nrow(data) * nrow(pairsmat), ncol = 5L)
    k <- 1L
    for (i in seq_len(nrow(data))) {
        for (j in seq_len(nrow(pairsmat))) {
            out[k, 1:3] <- c(i, pairsmat[j,1], pairsmat[j,2])
            if (is.na(data[i, pairsmat[j,1]])) {
                # left value is missing (or both left and right are missing)
                # values are already missing in `out`, so do nothing
            } else if (is.na(data[i, pairsmat[j,2]])) {
                # right value is missing, but left value is present
                if (isTRUE(na.span)) {
                    # if segments should span missing values
                    # specify the leftmost non-missing value to the right
                    #browser()
                    # get values for this row/observation
                    rowvals <- unlist(data[i, (pairsmat[j,2]):ncol(data), drop = TRUE])
                    if (length(rowvals) > 1) {
                        # if right isn't the last value in the row, find the first non-missing
                        nexty <- which(!is.na(rowvals))
                        if (length(nexty)) {
                            out[k, 3:5] <- c((pairsmat[j,2] + (nexty[1L] - 1)), # x2
                                             data[i, pairsmat[j,1]],  # y1
                                             rowvals[nexty[1L]]) # y2
                        }
                        rm(nexty)
                    }
                    rm(rowvals)
                }
                # otherwise do nothing
            } else {
                # both left and right values are present
                out[k, 4:5] <- c(data[i, pairsmat[j,1]], data[i, pairsmat[j,2]])
            }
            k <- k + 1L
        }
    }
    # return, optionally dropping missing values
    if (isTRUE(na.omit)) {
        out <- na.omit(out)
    }
    return(setNames(as.data.frame(out), c("row", "x1", "x2", "y1", "y2")))
}
