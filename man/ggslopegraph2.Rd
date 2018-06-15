% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggslopegraph2.R
\name{ggslopegraph2}
\alias{ggslopegraph2}
\title{Plot a Slopegraph a la Tufte using dplyr and ggplot2}
\usage{
ggslopegraph2(dataframe, times, measurement, grouping, title = "",
  subtitle = "", caption = "", xtextsize = 12, ytextsize = 3,
  titletextsize = 14, subtitletextsize = 10, captiontextsize = 8,
  linethickness = 1, linecolor = "ByGroup", datatextsize = 2.5)
}
\arguments{
\item{dataframe}{a dataframe or an object that can be coerced to a dataframe. Basic error checking is performed.}

\item{times}{a column inside the dataframe that will be plotted on the x axis. Traditionally this is some measure of time.  The function accepts a column of class ordered, factor or character.}

\item{measurement}{a column inside the dataframe that will be plotted on the y axis. Traditionally this is some measure such as a percentage.  Currently the function accepts a column of type integer or numeric.}

\item{grouping}{a column inside the dataframe that will be used to group and distinguish measurements.}

\item{title}{Optionally the title to be displayed. title = NULL will remove it entirely. title = "" will provide and empty title but retain the spacing.}

\item{subtitle}{Optionally the sub-title to be displayed.  subtitle = NULL will remove it entirely. subtitle = "" will provide and empty title but retain the sapcing.}

\item{caption}{Optionally the caption to be displayed. caption = NULL will remove it entirely. caption = "" will provide and empty title but retain the sapcing.}

\item{xtextsize}{Optionally the font size for the X axis labels to be displayed. xtextsize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales}

\item{ytextsize}{Optionally the font size for the Y axis labels to be displayed. ytextsize = 3 is the default must be a numeric. Note that X & Y axis text are on different scales}

\item{titletextsize}{Optionally the font size for the title to be displayed. titletextsize = 14 is the default must be a numeric.}

\item{subtitletextsize}{Optionally the font size for the subtitle to be displayed. subtitletextsize = 10 is the default must be a numeric.}

\item{captiontextsize}{Optionally the font size for the caption to be displayed. captiontextsize = 8 is the default must be a numeric.}

\item{linethickness}{Optionally the thickness of the plotted lines. linethickness = 1 is the default must be a numeric.}

\item{linecolor}{Optionally the color of the plotted lines. By default it will use the ggplot2 color palette for coloring by group. The user may override with one valid color of their choice e.g. "black" must be character.}

\item{datatextsize}{Optionally the font size of the plotted data points. datatextsize = 2.5 is the default must be a numeric.}
}
\value{
A \code{\link[ggplot2]{ggplot}} object.
}
\description{
Takes a dataframe as input, with three named columns being used to plot. Makes the required adjustments to the ggplot2 parameters and returns the plot.
}
\examples{

ggslopegraph2(newcancer, Year, Survival, Type)
ggslopegraph2(newcancer, Year, Survival, Type, title = "Estimates of Percent Survival Rates")
ggslopegraph2(newcancer, Year, Survival, Type, 
                title = "Estimates of Percent Survival Rates", 
                linecolor = "black", 
                linethickness = 1, 
                subtitle = NULL, 
                caption = NULL)

}
\seealso{
For a observation-by-time data frame interface, see \code{\link{ggslopegraph}}.
}
\author{
Chuck Powell
}