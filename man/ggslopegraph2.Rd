% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggslopegraph2.R
\name{ggslopegraph2}
\alias{ggslopegraph2}
\title{Plot a Slopegraph a la Tufte using dplyr and ggplot2}
\usage{
ggslopegraph2(dataframe, times, measurement, grouping, title = "",
  subtitle = "", caption = "", xtextsize = 12, ytextsize = 3,
  titletextsize = 14, subtitletextsize = 10, captiontextsize = 8,
  linethickness = 1, linecolor = "ByGroup", datatextsize = 2.5,
  datatextfamily = "sans", datatextface = "plain",
  labeltextfamily = "sans", labeltextface = "bold")
}
\arguments{
\item{dataframe}{a dataframe or an object that can be coerced to a dataframe. 
Basic error checking is performed, to include ensuring that the named columns 
exist in the dataframe. See the \code{\link{cancer2}} dataset for an example of
how the dataframe should be organized.}

\item{times}{a column inside the dataframe that will be plotted on the x axis. 
Traditionally this is some measure of time.  The function accepts a column of class
ordered, factor or character.  NOTE if your variable is currently a "date" class 
you must convert before using the function with \code{as.character(variablename)}.}

\item{measurement}{a column inside the dataframe that will be plotted on the y axis. 
Traditionally this is some measure such as a percentage.  Currently the function 
accepts a column of type integer or numeric.  The slopegraph will be most effective
when the measurements are not too disparate.}

\item{grouping}{a column inside the dataframe that will be used to group and distinguish measurements.}

\item{title}{Optionally the title to be displayed. title = NULL will remove it entirely. title = "" will provide an empty title but retain the spacing.}

\item{subtitle}{Optionally the sub-title to be displayed.  subtitle = NULL will remove it entirely. subtitle = "" will provide an empty subtitle but retain the spacing.}

\item{caption}{Optionally the caption to be displayed. caption = NULL will remove it entirely. caption = "" will provide an empty title but retain the spacing.}

\item{xtextsize}{Optionally the font size for the X axis labels to be displayed. xtextsize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales}

\item{ytextsize}{Optionally the font size for the Y axis labels to be displayed. ytextsize = 3 is the default must be a numeric. Note that X & Y axis text are on different scales}

\item{titletextsize}{Optionally the font size for the title to be displayed. titletextsize = 14 is the default must be a numeric.}

\item{subtitletextsize}{Optionally the font size for the subtitle to be displayed. subtitletextsize = 10 is the default must be a numeric.}

\item{captiontextsize}{Optionally the font size for the caption to be displayed. captiontextsize = 8 is the default must be a numeric.}

\item{linethickness}{Optionally the thickness of the plotted lines. linethickness = 1 is the default must be a numeric.}

\item{linecolor}{Optionally the color of the plotted lines. By default it will use 
the ggplot2 color palette for coloring by \code{Grouping}. The user may override 
with \bold{one} valid color of their choice e.g. "black" (see colors() for choices) 
\bold{OR} 
they may provide a vector of colors such as c("gray", "red", "green", "gray", "blue") 
\bold{OR} a named vector like c("Green" = "gray", "Liberal" = "red", "NDP" = "green", 
"Others" = "gray", "PC" = "blue"). Any input must be character, and the length 
of a vector \bold{should} equal the number of levels in \code{Grouping}. If the 
user does not provide enough colors they will be recycled.}

\item{datatextsize}{Optionally the font size of the plotted data points. datatextsize = 2.5 is the default must be a numeric.}

\item{datatextfamily}{Optionally the font family of the plotted data points. datatextfamily = "sans" is the default.}

\item{datatextface}{Optionally the font face of the plotted data points. datatextface = "plain" is the default.}

\item{labeltextfamily}{Optionally the font family of the grouping labels. labeltextfamily = "sans" is the default.}

\item{labeltextface}{Optionally the font face of the grouping labels. labeltextface = "bold" is the default.}
}
\value{
A \code{\link[ggplot2]{ggplot}} object.
}
\description{
Creates a "slopegraph" as conceptualized by Edward Tufte. Slopegraphs are minimalist
and efficient presentations of your data that can simultaneously convey the relative rankings, 
the actual numeric values, and the changes and directionality of the data over time.
Takes a dataframe as input, with three named columns being used to draw the plot.
Makes the required adjustments to the ggplot2 parameters and returns the plot.
}
\details{
formatting changes as well as major changes to linecolor
}
\examples{

# basic graph
ggslopegraph2(cancer2, Year, Survival, Type,
              title = "Estimates of Percent Survival Rates")

# with further customization
ggslopegraph2(cancer2, Year, Survival, Type, 
              title = "Estimates of Percent Survival Rates", 
              linecolor = "black", 
              linethickness = 1, 
              subtitle = NULL, 
              caption = NULL)
              
# demonstrating linecolor recyclng
ggslopegraph2(cancer2, Year, Survival, Type, 
              title = "Estimates of Percent Survival Rates", 
              subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
              caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
              linecolor = c("black", "red", "grey"), 
              linethickness = .5)


}
\seealso{
For a observation-by-time data frame interface, see \code{\link{ggslopegraph}}.
}
\author{
Chuck Powell
}
