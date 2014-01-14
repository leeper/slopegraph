\name{slopegraph}
\alias{slopegraph}
\title{Create Slopegraph from Dataframe}
\description{Convert an R dataframe (containing a panel dataset, where rows are observations and columns are time periods) into an Edward Tufte-inspired Slopegraph.}
\usage{
slopegraph(df,
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
}

\arguments{
    \item{df}{An observation-by-period dataframe, with at least two columns.}
	\item{xlim}{The x-axis limits of the plot. Automatically calculated by default.}
    \item{ylim}{The y-axis limits of the plot. Automatically calculated by default.}
    \item{main}{The main title of the plot. Default is \code{NULL} (none). See \code{? par}.}
    \item{bty}{The box type for the plot. Default is \code{'n'} (none). See \code{? par}.}
    \item{xaxt}{The x-axis type for the plot. Default is \code{'n'} (none). See \code{? par}.}
    \item{yaxt}{The y-axis type for the plot. Default is \code{'n'} (none). See \code{? par}.}
    \item{xlab}{The x-axis label for the plot. Default is \code{''} (none). See \code{? par}.}
    \item{ylab}{The y-axis label for the plot. Default is \code{''} (none). See \code{? par}.}
    \item{add.before}{An expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph). Default is \code{NULL}.}
    \item{add.after}{An expression to add something after adding the plot content. Default is \code{NULL}.}
    \item{labels}{The labels to use for the slopegraph periods. Default is \code{names(df)}.}
    \item{labpos.left}{The \code{pos} (positioning) parameter for the leftside observation labels. Default is \code{2}. See \code{? par}.}
    \item{decimals}{The number of decimals to display for values in the plot. Default is \code{NULL} (none).}
    \item{binval}{Threshold at which to force binning of labels and values (multiplier of the height of an "m"). Default is \code{1.5}.}
    \item{col.lines}{A vector of colors for the slopegraph lines. Default is \code{par('fg')}.}
    \item{col.lab}{A vector of colors for the observation labels. Default is \code{par('fg')}.}
    \item{col.num}{A vector of colors for the number values. Default is \code{par('fg')}.}
    \item{col.xaxt}{A character string containing the x-axis color. Default is \code{par('fg')}.}
    \item{offset.x}{A small offset for \code{segments}, to be used when positioning the numeric values. Default is \code{.1}.}
    \item{offset.lab}{A small offset for the observation labels. Default is \code{.1}.}
    \item{cex.lab}{Default is \code{1}. See \code{? par}.}
    \item{cex.num}{Default is \code{1}. See \code{? par}.}
    \item{font.lab}{Default is \code{1}. See \code{? text}.}
    \item{font.num}{Default is \code{1}. See \code{? text}.}
    \item{lty}{A vector of line type values for the slopegraph lines. Default is \code{par("lty")}. See \code{? par}.}
    \item{lwd}{A vector of line width values for the slopegraph lines. Default is \code{par("lwd")}. See \code{? par}.}
    \item{mai}{A margin specification. Default is \code{NULL}. See \code{? par}.}
    \item{\ldots}{Additional arguments to \code{plot}.}
}
\details{Convert an observation-by-period dataframe into a slopegraph.}

\value{\code{NULL}}

\references{
\url{http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk}

Tufte, Edward. \emph{The Visual Display of Quantitative Information}. Graphics Press.

Tufte, Edward. \emph{Beautiful Evidence}. Graphics Press.
}
\examples{

## The two examples below are included in the inst/examples directory

\dontrun{
## Tufte's Cancer Graph (to the correct scale)
data(cancer)
pdf('cancer-survival.pdf', height=16, width=12, family='Palatino')
slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5),
           labels=c('5 Year','10 Year','15 Year','20 Year'), binval=2.5)
dev.off()
}

\dontrun{
## Tufte's GDP Graph
data(gdp)
pdf('gdp.pdf', height=12, width=8, family='Palatino')
slopegraph(gdp, col.line='gray', labels=c('1970','1979'), 
    main='Current Receipts of Goverment as a Percentage of Gross Domestic Product',
    binval=3.75)
dev.off()
}
}
\author{Thomas J. Leeper.}
%\keyword{}
%\seealso{}
