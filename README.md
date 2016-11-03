# Tufte-Inspired Slopegraphs in R

This repository holds some working code for creating "slopegraphs" in R.

*This is very much a work in progress. Once it's more stable, I will release the package to CRAN.*

Pull requests welcome. Please report any issues on the [issues page](https://github.com/leeper/slopegraph/issues).

The package currently includes one mainfunction, `slopegraph()`, which produces a slopegraph from an observation-by-period data frame. Everything is more or less drawn automatically, but is highly customizable in terms of line and text colors, font sizes and styles, axes, titles, and plotting behind and in front of the slopegraph lines. An underlying function, `segmentize()` produces the data structure used for the actual plotting. And a new function, `ggslopegraph()` does the same as `slopegraph()` but using ggplot2 graphics.

## Examples

The current output of the `slopegraph()` function (for the examples included in documentation) are shown below. 

Tufte's most famous slopegraph example is probably the ["cancer survival graph,"](http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk) depicting 5, 10, 15, and 20 year survival rates for various cancers. The first example mimics this result but draws it to the correct scale (unlike Tufte's original):


```r
library("slopegraph")
data(cancer)
slopegraph(cancer, col.lines = 'gray', col.lab = "black", 
           xlim = c(-.5,5.5), cex.lab = 0.5, cex.num = 0.5,
           xlabels = c('5 Year','10 Year','15 Year','20 Year'))
```

![Cancer Survival](https://rawgithub.com/leeper/slopegraph/master/inst/examples/cancer-survival-1.svg)


The second example, also from Tufte, shows changes in gross domestic product for a small set of countries over two points in time:


```r
data(gdp)
slopegraph(gdp, col.lines = 'gray', col.lab = "black", xlabels = c('1970','1979'),  
           main = 'Current Receipts of Goverment as a Percentage of Gross Domestic Product')
```

![GDP](https://rawgithub.com/leeper/slopegraph/master/inst/examples/gdp-1.svg)

This third example comes from an 1878 publication ([a copy of which is available here](http://www.davidrumsey.com/luna/servlet/detail/RUMSEY~8~1~207741~3003452:Chart-Exhibiting-the-Relative-Rank-)), showing the relative ranking of the population of various U.S. states. This example features a reversed y-axis to better display the ranking and I demonstrate the `col.lines` argument to highlight South Carolina:


```r
data(states)
cols <- `[<-`(rep("black", 37), 7, "red")
slopegraph(states, xlim = c(-1, 12), ylim = c(37,0), offset.x = 0.06,
           col.lines = cols, col.lab = cols, 
           main = 'Relative Rank of U.S. State Populations, 1790-1870')
```

![states](https://rawgithub.com/leeper/slopegraph/master/inst/examples/states-1.svg)

As of v0.1.9, there is also a ggplot2-based function, `ggslopegraph()` that produces a similar representation but using ggplot2 graphics:


```r
require("ggplot2")
data(states)
cols <- `[<-`(rep("black", 37), 7, "red")
ggslopegraph(states, offset.x = 0.06, yrev = TRUE,
  col.lines = cols, col.lab = cols, 
  main = 'Relative Rank of U.S. State Populations, 1790-1870') +
 theme_bw()    
```

![ggstates](https://rawgithub.com/leeper/slopegraph/master/inst/examples/ggstates-1.svg)



## Installation

[![CRAN](http://www.r-pkg.org/badges/version/slopegraph)](http://cran.r-project.org/package=slopegraph)
[![Build Status](https://travis-ci.org/leeper/slopegraph.svg?branch=master)](https://travis-ci.org/leeper/slopegraph)
[![Build status](https://ci.appveyor.com/api/projects/status/t6nxndmvvcw3gw7f/branch/master?svg=true)](https://ci.appveyor.com/project/leeper/slopegraph/branch/master)
[![codecov.io](http://codecov.io/github/leeper/slopegraph/coverage.svg?branch=master)](http://codecov.io/github/leeper/slopegraph?branch=master)
[![Project Status: Work in Progress](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

To install the latest development version of **slopegraph** from GitHub:

```R
if (!require("ghit")) {
    install.packages("ghit")
}
ghit::install_github("leeper/slopegraph")
```

