# Tufte-Inspired Slopegraphs in R #

This repository holds some working code for creating slopegraphs in R.

*This is very much a work in progress. Once it's more stable, I will release the package to CRAN.*

Pull requests welcome. Please report any issues on the [issues page](https://github.com/leeper/slopegraph/issues).

The package currently includes only one function (`slopegraph`), which produces a slopegraph from an observation-by-period dataframe. Everything is more or less drawn automatically, but is highly customizable in terms of line and text colors, font sizes and styles, axes, titles, and plotting behind and in front of the slopegraph lines.

## Installation ##

[![travis-ci](https://travis-ci.org/leeper/slopegraph.svg)](https://travis-ci.org/leeper/slopegraph) 
![CRAN](http://www.r-pkg.org/badges/version/slopegraph)
[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip)

To install the latest development version of **slopegraph** from GitHub:

```R
if(!require("devtools")){
    install.packages("devtools")
    library("devtools")
}
install_github("leeper/slopegraph")
```

## Examples ##

The current output of the `slopegraph` function (for the examples included in documentation) is shown below. The two examples use the GDP and cancer survival data described [here](http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk) and in Tufte's books.

The examples aim to mimic the plots drawn by Tufte (but to correct scale) and thus do not showcase the customizability of the package.

Note: In order to obtain the desired look and feel, the examples below rely on the `pdf()` graphics device and are then converted to PNG format using the `convert` utility from [ImageMagick](http://www.imagemagick.org/script/index.php). This mostly relates to the handling of fonts, aspect ratio, and image resolution.

### Cancer Survival Data ###


```r
pdf('inst/examples/cancer-survival.pdf',height=16, width=12, family='Palatino')
slopegraph(cancer, col.line='gray', xlim=c(-.5,5.5), binval=2.5, 
           labels=c('5 Year','10 Year','15 Year','20 Year'))
dev.off()

# convert to png for web display
shell("convert -resize 900x1800 -density 300 inst/examples/cancer-survival.pdf inst/examples/cancer-survival.png")
```

![Cancer Survival](inst/examples/cancer-survival.png)


### GDP Data ###


```r
pdf('inst/examples/gdp.pdf', height=12, width=8, family='Palatino')
slopegraph(gdp, col.line='gray', labels=c('1970','1979'), binval=3.75, 
           main='Current Receipts of Goverment as a Percentage of Gross Domestic Product')
dev.off()

# convert to png for web display
shell("convert -resize 900x1200 -density 300 inst/examples/gdp.pdf inst/examples/gdp.png")
```

![GDP](inst/examples/gdp.png)


### State Population Ranks ###

This example shows a use case when there is missing data in the data.frame:


```r
pdf('inst/examples/states.pdf', height=10, width=12, family='Palatino')
slopegraph(states, col.line='black', 
           main='Relative Rank of U.S. State Populations, 1790-1870')
dev.off()

# convert to png for web display
shell("convert -resize 1200x1000 -density 300 inst/examples/states.pdf inst/examples/states.png")
```

Coming soon!
