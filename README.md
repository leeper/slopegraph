# Tufte-Inspired Slopegraphs in R #

[![Build Status](https://travis-ci.org/leeper/slopegraph.png?branch=master)](https://travis-ci.org/leeper/slopegraph)

This repository holds some working code for creating slopegraphs in R.

*This is very much a work in progress. Once it's more stable, I will release the package to CRAN.*

Pull requests welcome. Please report any issues on the [issues page](https://github.com/leeper/slopegraph/issues).

The package currently includes only one function (`slopegraph`), which produces a slopegraph from an observation-by-period dataframe. Everything is more or less drawn automatically, but is highly customizable in terms of line and text colors, font sizes and styles, axes, titles, and plotting behind and in front of the slopegraph lines.


## Examples ##

The current output of the `slopegraph` function (for the examples included in documentation) is shown below. The two examples use the GDP and cancer survival data described [here](http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk) and in Tufte's books.

The examples aim to mimic the plots drawn by Tufte (but to correct scale) and thus do not showcase the customizability of the package.

### Cancer Survival Data ###

![Cancer Survival](inst/examples/cancer-survival.png)


### GDP Data ###

![GDP](inst/examples/gdp.png)
