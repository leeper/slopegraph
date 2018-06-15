## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Install the development version from GitHub
# install.packages("devtools")
# devtools::install_github("leeper/slopegraph")
library(slopegraph)

## ----ggslope1, fig.height=10, fig.width=7--------------------------------
ggslopegraph2(cancer2, Year, Survival, Type)

## ----ggslope2, fig.height=11, fig.width=8.5------------------------------
ggslopegraph2(dataframe = cancer2,
                times = Year,
                measurement = Survival,
                grouping = Type,
                title = "Estimates of Percent Survival Rates",
                subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
                caption = NULL
                )

## ----ggslope3, fig.height=5, fig.width=5---------------------------------
moredata <- structure(list(Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                            .Label = c("11-May-18", "18-May-18", "25-May-18"), 
                                            class = "factor"), 
                           Party = structure(c(5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L), 
                                             .Label = c("Green", "Liberal", "NDP", "Others", "PC"), 
                                             class = "factor"), 
                           Pct = c(42.3, 28.4, 22.1, 5.4, 1.8, 41.9, 29.3, 22.3, 5, 1.4, 41.9, 26.8, 26.8, 5, 1.4)), 
                      class = "data.frame", 
                      row.names = c(NA, -15L))
#tail(moredata)
ggslopegraph2(moredata, 
              Date, 
              Pct, 
              Party, 
              title = "Notional data", 
              subtitle = NULL, 
              caption = NULL)

## ----ggslope4, fig.height=5, fig.width=5---------------------------------
ggslopegraph2(moredata, Date, Pct, Party, 
                title = "Notional data", 
                subtitle = "none", 
                caption = "imaginary",
                linecolor = "gray", 
                linethickness = .5,
                ytextsize = 4
                )

## ----ggslope5, fig.height=5, fig.width=5---------------------------------
ggslopegraph2(moredata, 
              Date, 
              Pct, 
              Party, 
              title = "Notional data", 
              subtitle = "none", 
              caption = "imaginary",
              linecolor = c("Green" = "gray", "Liberal" = "green", "NDP" = "red", "Others" = "gray", "PC" = "gray"), 
              linethickness = .5,
              ytextsize = 4
                )

## ----ggslope6, fig.height=12, fig.width=6--------------------------------
newgdp <- structure(list(Year = c("Year1970", "Year1979", "Year1970", "Year1979",  "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979",  "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979", "Year1970", "Year1979"), 
               Country = structure(c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 8L, 8L, 9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L, 13L, 13L, 14L, 14L, 15L, 15L), 
              .Label = c("Sweden", "Netherlands", "Norway", "Britain", "France", "Germany", "Belgium", "Canada", "Finland", "Italy", "US", "Greece", "Switzerland", "Spain", "Japan"), class = "factor"), 
               GDP = c(46.9, 57.4, 44, 55.8, 43.5, 52.2, 40.7, 39, 39, 43.4, 37.5, 42.9, 35.2, 43.2, 35.2, 35.8, 34.9, 38.2, 30.4, 35.7, 30.3, 32.5, 26.8, 30.6, 26.5, 33.2, 22.5, 27.1, 20.7, 26.6)), 
              row.names = c(NA, -30L), class = "data.frame")

ggslopegraph2(newgdp, 
                Year, 
                GDP, 
                Country, 
                title = "Gross GDP", 
                subtitle = NULL, 
                caption = NULL,
                linethickness = .5,
                ytextsize = 4,
                linecolor = c(rep("gray",3), "red", rep("gray",3), "red", rep("gray",10))
                )

## ----ggslope7, fig.height=7, fig.width=6---------------------------------
newgdp$rGDP <- signif(newgdp$GDP, 2)
ggslopegraph2(newgdp, 
                Year, 
                rGDP, 
                Country, 
                title = "Gross GDP", 
                subtitle = NULL, 
                caption = NULL,
                linethickness = .5,
                ytextsize = 4,
                linecolor = c(rep("gray",6), rep("green",3), rep("gray",10))
                )

