# slopegraph 0.1.12

* The order of some formal arguments in both `slopegraph()` and `ggslopegraph()` has been changed.

# slopegraph 0.1.10

* `segmentize()` gains new argument `na.span`, a logical indicating whether line segments should span periods with missing values. The default remains to not do this and simply omit segments when missing values are present. This argument is cascaded through to `slopegraph()` and `ggslopegraph()`. (h/t Joran Elias)
* Drawing of left- and right-side observation labels has been made optional (by setting `labpos.left` or `labpos.right` to `NULL`).

# slopegraph 0.1.9

* Added a new function, `ggslopegraph()` that produces a ggplot2-based slopegraph.
* Exported the internal function, `segmentize()`, which converts an input data frame into a matrix of line segment coordinates.
* Changed name of formal argumnet `df` to `data` in `slopegraph()`

# slopegraph 0.1.8

* Modified the internal code of slopegraph to work with a matrix of line segments created from the original data, which is now returned invisibly.
* Fixed some data errors in the `states` dataset.
* Fixed a bug in printing of decimals. (#11, h/t Tony Ladson)
* Colors of lines and labels now match by default. (#11, h/t Tony Ladson)
* Switched to roxygen documentation and devtools-based development workflow.

# slopegraph 0.1.1

* Add dataset containing relative ranking of U.S. state populations.

# slopegraph 0.1.0

* Initial release to Github.
