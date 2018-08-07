#' formatting changes as well as major changes to linecolor
#' @title Plot a Slopegraph a la Tufte using dplyr and ggplot2
#'
#' @description Creates a "slopegraph" as conceptualized by Edward Tufte. Slopegraphs are minimalist
#' and efficient presentations of your data that can simultaneously convey the relative rankings, 
#' the actual numeric values, and the changes and directionality of the data over time.
#' Takes a dataframe as input, with three named columns being used to draw the plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param dataframe a dataframe or an object that can be coerced to a dataframe. 
#' Basic error checking is performed, to include ensuring that the named columns 
#' exist in the dataframe. See the \code{\link{cancer2}} dataset for an example of
#' how the dataframe should be organized.
#' @param times a column inside the dataframe that will be plotted on the x axis. 
#' Traditionally this is some measure of time.  The function accepts a column of class
#' ordered, factor or character.  NOTE if your variable is currently a "date" class 
#' you must convert before using the function with \code{as.character(variablename)}.
#' @param measurement a column inside the dataframe that will be plotted on the y axis. 
#' Traditionally this is some measure such as a percentage.  Currently the function 
#' accepts a column of type integer or numeric.  The slopegraph will be most effective
#' when the measurements are not too disparate.
#' @param grouping a column inside the dataframe that will be used to group and distinguish measurements.
#' @param title Optionally the title to be displayed. title = NULL will remove it entirely. title = "" will provide an empty title but retain the spacing.
#' @param subtitle Optionally the sub-title to be displayed.  subtitle = NULL will remove it entirely. subtitle = "" will provide an empty subtitle but retain the spacing.
#' @param caption Optionally the caption to be displayed. caption = NULL will remove it entirely. caption = "" will provide an empty title but retain the spacing.
#' @param xtextsize Optionally the font size for the X axis labels to be displayed. xtextsize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param ytextsize Optionally the font size for the Y axis labels to be displayed. ytextsize = 3 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param titletextsize Optionally the font size for the title to be displayed. titletextsize = 14 is the default must be a numeric.
#' @param subtitletextsize Optionally the font size for the subtitle to be displayed. subtitletextsize = 10 is the default must be a numeric.
#' @param captiontextsize Optionally the font size for the caption to be displayed. captiontextsize = 8 is the default must be a numeric.
#' @param linethickness Optionally the thickness of the plotted lines. linethickness = 1 is the default must be a numeric.
#' @param datatextsize Optionally the font size of the plotted data points. datatextsize = 2.5 is the default must be a numeric.
#' @param linecolor Optionally the color of the plotted lines. By default it will use 
#' the ggplot2 color palette for coloring by \code{Grouping}. The user may override 
#' with \bold{one} valid color of their choice e.g. "black" (see colors() for choices) 
#' \bold{OR} 
#' they may provide a vector of colors such as c("gray", "red", "green", "gray", "blue") 
#' \bold{OR} a named vector like c("Green" = "gray", "Liberal" = "red", "NDP" = "green", 
#' "Others" = "gray", "PC" = "blue"). Any input must be character, and the length 
#' of a vector \bold{should} equal the number of levels in \code{Grouping}. If the 
#' user does not provide enough colors they will be recycled.
#' @param datatextfamily Optionally the font family of the plotted data points. datatextfamily = "sans" is the default.
#' @param datatextface Optionally the font face of the plotted data points. datatextface = "plain" is the default.
#' @param labeltextfamily Optionally the font family of the grouping labels. labeltextfamily = "sans" is the default.
#' @param labeltextface Optionally the font face of the grouping labels. labeltextface = "bold" is the default.
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @author Chuck Powell
#' @examples
#'
#' # basic graph
#' ggslopegraph2(cancer2, Year, Survival, Type,
#'               title = "Estimates of Percent Survival Rates")
#' 
#' # with further customization
#' ggslopegraph2(cancer2, Year, Survival, Type, 
#'               title = "Estimates of Percent Survival Rates", 
#'               linecolor = "black", 
#'               linethickness = 1, 
#'               subtitle = NULL, 
#'               caption = NULL)
#'               
#' # demonstrating linecolor recyclng
#' ggslopegraph2(cancer2, Year, Survival, Type, 
#'               title = "Estimates of Percent Survival Rates", 
#'               subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
#'               caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
#'               linecolor = c("black", "red", "grey"), 
#'               linethickness = .5)
#'
#'
#' @seealso For a observation-by-time data frame interface, see \code{\link{ggslopegraph}}.
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise %>% n enquo
#' @importFrom ggrepel geom_text_repel
#' @importFrom methods is
#' @export
ggslopegraph2 <-
  function(
    dataframe,
    times,
    measurement,
    grouping,
    title = "",
    subtitle = "",
    caption = "",
    xtextsize = 12,
    ytextsize = 3,
    titletextsize = 14,
    subtitletextsize = 10,
    captiontextsize = 8,
    linethickness = 1,
    linecolor = "ByGroup",
    datatextsize = 2.5,
    datatextfamily = "sans",
    datatextface = "plain",
    labeltextfamily = "sans",
    labeltextface = "bold"
  ) {
    # Since ggplot2 objects are just regular R objects, put them in a list
    my_special <- list(
      # Format tweaks
      scale_x_discrete(position = "top"), # move the x axis labels up top
      theme_bw(),
      theme(legend.position  = "none"), # Remove the legend
      theme(panel.border     = element_blank()), # Remove the panel border
      theme(axis.title.y     = element_blank()), # Remove just about everything from the y axis
      theme(axis.text.y      = element_blank()),
      theme(panel.grid.major.y = element_blank()),
      theme(panel.grid.minor.y = element_blank()),
      theme(axis.title.x     = element_blank()), # Remove a few things from the x axis
      theme(panel.grid.major.x = element_blank()),
      theme(axis.text.x.top      = element_text(size = xtextsize)), # and increase font size
      theme(axis.ticks       = element_blank()), # Remove x & y tick marks
      theme(plot.title       = element_text(size = titletextsize, face = "bold")), # Format title
      theme(plot.title       = element_text(hjust = 0.5)), # Center title & subtitle
      theme(plot.subtitle    = element_text(hjust = 0.5, size = subtitletextsize)),
      theme(plot.caption     = element_text(size = captiontextsize))
    )
    # for convenience store these
    Ndataframe <- deparse(substitute(dataframe)) # name of dataframe
    Ntimes <- deparse(substitute(times)) # name of times variable
    Nmeasurement <- deparse(substitute(measurement)) # name of measurement variable
    Ngrouping <- deparse(substitute(grouping)) # name of grouping variable
    # error checking and setup
    if (length(match.call()) <= 4) {
      stop("Not enough arguments passed... requires a dataframe, plus at least three variables")
    }
    argList <-  as.list(match.call()[-1])
    if (!exists(Ndataframe)) {
      stop("The first object in your list '", Ndataframe ,"' does not exist. It should be a dataframe", call. = FALSE)
    }
    if (!is(dataframe, "data.frame")) {
      stop(paste0("'", Ndataframe, "' does not appear to be a data frame"), call. = FALSE)
    }
    if (!Ntimes %in% names(dataframe)) {
      stop(paste0("'", Ntimes, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
    }
    if (!Nmeasurement %in% names(dataframe)) {
      stop(paste0("'", Nmeasurement, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
    }
    if (!deparse(substitute(grouping)) %in% names(dataframe)) {
      stop(paste0("'", Ngrouping, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
    }
    if (!class(dataframe[[Nmeasurement]]) %in% c("integer","numeric")) {
      stop(paste0("Sorry I need the measured variable '", Nmeasurement, "' to be a number"), call. = FALSE)
    }
    if (!"ordered" %in% class(dataframe[[Ntimes]])) { # keep checking
      if (!"character" %in% class(dataframe[[Ntimes]])) { # keep checking
        if ("factor" %in% class(dataframe[[Ntimes]])) { # impose order
          message(paste0("\nConverting '", Ntimes, "' to an ordered factor\n"))
          dataframe[[Ntimes]] <- factor(dataframe[[Ntimes]], ordered = TRUE)
        } else {
          stop(paste0("Sorry I need the variable '", Ntimes, "' to be of class character, factor or ordered"), call. = FALSE)
        }
      }
    }
    
    times <- enquo(times)
    measurement <- enquo(measurement)
    grouping <- enquo(grouping)
    
    if (length(linecolor) > 1) { 
      if (length(linecolor) < length(unique(dataframe[[Ngrouping]]))) {
        message(paste0("\nYou gave me ", length(linecolor), " colors I'm recycling because you have ", length(unique(dataframe[[Ngrouping]])), " ", Ngrouping, "\n"))
        linecolor <- rep(linecolor, length.out = length(unique(dataframe[[Ngrouping]])))
      }
      line_geom <- list(geom_line(aes_(color = grouping), size = linethickness), scale_color_manual(values = linecolor))
    } else {
      if (linecolor == "ByGroup") {
        line_geom <- list(geom_line(aes_(color = grouping, alpha = 1), size = linethickness))
      } else {
        line_geom <- list(geom_line(aes_(), size = linethickness, color = linecolor))
      }
    }
    
    dataframe %>%
      filter(!is.na(!! times), !is.na(!! measurement), !is.na(!! grouping))  %>%
      ggplot(aes_(group=grouping, y=measurement, x=times)) +
      line_geom +
      geom_text_repel(data = dataframe %>% filter(!! times == min(!! times)),
                      aes_(label = grouping) ,
                      hjust = "left",
                      fontface = labeltextface,
                      family = labeltextfamily,
                      size = ytextsize,
                      nudge_x = -.45,
                      direction = "y") +
      geom_text_repel(data = dataframe %>% filter(!! times == max(!! times)),
                      aes_(label = grouping),
                      hjust = "right",
                      fontface = labeltextface,
                      family = labeltextfamily,
                      size = ytextsize,
                      nudge_x = .5,
                      direction = "y") +
      geom_label(aes_(label = measurement), size = datatextsize, 
                 label.padding = unit(0.05, "lines"), label.size = 0.0,
                 fontface = datatextface, family = datatextfamily) +
      my_special +
      labs(
        title = title,
        subtitle = subtitle,
        caption = caption
      )
  }
