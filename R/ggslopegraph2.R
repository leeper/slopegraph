#' Plot a Slopegraph a la Tufte using dplyr and ggplot2
#'
#' Takes a dataframe as input, with three named columns being used to plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param dataframe a dataframe or an object that can be coerced to a dataframe. Basic error checking is performed.
#' @param Times a column inside the dataframe that will be plotted on the x axis. Traditionally this is some measure of time.  The function accepts a column of class ordered, factor or character.
#' @param Measurement a column inside the dataframe that will be plotted on the y axis. Traditionally this is some measure such as a percentage.  Currently the function accepts a column of type integer or numeric.
#' @param Grouping a column inside the dataframe that will be used to group and distinguish measurements.
#' @param Title Optionally the title to be displayed. Title = NULL will remove it entirely. Title = "" will provide and empty title but retain the sapcing.
#' @param SubTitle Optionally the sub-title to be displayed.  SubTitle = NULL will remove it entirely. SubTitle = "" will provide and empty title but retain the sapcing.
#' @param Caption Optionally the caption to be displayed. Caption = NULL will remove it entirely. Caption = "" will provide and empty title but retain the sapcing.
#' @param XTextSize Optionally the font size for the X axis labels to be displayed. XTextSize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param YTextSize Optionally the font size for the Y axis labels to be displayed. YTextSize = 3 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param TitleTextSize Optionally the font size for the Title to be displayed. TitleTextSize = 14 is the default must be a numeric.
#' @param SubTitleTextSize Optionally the font size for the SubTitle to be displayed. SubTitleTextSize = 10 is the default must be a numeric.
#' @param CaptionTextSize Optionally the font size for the Caption to be displayed. CaptionTextSize = 8 is the default must be a numeric.
#' @param LineThickness Optionally the thickness of the plotted lines. LineThickness = 1 is the default must be a numeric.
#' @param DataTextSize Optionally the font size of the plotted data points. DataTextSize = 2.5 is the default must be a numeric.
#' @param LineColor Optionally the color of the plotted lines. By default it will use the ggplot2 color palette for coloring by group. The user may override with one valid color of their choice e.g. "black" must be character.
#' 
#' 
#' @return a plot of type ggplot to the default plot device
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise %>% n
#' @importFrom ggrepel geom_text_repel
#'
#' @author Chuck Powell
#' @seealso \code{\link{newcancer}}
#' @examples
#'
#' ggslopegraph2(newcancer, Year, Survival, Type)
#' ggslopegraph2(newcancer, Year, Survival, Type, Title = "Estimates of Percent Survival Rates")
#' ggslopegraph2(newcancer, Year, Survival, Type, 
#'                 Title = "Estimates of Percent Survival Rates", 
#'                 LineColor = "black", 
#'                 LineThickness = 1, 
#'                 SubTitle = NULL, 
#'                 Caption = NULL)
#'
ggslopegraph2 <- function(dataframe, Times, Measurement, Grouping,
                            Title = "No title given",
                            SubTitle = "No subtitle given",
                            Caption = "No caption given",
                            XTextSize = 12,
                            YTextSize = 3,
                            TitleTextSize = 14,
                            SubTitleTextSize = 10,
                            CaptionTextSize = 8,
                            LineThickness = 1,
                            LineColor = "ByGroup",
                            DataTextSize = 2.5)
  {
  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
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
    theme(axis.text.x.top      = element_text(size = XTextSize)), # and increase font size
    theme(axis.ticks       = element_blank()), # Remove x & y tick marks
    theme(plot.title       = element_text(size = TitleTextSize, face = "bold")), # Format title
    theme(plot.title       = element_text(hjust = 0.5)), # Center title & subtitle
    theme(plot.subtitle    = element_text(hjust = 0.5, size = SubTitleTextSize)),
    theme(plot.caption     = element_text(size = CaptionTextSize))
  )
  # for convenience store these
  Ndataframe <- deparse(substitute(dataframe)) # name of dataframe
  NTimes <- deparse(substitute(Times)) # name of Times variable
  NMeasurement <- deparse(substitute(Measurement)) # name of Measurement variable
  NGrouping <- deparse(substitute(Grouping)) # name of Grouping variable
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
  if (!NTimes %in% names(dataframe)) {
    stop(paste0("'", NTimes, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (!NMeasurement %in% names(dataframe)) {
    stop(paste0("'", NMeasurement, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (!deparse(substitute(Grouping)) %in% names(dataframe)) {
    stop(paste0("'", deparse(substitute(Grouping)), "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (!class(dataframe[[NMeasurement]]) %in% c("integer","numeric")) {
    stop(paste0("Sorry I need the measured variable '", NMeasurement, "' to be a number"), call. = FALSE)
  }
  if (!"ordered" %in% class(dataframe[[NTimes]])) { # keep checking
    if (!"character" %in% class(dataframe[[NTimes]])) { # keep checking
      if ("factor" %in% class(dataframe[[NTimes]])) { # impose order
        warning("Converting to an ordered factor", call. = FALSE)
        dataframe[[NTimes]] <- factor(dataframe[[NTimes]], ordered = TRUE)
      } else {
        stop(paste0("Sorry I need the variable '", NTimes, "' to be of class character, factor or ordered"), call. = FALSE)
      }
    }
  }
  
  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)

  if (LineColor != "ByGroup" ) {
    LineGeom <- list(geom_line(aes_(), size = LineThickness, color = LineColor))
  } else {
    LineGeom <- list(geom_line(aes_(color = Grouping, alpha = 1), size = LineThickness))
  }

    dataframe %>%
      filter(!is.na(!! Times), !is.na(!! Measurement), !is.na(!! Grouping))  %>%
#      mutate(!!quo_name(Times) := factor(!!Times), !!quo_name(Measurement) := factor(!!Measurement)) %>%
      ggplot(aes_(group=Grouping, y=Measurement, x=Times)) +
      LineGeom +
#      geom_line(aes_(), size = LineThickness, color = "black") +
#      geom_line(aes_(color = Grouping, alpha = 1), size = LineThickness) +
      geom_text_repel(data = dataframe %>% filter(!! Times == min(!! Times)),
                        aes_(label = Grouping) ,
                        hjust = "left",
                        fontface = "bold",
                        size = YTextSize,
                        nudge_x = -.45,
                        direction = "y") +
        geom_text_repel(data = dataframe %>% filter(!! Times == max(!! Times)),
                        aes_(label = Grouping),
                        hjust = "right",
                        fontface = "bold",
                        size = YTextSize,
                        nudge_x = .5,
                        direction = "y") +
        geom_label(aes_(label = Measurement), size = DataTextSize, label.padding = unit(0.05, "lines"), label.size = 0.0) +
        MySpecial +
        labs(
              title = Title,
              subtitle = SubTitle,
              caption = Caption
            )
} # end of function
# title = "Estimates of Percent Survival Rates"
# subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176."
# caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk"
