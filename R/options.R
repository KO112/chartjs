# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Set Chart Padding
#' 
#' Set the padding around a \code{chartjs} object
#' 
#' See here for more detailed documentation:
#'   \url{https://www.chartjs.org/docs/latest/configuration/layout.html?h=padding}.
#' 
#' @param chart A \code{chartjs} object.
#' @param all The padding to apply to each side (numeric scalar).
#' @param left,right,top,bottom The padding to apply to each individual side (overrides \code{all}) (numeric scalar).
#'
#' @return A modified \code{chartjs} object.
#' @export
#'
#' @examples
#' 
set_padding <- function(chart, all = NULL, left = all, right = all, top = all, bottom = all) {
  chart$x$options$layout$padding <- non_null(list(left = left, right = right, top = top, bottom = bottom))
  return(chart)
}




#' Alter the Chart Legend
#' 
#' See here for more detailed documentation:
#' \url{https://www.chartjs.org/docs/latest/configuration/legend.html}.
#' 
#' @param chart A \code{chartjs} object.
#' @param ... Additional named arguments to add to the legend.
#' @param position 
#' @param align 
#' @param display 
#' @param fullWidth 
#' @param onClick,onHover,onLeave 
#' @param reverse 
#' @param labels 
#' @param rtl 
#' @param textDirection 
#'
#' @return A modified \code{chartjs} object.
#' @export
#'
#' @examples
#' 
alter_legend <- function(
  chart, ...,
  position = c("top", "left", "bottom", "right"),
  align = c("center", "start", "end"),
  display = TRUE, fullWidth = TRUE,
  onClick = NULL, onHover = NULL, onLeave = NULL,
  reverse = FALSE, labels = NULL, rtl = FALSE, textDirection = NULL
) {
  
  # Parse the function call & ensure the legend element exists in the chart options
  funCall <- match.call()
  if (is.null(chart$x$options$legend)) chart$x$options$legend <- list()
  
  # Add each passed argument to the chart legend, checking that arguments are valid
  as.list(funCall)[-(1:2)] %>% purrr::iwalk(~ {
    .x <- check_arg("position", .y, .x, c("top", "left", "bottom", "right"))
    .x <- check_arg("align", .y, .x, c("center", "start", "end"))
    chart$x$options$legend[[.y]] <<- .x
  })
  
  # Return the modified chart object
  return(chart)
  
}




#' Alter a Chart Axis
#' 
#' See here for more detailed documentation:
#'   \url{https://www.chartjs.org/docs/latest/axes/}.
#' 
#' @param chart A \code{chartjs} object.
#' @param id The id of the axis to alter (character scalar, should start with "x" or "y").
#' @param xOrY Whether the axis is an x-axis or y-axis object (character scalar, one of \code{c("x", "y")})..
#' @param ... Additional named arguments to add to the axis.
#' @param position One of: \code{c("left", "right", "bottom", "top")}.
#' @param display Whether to display the axis (boolean scalar).
#' @param grid Whether to draw grid lines for the axis (boolean scalar).
#' @param type The type of the axis (should keep as "linear") (character scalar).
#' @param title The title/label of the axis (character scalar).
#' @param percent Whether the axis should be displayed as a percent (boolean scalar).
#' @param decimals The number of decimal places to use for percentage axes (integer scalar).
#' @param min,max The minimum/maximum values to use for the axis (numeric/character scalar).
#' @param suggestedMin,suggestedMax The suggested minimum/maximum values to use for the axis (numeric/character scalar).
#'
#' @return A modified \code{chartjs} object.
#' @export
#'
#' @examples
#' 
alter_axis <- function(
  chart, id, xOrY = tolower(substr(id, 1, 1)), ...,
  position, display = TRUE, grid = TRUE, type = "linear",
  title, percent = FALSE, decimals = 2L,
  min, max, suggestedMin, suggestedMax
) {
  
  
  # Extract the scales object from the chart (fine if it doesn't exist), & parse the function call
  scales <- chart$x$options$scales
  funCall <- match.call()
  
  
  # Ensure the axis element exists
  if (tolower(xOrY[1]) == "x") {
    axisGroup <- "xAxes"
    if (is.null(scales$xAxes) || (length(scales$xAxes) == 0)) scales$xAxes <- list()
  } else if (tolower(xOrY[1]) == "y") {
    axisGroup <- "yAxes"
    if (is.null(scales$yAxes) || (length(scales$yAxes) == 0)) scales$yAxes <- list()
  } else {
    stop("The axis name must start with either 'x' or 'y' (or you must use the `xOrY` parameter).")
  }
  
  # Extract the desired axis object
  axis <- scales[[axisGroup]][[id]]
  if (is.null(axis)) axis <- list(id = id)
  
  
  # Format the axis as a percent?
  if (percent) {
    if (missing(suggestedMin)) suggestedMin <- 0
    axis$ticks$callback <- JS(glue::glue("
      function(value) {{
        // return (value / this.max * 100).toFixed({decimals}) + '%';
        return (value * 100).toFixed({decimals}) + '%';
      }}
    "))
  }
  
  
  # Set the title/grid status of the axis?
  if (!missing(title)) {
    axis$scaleLabel$display <- display
    axis$scaleLabel$labelString <- title
  }
  if (!missing(grid)) axis$gridLines <- list(drawOnChartArea = grid)
  
  # Set the min/max of the axis?
  if (!missing(min)) axis$ticks$min <- min
  if (!missing(max)) axis$ticks$max <- max
  if (!missing(suggestedMin)) axis$ticks$suggestedMin <- suggestedMin
  if (!missing(suggestedMax)) axis$ticks$suggestedMax <- suggestedMax
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "grid", "percent", "min", "max", "suggestedMin", "suggestedMax")] %>%
    # .[! names(.) %in% (args(alter_axis) %>% as.list() %>% names() %>% setdiff(c("", "...", "label", "decimals")))] %>%
    purrr::iwalk(~ {axis[[.y]] <<- .x})
  
  
  # Return the modified chart object
  scales[[axisGroup]][[id]] <- axis
  chart$x$options$scales <- scales
  return(chart)
  
  
}




#' Alter Chart Options
#' 
#' See here for more detailed documentation:
#'   \url{https://www.chartjs.org/docs/latest/general/options.html}.
#' 
#' @param chart A \code{chartjs} object.
#' @param ... Additional named arguments to add to the options.
#' @param animDur How long the initial plot building animation should take
#'   (in milliseconds) (numeric scalar).
#' @param hoverDelay How long to wait after hovering before showing a tooltip
#'   (in milliseconds) (numeric scalar).
#' @param resizeAnimDur How long the plot resizing animation should take
#'   (in milliseconds) (numeric scalar).
#' @param tension The default tension to use for lines between the points
#'   (0 for straight lines, 0.4 is default) (numeric scalar).
#'
#' @return A modified \code{chartjs} object.
#' @export
#'
#' @examples
#' 
alter_options <- function(
  chart, ..., animDur = 400, hoverDelay = 0, resizeAnimDur = 0, tension = 0.4
) {
  
  # Parse the function call, & extract the options object from the chart (fine if it doesn't exist)
  funCall <- match.call()
  opts <- chart$x$options
  
  # Handle some options specially
  if (!missing(animDur)) opts$animation$duration <- animDur
  if (!missing(hoverDelay)) opts$hover$animationDuration <- hoverDelay
  if (!missing(tension)) opts$elements$line$tension <- tension
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "animDur", "hoverDelay")] %>%
    purrr::iwalk(~ {chart$options[[.y]] <<- .x})
  
  # Return the modified chart object
  chart$x$options <- opts
  return(chart)
  
}




#' Modify an Axis/Chart Title
#' 
#' See here for more detailed documentation for axis titles:
#'   \url{https://www.chartjs.org/docs/latest/axes/labelling.html}.
#' 
#' See here for more detailed documentation for chart titles:
#'   \url{https://www.chartjs.org/docs/latest/configuration/title.html}.
#' 
#' @param chart A \code{chartjs} object.
#' @param id The ID of the axis to add a title to (character scalar).
#' @param xOrY Whether the axis is an x or y-axis (character scalar).
#' @param ... Additional named options to be used in the axis title (should have no effect here).
#' @param title The title of the axis/chart (character scalar).
#' @param display Whether to display the axis/chart title (boolean scalar).
#' @param lineHeight The height of an individual line of text
#'   (see \url{https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}) (character/numeric scalar).
#' @param fontColor The color of the title font (character scalar).
#' @param fontSize The size of the title font (numeric scalar).
#' @param fontStyle Space-separated combination of values from
#'   [normal, bold, italic, oblique, initial, inherit] (character scalar).
#' @param fontFamily The font family of the title text (character scalar).
#' @param padding The padding to use around the axis title (numeric scalar).
#' 
#' @return A modified \code{chartjs} object.
#' @rdname titles
#' @export
#' 
#' @examples
#' 
axis_title <- function(
  chart, id, xOrY = tolower(substr(id, 1, 1)), ...,
  title, display = TRUE, lineHeight = 1.2,
  fontColor = "#666", fontSize = 12, fontStyle = "normal",
  fontFamily = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
  padding = 4 # list(top = 4, bottom = 4)
) {
  
  # Parse the function call, & ensure the axis exists
  funCall <- match.call()
  chart <- alter_axis(chart, id, xOrY, title = title)
  
  # Set the axis group
  axisGroup <- if (tolower(xOrY[1]) == "x") "xAxes" else "yAxes"
  scaleLabel <- as.list(chart$x$options$scales[[axisGroup]][[id]]$scaleLabel)
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "title")] %>%
    purrr::iwalk(~ {scaleLabel[[.y]] <<- .x})
  
  # Return the modified chart object
  chart$x$options$scales[[axisGroup]][[id]]$scaleLabel <- scaleLabel
  return(chart)
  
}




#' @param position The position of the chart title (character scalar).
#'
#' @name titles
#' @export
#'
#' @examples
#' 
chart_title <- function(
  chart, ...,
  title, display = TRUE, lineHeight = 1.2,
  position = c("top", "left", "bottom", "right")[1],
  fontColor = "#666", fontSize = 12, fontStyle = "normal",
  fontFamily = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
  padding = 4 # list(top = 4, bottom = 4)
) {
  
  # Extract the title object
  titleObj <- as.list(chart$x$options$title)
  if (!missing(title)) {
    titleObj$display <- display
    titleObj$text <- title
  }
  
  # Add passed arguments to the title object
  as.list(match.call())[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "title", "display")] %>%
    purrr::iwalk(~ {titleObj[[.y]] <<- .x})
  
  # Return the modified chart object
  chart$x$options$title <- titleObj
  return(chart)
  
}
