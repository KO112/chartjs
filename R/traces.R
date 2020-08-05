# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Add a New Trace to a \code{chartjs} Plot
#' 
#' - \code{make_dataset} is an internal function used to make a dataset object
#'   in the format required for \code{chartjs} objects.
#' - \code{new_trace} is the general function used to add a new trace to a \code{chartjs} object.
#' - \code{new_bars}/\code{new_lines}/\code{new_scatter} are helper functions
#'   for \code{new_trace} where \code{type = "bars"/"lines"/"scatter"}, respectively.
#' 
#' To see the various ways that \code{fill} can be used,
#'   look at the "Area charts" examples on this page:
#'   \url{https://www.chartjs.org/samples/latest/}.
#' 
#' For more information on \code{barPerc}/\code{catPerc}, see here:
#'   \url{https://www.chartjs.org/docs/latest/charts/bar.html#barpercentage-vs-categorypercentage}.
#'
#' @param data The raw data object that \code{y} will be evaluated inside of.
#' @param y An expression that evaluates to a vector (see \code{x} in \code{eval_data}).
#' @param label The label to use for the trace (character scalar).
#' @param type The type of trace (character scalar, one of \code{c("bar", "line", "scatter")}).
#' @param ... Additional named arguments to add to the data.
#' @param xAxis,yAxis The name of the x/y-axis to add the trace to (character scalar).
#' @param order Controls the order that the traces are drawn in
#'   (0 is the default, lower numbers go on top, higher on bottom) (integer scalar).
#' @param legend Whether to show the trace in the legend (boolean scalar).
#' @param fill Whether to fill the trace in (see more in the details section).
#' @param bgCol,brdCol/brdWidth The color/width of the background/border of the trace (character scalar).
#' @param radius,hoverRadius The radius of the points/radius when hovered over (numeric scalar).
#' @param tension The tension of the lines between the points (0 for straight lines, 0.4 is default) (numeric scalar).
#' @param stepped Whether to draw the lines as stepped, or how to draw them as stepped (boolean/character scalar).
#' @param barPerc Fraction of the available width each bar should be
#'   within the category width (numeric scalar in \code{[0, 1]}).
#' @param catPerc Fraction of the available width each category should be
#'   within the sample width  (numeric scalar in \code{[0, 1]}).
#' 
#' @return \code{make_dataset}: a dataset object in the format required for \code{chartjs} objects.
#' @name traces
#' 
make_dataset <- function(
  data, y, label = NULL, type, ...,
  xAxis = NULL, yAxis = NULL, order = NULL, legend = NULL,
  fill = (type != "line"), bgCol = NULL,
  brdCol = bgCol, brdWidth = NULL,
  radius = NULL, hoverRadius = NULL, tension = NULL,
  stepped = list(NULL, FALSE, TRUE, "before", "after", "middle")[[1]],
  barPerc = NULL, catPerc = NULL
) {
  non_null(list(
    label = label, data = eval_data(data, y), type = type, ...,
    yAxisID = yAxis, xAxisID = xAxis, order = order, legend = legend,
    fill = fill, backgroundColor = bgCol,
    borderColor = brdCol, borderWidth = brdWidth,
    pointRadius = radius, pointHoverRadius = hoverRadius,
    lineTension = tension, steppedLine = stepped,
    barPercentage = barPerc, categoryPercentage = catPerc
  ))
}




#' @param chart A \code{chartjs} object.
#' 
#' @return \code{new_trace}: a modified \code{chartjs} object.
#' @rdname traces
#' @export
#' 
#' @examples
#' # Individual traces
#' chartjs(mtcars, x = 1:32) %>% new_trace(y = ~ mpg, type = "bar")
#' chartjs(mtcars, x = 1:32) %>% new_bars(y = ~ mpg)
#' chartjs(mtcars, x = 1:32) %>% new_lines(y = ~ mpg)
#' chartjs(mtcars, x = 1:32) %>% new_scatter(y = ~ mpg)
#' 
#' # Add multiple traces to the same chart
#' chartjs(mtcars, x = 1:32) %>% new_bars(y = ~ cyl) %>%
#'   new_lines(y = ~ mpg) %>% new_scatter(y = ~ qsec)
#' 
new_trace <- function(chart, y, label = guess_label(substitute(y)), yAxis = "y", type, ...) {
  args <- list(...)
  chart$x$data$datasets %<>%
    append(list(make_dataset(data = chart$x$rawData, y = y, label = label, type = type, yAxis = yAxis, ...)))
  chart <- alter_axis(chart = chart, id = yAxis, xOrY = "y")
  return(chart)
}


#' @rdname traces
#' @export
new_bars <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "bar", ...)
}


#' @rdname traces
#' @export
new_lines <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "line", ...)
}


#' @rdname traces
#' @export
new_scatter <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "scatter", ...)
}
