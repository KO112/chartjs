# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Make a dataset in the format required for \code{Chart.js}
make_dataset <- function(
  data, y, label = NULL, type, ...,
  yAxis = NULL, xAxis = NULL, order = NULL, legend = NULL,
  fill = (type != "line"), bgCol = NULL,
  brdCol = bgCol, brdWidth = NULL,
  radius = NULL, hoverRadius = NULL,
  barPerc = NULL, catPerc = NULL
) {
  non_null(list(
    label = label, data = eval_data(data, y), type = type, ...,
    yAxisID = yAxis, xAxisID = xAxis, order = order, legend = legend,
    fill = fill, backgroundColor = bgCol,
    borderColor = brdCol, borderWidth = brdWidth,
    pointRadius = radius, pointHoverRadius = hoverRadius,
    barPercentage = barPerc, categoryPercentage = catPerc
  ))
}




#' @export
new_trace <- function(chart, y, label = guess_label(substitute(y)), yAxis = "y1", type, ...) {
  args <- list(...)
  chart$x$data$datasets %<>%
    append(list(make_dataset(data = chart$x$rawData, y = y, label = label, type = type, yAxis = yAxis, ...)))
  chart <- alter_axis(chart = chart, id = yAxis, xOrY = "y")
  return(chart)
}


#' @export
new_bars <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "bar", ...)
}


#' @export
new_lines <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "line", ...)
}


#' @export
new_scatter <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "scatter", ...)
}
