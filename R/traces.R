# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Make a dataset in the format required for \code{Chart.js}
make_dataset <- function(
  data, y, label = NULL, type, ...,
  yAxis = NULL, xAxis = NULL, order = NULL,
  fill = (type != "line"), bgCol = NULL,
  brdCol = NULL, brdWidth = NULL,
  radius = NULL, hoverRadius = NULL
) {
  non_null(list(
    label = label, data = eval_data(data, y), type = type, ...,
    yAxisID = yAxis, xAxisID = xAxis, order = order,
    fill = fill, backgroundColor = bgCol,
    borderColor = brdCol, borderWidth = brdWidth,
    pointRadius = radius, pointHoverRadius = hoverRadius
  ))
}




#' @export
new_trace <- function(chart, y, label = guess_label(substitute(y)), type, ...) {
  args <- list(...)
  chart$x$data$datasets %<>%
    append(list(make_dataset(data = chart$x$rawData, y = y, label = label, type = type, ...)))
  if ("yAxis" %in% names(args))
    chart$x$options$scales %<>% append(list(make_scale(id = yAxis)))
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
