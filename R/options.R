# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Make a scale object in the format required for \code{Chart.js}
make_scale <- function(id, pos = c("left", "right"), display = TRUE, grid = TRUE) {
  list(
    id = id, position = pos[1], display = display,
    gridLines = list(drawOnChartArea = grid),
    type = "linear"
  )
}




#' @export
set_padding <- function(chart, all = NULL, left = all, right = all, top = all, bottom = all) {
  chart$x$options$layout$padding <- non_null(list(left = left, right = right, top = top, bottom = bottom))
  return(chart)
}




#' @export
set_legend <- function(
  chart, ..., display = TRUE,
  position = c("top", "left", "bottom", "right"),
  align = c("center", "start", "end"),
  fullWidth = TRUE, onClick = NULL, onHover = NULL, onLeave = NULL,
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




#' @export
alter_axis <- function(chart, xOrY = c("x", "y"), id = 1, ..., type, min, max, label) {
  
  # Parse the function call & ensure the legend element exists in the chart options
  funCall <- match.call()
  if (tolower(xOrY[1]) == "x") {
    axis <- "xAxes"
    if (is.null(chart$x$options$scales$xAxes)) chart$x$options$scales$xAxes <- list(list())
  } else {
    axis <- "yAxes"
    if (is.null(chart$x$options$scales$yAxes)) chart$x$options$scales$yAxes <- list(list())
  }
  
  # 
  if (!missing(min)) chart$x$options$scales[[axis]][[id]]$ticks$min <- min
  if (!missing(max)) chart$x$options$scales[[axis]][[id]]$ticks$max <- max
  
  # Add each passed argument to the chart legend, checking that arguments are valid
  as.list(funCall)[-(1:3)] %>% .[! names(.) %in% c("id", "min", "max")] %>% purrr::iwalk(~ {
    chart$x$options$scales[[axis]][[.y]] <<- .x
  })
  
  # Return the modified chart object
  return(chart)
  
}
