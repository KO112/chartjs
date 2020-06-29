# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Make a scale object in the format required for \code{Chart.js}
make_scale <- function(id, pos = c("left", "right", "bottom", "top"), display = TRUE, grid = TRUE) {
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
alter_axis <- function(
  chart, id, xOrY = tolower(substr(id, 1, 1)), ...,
  pos = c("left", "right", "bottom", "top"), display = TRUE, grid = TRUE,
  type, label, percent = FALSE, digits = 2L,
  min, max, suggestedMin, suggestedMax
) {
  
  
  # Extract the scales object from the chart (fine if it doesn't exist),
  #   parse the function call, & create a new axis (may not be used)
  scales <- chart$x$options$scales
  funCall <- match.call()
  newAxis <- make_scale(id = id, pos = pos, display = display, grid = grid)
  
  # Ensure the legend element exists in the chart options
  if (tolower(xOrY[1]) == "x") {
    axisName <- "xAxes"
    if (is.null(scales$xAxes) || (length(scales$xAxes) == 0)) scales$xAxes <- list()
  } else {
    axisName <- "yAxes"
    if (is.null(scales$yAxes) || (length(scales$yAxes) == 0)) scales$yAxes <- list()
  }
  
  # Extract the desired axis object
  axis <- scales[[axisName]][[id]]
  if (is.null(axis)) axis <- newAxis
  
  
  # Format the axis as a percent?
  if (percent) {
    if (missing(suggestedMin)) suggestedMin <- 0
    axis$ticks$callback <- JS(glue::glue("
      function(value) {{
        // return (value / this.max * 100).toFixed({digits}) + '%';
        return (value * 100).toFixed({digits}) + '%';
      }}
    "))
    # axis$scaleLabel <- list(display = TRUE, labelString = "Percentage")
  }
  
  
  # Set the position/min/max of the axis?
  if (!missing(pos)) axis$position <- pos
  if (!missing(min)) axis$ticks$min <- min
  if (!missing(max)) axis$ticks$max <- max
  if (!missing(suggestedMin)) axis$ticks$suggestedMin <- suggestedMin
  if (!missing(suggestedMax)) axis$ticks$suggestedMax <- suggestedMax
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "pos", "display", "grid", "percent", "min", "max", "suggestedMin", "suggestedMax")] %>%
    purrr::iwalk(~ {axis[[.y]] <<- .x})
  
  
  # Return the modified chart object
  scales[[axisName]][[id]] <- axis
  chart$x$options$scales <- scales
  return(chart)
  
  
}




#' @export
alter_options <- function(
  chart, ..., animDur = 400, hoverDelay = 0, resizeAnimDur = 0, tension = 0.4
) {
  
  # Extract the options object from the chart (fine if it doesn't exist), & parse the function call
  opts <- chart$x$options
  funCall <- match.call()
  
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
