# Allow convenient use of functions from other packages
#' @include utils.R
NULL




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




#' Alter an Axis
#' 
#' @param pos One of: c("left", "right", "bottom", "top")
#' @export
alter_axis <- function(
  chart, id, xOrY = tolower(substr(id, 1, 1)), ...,
  pos, display = TRUE, grid = TRUE, type = "linear",
  title, label, percent = FALSE, digits = 2L,
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
        // return (value / this.max * 100).toFixed({digits}) + '%';
        return (value * 100).toFixed({digits}) + '%';
      }}
    "))
  }
  
  
  # Set the title/position/grid status of the axis?
  if (!missing(title)) {
    axis$scaleLabel$display <- TRUE
    axis$scaleLabel$labelString <- title
  }
  if (!missing(pos)) axis$position <- pos
  if (!missing(grid)) axis$grid <- list(drawOnChartArea = grid)
  
  # Set the min/max of the axis?
  if (!missing(min)) axis$ticks$min <- min
  if (!missing(max)) axis$ticks$max <- max
  if (!missing(suggestedMin)) axis$ticks$suggestedMin <- suggestedMin
  if (!missing(suggestedMax)) axis$ticks$suggestedMax <- suggestedMax
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "pos", "grid", "percent", "min", "max", "suggestedMin", "suggestedMax")] %>%
    # .[! names(.) %in% (args(alter_axis) %>% as.list() %>% names() %>% setdiff(c("", "...", "label", "digits")))] %>%
    purrr::iwalk(~ {axis[[.y]] <<- .x})
  
  
  # Return the modified chart object
  scales[[axisGroup]][[id]] <- axis
  chart$x$options$scales <- scales
  return(chart)
  
  
}




#' @export
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




#' Modify an Axis Title
#' 
#' See: https://www.chartjs.org/docs/latest/axes/labelling.html
#'
#' @param chart A \code{chartjs} object.
#' @param id The ID of the axis to add a title to (character scalar).
#' @param xOrY Whether the axis is an x or y-axis (character scalar).
#' @param ... Additional options to be used in the axis title (has no effect here).
#' @param title The title of the axis (character scalar).
#' @param display Whether to display the axis title (boolean scalar).
#' @param lineHeight The height of an individual line of text
#'   (see \url{https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}) (character/numeric scalar).
#' @param fontColor The color of the title font (character scalar).
#' @param fontSize The size of the title font (numeric scalar).
#' @param fontStyle Space-separated combination of values from
#'   [normal, italic, oblique, initial, inherit] (character scalar).
#' @param fontFamily The font family of the title text (character scalar).
#' @param padding The padding to use around the axis title (numeric scalar).
#' 
#' @return A modified \code{chartjs} object.
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
  scaleLabel <- chart$x$options$scales[[axisGroup]][[id]]$scaleLabel
  if (is.null(scaleLabel)) scaleLabel <- list()
  
  # Add other passed arguments to the axis
  as.list(funCall)[-1] %>%
    .[! names(.) %in% c("chart", "id", "xOrY", "title")] %>%
    purrr::iwalk(~ {scaleLabel[[.y]] <<- .x})
  
  # Return the modified chart object
  chart$x$options$scales[[axisGroup]][[id]]$scaleLabel <- scaleLabel
  return(chart)
  
}
