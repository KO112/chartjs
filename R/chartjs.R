# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Create a \code{Chart.js} Plot Object
#' 
#' This is the starting function for creating a \code{charts} object in \code{R}.
#' It operates similarly to \code{plotly::plot_ly}.
#' 
#' See here for \code{Chart.js} examples: \url{https://www.chartjs.org/}.
#' 
#' And see here for the official \code{Chart.js} library documentation:
#'   \code{https://www.chartjs.org/docs/latest/}.
#' 
#' @param data The data that the plot is based on (a \code{data.frame}-like object).
#' @param x An expression that evaluates to a vector that will be used as the x-axis values
#'   (see \code{x} in \code{eval_data}).
#' @param type The type of trace (character scalar, one of \code{c("bar", "line", "scatter", "horizontalBar")}).
#' @param options A list containing the options to start with.
#' @param width/height The width/height of the \code{htmlwidget} container.
#' @param elementId An ID string for the widget (will be randomly assigned) (character scalar).
#' 
#' @return  A \code{chartjs} \code{htmlwidget} object.
#' @export
#' 
#' @examples
#' chartjs(mtcars, x = 1:32) %>% new_bars(y = ~ disp)
#' 
chartjs <- function(
  data, x, type = "bar", options = baseOptions,
  forceBig = FALSE, width = "100%", height = "100%", elementId = NULL
) {
  
  
  # Throw an error if the data is too big, unless told to ignore the size
  if (!forceBig && ((object.size(data) > 1e6) || (nrow(data) > 1e4))) {
    stop(
      "The data passed is very large (either > 1 MB, or > 10,000 rows).",
      "\n\t This will very likely cause major slowdowns and unnecessary bloat.",
      "\n\t The data should be summarized first to avoid this error.",
      "\n\t Alternatively, set `forceBig = TRUE` (may crash your RStudio session)."
    )
  }
  
  
  # Forward options using x
  message <- list(
    type = type,
    rawData = data,
    data = list(
      labels = eval_data(data, x),
      datasets = list()
    ),
    options = options
  )
  
  
  # Update the height 
  if (! grepl("%", height)) message$options$maintainAspectRatio <- FALSE
  
  
  # Create widget
  htmlwidgets::createWidget(
    name = "chartjs",
    x = message,
    width = width,
    height = height,
    package = "chartjs",
    elementId = elementId,
    
    # Before passing the data to JavaScript, select only the type/data/options objects, & remove axis names
    preRenderHook = function(widget) {
      widget$x %<>% .[c("type", "data", "options")]
      widget$x$options$scales$xAxes %<>% {if (!is.null(.)) unname(.)}
      widget$x$options$scales$yAxes %<>% {if (!is.null(.)) unname(.)}
      return(widget)
    }
    
  )
  
  
}




#' Check that an Object is a \code{Chart.js} Plot Object
#'
#' @param x The object to check (any R object).
#'
#' @return Whether the object inherits from \code{chartjs}.
#' @export
#'
#' @examples
#' is.chartjs(mtcars)                     # FALSE
#' is.chartjs(chartjs(mtcars, x = ~ mpg)) # TRUE
#' 
is.chartjs <- function(x) {
  inherits(x, "chartjs")
}
