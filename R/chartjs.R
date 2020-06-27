# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Create a \code{Chart.js} Plot
#' 
#' @param data 
#' @param x 
#' @param type 
#' @param options 
#' @param width 
#' @param height 
#' @param elementId 
#' 
#' @return
#' @export
#' 
#' @examples
#' chartjs(mtcars, x = ~ mpg) %>% new_bars(y = ~ disp)
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
    preRenderHook = function(widget) {
      widget$x %<>% .[c("type", "data", "options")]
      return(widget)
    }
  )
  
  
}
