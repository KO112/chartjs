#' Shiny Bindings for chartjs
#'
#' Output and render functions for using chartjs within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId Output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended
#' @param expr An expression that generates a chartjs
#' @param env The environment in which to evaluate \code{expr}
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})?
#'   This is useful if you want to save an expression in a variable.
#'
#' @name chartjs-shiny
#'
#' @export
chartjsOutput <- function(outputId, width = "100%", height = "400px"){
  htmlwidgets::shinyWidgetOutput(outputId, "chartjs", width, height, package = "KO")
}


#' @rdname chartjs-shiny
#' @export
renderChartjs <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, chartjsOutput, env, quoted = TRUE)
}
