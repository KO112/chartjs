# Import pipe operators for use inside the package
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
NULL


# Export JS
#' @importFrom htmlwidgets JS
#' @export
htmlwidgets::JS


#' Remove NULLs
non_null <- function(x) {
  Filter(Negate(is.null), x)
}
