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


#' Check arguments
check_arg <- function(arg, name, val, validVals) {
  if ((arg == name) && (! val %in% validVals)) {
    warning(immediate. = TRUE,
      glue::glue("'{val}' is not a valid setting for '{name}'. '{validVals[1]}' will be used instead.")
    )
    return(validVals[1])
  }
  return(val)
}
