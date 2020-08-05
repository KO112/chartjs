# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Remove NULLs from a List
#'
#' @param x The vector/list to remove NULL elements from.
#'
#' @return A filtered vector/list.
#'
#' @examples
#' non_null(1:10)
#' non_null(list(1, "a", NULL, 10L))
#' 
non_null <- function(x) {
  Filter(Negate(is.null), x)
}




#' Guess Chart Label
#' 
#' Deparse some input code to guess a good label for a chart.
#'
#' @param x The input code (passed through \code{substitute} first) to deparse.
#'
#' @return A guess for an appropriate label to use (character scalar).
guess_label <- function(x) {
  deparse(x) %>% gsub("^~", "", .) %>% paste0(collapse = "")
}




#' Evaluate a Data Expression
#' 
#' Evaluate some data expression in the proper context.
#' 
#' \code{x} can be either a:
#' 
#' - Formula: will be evaluated as an expression inside of \code{data}.
#' - Function: will be evaluated as-is, so must return the desired object in any context.
#' - Vector: will be used as-is.
#' - Character Scalar: will be used as the name of a column in the data.
#'
#' @param data The raw data object that \code{x} will be evaluated inside of.
#' @param x The data expression to evaluate.
#'
#' @return A vector.
#'
#' @examples
#' print(eval_data(mtcars, ~ mpg))
#' print(eval_data(x = function() mtcars$mpg))
#' print(eval_data(mtcars, mtcars$mpg))
#' print(eval_data(mtcars, "mpg"))
#' 
eval_data <- function(data, x) {
  
  # Evaluate formulas inside the data
  labels <- if (class(x) == "formula") {
    environment(x) <- .GlobalEnv
    lazyeval::f_eval(x, data)
    
  # Evaluate functions
  } else if (is.function(x)) {
    x()
    
  # Directly use a vector of the proper length
  } else if (is.vector(x) && (length(x) == nrow(data))) {
    x
    
  # Search in the data for column names
  } else if (is.character(x) && (length(x) == 1)) {
    if (! x %in% colnames(data)) stop("There is no column named '", x, "' in the chart data.")
    data[[x]]
    
  # Else throw an error
  } else {
    stop(
      "x/y must either be a formula to be evaluated within the chart data,",
      "\n\ta function that evaluates to a vector,",
      "\n\ta vector that has the same length as the number of rows in the chart data,",
      "\n\tor the name of a column in the chart data."
    )
  }
  
} 





#' Check/Validate Arguments
#' 
#' Check that the arguments passed for a parameter are in a set of valid values,
#'   and throw a warning & return a default value if validation fails.
#' 
#' This is an internal function meant to be called unconditionally inside a loop.
#' It will only perform validation if \code{arg == name}.
#'
#' @param arg The parameter name to validate values for (character scalar).
#' @param name The name of the current loop variable (character scalar).
#' @param val The value of the current loop variable.
#' @param validVals A vector/list of the valid values that the parameter can take.
#' @param silent Whether to silence the warning that usually appears when validation fails (boolean scalar).
#'
#' @return If \code{! val \%in\% validVals}, \code{validVals[1]} will be returned, else \code{val}.
#'
#' @examples
#' # Valid value, "apple" is returned
#' check_arg("fruit", "fruit", "apple", c("apple", "banana", "cherry"))
#' 
#' # Invalid value, "banana" is returned
#' check_arg("fruit", "fruit", "apple", c("banana", "cherry"), silent = TRUE)
#' 
#' # Throws a warning
#' \dontrun{
#'   check_arg("fruit", "fruit", "apple", c("banana", "cherry"))
#' }
#' 
check_arg <- function(arg, name, val, validVals, silent = FALSE) {
  if ((arg == name) && (! val %in% validVals)) {
    if (!silent) warning(immediate. = TRUE,
      glue::glue("'{val}' is not a valid setting for '{name}'. '{validVals[1]}' will be used instead.")
    )
    return(validVals[1])
  }
  return(val)
}
