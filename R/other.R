# Allow convenient use of functions from other packages
#' @include utils.R
NULL




#' Deparse some input code to guess a good label for a chart
guess_label <- function(x) {
  deparse(x) %>% gsub("^~", "", .) %>% paste0(collapse = "")
}




#' Evaluate the data in the proper context
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
      "'x'/'y' must either be a formula to be evaluated within the chart data,",
      "\n\ta function that evaluates to a vector,",
      "\n\ta vector that has the same length as the number of rows in the chart data,",
      "\n\tor the name of a column in the chart data."
    )
  }
  
} 
