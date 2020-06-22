# Allow convenient use of functions from other packages
#' @include utils.R
NULL


# Download chart.js files (need to update version number)
# dir.create("./inst/htmlwidgets/lib/Chart.js/2.9.3", recursive = TRUE)
# download.file(
#   url = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.bundle.min.js",
#   destfile = "./inst/htmlwidgets/lib/Chart.js/2.9.3/Chart.bundle.min.js"
# )
# download.file(
#   url = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.css",
#   destfile = "./inst/htmlwidgets/lib/chart.js/2.9.3/Chart.css"
# )
# download.file(
#   url = "https://www.chartjs.org/samples/latest/utils.js",
#   destfile = "./inst/htmlwidgets/lib/chart.js/2.9.3/utils.js"
# )




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
#' 
#' @examples
#' chartjs(mtcars, x = ~ mpg) %>% new_bars(y = ~ disp)
#' 
chartjs <- function(
  data, x, type = "bar",
  options = baseOptions,
  forceBig = FALSE, width = "100%", height = "100%", elementId = NULL
) {
  
  
  # Show a warning if the data is too big, unless told not to
  if (!forceBig && ((object.size(data) > 1e6) || (nrow(data) > 1e4))) {
    stop(
      "The data passed is very large (either > 1 MB, or > 10,000 rows).",
      "\n\t This will very likely cause major slowdowns and unnecessary bloat.",
      "\n\t The data should be summarized first to avoid this error.",
      "\n\t Alternatively, set `forceBig = TRUE` (may crash your RStudio session)."
    )
  }
  
  
  # Forward options using x
  environment(x) <- .GlobalEnv
  x <- list(
    type = type,
    rawData = data,
    data = list(
      labels = lazyeval::f_eval(x, data),
      datasets = list()
    ),
    options = options
  )
  
  
  # Create widget
  htmlwidgets::createWidget(
    name = "chartjs",
    x = x,
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




#' Make a dataset in the format required for \code{Chart.js}
  make_dataset <- function(
  data, y, label = NULL, type, ...,
  yAxis = NULL, xAxis = NULL, order = NULL,
  fill = (type != "line"), bgCol = NULL,
  brdCol = NULL, brdWidth = NULL,
  radius = NULL, hoverRadius = NULL
) {
  list(
    label = label, data = lazyeval::f_eval(y, data), type = type, ...,
    yAxisID = yAxis, xAxisID = xAxis, order = order,
    fill = fill, backgroundColor = bgCol,
    borderColor = brdCol, borderWidth = brdWidth,
    pointRadius = radius, pointHoverRadius = hoverRadius
  ) %>% non_null()
}




#' Make a scale object in the format required for \code{Chart.js}
make_scale <- function(id, pos = c("left", "right"), display = TRUE, grid = TRUE) {
  list(
    id = id, position = pos[1], display = display,
    gridLines = list(drawOnChartArea = grid),
    type = "linear"
  )
}




#' @export
new_trace <- function(chart, y, label = guess_label(substitute(y)), type, ...) { # data = p$x$rawData
  args <- list(...)
  chart$x$data$datasets %<>%
    append(list(make_dataset(data = chart$x$rawData, y = y, label = label, type = type, ...)))
  if ("yAxis" %in% names(args))
    chart$x$options$scales %<>% append(list(make_scale(id = yAxis)))
  return(chart)
}


#' @export
new_bars <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "bar", ...)
}


#' @export
new_lines <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "line", ...)
}


#' @export
new_scatter <- function(chart, y, label = guess_label(substitute(y)), ...) {
  new_trace(chart = chart, y = y, label = label, type = "scatter", ...)
}




#' Deparse some input code to guess a good label for a chart
guess_label <- function(x) {
  deparse(x) %>% gsub("^~", "", .) %>% paste0(collapse = "")
}




#' @export
set_padding <- function(chart, left = NULL, right = NULL, top = NULL, bottom = NULL) {
  chart$x$layout$padding <- non_null(list(left = left, right = right, top = top, bottom = bottom))
}




#' 
f <- function() {
  
  # 
  devtools::load_all("~/GitHub/chartjs")
  
  # 
  p <- chartjs(mtcars[order(mtcars$mpg), ], x = ~ as.character(1:32), width = "75%") %>%
    new_bars(y = ~ disp) %>%
    new_lines(y = ~ wt * 100, bgCol = "#0F0", brdCol = "#F00", radius = 0) %>%
    print()
  
  # 
  p <- ggplot2::diamonds %>%
    dplyr::mutate(cut = cut(carat, 50)) %>%
    dplyr::group_by(cut) %>%
    dplyr::summarize(price = mean(price), x = mean(y)) %>%
    chartjs(x = ~ cut, width = "75%") %>%
    new_bars(y = ~ price) %>%
    new_lines(~ x * mean(price) / mean(x)) %>%
    print()
  
  # 
  htmltools::html_print(purrr::map(1:5, ~ p), background = "#FFF")
  htmltools::save_html(purrr::map(1:100, ~ p), file = "~/Downloads/Tag-List.html", background = "#FFF")
  
  # 
  purrr::map(1:100, ~ {
    dplyr::tibble(Index = 1:100, Value = runif(100) %>% sort()) %>%
      chartjs(~ Index, width = "49.5%") %>%
      new_lines(~ Value, bgCol = "#F00", brdCol = "#F00")
  }) %>%
    htmltools::tagList(., htmltools::tags$style(".chartjs { display: inline-block; }")) %T>%
    htmltools::html_print(.) %>%
    htmltools::save_html(., file = "~/Downloads/Random-Charts.html")
  
  # 
  devtools::load_all("~/Projects/rsavis")
  compData <- readRDS("~/Projects/Competitiveness/Home/Data/Home_Raw_Model_Data.rds")
  sumData <- purrr::map(colnames(compData), ~ {
    feat <- vecData(compData[[.x]])
    compData[, c("Converted", .x), with = FALSE] %>%
      .[, `:=`(Cut = band_feat(feat = feat))] %>%
      .[, .(
        Label = as.character(Cut) %>% strsplit(",") %>% {
          valRange <- purrr::map_chr(., ~ gsub("[][()]", "", .)[1])
          return(if (feat$isNumericIsh) as.numeric(valRange) else valRange)
        },
        Converted = mean(Converted)
      ), keyby = Cut]
  }) %>% .[purrr::map_int(., ~ sum(is.na(.$Label))) == 0]
  
  # 
  purrr::map(sumData, ~ chartjs(.x, x = ~ Label, width = "75%") %>% new_bars(y = ~ Converted, bgCol = "#0C0")) %T>%
    htmltools::html_print(.) %>%
    htmltools::save_html(., file = "~/Downloads/Model-Plots.html")
  
}
