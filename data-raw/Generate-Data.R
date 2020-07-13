# Download chart.js files (need to update version number)
dir.create("./inst/htmlwidgets/lib/Chart.js/2.9.3", recursive = TRUE)
download.file(
  url = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.bundle.min.js",
  destfile = "./inst/htmlwidgets/lib/Chart.js/2.9.3/Chart.bundle.min.js"
)
download.file(
  url = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.css",
  destfile = "./inst/htmlwidgets/lib/chart.js/2.9.3/Chart.css"
)
download.file(
  url = "https://www.chartjs.org/samples/latest/utils.js",
  destfile = "./inst/htmlwidgets/lib/chart.js/2.9.3/utils.js"
)


# Base options
baseOptions <- list(
  tooltips = list(
    mode = "index",
    intersect = FALSE
  ),
  legend = list(
    labels = list(
      filter = JS("
        function (legendItem, chartData) {
          return (chartData.datasets[legendItem.datasetIndex].legend !== false)
        }
      ")
    )
  )
)


# Save the data for internal package use
usethis::use_data(
  internal = TRUE, overwrite = TRUE,
  baseOptions
)
