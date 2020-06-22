# Base options
baseOptions <- list(
  list(
    tooltips = list(
      mode = "index",
      intersect = FALSE
    )
  )
)


# Save the data for internal package use
usethis::use_data(
  internal = TRUE, overwrite = TRUE,
  baseOptions
)
