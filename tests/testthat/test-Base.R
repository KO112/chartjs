context("Base")


# Test chartjs
test_that("chartjs", {
  expect_equal(
    chartjs(),
  )
})




# 
devtools::load_all("~/GitHub/chartjs")

# 
p01 <- chartjs(mtcars[order(mtcars$mpg), ], x = ~ as.character(1:32), width = "75%") %>%
  new_bars(y = ~ disp) %>%
  new_lines(y = ~ wt * 100, bgCol = "#0F0", brdCol = "#F00", radius = 0) %>%
  print()

# 
p02 <- ggplot2::diamonds %>%
  dplyr::mutate(cut = cut(carat, 50)) %>%
  dplyr::group_by(cut) %>%
  dplyr::summarize(price = mean(price), x = mean(y)) %>%
  chartjs(x = ~ cut) %>%
  new_bars(y = ~ price) %>%
  new_lines(~ x * mean(price) / mean(x)) %>%
  print()

# 
htmltools::html_print(purrr::map(1:5, ~ p01), background = "#FFF")
htmltools::save_html(purrr::map(1:100, ~ p01), file = "~/Downloads/Tag-List.html", background = "#FFF")

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

# 
# colnames(mtcars) %>%
#   purrr::map(~ mtcars %>% dplyr::select(mpg, Resp = .x)) %>%
#   tail(-1) %>%
#   purrr::map(~ chartjs(.x, x = ~ mpg, width = "75%") %>% new_bars(y = ~ Resp)) %>%
#   htmltools::html_print(.)




# Simple test
p03 <- chartjs(mtcars %>% dplyr::arrange(mpg), 1:32) %>%
  new_lines(~ mpg, bgCol = "#3C3", brdWidth = 1) %>%
  new_bars(~ disp, bgCol = "red", brdCol = "red", brdWidth = 1, yAxis = "y2") %>%
  alter_axis("y2", min = 0, position = "right") %>% print()

# Intermediate test
p04 <- chartjs(mtcars %>% dplyr::arrange(mpg), 1:32) %>%
  new_lines(~ mpg, bgCol = "#3C3", brdWidth = 2, radius = 2.5) %>%
  new_bars(~ disp / max(disp), bgCol = "#666", brdCol = "red", brdWidth = 1.5, yAxis = "y2", barPerc = 1, catPerc = 1) %>%
  alter_axis("y2", min = 0, position = "right", percent = TRUE) %>% print()

# Categorical test
p05 <- data.table::data.table(Fruit = c("Apple", "Banana", "Blueberry", "Cherry", "Strawberry", "Orange"))[, Length := nchar(Fruit)] %>%
  chartjs(x = ~ Fruit) %>%
  new_bars(y = ~ Length) %>%
  new_bars(y = ~ Length * 2, label = "Double Length") %>%
  alter_axis("y", min = 0) %>% print()

# Filled area + hidden label
p06 <- data.table::data.table(Fruit = c("Apple", "Banana", "Blueberry", "Cherry", "Strawberry", "Orange"))[, Length := nchar(Fruit)] %>%
  chartjs(x = ~ Fruit) %>%
  new_lines(y = "Length", fill = "+2", brdWidth = 0, radius = 0, legend = FALSE) %>%
  new_lines(y = ~ Length * 1.5, bgCol = "green", radius = 0) %>%
  new_lines(y = ~ Length * 2, brdWidth = 0, radius = 0, legend = FALSE) %>%
  alter_axis("y", min = 0, percent = TRUE) %>% print()

# Hidden label
p07 <- dplyr::arrange(mtcars, mpg) %>% chartjs(x = 1:32) %>%
  new_lines(~ mpg, legend = FALSE) %>%
  new_bars(~ disp) %>% print()

# Sample data/stepped lines
p08 <- rsavis::sampleData[, .(Int100Clean = mean(Int100Clean)), keyby = Int20Clean] %>%
  chartjs(x = ~ as.character(Int20Clean), height = 300, type = "line") %>%
  new_lines(y = ~ Int100Clean, tension = 0, bgCol = "#F00") %>%
  new_lines(y = ~ Int100Clean, stepped = "middle", bgCol = "#0F0") %>%
  new_lines(y = ~ Int100Clean, bgCol = "#00F") %>%
  alter_options(animDur = 0) %>% print()

# Axis titles
p09 <- chartjs(mtcars, x = 1:32) %>% new_lines(y = ~ mpg) %>% alter_axis("y", min = 0) %>%
  axis_title(
    "y", title = "mpg", lineHeight = 5, fontColor = "#000", fontSize = 20,
    fontStyle = "italic bold", fontFamily = "monospace", padding = 10
  ) %>%
  axis_title("x", title = "Index") %>%
  axis_title("x", display = FALSE) %>% print()

# Chart title
p10 <- chartjs(mtcars, x = 1:32) %>% new_lines(y = ~ mpg) %>% alter_axis("y", min = 0) %>%
  chart_title(
    title = "Hello World", position = "top", display = TRUE, lineHeight = 2,
    fontColor = "#000", fontSize = 20, fontStyle = "bold", fontFamily = "monospace", padding = 5
  ) %>% print()

# Trace ordering
p11 <- chartjs(mtcars, x = 1:32) %>%
  new_lines(y = ~ mpg, bgCol = "#F00") %>%
  new_bars(y = ~ disp, yAxis = "y2", bgCol = "#000") %>%
  alter_axis("y2", position = "right", grid = FALSE) %>% print()
p12 <- chartjs(mtcars, x = 1:32) %>%
  new_lines(y = ~ mpg, bgCol = "#F00") %>%
  new_bars(y = ~ disp, yAxis = "y2", bgCol = "#000", order = -1) %>%
  alter_axis("y2", position = "right", grid = FALSE) %>% print()


# View all plots
ls(pattern = "^p\\d+") %>% setNames(., .) %>% purrr::map(get) %>% Filter(is.chartjs, .) %>% rev() %>%
  purrr::imap(~ htmltools::div(htmltools::h2(.y, style = "text-align: center;"), .x)) %>%
  htmltools::tagList(.) %>% htmltools::html_print(.)
