library(teal.modules.general)
library(teal.modules.clinical)

data <- teal_data() |>
  within(code, code = parse(text = readLines("dev/workshop/preprocess.R")))

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_missing_data(),
    tm_variable_browser()
  )
)

shinyApp(app$ui, app$server)
