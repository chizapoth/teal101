library(teal.modules.general)
library(teal.modules.clinical)
library(teal)

data <- teal_data() |>
  within({
    path <- "data/cdisc"
    ADSL <- read.csv(file.path(path, "adsl.csv"))
    ADAE <- read.csv(file.path(path, "adae.csv"))
  })

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_missing_data(),
    tm_variable_browser()
  )
)

shinyApp(app$ui, app$server)
