library(teal)
library(teal.modules.general)
library(teal.modules.clinical)

path <- "data/cdisc"
ADSL <- read.csv(file.path(path, 'adsl.csv'))


data <- teal_data(
  ADSL = ADSL,
  code = '
    path <- "data/cdisc"
    ADSL <- read.csv(file.path(path, "adsl.csv"))
  '
)

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_missing_data(),
    tm_variable_browser()
  )
)

shinyApp(app$ui, app$server)
