library(teal.modules.general)
library(teal.modules.clinical)

data <- teal_data() |>
  within(code, code = parse(text = readLines("dev/workshop/preprocess.R")))

# Set up join_keys
join_keys(data) <- teal.data::default_cdisc_join_keys[c("ADAE", "ADSL")]

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_missing_data(),
    tm_variable_browser()
  )
)

shinyApp(app$ui, app$server)
