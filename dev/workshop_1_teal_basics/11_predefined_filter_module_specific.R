library(teal.modules.general)
library(teal.modules.clinical)

data <- teal_data() |>
  within(code, code = parse(text = readLines("code/day1/preprocess.R")))

# setting it up manually
join_keys(data) <- join_keys(
  join_key("ADSL", keys = c("STUDYID", "USUBJID")),
  join_key("ADAE", keys = c("STUDYID", "USUBJID", "ASTDTM", "AETERM", "AESEQ")),
  join_key("ADSL", "ADAE", keys = c("STUDYID", "USUBJID"))
)

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_missing_data(),
    tm_variable_browser()
  ),
  filter = teal_slices(
    teal_slice(
      dataname = "ADSL",
      varname = "COUNTRY",
      id = "country",
      selected = "ITA",
      fixed = TRUE
    ),
    teal_slice(
      dataname = "ADSL",
      varname = "SEX",
      id = "sex",
      selected = "F",
      anchored = TRUE
    ),
    module_specific = TRUE,
    mapping = list(
      `Data Table` = "country",
      global_filters = "sex"
    )
  )
) |>
  modify_header(h1("My teal app"))

shinyApp(app$ui, app$server)
