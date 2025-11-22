library(teal)
library(teal.modules.general)
# general data example
data <- teal_data()
data <- within(data, {
  CO2 <- CO2
  CO2[["primary_key"]] <- seq_len(nrow(CO2))
})
join_keys(data) <- join_keys(join_key("CO2", "CO2", "primary_key"))

vars <- choices_selected(variable_choices(
  data[["CO2"]],
  c("Plant", "Type", "Treatment")
))

app <- init(
  data = data,
  modules = modules(
    tm_outliers(
      outlier_var = list(
        data_extract_spec(
          dataname = "CO2",
          select = select_spec(
            label = "Select variable:",
            choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
            selected = "uptake",
            multiple = FALSE,
            fixed = FALSE
          )
        )
      ),
      categorical_var = list(
        data_extract_spec(
          dataname = "CO2",
          filter = filter_spec(
            vars = vars,
            choices = value_choices(data[["CO2"]], vars$selected),
            selected = value_choices(data[["CO2"]], vars$selected),
            multiple = TRUE
          )
        )
      )
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}


# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

fact_vars_adsl <- names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
vars <- choices_selected(variable_choices(data[["ADSL"]], fact_vars_adsl))


app <- init(
  data = data,
  modules = modules(
    tm_outliers(
      outlier_var = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            label = "Select variable:",
            choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
            selected = "AGE",
            multiple = FALSE,
            fixed = FALSE
          )
        )
      ),
      categorical_var = list(
        data_extract_spec(
          dataname = "ADSL",
          filter = filter_spec(
            vars = vars,
            choices = value_choices(data[["ADSL"]], vars$selected),
            selected = value_choices(data[["ADSL"]], vars$selected),
            multiple = TRUE
          )
        )
      )
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
