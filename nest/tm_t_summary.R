library(teal)
library(teal.modules.clinical)
# Preparation of the test case - use `EOSDY` and `DCSREAS` variables to demonstrate missing data.
data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADSL$EOSDY[1] <- NA_integer_
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_summary(
      label = "Demographic Table",
      dataname = "ADSL",
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
      add_total = TRUE,
      summarize_vars = choices_selected(
        c("SEX", "RACE", "BMRKR2", "EOSDY", "DCSREAS", "AGE"),
        c("SEX", "RACE")
      ),
      useNA = "ifany"
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
