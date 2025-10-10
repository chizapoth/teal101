library(teal)
library(teal.modules.clinical)


data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl
  ADAE <- tmc_ex_adae
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_t_events(
      label = "Adverse Event Table",
      dataname = "ADAE",

      # teal.transform functions
      arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),

      # variable with high level term for events
      llt = choices_selected(
        # choices = variable_choices(ADAE, c("AETERM", "AEDECOD")),
        choices = c('AETERM', 'AEDECOD'),
        selected = c("AEDECOD")
      ),

      # variable with low level for events
      hlt = choices_selected(
        choices = variable_choices(ADAE, c("AEBODSYS", "AESOC")),
        selected = "AEBODSYS"
      ),
      # whether add total number of patient
      add_total = TRUE,
      event_type = "adverse event"
    )
  )
)
#if (interactive()) {
shinyApp(app$ui, app$server)
#}
