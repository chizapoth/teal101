# test out teal clinical 
library(dplyr)
library(teal.modules.general)
library(teal.modules.clinical)


# data ----
# Prepare data object
# ? what does default_cdisc_join_keys do?
data <- teal_data()
data <- within(data, {
  ADSL <- rADSL
})
join_keys(data) <- default_cdisc_join_keys["ADSL"]



# another data template: has 2 parts
# use this one for the tm_t_pp_medical_history module
data2 <- teal_data()
data2 <- within(data2, {
  ADSL <- tmc_ex_adsl
  ADMH <- tmc_ex_admh
})
join_keys(data2) <- default_cdisc_join_keys[names(data2)]






# define inputs ----
# Prepare module inputs
# this step is not strictly necessary, since only one dataset is involved
ADSL <- data[["ADSL"]]


# choices_selected is from teal.transform
# define arm 
# ARMCD: arm code (a, b, c)
# ARM: arm name (drug, placebo, combination)
# (probably always fixed)
cs_arm_var <- choices_selected(
  choices = variable_choices(ADSL, subset = c("ARMCD", "ARM")),
  selected = "ARM"
)


# demographic variables
# selected the numeric and factor variables
# i.e. date, dttm, chr are excluded
demog_vars_adsl <- ADSL |>
  select(where(is.numeric) | where(is.factor)) |>
  names()

glimpse(ADSL)

# these are the excluded ones (study ID, subject ID, ...)
ADSL |>
  select(!(where(is.numeric) | where(is.factor))) |>
  names()







# _______ ----
# define modules ----

# tm_data_table -----
mod_dt <- tm_data_table("Data Table")


# tm_t_summary -----
mod_summary <- tm_t_summary(
  label = "Demographic Table",
  
  # add which dataset to use
  dataname = "ADSL",
  arm_var = cs_arm_var,
  summarize_vars = choices_selected(
    choices = variable_choices(ADSL, demog_vars_adsl),
    selected = c("SEX", "AGE", "RACE")
  )
)



# tm_t_pp_medical_history -----
# (patient specific)
# data requires: ADMH, ADSL
# tm_t_pp_medical_history(
#   label,
#   dataname = "ADMH",
#   parentname = "ADSL",
#   patient_col = "USUBJID",
#   mhterm = NULL,
#   mhbodsys = NULL,
#   mhdistat = NULL,
#   pre_output = NULL,
#   post_output = NULL,
#   transformators = list(),
#   decorators = list()
# )


data2$ADMH$MHTERM %>% table()
data2$ADMH$MHBODSYS %>% table()
data2$ADMH$MHDISTAT %>% table()

ADSL <- data2[["ADSL"]]
ADMH <- data2[["ADMH"]]

mod_mh <- tm_t_pp_medical_history(
  label = "Medical History",
  dataname = "ADMH", # data: medical history
  parentname = "ADSL", # data: adsl that has demographics
  patient_col = "USUBJID",
  
  # define variables 
  # Reported Term for the Medical History
  mhterm = choices_selected(
    choices = variable_choices(ADMH, c("MHTERM")),
    selected = "MHTERM"
  ),
  # Body System or Organ Class
  mhbodsys = choices_selected(
    choices = variable_choices(ADMH, "MHBODSYS"),
    selected = "MHBODSYS"
  ),
  # Status of Disease
  mhdistat = choices_selected(
    choices = variable_choices(ADMH, "MHDISTAT"),
    selected = "MHDISTAT"
  )
)

# _______ -----
# Create app -----
app <- init(
  # single data source: adsl
  # data = data,
  # modules = list(
  #   
  #   # module 1, display data
  #   mod_dt,
  #   
  #   # module 2, summary table
  #   mod_summary,
  # )
  # for medical history: use data 2
  data = data2,
  module = mod_mh

)

if (Sys.getenv("QUARTO_ROOT") == "") {
  shinyApp(app$ui, app$server)
}

# modify name ----
# app <- init(
#   data = data,
#   modules = list(
#     example_module()
#   )
# )
# # ?modify_title
# # modify_title(app, "Test teal")
# modify_header(app, h3("Test teal 2"))  
#   # # this is the header
#   # modify_header(h3("Test teal 2")) |>
#   # modify_footer(tags$div(a("Powered by teal", href = "https://insightsengineering.github.io/teal/latest-tag/")))



# app <- init(
#   data = data,
#   modules = list(
#     example_module()
#   )
# ) |>
#   modify_title("My new title") |>
#   modify_header("My new header") |>
#   modify_footer("My new footer")



