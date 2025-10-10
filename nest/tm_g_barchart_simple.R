library(nestcolor)
library(dplyr)

data <- teal_data()
data <- within(data, {
  ADSL <- tmc_ex_adsl %>%
    mutate(
      ITTFL = factor("Y") %>%
        with_label("Intent-To-Treat Population Flag")
    )
  ADAE <- tmc_ex_adae %>%
    filter(!((AETOXGR == 1) & (AESEV == "MILD") & (ARM == "A: Drug X")))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

ADSL <- data[["ADSL"]]
ADAE <- data[["ADAE"]]

app <- init(
  data = data,
  modules = modules(
    tm_g_barchart_simple(
      label = "ADAE Analysis",
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(
            ADSL,
            c(
              "ARM",
              "ACTARM",
              "SEX",
              "RACE",
              "ITTFL",
              "SAFFL",
              "STRATA2"
            )
          ),
          selected = "ACTARM",
          multiple = FALSE
        )
      ),
      fill = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM",
                "ACTARM",
                "SEX",
                "RACE",
                "ITTFL",
                "SAFFL",
                "STRATA2"
              )
            ),
            selected = "SEX",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      x_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = "AETOXGR",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM",
                "ACTARM",
                "SEX",
                "RACE",
                "ITTFL",
                "SAFFL",
                "STRATA2"
              )
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      ),
      y_facet = list(
        data_extract_spec(
          dataname = "ADAE",
          select = select_spec(
            choices = variable_choices(ADAE, c("AETOXGR", "AESEV", "AESER")),
            selected = "AESEV",
            multiple = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            choices = variable_choices(
              ADSL,
              c(
                "ARM",
                "ACTARM",
                "SEX",
                "RACE",
                "ITTFL",
                "SAFFL",
                "STRATA2"
              )
            ),
            selected = NULL,
            multiple = FALSE
          )
        )
      )
    )
  )
)
if (interactive()) {
  shinyApp(app$ui, app$server)
}
