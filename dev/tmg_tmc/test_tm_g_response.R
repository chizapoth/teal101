library(teal)
library(teal.modules.general)

data <- teal_data()
data <- within(data, {
  require(nestcolor)
  mtcars <- mtcars
  for (v in c("cyl", "vs", "am", "gear")) {
    mtcars[[v]] <- as.factor(mtcars[[v]])
  }
})

app <- init(
  data = data,
  modules = modules(
    tm_g_response(
      label = "Response Plots",
      response = data_extract_spec(
        dataname = "mtcars",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["mtcars"]], c("cyl", "gear")),
          selected = "cyl",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      x = data_extract_spec(
        dataname = "mtcars",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["mtcars"]], c("vs", "am")),
          selected = "vs",
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_response
#if (interactive()) {
shinyApp(app$ui, app$server)
#}
