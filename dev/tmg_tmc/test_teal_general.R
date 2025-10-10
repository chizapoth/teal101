# this is my first teal app, with iris data

library(teal)
library(teal.modules.general)

# create empty `teal_data` object
data <- teal_data()

# execute code within it
data <- within(data, {
  IRIS <- iris
  # MTCARS <- mtcars
})


# distribution ----
mod_distribution <- tm_g_distribution(
  dist_var = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(variable_choices("IRIS"), "Petal.Length")
    )
  )

# scatterplot ----
mod_scatter <- tm_g_scatterplot(
  label = "Single wide dataset",
  x = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["IRIS"]], c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      selected = "Petal.Width",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["IRIS"]], c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length")),
      selected = "Petal.Length",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_by = data_extract_spec(
    dataname = "IRIS",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["IRIS"]], c("Species")),
      selected = NULL,
      multiple = TRUE,
      fixed = FALSE
    )
  )
)


# regression ----
mod_regression <- tm_a_regression(
      label = "Regression",
      response = data_extract_spec(
        dataname = "IRIS",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["IRIS"]], c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
          selected = "Sepal.Length",
          multiple = FALSE,
          fixed = TRUE
        )
      ),
      regressor = data_extract_spec(
        dataname = "IRIS",
        select = select_spec(
          label = "Select variables:",
          choices = variable_choices(data[["IRIS"]], c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
          selected = "Sepal.Width",
          multiple = TRUE,
          fixed = FALSE
        )
      )
    )



app <- init(
  data = data,
  modules = list(
    # example_module(),
    # data preview module
    # tm_data_table("Data Table"),
    # tm_variable_browser(
    #     label = "Variable Browser"
    # ),
    mod_distribution
    # mod_regression
    # mod_scatter

  )

)

if (Sys.getenv("QUARTO_ROOT") == "") {
shinyApp(app$ui, app$server)
}




