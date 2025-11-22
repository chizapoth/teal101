# https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html

library(teal)

data <- within(teal_data(), {
  iris <- iris
  mtcars <- mtcars
})

transformator_iris <- teal_transform_module(
  label = "Custom transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      numericInput(
        ns("n_rows"),
        "Number of rows to display",
        value = 6,
        min = 1,
        max = 150,
        step = 1
      )
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(
          data(),
          iris <- head(iris, num_rows),
          num_rows = input$n_rows
        )
      })
    })
  }
)

transformator_iris_scale <- teal_transform_module(
  label = "Scaling transformator for iris",
  ui = function(id) {
    ns <- NS(id)
    uiOutput(ns("scaled_columns_container"))
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      scalable_columns <- names(Filter(is.numeric, data()[["iris"]])) |>
        isolate()

      output$scaled_columns_container <- renderUI({
        selectInput(
          inputId = ns("scaled_columns"),
          label = "Columns to scale",
          choices = scalable_columns,
          selected = input$scaled_columns,
          multiple = TRUE
        )
      })

      reactive({
        within(
          data(),
          {
            iris[scaled_columns] <- scale(iris[scaled_columns])
          },
          scaled_columns = input$scaled_columns
        )
      })
    })
  }
)

transformator_mtcars <- teal_transform_module(
  label = "Custom transformator for mtcars",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      "Adding rownames column to mtcars"
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        within(data(), {
          mtcars$rownames <- rownames(mtcars)
          rownames(mtcars) <- NULL
        })
      })
    })
  }
)
my_transformators <- list(
  transformator_iris,
  transformator_iris_scale,
  transformator_mtcars
)


app <- init(
  data = data,
  modules = example_module(
    transformators = my_transformators
  )
)

if (interactive()) {
  shinyApp(app$ui, app$server)
}
