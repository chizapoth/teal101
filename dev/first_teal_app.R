# this would seem that ui and server are wrapped inside
# the customised functions

library(teal)

app <- init(
  # add data
  # these two datasets are already available in baseR
  # need to pass by the teal_data module
  data = teal_data(IRIS = iris, MTCARS = mtcars),

  # add modules
  modules = modules(
    example_module("Module IRIS 1"),
    example_module("Module IRIS 2")
  ),

  # add filter (teal slice)
  filter = teal_slices(
    teal_slice(dataname = "IRIS", varname = "Species", selected = "setosa")
  )
) |> # the app init ends here, add
  modify_title("Test teal") |>

  # this is the header
  modify_header(h3("Test teal 2")) |>
  modify_footer(tags$div(a(
    "Powered by teal",
    href = "https://insightsengineering.github.io/teal/latest-tag/"
  )))


if (interactive()) {
  shinyApp(app$ui, app$server)
}

# library(ggplot2)
# ggplot(mtcars, aes(x = mpg, y = wt)) +
#   geom_point() +
#   labs(title = "Scatterplot of mpg vs wt in mtcars dataset")

app <- init(
  # add data
  data = teal_data(iris = iris),

  # add modules (as a list)
  modules = list(
    module(
      label = "iris histogram",

      # server
      server = function(input, output, session, data) {
        updateSelectInput(
          session = session,
          inputId = "var",
          choices = names(data()[["iris"]])[1:4]
        )

        # plot
        output$hist <- renderPlot({
          req(input$var)
          hist(
            x = data()[["iris"]][[input$var]],
            main = sprintf("Histogram of %s", input$var),
            xlab = input$var
          )
        })
      },

      # ui
      ui = function(id) {
        ns <- NS(id)
        list(
          selectInput(
            inputId = ns("var"),
            label = "Column name",
            choices = NULL
          ),
          plotOutput(outputId = ns("hist"))
        )
      }
    )
  )
)


shinyApp(app$ui, app$server)
