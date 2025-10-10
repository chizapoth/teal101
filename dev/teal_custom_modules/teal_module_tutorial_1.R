# follwing this: https://insightsengineering.github.io/teal/latest-tag/articles/creating-custom-modules.html
dataset <- data.frame(
  a = seq(1, 10),
  b = seq(11, 20)
)


dataset
# can sepcify which var, to make a plot
vars <- 'b'

my_plot <- hist(
  dataset[[vars]],
  las = 1,
  main = paste("Histogram of", vars),
  xlab = vars,
  col = "lightblue",
  border = "black"
)


# custom module ----
library(teal)

# UI function for the custom histogram module

histogram_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("dataset"),
      "Select Dataset",
      choices = c("iris", "mtcars")
    ),
    shiny::selectInput(
      ns("variable"),
      "Select Variable",
      choices = c(names(iris), names(mtcars))
    ),
    shiny::plotOutput(ns("histogram_plot")),
    shiny::verbatimTextOutput(ns("plot_code")) # To display the reactive plot code
  )
}
