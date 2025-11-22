library(teal.transform)
library(teal.data)
library(shiny)
library(ggplot2)

# Use iris dataset for simplicity
data(iris)
iris$Category <- sample(c("A", "B", "C"), nrow(iris), replace = TRUE)

# create a list of reactive data.frame objects
datasets <- list(
  iris = reactive(iris)
)

# X-axis variable selection
x_des <- data_extract_spec(
  dataname = "iris",
  select = select_spec(
    label = "X-axis variable:",
    choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    selected = "Sepal.Length",
    multiple = FALSE
  )
)

# Y-axis variable selection
y_des <- data_extract_spec(
  dataname = "iris",
  select = select_spec(
    label = "Y-axis variable:",
    choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    selected = "Sepal.Width",
    multiple = FALSE
  )
)

# Color variable selection
color_des <- data_extract_spec(
  dataname = "iris",
  select = select_spec(
    label = "Color variable:",
    choices = c("Species", "Category"),
    selected = "Species",
    multiple = FALSE
  )
)

extract_ui <- function(id, x_extract, y_extract, color_extract) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Variable Selection"),
      data_extract_ui(ns("x_extract"), label = "X Variable", x_extract),
      br(),
      data_extract_ui(ns("y_extract"), label = "Y Variable", y_extract),
      br(),
      data_extract_ui(
        ns("color_extract"),
        label = "Color Variable",
        color_extract
      )
    ),
    mainPanel(
      h3("Scatterplot"),
      plotOutput(ns("scatterplot"))
    )
  )
}

extract_srv <- function(id, datasets, x_extract, y_extract, color_extract) {
  moduleServer(id, function(input, output, session) {
    # X-axis variable extraction
    x_extract_input <- data_extract_srv(
      "x_extract",
      datasets,
      x_extract
    )

    # Y-axis variable extraction
    y_extract_input <- data_extract_srv(
      "y_extract",
      datasets,
      y_extract
    )

    # Color variable extraction
    color_extract_input <- data_extract_srv(
      "color_extract",
      datasets,
      color_extract
    )

    plot_data <- reactive({
      req(x_extract_input(), y_extract_input())

      x_input <- x_extract_input()
      y_input <- y_extract_input()
      color_input <- color_extract_input()

      # Get the dataset
      dataset_name <- x_input$dataname
      plot_data <- datasets[[dataset_name]]()

      # Get selected variables
      x_var <- x_input$select
      y_var <- y_input$select
      color_var <- color_input$select

      list(
        data = plot_data,
        x_var = x_var,
        y_var = y_var,
        color_var = color_var
      )
    })

    output$scatterplot <- renderPlot({
      req(plot_data())

      data_info <- plot_data()

      ggplot(
        data_info$data,
        aes_string(
          x = data_info$x_var,
          y = data_info$y_var,
          color = data_info$color_var
        )
      ) +
        geom_point(size = 3, alpha = 0.7) +
        labs(
          title = paste("Scatterplot:", data_info$y_var, "vs", data_info$x_var),
          x = data_info$x_var,
          y = data_info$y_var
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          legend.position = "bottom"
        )
    })
  })
}

shinyApp(
  ui = bslib::page_fluid(extract_ui("data_extract", x_des, y_des, color_des)),
  server = function(input, output, session) {
    extract_srv("data_extract", datasets, x_des, y_des, color_des)
  }
)
