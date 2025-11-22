# example of a shiny module
library(shiny)

data <- iris

# steps: define ui, define server, then combine
ui <- fluidPage(
  # Application title
  titlePanel("Data display"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs (for now empty)
    sidebarPanel(
      # Add some placeholder controls here if needed
      h4("Controls")
    ),

    # Main panel for displaying outputs
    mainPanel(
      tableOutput("data_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$data_table <- renderTable({
    # Limit rows based on user input
    data
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)


# convert to module -----

# 1. ui function
table_ui <- function(id) {
  ns <- NS(id)
  tableOutput(ns("data_table"))
}
# 2. server function
# data has to be reactive
table_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(data)
    output$data_table <- renderTable(
      data()
    )
  })
}
# 3. main ui and server
ui <- fluidPage(
  titlePanel("Data display"),
  sidebarLayout(
    sidebarPanel(
      h4("Controls")
    ),
    mainPanel(
      table_ui("table1")
    )
  )
)
server <- function(input, output, session) {
  table_server("table1", data)
}

# run app
shinyApp(ui = ui, server = server)
