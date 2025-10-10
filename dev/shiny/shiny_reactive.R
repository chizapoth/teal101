library(shiny)
library(bslib)


# test reactivity


ui <- fluidPage(
  textInput(inputId = "name", 
            label = "What's your name?"),
  
  textOutput(outputId = "greeting")
)



server <- function(input, output, session) {
  
  # output$greeting <- renderText({
  #   paste0("Hello ", input$name, "!")
  # })
  
  # add reactive 
  # remember: when calling the reactive expression, need () after the expression
  string <- reactive(paste0("Hello ", input$name, "!"))
  output$greeting <- renderText(string())
  
}



# run app
shinyApp(ui, server)
