# tutorial 
library(shiny)
library(bslib)
library(ggplot2)
# load data
# load("movies.RData")



# Define UI

# page_sidebar: creates a screen-filling page with sidebar
# layout
ui <- page_sidebar(
  
  # text ----
  textInput(
    inputId = "custom_text",
    label = "Input some text here"
  ),
  
  strong("Text is shown below:"),
  # output 
  textOutput(outputId = "user_text"),
  
# input controls
  # two dropdown menus with selectInput() function
  sidebar = sidebar(
    # yaxis ----
    # Select variable for y-axis
    selectInput(
      
      # this is the input value the app access (input$x1)
      inputId = "x1", 
      
      # label to display
      label = "Y-axis:",
      
      # these are column names
      choices = c("imdb_rating", 
                  "imdb_num_votes", 
                  "critics_score", 
                  "audience_score", 
                  "runtime"),
      selected = "audience_score"
    ),
    
    # xaxis ----
    # Select variable for x-axis
    selectInput(
      inputId = "x2",
      label = "X-axis:",
      choices = c("imdb_rating", 
                  "imdb_num_votes", 
                  "critics_score", 
                  "audience_score",
                  "runtime"),
      selected = "imdb_num_votes"
    ),
    
    # color ----
    # Select variable for color
    selectInput(
      inputId = "x3",
      label = "Color:",
      choices = c("title_type", 
                  "genre", 
                  "critics_rating", 
                  "audience_rating", 
                  "mpaa_rating"),
      selected = "mpaa_rating"
    )
    
  ),
  
  
  # Output: Show scatterplot
  # this is the main panel
  card(plotOutput(outputId = "scatterplot"))
)

# Define server

server <- function(input, output, session) {
  
  # define the output list
  # x, y variables are defined in the ui
  output$scatterplot <- renderPlot({
    
    # input x and y are passed here
    # not sure why it has to be aes_string rather than aes, like in normal ggplot
    # need to match the inputID
    
    # this is the old way: aes_string()
    # ggplot(data = movies, 
    #        aes_string(x = input$x1, 
    #                   y = input$x2,
    #                   col = input$x3)) + geom_point()
    
    # new way: aes
    ggplot(data = movies, 
           aes(x = .data[[input$x1]], 
               y = .data[[input$x2]], 
               col = .data[[input$x3]])) + geom_point()
    
    
  })
  output$user_text <- renderText({ input$custom_text })
  
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)

#ggplot(data = movies, aes(x = audience_score, y = imdb_rating)) +
#  geom_point()



# a text example -----
# ui <- page_fluid(
#   
#   textInput(
#     inputId = "custom_text",
#     label = "Input some text here"
#   ),
#   
#   strong("Text is shown below:"),
#   # output 
#   textOutput(outputId = "user_text")
#   
# )
# 
# server <- function(input, output, session){
#   
#   output$user_text <- renderText({ input$custom_text })
#   
# }
# 
# shinyApp(ui = ui, server = server)





