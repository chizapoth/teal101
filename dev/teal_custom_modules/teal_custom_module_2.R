# create the ui and template for the custom module
# in this example, create plots (histogram)



library(teal)

# define new module ----
tealmodule_ui <- function(id){
  ns <- NS(id)
  # p means paragraph
  # tags$p('Hello World') # UNCOMMENT THIS LINE TO SHOW THIS MODULE
  
  # div means put things together
  tags$div(
    
    # input 1
    shiny::selectInput(
      inputId = ns('datasets'), # wrap in namespace
      label = 'Datasets',
      choices = NULL 
    ), 
    
    # input 2
    shiny::selectInput(
      inputId = ns('variables'), # wrap in namespace
      label = 'Variables',
      choices = NULL 
    ), 
    # input 3
    shiny::sliderInput(
      inputId = ns('binwidth'), # wrap in namespace
      label = 'Binwidth',
      value = 2,
      min = 1, 
      max = 5, 
      step = 0.5
    ), 
    
    # output: plot
    shiny::plotOutput(ns('plt')) # wrap in namespace
    
  )
}

tealmodule_server <- function(id, data){
  
  moduleServer(id, function(input, output, session){
    
    shiny::updateSelectInput(
      inputId = 'datasets',
      
      # this data is a function!!! not a dataset
      # datanames(data) # this tells you what names there are
      choices = datanames(data())
      # lists all the datanames
    )
    
    # instead of a table, use a plot
    
    
    observeEvent(input$datasets, {

      # create indicator
      #only_numeric <- sapply(data()[[input$datasets]], is.numeric)
      
      shiny::updateSelectInput(
        inputId = 'variables',
        # colnames
        # select only numerics
        choices = names(data()[[input$datasets]])#[only_numeric]
      )
      
      
    }) # end of observeEvent

    
  }) # end of moduleServer
}


custom_module <- function(label = 'My custom teal module'){
  
  # this module function comes from teal
  module(
    
    label = label,
    # insert customized ui
    ui = tealmodule_ui, 
    # insert customized server
    server = tealmodule_server,
    datanames = 'all'
  )
}


# teal app ----

# data 
# create data object
# add data, for now use the cdisc data
data <- teal_data(
  adsl = teal.data::rADSL,
  adae = teal.data::rADAE
)

# data
# data$adae
# data[['adae']]
# get_var(data,'adae')

# if want to reproduce: 
# data <- teal_data(
#   adsl = teal.data::rADSL,
#   adae = teal.data::rADAE, 
#   code = "
#     adsl <- teal.data::rADSL
#     adae <- teal.data::rADAE
#   "
# )



# app 
app <- init(
  data = data, 
  modules = modules(
    # replace with custom_module you defined yourself
    # example_module(label = 'my module')
    # can leave the label empty, it'll be the label you defined already above
    custom_module()
  ),
  header = 'my teal app'
)


shinyApp(app$ui, app$server)



