#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load necessary libraries
library(shiny)
library(datasets)

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive Exploration of Datasets: mtcars, USArrests, and uspop"),
  
  # Use CSS to create a layout where the left side has variable selection (30%) and data table (70%)
  tags$style(HTML("
        .left-panel {
            display: grid;
            grid-template-rows: 30% 70%;
            height: 100vh; /* Full viewport height */
            border-right: 2px solid #ccc;
        }
        .var-selection {
            padding: 10px;
            background-color: #f9f9f9;
            border-right: 1px solid #ccc;
        }
        .data-table {
            padding: 10px;
            background-color: #fff;
            overflow-y: scroll;
        }
    ")),
  
  fluidRow(
    column(4, div(class = "left-panel",
                  # Variable selection section (30%)
                  div(class = "var-selection",
                      selectInput("dataset", "Choose a Dataset:", 
                                  choices = list("mtcars" = "mtcars", 
                                                 "USArrests" = "USArrests", 
                                                 "uspop" = "uspop")),
                      
                      # Conditional input for selecting variables (mtcars and USArrests only)
                      conditionalPanel(
                        condition = "input.dataset == 'mtcars' || input.dataset == 'USArrests'",
                        selectInput("xvar", "X-axis variable:", choices = NULL),
                        selectInput("yvar", "Y-axis variable:", choices = NULL)
                      ),
                      
                      # Input for setting number of bins (for uspop only)
                      conditionalPanel(
                        condition = "input.dataset == 'uspop'",
                        sliderInput("bins", "Number of Bins:", min = 5, max = 20, value = 10)
                      )
                  ),
                  
                  # Data table section (70%)
                  div(class = "data-table",
                      tableOutput("data")
                  )
    )),
    
    # Main panel for displaying the plot (rest of the page)
    column(8,
           plotOutput("plot", height = "600px")
    )
  )
)

# Define server logic required to draw plots and display datasets
server <- function(input, output, session) {
  
  # Reactive function to return the selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "USArrests" = USArrests,
           "uspop" = uspop)
  })
  
  # Update select inputs for x and y variables based on dataset choice
  observe({
    if (input$dataset == "mtcars" || input$dataset == "USArrests") {
      updateSelectInput(session, "xvar", "X-axis variable:", 
                        choices = names(datasetInput()))
      updateSelectInput(session, "yvar", "Y-axis variable:", 
                        choices = names(datasetInput()))
    }
  })
  
  # Render table of the selected dataset
  output$data <- renderTable({
    datasetInput()
  })
  
  # Render plot based on user selections
  output$plot <- renderPlot({
    data <- datasetInput()
    
    if (input$dataset == "mtcars" || input$dataset == "USArrests") {
      # Scatter plot for mtcars and USArrests
      plot(data[[input$xvar]], data[[input$yvar]], 
           xlab = input$xvar, ylab = input$yvar, 
           main = paste("Scatterplot of", input$yvar, "vs", input$xvar),
           col = "blue", pch = 16)
      
    } else if (input$dataset == "uspop") {
      # Histogram for uspop
      hist(data, 
           breaks = input$bins, 
           main = "Population Histogram", 
           xlab = "Year", 
           col = "lightgreen", 
           border = "darkgreen")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
