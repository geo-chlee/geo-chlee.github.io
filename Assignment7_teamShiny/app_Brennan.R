library(shiny)
library(shinydashboard)
library(ggplot2)
library(bslib)


ui <- dashboardPage(
    dashboardHeader(
      title= div(h3("Assignment 7: Iris Dataset", style="margin: 0;"), h4('https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/iris', style="margin: 0;"))
    ),

  sidebar = dashboardSidebar(
    selectInput("x_var", "X-axis variable:",
                choices = names(iris)[1:2],
                selected = "Sepal.Length"),
    selectInput("y_var", "Y-axis variable:",
                choices = names(iris)[3:4],
                selected = "Sepal.Width"),
    hr(),
    sliderInput("bins", "Number of bins:",
                min = 5, max = 50, value = 30),
    "Iris Dataset used.",
    uiOutput("tab")
    
  ),
  dashboardBody(
    col_widths = c(6, 6),
    card(
      card_header("Scatter Plot"),
      plotOutput("scatter_plot"),
      full_screen = TRUE
    ),
    card(
      card_header("Histogram"),
      plotOutput("histogram"),
      full_screen = TRUE
    ),
    card(
      card_header("Bubble Chart"),
      plotOutput("bubble"),
      full_screen = TRUE
    )
  )
)

server <- function(input, output) {
  
  url <- a("Iris Data", href="https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/iris")
  output$tab <- renderUI({
    tagList("Dataset:", url)})
  
  output$scatter_plot <- renderPlot({
    ggplot(iris, aes_string(x = input$x_var, y = input$y_var, color = "Species")) +
      geom_point(size = 3, alpha = 0.6) +
      theme_minimal() +
      labs(title = paste(input$x_var, "vs", input$y_var),
           color = "Species") +
      theme(legend.position = "bottom")
  })
  
  output$histogram <- renderPlot({
    ggplot(iris, aes_string(x = input$x_var, fill = "Species")) +
      geom_histogram(bins = input$bins, alpha = 0.7, position = "identity") +
      theme_minimal() +
      labs(title = paste("Distribution of", input$x_var),
           fill = "Species") +
      theme(legend.position = "bottom")
  })
  
  output$bubble <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, size = Petal.Width, color = Species)) +
      geom_point() +
      scale_size_area(max_size = 10) + 
      theme_minimal() +
      labs(title = paste("Sepal.Length vs Petal.Length vs Petal.Width"),
           color = "Species") +
      theme(legend.position = "top")
  })
}

shinyApp(ui, server)
