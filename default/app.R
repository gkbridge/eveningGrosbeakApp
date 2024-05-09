library(shiny)

ui <- fluidPage(
  titlePanel("Iris"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-axis Variable", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
      selectInput("y_var", "Y-axis Variable", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
    ),
    mainPanel(
      plotOutput("histogram")
    )
  )
)

server <- function(input, output) {
  
  output$histogram <- renderPlot({
    summary(iris[[input$x_var]])
    scatter.smooth(iris[[input$x_var]], main = paste("Scatterplot of", input$x_var))
  })
}

shinyApp(ui = ui, server = server)