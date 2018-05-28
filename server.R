library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$cohort <- renderText({
    print(input$cohort)
  })
  
})
