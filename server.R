library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  observeEvent(input$affirm1,{toggle(id = 'advanced', anim = TRUE)})
  output$cohort <- renderText({
    print(input$cohort)
  })
  
})
