library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  observe({
    if(input$affirm1 == 'Yes') {toggle(id = 'advanced', anim = TRUE)}
    if(input$affirm1 == 'No') {hideElement(id = 'advanced', anim = TRUE)}
  })
  
  output$cohort <- renderText({
    print(input$affirm1)
  })
  
})
