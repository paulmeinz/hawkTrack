library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  # toggle enrollment selections
  
  # toggle comparison options
  observe({
    if(input$affirm1 == 'Yes') {toggle(id = 'enrollSelect', anim = TRUE)}
    if(input$affirm1 == 'No') {hideElement(id = 'enrolSelect', anim = TRUE)}
  })
  
  # render a message based on cohort
  output$cohort <- renderText({
    print(input$affirm1)
  })
  
})
