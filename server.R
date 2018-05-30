library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  # toggle enrollment selections
  observe({
    if(input$affirmEnroll == 'Yes') {toggle(id = 'enrollSelect', anim = TRUE)}
    if(input$affirmEnroll == 'No') {hideElement(id = 'enrollSelect', anim = TRUE)}
  })
  
  # toggle comparison options
  observe({
    if(input$enroll != '[Select One]') 
      {toggle(id = 'enrollComp', anim = TRUE)}
    if(input$enroll == '[Select One]') 
      {hideElement(id = 'enrollComp', anim = TRUE)}
  })
  
  # render a message based on cohort
  output$cohort <- renderText({
    print(input$affirm1)
  })
  
})
