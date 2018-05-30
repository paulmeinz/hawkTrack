library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  # toggle enrollment selections
  observe({
    if(input$affirmEnroll == 'Yes') {toggle(id = 'enrollSelect', anim = TRUE)}
    if(input$affirmEnroll == 'No') {
      hideElement(id = 'enrollSelect', anim = TRUE)
    }
  })
  
  # toggle comparison options
  observe({
    if(input$enroll != '[Select One]') 
      {showElement(id = 'enrollComp', anim = TRUE)}
    if(input$enroll == '[Select One]') 
      {hideElement(id = 'enrollComp', anim = TRUE)}
  })
  
  # toggle equity selector
  observe({
    if(input$demoEnroll != 'None')
      {showElement(id = 'enrollEquity', anim = TRUE)}
    if(input$demoEnroll == 'None')
      {hideElement(id = 'enrollEquity', anim = TRUE)}
  })
  
  # resets
  observe({
    if(input$affirmEnroll == 'No') {reset('enroll')}
    if(input$enroll == '[Select One]') {
      reset('demoEnroll')
      reset('optionEnroll')
    }
    if(input$demoEnroll == 'None') {reset('equityEnroll')}
  })
  
  # render a message based on cohort
  output$cohort <- renderText({
    print(input$affirm1)
  })
  
})
