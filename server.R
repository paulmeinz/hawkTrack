library(shiny)
library(hawkTrackHelp)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  
################################################################################
  
#                              REACTIVE UI
  
################################################################################
  
  
  # Reactive UI for enrollment tab----------------------------------------------
  
  
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
  output$cohort1 <- renderUI({
    txt <- cohortMessage(input$cohort, input$definition)
    HTML(paste(txt))
  })
  
  
  #Reactive UI for achievements tab---------------------------------------------
  
  
  # toggle enrollment selections
  observe({
    if(input$affirmAchieve == 'Yes') {toggle(id = 'achieveSelect', anim = TRUE)}
    if(input$affirmAchieve == 'No') {
      hideElement(id = 'achieveSelect', anim = TRUE)
    }
  })
  
  # toggle comparison options
  observe({
    if(input$achieve != '[Select One]') 
    {showElement(id = 'achieveComp', anim = TRUE)}
    if(input$achieve == '[Select One]') 
    {hideElement(id = 'achieveComp', anim = TRUE)}
  })
  
  # toggle equity selector
  observe({
    if(input$demoAchieve != 'None')
    {showElement(id = 'achieveEquity', anim = TRUE)}
    if(input$demoAchieve == 'None')
    {hideElement(id = 'achieveEquity', anim = TRUE)}
  })
  
  # resets
  observe({
    if(input$affirmAchieve == 'No') {reset('achieve')}
    if(input$achieve == '[Select One]') {
      reset('demoAchieve')
      reset('optionAchieve')
    }
    if(input$demoEnroll == 'None') {reset('equityAchieve')}
  })
  
  # render a message based on cohort
  output$cohort2 <- renderUI({
    txt <- cohortMessage(input$cohort, input$definition)
    HTML(paste(txt))
  })
  
  
})
