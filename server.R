library(shiny)
library(hawkTrackHelp)

# Load data
load('cohorts.rdata')

# Color blind palette
colors <- c("#D55E00", "#0072B2", "#E69F00", "#009E73", "#999999", 
            "#F0E442", "#000000", "#56B4E9", "#CC79A7", "#999900") 

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
    if(input$demoAchieve == 'None') {reset('equityAchieve')}
  })
  
  # render a message based on cohort
  output$cohort2 <- renderUI({
    txt <- cohortMessage(input$cohort, input$definition)
    HTML(paste(txt))
  })
  
  



################################################################################

#                           Pick a cohort tab

################################################################################


output$ethnicity <- renderChart({
  
  # Pull selected cohort
  temp <- cohorts %>% filter(cohortyear == input$cohort & term == 1)
  
  # Specify filter column based on cohort selection
  names(temp)[names(temp) == input$definition] <- 'filt'
  
  plotSet <- temp %>%
    filter(!is.na(filt)) %>%
    group_by(ethnicity) %>%
    summarise(headcount = n()) %>%
    mutate(percent = headcount/sum(headcount))
  
  form <- formula(paste('percent ~', 'ethnicity'))
  
  n1 <- nPlot(form,
              data = plotSet,
              type = "discreteBarChart",
              width = session$clientData[["output_plot1_width"]])
  
  n1$addParams(dom = 'ethnicity')
  n1$chart(color = colors)
  return(n1)
  
})
  
})  
