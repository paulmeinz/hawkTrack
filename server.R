library(shiny)

# Load data
load('cohorts.rdata')

# Choices for lookup
definition <- c('All' = 'emplid', 
                'Two Year Path' = 'twoyear', 
                'Three Year Path' = 'threeyear', 
                'Certificate Path' = 'cert', 
                'Deg/Trans/Cert Seeking' = 'degreeseek')

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
    if(input$enroll != 'None') 
      {showElement(id = 'enrollComp', anim = TRUE)}
    if(input$enroll == 'None') 
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
    def <- names(definition)[definition == input$definition]
    txt <- cohortMessage(input$cohort, def, 
                         createTermString(currentTerm()))
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
    def <- names(definition)[definition == input$definition]
    txt <- cohortMessage(input$cohort, def, 
                         createTermString(currentTerm()))
    HTML(paste(txt))
  })


################################################################################

#                           Pick a cohort tab

################################################################################
  
  
  currentTerm <- reactive({
    term <- max(cohorts$term[cohorts$cohortyear == input$cohort])
    term
  })
  
  
  output$cohortSize <- renderUI({
    data <- cohorts %>% filter(cohortyear == input$cohort & term == 1)
    den <- data %>% summarise(headcount = n())
    names(data)[names(data) == input$definition] <- 'filt'
    num <- data %>% filter(!is.na(filt)) %>% summarise(headcount = n())
    msg <- paste('Displaying data for ', num[1, 1], ' out of ', den[1, 1],
                 ' students in the ', input$cohort, ' fall cohort.',
                 'This cohort is currently enrolled in their',
                 createTermString(currentTerm()), ' term.')
    
    HTML(paste(msg))
  })
  
  
  output$def <- renderUI({
    if (input$definition == 'emplid') {
      text <- "Students who were first time new at CRC in the fall with 
      no experience in the District prior to summer (aside from dual 
      enrollment)."
    }
    
    if (input$definition == 'twoyear') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment) 
      who in their first term: 1) Enrolled in 15 or more units, 2) declared a
      major, 3) enrolled in math and English, 4) and completed a comprehensive
      educational plan."
    }
    
    if (input$definition == 'threeyear') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment) 
      who in their first term: 1) Enrolled in 6 to 14.9 units, 2) declared a
      major, 3) enrolled in math or English, 4) and completed a comprehensive
      educational plan."
    }
    
    if (input$definition == 'cert') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment) 
      who in their first term: 1) Declared a major, 2) completed a 
      comprehensive educational plan, and enrolled in at least 3 units in the
      same 2 digit cip code as their major"
    }
    
    if (input$definition == 'degreeseek') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment) 
      who have completed at least 6 units at CRC and attempted math or English"
    }
    
    intro <- '<strong> Cohort Definition: </strong>'
    text <- paste(intro, text, sep = '')
    
    HTML(paste(text))
    
  })
  
  
  output$ethnicity <- renderChart({
  
    # Pull selected cohort data
    plotSet <- cohortSelectData(input$cohort, input$definition, 'ethnicity',
                                cohorts)
  
    form <- formula(paste('percent ~', 'ethnicity'))
    
    # Make a plot
    n1 <- nPlot(form,
                data = plotSet,
                type = "discreteBarChart",
                width = session$clientData[["output_plot1_width"]])
 
    # Do some aesthetic stuff
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip())
    n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
             width = 50)
    n1$xAxis(rotateLabels = -25)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = tooltip)
    
    
    # Display the chart
    n1$addParams(dom = 'ethnicity')
    return(n1)
  
  })
  
 
  output$gender <- renderChart({
    
    # Pull selected cohort data
    plotSet <- cohortSelectData(input$cohort, input$definition, 'gender',
                                cohorts)
    
    form <- formula(paste('percent ~', 'gender'))
    
    n1 <- nPlot(form,
                data = plotSet,
                type = "pieChart",
                width = session$clientData[["output_plot2_width"]])
    
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip(type = 'pie'))
    n1$addParams(dom = 'gender')
    n1$chart(color = colors,
             donut = TRUE,
             tooltipContent = tooltip,
             showLabels = FALSE)
    return(n1)
    
  }) 
  
 
  output$age <- renderChart({
    
    # Pull selected cohort data
    plotSet <- cohortSelectData(input$cohort, input$definition, 'age',
                                cohorts)
    
    form <- formula(paste('percent ~', 'age'))
    
    # Make a plot
    n1 <- nPlot(form,
                data = plotSet,
                type = "discreteBarChart",
                width = session$clientData[["output_plot3_width"]])
    
    # Do some aesthetic stuff
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip())
    n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
             width = 50)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = tooltip)
    
    
    # Display the chart
    n1$addParams(dom = 'age')
    return(n1)
    
  })
  
  
  output$special <- renderChart ({
    dsps <- cohortSelectData(input$cohort, input$definition, 'dsps', cohorts)
    veteran <- cohortSelectData(input$cohort, input$definition, 'veteran',
                                cohorts)
    foster <- cohortSelectData(input$cohort, input$definition, 'foster', 
                               cohorts)
    
    names(dsps)[1] <- 'demo'
    names(veteran)[1] <- 'demo'
    names(foster)[1] <- 'demo'
    
    plotSet <- data.frame(rbind(foster, veteran, dsps))
    plotSet[is.na(plotSet)] <- 0
    plotSet <- plotSet %>% filter(demo %in% c('Foster Youth', 'Veteran',
                                              'Reported Disability'))
    
    n1 <- nPlot(percent ~ demo,
                data = plotSet,
                type = "discreteBarChart",
                width = session$clientData[["output_plot4_width"]])

    # Do some aesthetic stuff
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip())
    n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
             width = 50)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = tooltip)
    n1$set(title = 'Current enrollment snapshot')
    
    
    # Display the chart
    n1$addParams(dom = 'special')
    return(n1)      
  })


################################################################################
  
#                            CURRENT ENROLLMENT TAB

################################################################################
  
  
  output$enrollTitle <- renderUI({
    text <- paste('Current Enrollment Snapshot for the ', input$cohort,
                  ' Cohort')  
    
    HTML(paste(text))
  })
  
  
  observe({
    if (input$affirmEnroll == 'No' | input$enroll == 'None') {
      showElement(id = 'snapshot', anim = TRUE)
      hideElement(id = 'compare', anim = TRUE)
    }
    
    if (input$affirmEnroll == 'Yes' & input$enroll != 'None') {
      showElement(id = 'compare', anim = TRUE)
      hideElement(id = 'snapshot', anim = TRUE)
    }
  })
  
  
  # SNAPSHOT Plot---------------------------------------------------------------
  
  
  output$enrollPerc <- renderChart({
    
    # Filter based on input
    temp <- cohorts[cohorts$cohortyear == input$cohort & 
                    cohorts$term == currentTerm(),]
    
    # Calculate percentage variables
    percent <- temp %>% summarise('% Enrolled' = mean(enrolled) * 100,
                                  '% Fulltime (12 Units)' = mean(units12) * 100, 
                                  '% Fulltime (15 Units)' = mean(units15) * 100,
                                  'Enrolled in English' = mean(English) * 100,
                                  'Enrolled in math' = mean(Math) * 100)
    percent <- gather(percent, 'variable', 'percent', 1:5)
    
    headcount <- temp %>% summarise('% Enrolled' = sum(enrolled),
                                    '% Fulltime (12 Units)' = sum(units12),
                                    '% Fulltime (15 Units)' = sum(units15),
                                    'Enrolled in English' = sum(English),
                                    'Enrolled in math' = sum(Math))
    headcount <- gather(headcount, 'variable', 'headcount', 1:5)
    
    total <- temp %>% summarise(total = n())
    total <- total[1,1]
    
    percent <- percent %>% left_join(headcount)
    percent <- data.frame(percent)
    percent$total <- total
    
    # Percentage Plot
    n1 <- nPlot(percent ~ variable,
                data = percent,
                type = "discreteBarChart",
                width = session$clientData[["output_plot5_width"]])
    
    n1$yAxis(axisLabel = 'Proportion of UNDUPLICATED Students (%)', 
             width = 50)
    n1$xAxis(rotateLabels = -15)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = makeDemoToolTip())
    
    # Display the chart
    n1$addParams(dom = 'enrollPerc')
    return(n1) 
  })
  
  
  # ENROLLMENT COMPARISON PLOT--------------------------------------------------

})  
