library(shiny)

# Load data
load('cohorts.rdata')

# Cohort definition choices for lookup
definition <- c('All' = 'emplid', 
                'Two Year Path' = 'twoyear', 
                'Three Year Path' = 'threeyear', 
                'Certificate Path' = 'cert', 
                'Deg/Trans/Cert Seeking' = 'degreeseek')

# Lookup for comparison type
compType <- c('None' = 'None', 
              'units' = 'avg', 
              'enrolled' = '%',
              'units12' = '%',
              'units15' = '%',
              'Math' = '%',
              'English' = '%',
              'wunits' = 'avg')

# Enrollment Lookup
enrollment <- c('[Select One]' = 'None', 
                'Average Units' = 'units', 
                '% Enrolled' = 'enrolled',
                '% Enrolled Full Time (12 units)' = 'units12',
                '% Enrolled Full Time (15 units)' = 'units15',
                '% Enrolled in math' = 'Math',
                '% Enrolled in English' = 'English',
                'Average Units Withdrawn' = 'wunits')

# Color blind palette
colors <- c("#D55E00", "#0072B2", "#E69F00", "#009E73", "#999999", 
            "#F0E442", "#000000", "#56B4E9", "#CC79A7", "#999900") 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
     
################################################################################
  
#                              REACTIVE UI
  
################################################################################
 
  
  # Get the current term for the selected cohort
  currentTerm <- reactive({
    term <- max(cohorts$term[cohorts$cohortyear == input$cohort])
    term
  })
  
  # Get the most recent term description
  currentTermDesc <- reactive({
    desc <- unique(cohorts$termdescr[cohorts$cohortyear == input$cohort &
                                       cohorts$term == currentTerm()]
    )
  })
  
  
  # Reactive UI for enrollment tab----------------------------------------------
  
  
  # toggle enrollment selections
  observe({
    if(input$affirmEnroll == 'Yes') 
      {toggle(id = 'enrollSelect', anim = TRUE)}
    if(input$affirmEnroll == 'No') 
      {hideElement(id = 'enrollSelect', anim = TRUE)}
  })
  
  # toggle comparison options
  observe({
    if(input$enroll != 'None') 
      {showElement(id = 'enrollComp', anim = TRUE)}
    if(input$enroll == 'None') 
      {hideElement(id = 'enrollComp', anim = TRUE)}
  })
  
  # toggle term selector
  observe({
    if(input$optionEnroll != 'years')
    {showElement(id = 'enrollTerm', anim = TRUE)}
    if(input$demoEnroll == 'years')
    {hideElement(id = 'enrollTerm', anim = TRUE)}
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
    if(input$enroll == 'None') {
      reset('demoEnroll')
      reset('optionEnroll')
    }
    if(input$demoEnroll == 'None') {reset('equityEnroll')}
    if(input$optionEnroll == 'years') {reset('termEnroll')}
  })
  
  # render a message based on cohort
  output$cohort1 <- renderUI({
    def <- names(definition)[definition == input$definition]
    txt <- cohortMessage(input$cohort, def)
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
    if(input$achieve != 'None') 
    {showElement(id = 'achieveComp', anim = TRUE)}
    if(input$achieve == 'None') 
    {hideElement(id = 'achieveComp', anim = TRUE)}
  })
  
  # toggle term selector
  observe({
    if(input$optionAchieve != 'years')
    {showElement(id = 'achieveTerm', anim = TRUE)}
    if(input$demoAchieve == 'years')
    {hideElement(id = 'achieveTerm', anim = TRUE)}
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
    txt <- cohortMessage(input$cohort, def)
    HTML(paste(txt))
  })


################################################################################

#                           Pick a cohort tab

################################################################################
  

  output$cohortSize <- renderUI({
    data <- cohorts %>% filter(cohortyear == input$cohort & term == 1)
    den <- data %>% summarise(headcount = n())
    names(data)[names(data) == input$definition] <- 'filt'
    num <- data %>% filter(!is.na(filt)) %>% summarise(headcount = n())
    msg <- paste('Displaying data for ', num[1, 1], ' out of ', den[1, 1],
                 ' students in the ', input$cohort, ' fall cohort.')
    
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
    text <- paste(currentTermDesc(), ' Enrollment Snapshot for the ', 
                  input$cohort,
                  ' Cohort')  
    
    HTML(paste(text))
  })
  
  
  output$enrollcompTitle <- renderUI({
    if (input$optionEnroll == 'years') {
      text <- paste('Trends for the ', input$cohort, ' Cohort')
    }

    if (input$optionEnroll == 'cohorts') {
      text <- paste(input$cohort, ' Compared to Four Previous Cohorts ',
                    'in the ', createTermString(input$termEnroll), ' Term')
    }
    
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

  output$enrollCompPlt <- renderChart({
    
    type <- compType[input$enroll]
    type <- ifelse(input$equityEnroll == 'Yes', '%', type)
    
    temp <- outcomeDisag(input$enroll,
                         input$optionEnroll,
                         input$cohort,
                         input$definition,
                         input$termEnroll,
                         input$equityEnroll,
                         input$demoEnroll,
                         data = cohorts,
                         type = type)
    
    
    yax <- c(0,100)
    yax[yax == 100 & type != '%'] <- max(temp$outcome) + 3
    
    title <- names(enrollment)[enrollment == input$enroll]
    
    if (input$demoEnroll == 'None') {
      
      tooltip <- ifelse(type == '%', 'bar', 'baravg')
      
      n1 <- nPlot(outcome ~ order,
                  data = temp,
                  type = "discreteBarChart",
                  width = session$clientData[["output_plot6_width"]]) 

      n1$yAxis(axisLabel = title, 
               width = 50)
      n1$chart(color = colors,
               forceY = yax,
               tooltipContent = makeDemoToolTip(tooltip))
    }
    
    if (input$demoEnroll != 'None' & input$equityEnroll == 'No') {
      
      tooltip <- ifelse(type == '%', 'demo', 'demoavg')
      
      n1 <- nPlot(outcome ~ demo, group = "order", 
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot6_width"]])
      
      n1$yAxis(axisLabel = title, 
               width = 50)
      n1$xAxis(rotateLabels = -25)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = yax,
               tooltipContent = makeDemoToolTip(tooltip))
    }
    
    if (input$demoEnroll != 'None' & input$equityEnroll == 'Yes') {
      n1 <- nPlot(outcome ~ demo, group = "order", 
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot6_width"]])
      
      n1$yAxis(axisLabel = title, 
               width = 50)
      n1$xAxis(rotateLabels = -25)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = c(floor(min(temp$outcome)) * .9, 
                          floor(max(temp$outcome)) * 1.1),
               tooltipContent = makeDemoToolTip('equity'))
    }
    
    n1$addParams(dom = 'enrollCompPlt')
    return(n1) 
  })
  
  
  ################################################################################
  
  #                            Achievements TAB
  
  ################################################################################
  
  
  output$achTitle <- renderUI({
    text <- paste(currentTermDesc(), ' Enrollment Snapshot for the ', 
                  input$cohort,
                  ' Cohort')  
    
    HTML(paste(text))
  })
  
  
  output$achcompTitle <- renderUI({
    if (input$optionAchieve == 'years') {
      text <- paste('Trends for the ', input$cohort, ' Cohort')
    }
    
    if (input$optionAchieve == 'cohorts') {
      text <- paste(input$cohort, ' Compared to Four Previous Cohorts ',
                    'in the ', createTermString(input$termAchieve), ' Term')
    }
    
    HTML(paste(text))
  })
  
  
  observe({
    if (input$affirmAchieve == 'No' | input$achieve == 'None') {
      showElement(id = 'achsnapshot', anim = TRUE)
      hideElement(id = 'achcompare', anim = TRUE)
    }
    
    if (input$affirmAchieve == 'Yes' & input$achieve != 'None') {
      showElement(id = 'achcompare', anim = TRUE)
      hideElement(id = 'achsnapshot', anim = TRUE)
    }
  })  
  
})  
