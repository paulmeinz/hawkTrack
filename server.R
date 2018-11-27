library(shiny)

# Load data
load('cohorts.rdata')

# Cohort definition choices for lookup, I'll probably pull these from a 
# db or R file at a later date but for now....
definition <- c('All' = 'emplid',
                'Two Year Path' = 'twoyear',
                'Three Year Path' = 'threeyear',
                'Certificate Path' = 'cert',
                'Deg/Trans/Cert Seeking' = 'degreeseek',
                'Recent High School' = 'recenths')

# Lookup for comparison type
compType <- c('None' = 'None',
              'units' = 'avg',
              'enrolled' = '%',
              'gpa' = 'avg',
              'units12' = '%',
              'units15' = '%',
              'Math' = '%',
              'English' = '%',
              'wunits' = 'avg',
              'comprehensive' = '%',
              'cumTransEnglish' = '%',
              'cumTransMath' = '%',
              'mile15' = '%',
              'mile30' = '%',
              'mile45' = '%',
              'mile60' = '%',
              'compcum' = '%')

# Enrollment Lookup
enrollment <- c('[Select One]' = 'None',
                'Average Units' = 'units',
                '% Enrolled' = 'enrolled',
                '% Enrolled Full Time (12 units)' = 'units12',
                '% Enrolled Full Time (15 units)' = 'units15',
                '% Enrolled in math' = 'Math',
                '% Enrolled in English' = 'English',
                'Average Units Withdrawn' = 'wunits')

# Mileston Lookup
milestones <- c('[Select One]' = 'None',
                'Average GPA' = 'gpa',
                '% Comp Ed Plan' = 'comprehensive',
                '% Transfer English' = 'cumTransEnglish',
                '% Transfer Math' = 'cumTransMath',
                '% 15 Transfer Units' = 'mile15',
                '% 30 Transfer Units' = 'mile30',
                '% 45 Transfer Units' = 'mile45',
                '% 60 Transfer Units' = 'mile60',
                '% Completion' = 'compcum'
)

# Demo lookup
demos <- c(None = 'None', 
           Age = 'age',
           'Calworks' = 'calworks',
           'CARE' = 'care',
           'Diop' = 'diop',
           'EGUSD Students' = 'egusd', 
           'EOPS' = 'eops',
           Ethnicity = 'ethnicity',
           'First Generation' = 'firstgen',
           'Foster Youth' = 'foster', 
           'First Year Experience' = 'fye',
           Gender = 'gender',
           'Puente' = 'puente',
           'Reported Disability' = 'dsps', 
           'Veteran Status' = 'veteran')

# Color blind palette
colors <- c("#E87722", "#001A72", "#E69F00", "#009E73", "#999999",
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
  
  # Reset enrollment/achieve tabs if a cohort is selected
  observeEvent(input$cohort, {
    reset('affirmEnroll')
    reset('affirmAchieve')
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
  
  # Update term input selections based on cohort selected
  observe({
    terms <- unique(cohorts$term[cohorts$cohortyear == input$cohort])
    names(terms) <- createTermString(terms)
    terms <- terms[order(terms)]
    
    # Add asterisks to terms
    actTerms <- cohorts[cohorts$cohortyear == input$cohort
                        & cohorts$livestatusenroll == 'Live', 'term']
    names(terms)[terms %in% actTerms] <- paste(names(terms)[terms %in% actTerms],
                                               '*', sep = '')
    
    # Label the "Current" term or "Last Term"
    a <- grepl('[*]', names(terms)[length(names(terms))])
    add <- ifelse(a == TRUE, ' (Current Term)', ' (Last Tracked Term)')
    names(terms)[length(terms)] <- paste(names(terms)[length(terms)], 
                                         add , sep = '')
    
    updateSelectInput(session, 'termEnroll',
                      label = 'Select a term',
                      choices = terms,
                      selected = max(terms))
  })
  
  # Update enrollment filter options
  observe({
    if(input$filtEnroll != 'None') {
      choices <- as.character(cohorts[cohorts$term == input$termEnroll &
                                      cohorts$cohortyear == input$cohort, 
                                      input$filtEnroll])
      updateSelectInput(session, 'filtOptionEnroll',
                        choices = unique(choices))
    } else {
      updateSelectInput(session, 'filtOptionEnroll', choices = 'None')
    }
  })

  # Toggle select specific filter menu and edit filter options
  observe({
    if(input$filtEnroll != 'None')
      {showElement(id = 'filtE', anim = FALSE)}
    if(input$filtEnroll == 'None')
      {hideElement(id = 'filtE', anim = FALSE)}
  })
  
  # toggle equity selector
  observe({
    if(input$demoEnroll != 'None' & input$filtEnroll == 'None')
      {showElement(id = 'enrollEquity', anim = TRUE)}
    if(input$demoEnroll == 'None' | input$filtEnroll != 'None')
      {hideElement(id = 'enrollEquity', anim = TRUE)}
  })

  # resets
  observe({
    if(input$affirmEnroll == 'No') {reset('enroll')}
    if(input$enroll == 'None') {
      reset('demoEnroll')
      reset('optionEnroll')
      reset('filtEnroll')
    }
    if(input$demoEnroll == 'None' | input$filtEnroll != 'None') {
      reset('equityEnroll')
    }
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
  
  # Update term input selections based on cohort selected
  observe({
    terms <- unique(cohorts$term[cohorts$cohortyear == input$cohort])
    names(terms) <- createTermString(terms)
    terms <- terms[order(terms)]
    
    # Add asterisks to terms
    actTerms <- cohorts[cohorts$cohortyear == input$cohort
                        & cohorts$livestatuscomp == 'Live', 'term']
    names(terms)[terms %in% actTerms] <- paste(names(terms)[terms %in% actTerms], 
                                               '*', sep = '')
    
    # Label the "Current" term or "Last Term"
    a <- grepl('[*]', names(terms)[length(names(terms))])
    add <- ifelse(a == TRUE, ' (Current Term)', ' (Last Tracked Term)')
    names(terms)[length(terms)] <- paste(names(terms)[length(terms)], 
                                         add , sep = '')
    
    # Update choices
    updateSelectInput(session, 'termAchieve',
                      label = 'Select a term',
                      choices = terms,
                      selected = max(terms))
  })

  # Update achievement filter options
  observe({
    if(input$filtAchieve != 'None') {
      choices <- as.character(cohorts[cohorts$term == input$termAchieve &
                                      cohorts$cohortyear == input$cohort, 
                                      input$filtAchieve])
      updateSelectInput(session, 'filtOptionAchieve',
                        choices = unique(choices))
    } else {
      updateSelectInput(session, 'filtOptionAchieve', choices = 'None')
    }
    
  })
  
  # toggle term selector
  observe({
    if(input$optionAchieve != 'years')
      {showElement(id = 'achieveTerm', anim = TRUE)}
    if(input$optionAchieve == 'years')
      {hideElement(id = 'achieveTerm', anim = TRUE)}
  })
  
  # Toggle select specific filter menu and edit filter options
  observe({
    if(input$filtAchieve != 'None')
      {showElement(id = 'filtA', anim = FALSE)}
    if(input$filtAchieve == 'None')
      {hideElement(id = 'filtA', anim = FALSE)}
  })

  # toggle equity selector
  observe({
    if(input$demoAchieve != 'None' & input$filtAchieve == 'None')
      {showElement(id = 'achieveEquity', anim = TRUE)}
    if(input$demoAchieve == 'None' | input$filtAchieve != 'None')
      {hideElement(id = 'achieveEquity', anim = TRUE)}
  })


  # resets
  observe({
    if(input$affirmAchieve == 'No') {reset('achieve')}
    if(input$achieve == 'None') {
      reset('demoAchieve')
      reset('optionAchieve')
      reset('filtAchieve')
    }
    if(input$demoAchieve == 'None' | input$filtAchieve != 'None') {
      reset('equityAchieve')
    }
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


  # Display the number of students in a selected cohort
  output$cohortSize <- renderUI({
    
    # pull cohort year data
    data <- cohorts %>% filter(cohortyear == input$cohort & term == 1)
    den <- data %>% summarise(headcount = n()) # get headcount
    
    # label cohort definition and filter
    names(data)[names(data) == input$definition] <- 'filt'
    num <- data %>% filter(!is.na(filt)) %>% summarise(headcount = n())
    
    # write html
    msg <- paste('Displaying data for ', 
                 tolower(names(definition)[definition == input$definition]), 
                 ' students in the Cosumnes River College ', 
                 ' fall ', input$cohort, ' cohort - ', 
                 '<strong>', num[1, 1], '</strong>', ' out of ', 
                 '<strong>', den[1, 1], '</strong>',
                 ' new students.' 
                 )

    HTML(paste(msg))
  })


  # Cohort definitions that get displayed in the upper right
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
      same 2 digit cip code as their major."
    }

    if (input$definition == 'degreeseek') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment)
      who have completed at least 6 units at CRC and attempted math or English."
    }
    
    if (input$definition == 'recenths') {
      text <- "Students who were first time new at CRC in the fall with no
      experience in the District prior to summer (aside from dual enrollment)
      who graduated high school in the prior (spring) term."
    }

    intro <- '<strong> Cohort Definition: </strong>'
    text <- paste(intro, text, sep = '')

    HTML(paste(text))

  })


  # Ethnicity plot
  output$ethnicity <- renderChart({

    # Pull selected cohort data
    plotSet <- cohortSelectData(input$cohort, input$definition, 'ethnicity',
                                cohorts)

    form <- formula(paste('percent ~', 'ethnicity')) #ugly code

    # Make a plot
    n1 <- nPlot(form,
                data = plotSet,
                type = "discreteBarChart",
                width = session$clientData[["output_plot1_width"]])

    # Do some aesthetic stuff
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip())
    n1$yAxis(axisLabel = 'Percent of Students',
             width = 50)
    n1$xAxis(rotateLabels = -15)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = tooltip)


    # Display the chart
    n1$addParams(dom = 'ethnicity')
    return(n1)

  })


  # Gender plot
  output$gender <- renderChart({

    # Pull selected cohort data
    plotSet <- cohortSelectData(input$cohort, input$definition, 'gender',
                                cohorts)

    form <- formula(paste('percent ~', 'gender'))

    n1 <- nPlot(form,
                data = plotSet,
                type = "pieChart",
                width = session$clientData[["output_plot2_width"]])

    # Aesthetics
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip(type = 'pie'))
    n1$addParams(dom = 'gender')
    n1$chart(color = colors,
             donut = TRUE,
             tooltipContent = tooltip,
             showLabels = FALSE)
    return(n1)

  })


  # Other demographics plot
  output$special <- renderChart ({
    
    # Calc percentages for each demo in the plot
    age <- cohortSelectData(input$cohort, input$definition, 'age', cohorts)
    firstgen <- cohortSelectData(input$cohort, input$definition, 'firstgen', 
                                 cohorts)
    dsps <- cohortSelectData(input$cohort, input$definition, 'dsps', cohorts)
    veteran <- cohortSelectData(input$cohort, input$definition, 'veteran',
                                cohorts)
    foster <- cohortSelectData(input$cohort, input$definition, 'foster',
                               cohorts)
    
    # chenge the demographic column name so theyll stack
    names(age)[1] <- 'demo'
    names(firstgen)[1] <- 'demo'
    names(dsps)[1] <- 'demo'
    names(veteran)[1] <- 'demo'
    names(foster)[1] <- 'demo'
    
    # stack em
    plotSet <- data.frame(rbind(age, firstgen, foster, veteran, dsps))
    
    # replace all NA values with 0
    plotSet[is.na(plotSet)] <- 0
    
    # Only select certain groups (theyre all binary so only one level is nec.)
    plotSet <- plotSet %>% filter(demo %in% c('Foster Youth', 'Veteran',
                                              'Reported Disability',
                                              '24 and younger',
                                              'First Generation'))
    # Order by demo
    plotSet <- plotSet[order(as.character(plotSet$demo)),]

    # make plot
    n1 <- nPlot(percent ~ demo,
                data = plotSet,
                type = "discreteBarChart",
                width = session$clientData[["output_plot4_width"]])

    # Do some aesthetic stuff
    tooltip <- gsub("[\r\n]", "", makeDemoToolTip())
    n1$yAxis(axisLabel = 'Percent of Students',
             width = 50)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = tooltip)
    n1$xAxis(rotateLabels = -15)
    n1$set(title = 'Current enrollment snapshot')


    # Display the chart
    n1$addParams(dom = 'special')
    return(n1)
  })


################################################################################

#                            CURRENT ENROLLMENT TAB

################################################################################


  # Create the enrollment plot title
  output$enrollTitle <- renderUI({
    
    # Filter term and cohort
    desc <- unique(cohorts$termdescr[cohorts$cohortyear == input$cohort &
                                       cohorts$term == input$termEnroll]
    )
    
    # Create enrollment plot title
    def <- names(definition)[definition == input$definition]
    text <- paste(input$cohort, ' Cohort: Enrollment Snapshot, ', desc, '<br/>',
                  '(', createTermString(input$termEnroll), ' term, ', 
                  tolower(def),
                  ' students)', sep = '')

    HTML(paste(text))
  })


  # Create comparison plot title
  output$enrollcompTitle <- renderUI({
    
    def <- names(definition)[definition == input$definition]
    # title depends on the particular comparison selected
    if (input$optionEnroll == 'years') {
      desc <- unique(cohorts$termdescr[cohorts$cohortyear == input$cohort &
                                         cohorts$term == input$termEnroll]
      )
      text <- paste(input$cohort, 'Cohort: Trends on or before the ', 
                    createTermString(input$termEnroll), ' term (',
                    desc, ')', '<br/>',
                    '(', tolower(def), ' students)',
                    sep = '')
    }

    if (input$optionEnroll == 'cohorts') {
      text <- paste(input$cohort, ' Compared to Four Previous Cohorts ',
                    'in their ', createTermString(input$termEnroll), ' Term',
                    '<br/>', '(', tolower(def), ' students)', sep = '')
    }

    HTML(paste(text))
  })


  # hide and show the snapshot/comparison plot depending on the selections
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
  
  # Hide or show warning message based on active data status
  observe({
    term <- input$termEnroll
    
    # This is wonky but when a new term is selected
    # The selection values arent updated quick enough...
    # resulting in a crash if you switch from an early cohort to a leter
    x <- cohorts %>% 
      filter(cohortyear == input$cohort) %>%
      summarise(max(term))
    x <- x[1,1]
    
    # So compare the max sterm for a particular cohort to the input
    # and replace if the max strm is higher
    if(term > x) {term <- x}
    
    active <- unique(cohorts[cohorts$term == term &
                             cohorts$cohortyear == input$cohort,
                             'livestatusenroll'])
    
    # if the status is live then show the warning message box
    if (active == 'Live') {
      showElement(id = 'enwarn', anim = TRUE)
    }
    
    if (active == 'Archive') {
      hideElement(id = 'enwarn', anim = TRUE)
    }
  })
  
  # Warning message for live data
  output$warn1 <- renderUI({
    text <- '*Data from the selected term is NOT FINALIZED and may change
      on a day to day basis'
    
    HTML(paste(text))
  })
  
  # Hide or show warning message based on active data status (comparison pane)
  observe({
    term <- input$termEnroll
    
    # see previous comments... could make this into a helper function
    x <- cohorts %>% 
      filter(cohortyear == input$cohort) %>%
      summarise(max(term))
    x <- x[1,1]
    
    if(term > x) {term <- x}
    
    active <- unique(cohorts[cohorts$term == term &
                             cohorts$cohortyear == input$cohort,
                             'livestatusenroll'])

    if (active == 'Live') {
      showElement(id = 'enwarn2', anim = TRUE)
    }
    
    if (active == 'Archive') {
      hideElement(id = 'enwarn2', anim = TRUE)
    }
  })
  
  # Warning message for live data
  output$warn2 <- renderUI({
    text <- '*Some data in this plot is NOT FINALIZED and may change
      on a day to day basis. Incomplete data is indicated by an asterisk (*)
      on an X-Axis label'
    
    HTML(paste(text))
  })


  # SNAPSHOT Plot---------------------------------------------------------------


  output$enrollPerc <- renderChart({

    # Filter based on input
    temp <- cohorts[cohorts$cohortyear == input$cohort &
                    cohorts$term == input$termEnroll,]

    # Filter cohort definition
    names(temp)[names(temp) == input$definition] <- 'filt'
    temp <- temp[!is.na(temp$filt),]

    # Calculate percentage variables
    percent <- temp %>% summarise('% Enrolled' = mean(enrolled) * 100,
                                  '% Fulltime (12 Units)' = mean(units12) * 100,
                                  '% Fulltime (15 Units)' = mean(units15) * 100,
                                  'Enrolled in English' = mean(English) * 100,
                                  'Enrolled in math' = mean(Math) * 100)
    percent <- gather(percent, 'variable', 'percent', 1:5)

    # calculate headcounts
    headcount <- temp %>% summarise('% Enrolled' = sum(enrolled),
                                    '% Fulltime (12 Units)' = sum(units12),
                                    '% Fulltime (15 Units)' = sum(units15),
                                    'Enrolled in English' = sum(English),
                                    'Enrolled in math' = sum(Math))
    headcount <- gather(headcount, 'variable', 'headcount', 1:5)

    # get the total and assign it to each row
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

    n1$yAxis(axisLabel = 'Percent of Students',
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
    
    # this helps determin the tooltip type
    type <- ifelse(input$equityEnroll == 'Yes', '%', type)

    # disaggregate function
    temp <- outcomeDisag(input$enroll,
                         input$optionEnroll,
                         input$cohort,
                         input$definition,
                         input$termEnroll,
                         input$filtEnroll,
                         input$filtOptionEnroll,
                         input$equityEnroll,
                         input$demoEnroll,
                         data = cohorts,
                         type = type)
    
    temp <- activeData(temp, input$termEnroll, cohorts, type = 'enroll')

    # default x axis
    yax <- c(0,100)
    
    # x axis if it isnt a percentage plot
    yax[yax == 100 & type != '%'] <- max(temp$outcome) + 3

    # input name is the title
    title <- names(enrollment)[enrollment == input$enroll]

    # plot if no demo is selected
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

    # plot if demo selected but equity evaluation is not selected
    if (input$demoEnroll != 'None' & input$equityEnroll == 'No') {

      tooltip <- ifelse(type == '%', 'demo', 'demoavg')

      n1 <- nPlot(outcome ~ demo, group = "order",
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot6_width"]])

      n1$yAxis(axisLabel = title,
               width = 50)
      n1$xAxis(rotateLabels = -15)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = yax,
               tooltipContent = makeDemoToolTip(tooltip))
    }

    # plot if demo selected and equity eveluation selected
    if (input$demoEnroll != 'None' & input$equityEnroll == 'Yes') {
      n1 <- nPlot(outcome ~ demo, group = "order",
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot6_width"]])

      n1$yAxis(axisLabel = title,
               width = 50)
      n1$xAxis(rotateLabels = -15)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = c(floor(min(temp$outcome)) * .9,
                          floor(max(temp$outcome)) * 1.1),
               tooltipContent = makeDemoToolTip('equity',
                                                compType[input$enroll]))
    }

    n1$addParams(dom = 'enrollCompPlt')
    return(n1)
  })


  ################################################################################

  #                            Achievements TAB

  ################################################################################


  # generate the title of the snapshot plot depending on term/cohort
  output$achTitle <- renderUI({
    desc <- unique(cohorts$termdescr[cohorts$cohortyear == input$cohort &
                                       cohorts$term == input$termAchieve]
    )
    def <- names(definition)[definition == input$definition]
    text <- paste(input$cohort, ' Cohort: Milestone Snapshot, ', desc,'<br/>',
                  '(', createTermString(input$termEnroll), ' term, ', 
                  tolower(def),
                  ' students)', sep = '')
    
    HTML(paste(text))
  })

  # comparison plot title
  output$achcompTitle <- renderUI({
    
    def <- names(definition)[definition == input$definition]
    # create a comparison title based on the comparison selected
    if (input$optionAchieve == 'years') {
      desc <- unique(cohorts$termdescr[cohorts$cohortyear == input$cohort &
                                         cohorts$term == input$termAchieve]
      )
      text <- paste(input$cohort, 'Cohort: Trends on or before the ', 
                    createTermString(input$terAchieve), ' term (',
                    desc, ')', '<br/>', '(', tolower(def), ' students)'
                    , sep = '')
    }

    if (input$optionAchieve == 'cohorts') {
      text <- paste(input$cohort, ' Compared to Four Previous Cohorts ',
                    'in their ', createTermString(input$termAchieve), ' Term',
                    '<br/>', '(', tolower(def), ' students)', sep = '')
    }

    HTML(paste(text))
  })

  # show/hide plots depending on selections
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
  
  # Hide or show warning message based on active data status
  observe({
    term <- input$termAchieve
    
    # see previous explanations of this code
    # basically ensures that the term selection exists
    x <- cohorts %>% 
      filter(cohortyear == input$cohort) %>%
      summarise(max(term))
    x <- x[1,1]
    
    if(term > x) {term <- x}
    
    active <- unique(cohorts[cohorts$term == term &
                               cohorts$cohortyear == input$cohort,
                             'livestatuscomp'])

    # display warning selection depending on term active status
    if (active == 'Live') {
      showElement(id = 'acwarn', anim = TRUE)
    }
    
    if (active == 'Archive') {
      hideElement(id = 'acwarn', anim = TRUE)
    }
  })
  
  # Warning message for live data
  output$warn3 <- renderUI({
    text <- '*Data from the selected term is NOT FINALIZED and may change
    on a day to day basis'
    
    HTML(paste(text))
  })
  
  # Hide or show warning message based on active data status (comparison pane)
  observe({
    term <- input$termAchieve
    
    # oof shoulda made this a helper function...theres four of these
    x <- cohorts %>% 
      filter(cohortyear == input$cohort) %>%
      summarise(max(term))
    x <- x[1,1]
    
    if(term > x) {term <- x}
    
    active <- unique(cohorts[cohorts$term == term &
                               cohorts$cohortyear == input$cohort,
                             'livestatuscomp'])

    if (active == 'Live') {
      showElement(id = 'acwarn2', anim = TRUE)
    }
    
    if (active == 'Archive') {
      hideElement(id = 'acwarn2', anim = TRUE)
    }
  })
  
  # Warning message for live data
  output$warn4 <- renderUI({
    text <- '*Some data in this plot is NOT FINALIZED and may change
      on a day to day basis. Incomplete data is indicated by an asterisk (*)
      on an X-Axis label'
    
    HTML(paste(text))
  })

  # SNAPSHOT Plot---------------------------------------------------------------


  output$achPerc <- renderChart({

    # Filter based on input
    temp <- cohorts[cohorts$cohortyear == input$cohort &
                      cohorts$term == input$termAchieve,]

    # Filter cohort definition
    names(temp)[names(temp) == input$definition] <- 'filt'
    temp <- temp[!is.na(temp$filt),]

    # Calculate percentage variables
    percent <- temp %>%
      summarise('% Comp Ed Plan' = mean(comprehensive) * 100,
                '% Transfer English' = mean(cumTransEnglish) * 100,
                '% Transfer Math' = mean(cumTransMath) * 100,
                '% 15 Transfer Units' = mean(mile15) * 100,
                '% 30 Transfer Units' = mean(mile30) * 100,
                '% 45 Transfer Units' = mean(mile45) * 100,
                '% 60 Transfer Units' = mean(mile60) * 100,
                '% Completion' = mean(compcum) * 100)
    percent <- gather(percent, 'variable', 'percent', 1:8)

    # calculate headcounts
    headcount <- temp %>%
      summarise('% Comp Ed Plan' = sum(comprehensive),
                '% Transfer English' = sum(cumTransEnglish),
                '% Transfer Math' = sum(cumTransMath),
                '% 15 Transfer Units' = sum(mile15),
                '% 30 Transfer Units' = sum(mile30),
                '% 45 Transfer Units' = sum(mile45),
                '% 60 Transfer Units' = sum(mile60),
                '% Completion' = sum(compcum))

    headcount <- gather(headcount, 'variable', 'headcount', 1:8)

    # calculate total
    total <- temp %>% summarise(total = n())
    total <- total[1,1]

    # join headcounts onto percentage
    percent <- percent %>% left_join(headcount)
    percent <- data.frame(percent)
    
    # add total value to total column
    percent$total <- total

    # Percentage Plot
    n1 <- nPlot(percent ~ variable,
                data = percent,
                type = "discreteBarChart",
                width = session$clientData[["output_plot7_width"]])

    n1$yAxis(axisLabel = 'Percent of Students',
             width = 50)
    n1$xAxis(rotateLabels = -15)
    n1$chart(color = colors,
             forceY = c(0, 100),
             tooltipContent = makeDemoToolTip())

    # Display the chart
    n1$addParams(dom = 'achPerc')
    return(n1)
  })


  # Achievement COMPARISON PLOT--------------------------------------------------

  output$achCompPlt <- renderChart({

    type <- compType[input$achieve]
    
    # helps select tooltip type
    type <- ifelse(input$equityAchieve == 'Yes', '%', type)

    temp <- outcomeDisag(input$achieve,
                         input$optionAchieve,
                         input$cohort,
                         input$definition,
                         input$termAchieve,
                         input$filtAchieve,
                         input$filtOptionAchieve,
                         input$equityAchieve,
                         input$demoAchieve,
                         data = cohorts,
                         type = type)
    
    temp <- activeData(temp, input$termAchieve, cohorts, type = 'comp')

    yax <- c(0,100)
    yax[yax == 100 & type != '%'] <- max(temp$outcome) + 3

    title <- names(milestones)[milestones == input$achieve]


    if (input$demoAchieve == 'None') {

      tooltip <- ifelse(type == '%', 'bar', 'baravg')

      n1 <- nPlot(outcome ~ order,
                  data = temp,
                  type = "discreteBarChart",
                  width = session$clientData[["output_plot8_width"]])

      n1$yAxis(axisLabel = title,
               width = 50)
      n1$chart(color = colors,
               forceY = yax,
               tooltipContent = makeDemoToolTip(tooltip))
    }

    if (input$demoAchieve != 'None' & input$equityAchieve == 'No') {

      tooltip <- ifelse(type == '%', 'demo', 'demoavg')

      n1 <- nPlot(outcome ~ demo, group = "order",
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot8_width"]])

      n1$yAxis(axisLabel = title,
               width = 50)
      n1$xAxis(rotateLabels = -15)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = yax,
               tooltipContent = makeDemoToolTip(tooltip))
    }

    if (input$demoAchieve != 'None' & input$equityAchieve == 'Yes') {
      n1 <- nPlot(outcome ~ demo, group = "order",
                  data = temp,
                  type = 'multiBarChart',
                  width = session$clientData[["output_plot8_width"]])

      n1$yAxis(axisLabel = title,
               width = 50)
      n1$xAxis(rotateLabels = -15)
      n1$chart(color = colors,
               showControls = F,
               reduceXTicks = F,
               forceY = c(floor(min(temp$outcome)) * .9,
                          floor(max(temp$outcome)) * 1.1),
               tooltipContent = makeDemoToolTip('equity', 
                                                compType[input$achieve]))
    }

    n1$addParams(dom = 'achCompPlt')
    return(n1)
  })


})
