library(shiny)
library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)

load('cohorts.rdata')

crcLink <- "https://researchapps.crc.losrios.edu/CRC_Research_Data_Request_Form"

################################################################################

#                             DEFINE BUTTON OPTIONS

################################################################################


# Cohort Selection--------------------------------------------------------------
cohort <- unique(cohorts$acad_year[cohorts$term == 1])
definition <- c('All', 'Two Year Path', 'Three Year Path', 
                'Degree/Transfer/Certificate Seeking')

# Current Enrollment/Milestone Achievement--------------------------------------
affirm <- c('No', 'Yes')
demos <- c(None = 'None', Age = 'age', Ethnicity = 'ethnicity', 
           'Foster Youth' = 'foster', Gender = 'gender', 
           'Reported Disability' = 'dsps', 'Veteran Status' = 'veteran')
options <- c('None', 'Compare to previous cohorts', 'Compare to previous years')
enrollment <- c('[Select One]','thisisametric','spam','beautifulspam')

################################################################################

#                                  DEFINE UI

################################################################################


shinyUI(fluidPage(
  theme = 'style.css',
  
  useShinyjs(),
  

  # Welcome page----------------------------------------------------------------
  
  
  navbarPage(title = 'CRC HawkTrack',
    tabPanel(title = 'Welcome',
      fluidRow(id = 'welcome-top',
               column(12,
                      h1(id = 'welcome-header',
                         'Welcome to the CRC HawkTrack!')
                      ),
               p(class = 'welcome-text', id = 'specific',
                 "Click on the tabs above to select a cohort,",
                 "look at current enrollment information, and ",
                 "see which milestones they have achieved.")
      ),
      fluidRow(id = 'welcome-mid',
               column(6,
                      p(class = 'welcome-text', id = 'specific',
                        "If you have questions, please contact:",
                        br(),
                        a(href = 'mailto:CRC-Research@crc.losrios.edu',
                          style = 'color: #ffffff',
                          "CRC-Research@crc.losrios.edu"))
                      
               ),
               column(6,
                      p(class = 'welcome-text', id = 'specific',
                        "If you have a research question or want",
                        br(),
                        "additional data ",
                        a(href = crcLink, style = 'color:#ffffff',
                          target= '_blank',
                          "CLICK HERE")) 
               )
      ),
      fluidRow(id = 'copyright',
               column(12,
                      p(id = 'info',
                        'Product of the CRC Office of',
                        'Institutional Effectiveness')
               )
      )
    ),
    

    # Cohort selection page-----------------------------------------------------
    
    
    tabPanel(title = 'Select a Cohort',
      verticalLayout(
        fluidRow(
          column(4,
            inputPanel(
              selectInput('cohort', 'Pick a cohort', cohort),
              selectInput('definition', 'Select a cohort definition', 
                          definition)
            )
          ),
          column(4),
          column(4, textOutput('def'))
        ),
        fluidRow(
          column(3, 
                 chartOutput('ethnicity', lib = 'nvd3'),
                 htmlOutput('defeth')
          ),
          column(3,
                 chartOutput('gender', lib = 'nvd3'),
                 htmlOutput('defgen')
          ),
          column(3,
                 chartOutput('age', lib = 'nvd3'),
                 htmlOutput('defage')
          ),
          column(3,
                 chartOutput('special', lib = 'nvd3'),
                 htmlOutput('defspec')
          )
        )
      )
    ),
    

    tabPanel(title = 'Cohort Enrollment',
      sidebarLayout(
        sidebarPanel(
          textOutput('cohort'),
          radioButtons('affirmEnroll', 'Perform a comparison?', affirm,
                       selected = 'No',
                       inline = TRUE
          ),
          hidden(
            div(id = 'enrollSelect',
                selectInput('enroll', 'Select an enrollment metric', enrollment)
            )
          ),
          hidden(
            div(id = 'enrollComp', 
                radioButtons('optionEnroll', 'Comparisons', options),
                selectInput('demoEnroll', 'Select a demographic', demos)
            )
          ),
          hidden(
            div(id = 'enrollEquity',
                radioButtons('equityEnroll', 'Evaluate equity?', affirm,
                             inline = TRUE
                )
            )  
          )
        ),
        mainPanel(
          chartOutput('enrollment', lib = 'nvd3'),
          htmlOutput('defenr')
        )
      )
    ),
    
    
    tabPanel(title = 'Cohort Achievements')
  )

    
))
             
  
  
