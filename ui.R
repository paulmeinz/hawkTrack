library(shiny)
library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)


crcLink <- "https://researchapps.crc.losrios.edu/CRC_Research_Data_Request_Form"

################################################################################

#                             DEFINE BUTTON OPTIONS

################################################################################


# Cohort Selection--------------------------------------------------------------
cohorts <- c('2014-2015', '2015-2016')
definition <- c('All', 'Two Year Path', 'Three Year Path', 
                'Degree/Transfer/Certificate Seeking')

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
                 "Click on the tabs above to select a cohort",
                 ", look at current enrollment information, and ",
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
              selectInput('cohort', 'Pick a cohort', cohorts),
              selectInput('definition', 'Select a cohort definition', 
                          definition)
            )
          )
        ),
        fluidRow(
          column(3, 
                 chartOutput('ethnicity', lib = 'nvd3'),
                 htmlOutput('defeth')
                 ),
          column(3),
          column(3),
          column(3)
        )
      )
    ),
    
    
    tabPanel(title = 'Cohort Enrollment'),
    
    
    tabPanel(title = 'Cohort Achievements')
  )

    
))
             
  
  
