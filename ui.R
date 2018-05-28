library(shiny)
library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)


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
  useShinyjs(),
  
  # Application title
  navbarPage(title = 'CRC HawkTrack',
    tabPanel(title = 'Welcome'),         
    tabPanel(title = 'Select a Cohort'),
    tabPanel(title = 'Cohort Profile'),
    tabPanel(title = 'Cohort Achievements')
  )

    
))
             
  
  
