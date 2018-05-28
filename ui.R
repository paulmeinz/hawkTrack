library(shiny)
library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)


################################################################################

#                                  DEFINE UI

################################################################################


shinyUI(fluidPage(
  useShinyjs(),
  
  # Application title
  navbarPage(title = 'CRC HawkTrack',
     tabPanel(title = 'Select a Cohort'),
     tabPanel(title = 'Cohort Profile'),
     tabPanel(title = 'Cohort Achievements')
  )

    
))
             
  
  
