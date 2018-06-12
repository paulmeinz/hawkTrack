library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)
library(hawkTrackHelp)

load('cohorts.rdata')

crcLink <- "https://researchapps.crc.losrios.edu/CRC_Research_Data_Request_Form"


################################################################################

#                             DEFINE BUTTON OPTIONS

################################################################################


# Cohort Selection--------------------------------------------------------------
cohort <- unique(cohorts$cohortyear[cohorts$term == 1])
cohort <- cohort[order(cohort, decreasing = TRUE)]
definition <- c('All' = 'emplid',
                'Two Year Path' = 'twoyear',
                'Three Year Path' = 'threeyear',
                'Certificate Path' = 'cert',
                'Deg/Trans/Cert Seeking' = 'degreeseek')


# Current Enrollment/Milestone Achievement--------------------------------------
affirm <- c('No', 'Yes')
demos <- c(None = 'None', Age = 'age', Ethnicity = 'ethnicity',
           'First Generation' = 'firstgen',
           'Foster Youth' = 'foster', Gender = 'gender',
           'Reported Disability' = 'dsps', 'Veteran Status' = 'veteran')
options <- c('Look at trends for this cohort' = 'years',
             'Compare to previous cohorts' = 'cohorts')
enrollment <- c('[Select One]' = 'None',
                'Average Units' = 'units',
                'Average Units Withdrawn' = 'wunits',
                '% Enrolled' = 'enrolled',
                '% Enrolled Full Time (12 units)' = 'units12',
                '% Enrolled Full Time (15 units)' = 'units15',
                '% Enrolled in math' = 'Math',
                '% Enrolled in English' = 'English')
milestones <- c('[Select One]' = 'None',
                'Averge GPA' = 'gpa',
                '% Comp Ed Plan' = 'comprehensive',
                '% Transfer English' = 'cumTransEnglish',
                '% Transfer Math' = 'cumTransMath',
                '% 15 Transfer Units' = 'mile15',
                '% 30 Transfer Units' = 'mile30',
                '% 45 Transfer Units' = 'mile45',
                '% 60 Transfer Units' = 'mile60',
                '% Completion' = 'compcum'
                )
terms <- c('6th' = 6, '5th' = 5, '4th' = 4, '3rd' = 3, '2nd' = 2, '1st' = 1)


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
          column(4, htmlOutput('cohortSize')),
          column(4, htmlOutput('def'))
        ),
        fluidRow(
          column(4,
                 chartOutput('ethnicity', lib = 'nvd3'),
                 plotOutput('plot1', height = '0px')
          ),
          column(4,
                 chartOutput('gender', lib = 'nvd3'),
                 plotOutput('plot2', height = '0px')
          ),
          column(4,
                 chartOutput('special', lib = 'nvd3'),
                 plotOutput('plot4', height = '0px')
          )
        )
      )
    ),


    # Cohort Enrollment page----------------------------------------------------


    tabPanel(title = 'Cohort Enrollment',
      sidebarLayout(
        sidebarPanel(
          div(id = 'cohortMessage', class = 'cohortMsg', uiOutput('cohort1')),
          div(radioButtons('affirmEnroll', 'Conduct a comparison?', affirm,
                           selected = 'No',
                           inline = TRUE
              )
          ),
          hidden(
            div(id = 'enrollSelect',
                selectInput('enroll', 'Select an enrollment metric', enrollment)
            )
          ),

          hidden(
            div(id = 'enrollComp',
                radioButtons('optionEnroll', 'Comparisons', options),
                hidden(
                  div(id = 'enrollTerm',
                      selectInput('termEnroll', 'Select a comparison term',
                                  terms, selected = 1)
                  )
                ),
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
          hidden(
            div(id = 'snapshot',
              fluidRow(
                column(8, htmlOutput('enrollTitle'))
              ),
              fluidRow(
                column(8,
                       chartOutput('enrollPerc', lib = 'nvd3'),
                       plotOutput('plot5', height = '0px')
                )
              )
            )
          ),
          hidden(
            div(id = 'compare',
              fluidRow(
                  column(8, htmlOutput('enrollcompTitle'))
              ),
              fluidRow(
                column(8,
                       chartOutput('enrollCompPlt', lib = 'nvd3'),
                       plotOutput('plot6', height = '0px')
                )
              )
            )
          )
        )
      )
    ),


    # Cohort milestones page----------------------------------------------------

    tabPanel(title = 'Cohort Achievements',
      sidebarLayout(
        sidebarPanel(
          div(id = 'cohortMessage2', class = 'cohortMsg', uiOutput('cohort2')),
          div(radioButtons('affirmAchieve', 'Conduct a comparison?', affirm,
                           selected = 'No',
                           inline = TRUE
              )
          ),
          hidden(
            div(id = 'achieveSelect',
                selectInput('achieve', 'Select a milestone', milestones
                )
            )
          ),
          hidden(
            div(id = 'achieveComp',
              radioButtons('optionAchieve', 'Comparisons', options),
              hidden(
                div(id = 'achieveTerm',
                    selectInput('termAchieve', 'Select a comparison term',
                                terms, selected = 1)
                )
              ),
              selectInput('demoAchieve', 'Select a demographic', demos)
            )
          ),
          hidden(
            div(id = 'achieveEquity',
              radioButtons('equityAchieve', 'Evaluate equity?', affirm,
                           inline = TRUE
              )
            )
          )
        ),
        mainPanel(
          hidden(
            div(id = 'achsnapshot',
                fluidRow(
                  column(8, htmlOutput('achTitle'))
                ),
                fluidRow(
                  column(8,
                         chartOutput('achPerc', lib = 'nvd3'),
                         plotOutput('plot7', height = '0px')
                  )
                )
            )
          ),
          hidden(
            div(id = 'achcompare',
                fluidRow(
                  column(8, htmlOutput('achcompTitle'))
                ),
                fluidRow(
                  column(8,
                         chartOutput('achCompPlt', lib = 'nvd3'),
                         plotOutput('plot8', height = '0px')
                  )
                )
            )
          )
        )
      )
    )
  )


))



