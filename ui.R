library(shiny)
library(dplyr)
library(rCharts)
library(tidyr)
library(shinyjs)
library(shinyBS)
library(hawkTrackHelp)

load('cohorts.rdata')

crcLink <- "https://researchapps.crc.losrios.edu/CRC_Research_Data_Request_Form"
defLink <- "https://www.crc.losrios.edu/files/ie/HawkTrack_Data_Element_Definitions.pdf"


################################################################################

#                             DEFINE BUTTON OPTIONS

################################################################################


# Cohort Selection--------------------------------------------------------------
cohort <- unique(cohorts$cohortyear[cohorts$term == 1])
cohort <- cohort[order(cohort, decreasing = TRUE)]
definition <- c('All' = 'emplid',
                'Certificate Path' = 'cert',
                'Deg/Trans/Cert Seeking' = 'degreeseek',
                'Two Year Path' = 'twoyear',
                'Three Year Path' = 'threeyear',
                'Recent High School Grads' = 'recenths')


# Current Enrollment/Milestone Achievement--------------------------------------
affirm <- c('No', 'Yes')
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
terms <- c('6th' = 6, '5th' = 5, '4th' = 4, '3rd' = 3, '2nd' = 2, '1st' = 1)


################################################################################

#                                  DEFINE UI

################################################################################


shinyUI(fluidPage(
  theme = 'style.css',

  useShinyjs(),

  navbarPage(title = 'CRC HawkTrack', id = 'navbar',


    # Cohort selection page-----------------------------------------------------


    tabPanel(title = 'Select a Cohort',
      verticalLayout(
        fluidRow(
          column(4,
            inputPanel(
              selectInput('cohort', 'Pick a cohort', cohort),
              selectInput('definition', 'Pick a cohort definition',
                          definition),
              actionButton('showtabs', 'See data for this cohort')
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
          fluidRow(
            column(6,
              selectInput('termEnroll', 'Select a term',
                          terms, selected = 1)
            ),
            column(6,
                   radioButtons('affirmEnroll', 'Conduct a comparison?', affirm,
                                selected = 'No',
                                inline = TRUE
                   )
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
                selectInput('demoEnroll', 
                            'Break the results down by
                            groups:', 
                            demos),
                fluidRow(
                  column(6,
                         selectInput('filtEnroll', 
                                     'And/or look within a group:', 
                                     demos)
                  ),
                  hidden(column(id = 'filtE', 6,
                         selectInput('filtOptionEnroll', 
                                     'Select specific group', 
                                     'None', selected = 1))
                  )
                )
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
                ),
                column(3, 
                       hidden(div(id = 'enwarn', htmlOutput('warn1'))),
                       div(class = 'defLink', 
                           a(href = defLink, style = 'color: #ffffff',
                             target= '_blank',"CLICK HERE for metric and
                             cohort definitions")
                          )
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
                ),
                column(3, 
                       hidden(div(id = 'enwarn2', htmlOutput('warn2'))),
                       div(class = 'defLink', 
                           a(href = defLink, style = 'color: #ffffff',
                             target= '_blank',"CLICK HERE for metric and
                             cohort definitions")
                       )
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
          fluidRow(
            column(6,
                   selectInput('termAchieve', 'Select a term',
                               terms, selected = 1)
            ),
            column(6,
                   radioButtons('affirmAchieve', 'Conduct a comparison?', 
                                affirm,
                                selected = 'No',
                                inline = TRUE
                   )
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
              selectInput('demoAchieve', 
                          'Break the results down by groups:', 
                          demos),
              fluidRow(
                column(6,
                       selectInput('filtAchieve', 
                                   'And/or look within a group:', 
                                   demos)
                ),
                hidden(column(id = 'filtA', 6,
                       selectInput('filtOptionAchieve', 
                                   'Select a specific group', 
                                   'None'))
                )
              )
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
                  ),
                  column(3, 
                         hidden(div(id = 'acwarn', htmlOutput('warn3'))),
                         div(class = 'defLink', 
                             a(href = defLink, style = 'color: #ffffff',
                               target= '_blank',"CLICK HERE for metric and
                               cohort definitions")
                         )
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
                  ),
                  column(3, 
                         hidden(div(id = 'acwarn2', htmlOutput('warn4'))),
                         div(class = 'defLink', 
                             a(href = defLink, style = 'color: #ffffff',
                               target= '_blank',"CLICK HERE for metric and
                               cohort definitions")
                         )
                  )
                )
            )
          )
        )
      )
    ),
    
    # Help page ----------------------------------------------------------------
    tabPanel(title = 'Need Help?',
             fluidRow(id = 'welcome-top',
                      column(6,
                             h1(id = 'welcome-header',
                                'Welcome to the CRC HawkTrack!'),
                             p(class = 'welcome-text', id = 'specific1',
                               "Someday soon 
                                a tutorial video will be on this page...
                               "),
                             br(),
                             br(),
                             p(class = 'welcome-text', id = 'specific',
                               "In the meantime, 
                               if you have questions, please contact:",
                               br(),
                               a(href = 'mailto:meinzp@crc.losrios.edu',
                                 style = 'color: #ffffff',
                                 "meinzp@crc.losrios.edu")),
                             br(),
                             br(),
                             p(class = 'welcome-text', id = 'specific',
                               "Otherwise, if you want additional data ",
                               br(),
                               a(href = crcLink, style = 'color:#ffffff',
                                 target= '_blank',
                                 "CLICK HERE"))
                      ),
                      column(6, id = 'place', 
                             a(href="https://placekitten.com/"),
                             img(src = "https://placekitten.com/400/400"))
             ),
             fluidRow(id = 'copyright',
                      column(12,
                             p(id = 'info',
                               'Product of the CRC Office of',
                               'Institutional Effectiveness  ')
                      )
             )
    )
  )


))



