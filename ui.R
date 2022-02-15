
################# ################# ################# ################# 
#################  TAB PANEL Fluview Anomaly
################# ################# ################# ################# 
library(shiny)
library(tidyverse)
library(ISOweek)
library(Cairo)
library(anomalize)
library(shinythemes)
options(shiny.usecairo=TRUE)
source("helpers.R")

navbarPage("FluView Mortality Anomaly Detection", theme = shinytheme("slate"),
           tabPanel("National Data (Data Automatically Loaded)",
                    sidebarLayout(
                      sidebarPanel(h3(em(strong("Options")))
                                  ,p(tags$em("Option 1: Choose the width of the normal range")
                                     )
                                  ,p(tags$ul(
                                      tags$li("Higher values result in more anomalies detected."),
                                      tags$li("Increase this value for improving sensitivity, though it may increase false positives.")
                                   ))
                                   ,sliderInput('alphaslider', "",
                                               min=0.01, max=0.5, value=0.05, step=0.01)
                                  ,p(tags$em("Option 2: Choose the maximum proportion of anomalies permitted to be identified.")
                                  )
                                  ,p(tags$ul(
                                    tags$li("Increase this value as amount of data increases to avoid false negatives.")
                                  )) 
                                  ,sliderInput('anomslider', "",
                                               min=0.05, max=1, value=.2, step=.05)
                                   ,p(tags$a(href="https://www.cdc.gov/flu/weekly/index.htm", "Data Source - CDC FluView Pneumonia & Influenza Mortality Surveillance"), style = "font-size:10px")
                                   
                                   
                        
                     ), # close sidebar panel
                      
                      mainPanel(
                        plotOutput('contents_flumort', height=600)
                        
                      ) # close mainpanel 
                    ) # close sidebar Layout
           ), # close tabpanel 1
           
   #----------------------------------------------------------------------------_#        
           ### tab 2
           tabPanel("State-Level Data",
                    sidebarLayout(
                      sidebarPanel(h3(em(strong("Options")))
                                   ,p(tags$a(href="https://gis.cdc.gov/grasp/fluview/mortality.html", "Upload your data from FluView Interactive"))
                                   ,fileInput('file1', strong(""),
                                             accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv'))
                                   ,uiOutput("pls")
                                   
                                   ,p(tags$em("Option 1: Choose the width of the normal range")
                                   )
                                   ,p(tags$ul(
                                     tags$li("Higher values result in more anomalies detected."),
                                     tags$li("Increase this value for improving sensitivity, though it may increase false positives.")
                                   ))
                                   ,sliderInput('alphaslider_state', "",
                                                min=0.01, max=0.5, value=0.05, step=0.01)
                                   ,p(tags$em("Option 2: Choose the maximum proportion of anomalies permitted to be identified.")
                                   )
                                   ,p(tags$ul(
                                     tags$li("Increase this value as amount of data increases to avoid false negatives.")
                                   )) 
                                   ,sliderInput('anomslider_state', "",
                                                min=0.05, max=1, value=.2, step=.05)
                                   ,p(tags$a(href="https://gis.cdc.gov/grasp/fluview/mortality.html", "Data Source - CDC FluView Interactive Pneumonia & Influenza Mortality Surveillance"), style = "font-size:10px")
                                   ,br()
                                   ,h3(em("Wondering how to obtain the data for upload? See the video below!"))
                                   ,HTML('<iframe width="100%" height="400" src="//www.youtube.com/embed/xqPjAq_QwGI" frameborder="0" allowfullscreen></iframe>')
                                   

                      ), ### close sidebar Panel  
                      
                      mainPanel(
                        plotOutput('contents_flumort_state', height=600)
                        ,p("Red dots = anomalies")
                      ) # close main panel 2
                  ) # close sidebar layout 
           ) ## close tab 2
) ### close navbar
################# END  