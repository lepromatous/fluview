library(shiny)
library(tidyverse)
library(ISOweek)
library(Cairo)
library(anomalize)
library(shinythemes)
options(shiny.usecairo=TRUE)
source("helpers.R")


function(input, output, session) {
  #### START TAB PANEL: plot render ####
  
  output$contents_flumort <- renderPlot(execOnResize = T,{
    anomaly_fluview<-anomaly_fluview(alpha=input$alphaslider, max_anoms=input$anomslider)
    
  })
  
  getData <- reactive({
    file1 <- input$file1
    if (is.null(file1)) {
      return()
    }
    data_statez = read.csv(file=file1$datapath)
    stuff<-(unique(data_statez["SUB.AREA"]))
    return(stuff)
  })

  output$pls<-renderUI({
    selectInput("var", "Choose State:", choices= getData(), selected="Missouri")
  })


  output$contents_flumort_state<-renderPlot({
    shiny::validate(
      need(input$var, "Waiting for data upload. See video on the bottom left for instructions on obtaining FluView Interactive data.")
    )
    inFile<-input$file1
    if(is.null(inFile)) return(NULL)
    data_state<-read.csv(inFile$datapath, sep = ",", header = T, fill = T)
    anomaly_fluview_state(dataz=data_state, alpha=input$alphaslider_state, max_anoms=input$anomslider_state, state=input$var)
  })
  
}