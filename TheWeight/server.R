#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(pins)
library(DT)
library(shinyWidgets)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  reactive_chem= pin_reactive("EstProbMon_Sed_Chem_2015_2019_WOE", board = "rsconnect") 
  
  observe({
   
  
     updatePickerInput(session = session, inputId = "Year_input", 
                       choices =sort(unique(as.character(reactive_chem()$YEAR))), selected = 2019)
  })
  
  
  output$sed_table <- renderDataTable({
    
    reactive_chem() %>%
      filter(.$YEAR %in% input$Year_input) %>%
      select_if(negate(is.list))
   
    
  })
  
})
