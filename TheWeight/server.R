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
  
  reactive_fish= pin_reactive("FishTissue_Metals_results", board = "rsconnect") 
  
  
  observe({
   
     fish_years=reactive_fish() %>% select(Year) %>% arrange(desc((Year)))
     updatePickerInput(session = session, inputId = "Year_input", choices =fish_years, selected = 2019)
  })
  
  
  output$Fish_table <- renderDataTable({
    
    reactive_fish() %>%
      filter(.$Year %in% input$Year_input)
   
    
  })
  
})
