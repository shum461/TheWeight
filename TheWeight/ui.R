#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

conn <- config::get("connectionSettings")
board_register_rsconnect(server = conn$CONNECT_SERVER, key = conn$CONNECT_API_KEY)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Fish Finder"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      pickerInput(inputId = "Year_input", "Select a year", c("loading..."),2017:2019,multiple = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("Fish_table")
    )
  )
))
