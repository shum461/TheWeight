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
navbarPage("The Weight",
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        pickerInput(inputId = "Year_input", "Select a year", choices = 2015:2019,
                                    multiple = TRUE)
                        ),#sidebar panel
                       mainPanel(
      dataTableOutput("sed_table")
                      ) #main panel
                    )
           ),
tabPanel("Summary",
                verbatimTextOutput("summary")
  
    )#tab panel
  )

