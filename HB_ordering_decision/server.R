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
source("import.R", local = TRUE)

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
        
        #Using RenderUI for the checkboxes so that UI scope doesn't need to
        #have the full dataframe imported.
        output$lab_steps <- renderUI({
                checkboxGroupInput(inputId = "lab_steps",
                                   label = "Select steps that would be cut with ordering",
                                   choiceNames = lab_steps,
                                   choiceValues = seq_along(lab_steps))
        })
        
        
        output$summary <- renderTable(digits = 1,{
                step_values[as.numeric(input$lab_steps), ] %>%
                        summarise(avg_lab_days = (sum(min_time) + sum(max_time)) / (24*2),
                                  researcher_hrs = sum(researcher_time))
        })
        
        
})