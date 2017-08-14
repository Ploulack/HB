library(shiny)
library(tidyverse)
#source("import.R", local = TRUE)

shinyServer(function(input, output) {
        
        #Using RenderUI for the checkboxes so that UI scope doesn't need to
        #have the full dataframe imported.
        # output$lab_steps <- renderUI({
        #         checkboxGroupInput(inputId = "lab_steps",
        #                            label = "Select steps that would be cut with ordering",
        #                            choiceNames = lab_steps,
        #                            choiceValues = seq_along(lab_steps))
        # })
        
        output$summary <- renderTable(digits = 1,{
                position_values <- hr_costs[hr_costs$Position == input$position, ]
                step_values[as.numeric(input$lab_steps), ] %>%
                        summarise(`Lab days avg` = (sum(min_time) + sum(max_time)) / (24*2),
                                  `Researcher hours` = sum(researcher_time),
                                  Cost = paste0(position_values$Hourly_Rate * `Researcher hours`,"$")
                                  )
        })
        
        output$dna <- renderTable(digits = 1, {
                complexity <- dna_costs[dna_costs$complexity == input$complexity, ]
                complexity %>%
                        select("Order days avg" = delay, "Order cost" = cost)
        })
        
        
})