
decision <- function(input, output, session) {
        
        #Get data from Google 'Assembly Cost'
        source("order_dna_decision/import_dna_decision_data.R")
        assemply_sheets <- get_assembly_costs(url)
        step_values <- assemply_sheets$steps
        hr_costs <- assemply_sheets$HR
        dna_costs <- assemply_sheets$DNA
        
        updateCheckboxGroupInput(session, inputId = "lab_steps",
                                 choiceNames = step_values$`Assembly step`,
                                 choiceValues = seq_along(step_values$`Assembly step`))
        
        updateSelectInput(session, inputId = "position",
                          choices = hr_costs$Position)
        
        updateSelectInput(session, inputId = "complexity",
                          choices = dna_costs$complexity)
        
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
}