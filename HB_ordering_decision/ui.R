library(shiny)
# source("import.R", local = TRUE)


shinyUI(fluidPage(
        
        # Application title
        titlePanel("Assemble vs Ordering"),
        
        sidebarLayout(
                sidebarPanel(
                        checkboxGroupInput(inputId = "lab_steps",
                                           label = "Select steps that would be cut with ordering",
                                           choiceNames = step_values$`Assembly step`,
                                           choiceValues = seq_along(step_values$`Assembly step`)),
                        
                        selectInput(inputId = "position",
                                    label = "What's your position?",
                                    choices = hr_costs$Position),
                        
                        selectInput(inputId = "complexity",
                                    label = "What's your assembly complexity?",
                                    choices = dna_costs$complexity)
                        
                ),
                
                mainPanel(
                        titlePanel("Results"),
                        h3("Lab delays & costs"),
                        tableOutput("summary"),
                        h3("DNA order delays & costs"),
                        tableOutput("dna")
                )
        )
))