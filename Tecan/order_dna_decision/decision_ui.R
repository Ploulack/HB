
decisionUI <- function(id) {
        #Create a function provided the id
        ns <- NS(id)
        
        wait_msg <- "Waiting from Google..."
        
        sidebarLayout(
                sidebarPanel(
                        checkboxGroupInput(inputId = ns("lab_steps"),
                                           label = "Select steps that would be cut with ordering",
                                           choices = wait_msg),
                        
                        selectInput(inputId = ns("position"),
                                    label = "What's your position?",
                                    choices = wait_msg),
                        
                        selectInput(inputId = ns("complexity"),
                                    label = "What's your assembly complexity?",
                                    choices = wait_msg)
                ),

                mainPanel(
                        titlePanel("Results"),
                        h3("Lab delays & costs"),
                        #Gets stuck here, added the missing NS statements
                        tableOutput(ns("summary")),
                        h3("DNA order delays & costs"),
                        tableOutput(ns("dna"))
                )
                       
        )
        }