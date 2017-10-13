
decisionUI <- function(id) {
        #Create a function provided the id
        ns <- NS(id)
        
        wait_msg <- "Waiting from Google Drive..."
        
        sidebarLayout(
                sidebarPanel(width = 3,
                        checkboxGroupInput(inputId = ns("lab_steps"),
                                label = "Select steps involved in your experiment",
                                choices = wait_msg),
                        uiOutput(outputId = ns("parts_nb"))
                        ,
                        selectInput(inputId = ns("position"),
                                label = "What's your position?",
                                choices = wait_msg),
                        
                        numericInput(inputId = ns("bp_length"),
                                label = "Length of your planned DNA",
                                value = 1,
                                min = 1,
                                max = 20000)
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