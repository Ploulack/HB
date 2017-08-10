library(shiny)
# source("import.R", local = TRUE)


shinyUI(fluidPage(
        
        # Application title
        titlePanel("Assemble vs Ordering"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        uiOutput("lab_steps")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        titlePanel("Results"),
                        tableOutput("summary")
                )
        )
))