library(shiny)

MS_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                titlePanel("Testing systematic Google auth"),
                
                mainPanel(
                        
                        shiny::uiOutput(ns("button")),
                        tableOutput(ns("token"))
                )
        )
}
