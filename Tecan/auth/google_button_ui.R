library(shiny)

google_auth_button_ui <- function(id) {
        ns <- NS(id)
        uiOutput(ns("button"))
        
}