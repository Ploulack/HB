library(shiny)

source("auth/google_button_ui.R")

MS_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                titlePanel("Testing systematic Google auth"),
                mainPanel(
                        google_auth_button_ui(ns("Auth")),
                        tableOutput(ns("token"))
                )
        )
}
