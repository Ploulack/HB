library(shiny)

source("order_dna_decision/decision_ui.R")
source("MS/MS_ui.R")
source("tecan/tecan_ui.R")
source("gel/gel_ui.R")
source("auth/google_button_ui.R")

#Includes all panels with the conditional buttons:
source("root_ui_panels.R")

tabsetPanel(id = "tab",
        conditionalPanel(
                condition = "output.token_exists == false",
                google_auth_button_ui("Auth")),
        tecan_tab(),
        order_tab(),
        gel_tab(),
        hamilton_tab()
)