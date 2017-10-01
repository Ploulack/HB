library(shiny)

#TEST IF CAN BE INSERTED IN TABPANEL CODE
source("order_dna_decision/decision_ui.R")
source("MS/MS_ui.R")
source("tecan/tecan_ui.R")
source("gel/gel_ui.R")
source("auth/google_button_ui.R")


navbarPage("HB lab",
        conditionalPanel(
                condition = "output.token_exists == false",
                google_auth_button_ui("Auth")
        ),
        tabsetPanel(
                id = "tab",
                tabPanel("Tecan",
                conditionalPanel(
                        condition = "output.token_exists == true",
                                conditionalPanel(
                                        condition = "input.tecan_go == 0",
                                        actionButton("tecan_go", "Launch Tecan tool")
                                ),
                                conditionalPanel(
                                        condition = "input.tecan_go > 0",
                                        tecan_ui("Tecan")
                                )
                        ))
                ,
                tabPanel("Order Decision Tool",
                                conditionalPanel(
                                        condition = "input.decision_go == 0",
                                        actionButton("decision_go", "Launch Order Decision Tool")
                                ),
                                conditionalPanel(
                                        condition = "input.decision_go > 0",
                                        decisionUI("decision")
                                )
                        ),
                tabPanel("Gel",
                        conditionalPanel(
                                condition = "output.token_exists == true",
                                conditionalPanel(
                                        condition = "input.gel_go == 0",
                                        actionButton("gel_go", "Start Gel viewer")
                                ),
                                conditionalPanel(
                                        condition = "input.gel_go > 0",
                                        gel_ui("Gel")
                                )
                        )
                                
                                
                        )
                )
                
        )
        # tabPanel("MS Analysis",
        #         MS_ui("MS")
