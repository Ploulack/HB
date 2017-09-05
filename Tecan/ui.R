library(shiny)

#TEST IF CAN BE INSERTED IN TABPANEL CODE
source("order_dna_decision/decision_ui.R")
source("MS/MS_ui.R")


navbarPage("HB lab",
           tabsetPanel(id = "tab",
                   tabPanel("Tecan",
                            sidebarPanel(
                                    textInput("absorbance",
                                              "Absorbance Coef",
                                              value = 0.02,
                                              placeholder = "in ug / (ml.cm)"),
                                    textInput("path",
                                              "Path Length",
                                              value = .19,
                                              placeholder = "in cm"),
                                    actionButton("refresh",
                                                 label = "Check for new Tecan files"),
                                    selectInput("file", "Select from latest Tecan Files",
                                                choices = list("Waiting from dropbox")),
                                    tableOutput("summary")
                            ),
                            mainPanel(
                                    fluidRow(
                                            column(12,
                                                   plotOutput("hist"))
                                    ),
                                    dataTableOutput("batch")
                            )
                   ),
                   tabPanel("Order Decision Tool",
                            decisionUI("decision")
                   ),
                   tabPanel("MS Analysis",
                           MS_ui("MS"))
           )
)