library(shiny)

shinyUI(navbarPage("HB lab", 
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
                                    selectInput("files", "Select from latest Tecan Files",
                                                   choices = list("Waiting from dropbox", "B")),
                                    tableOutput("summary")
                            ),
                            mainPanel(
                                    fluidRow(
                                            column(12,
                                                   plotOutput("hist"))
                                    ),
                                    dataTableOutput("batch")
                            )
                            )
                   ))