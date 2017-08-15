library(shiny)

shinyUI(navbarPage("HB lab", 
                   tabPanel("Tecan",
                            sidebarPanel(
                                            tableOutput("summary")
                                    ),
                            mainPanel(
                                    dataTableOutput("batch")
                            )
                            )
                   ))