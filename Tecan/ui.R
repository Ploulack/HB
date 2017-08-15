library(shiny)

shinyUI(navbarPage("HB lab", 
                   tabPanel("Tecan",
                            mainPanel(
                                    dataTableOutput("batch")
                            )
                            )
                   ))