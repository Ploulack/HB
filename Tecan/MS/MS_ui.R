ms_ui <- function(id) {
        ns <- NS(id)
        source(file = "helpers/delete_file_button_module.R")
        source("helpers/ui_generics/select_file_ui.R")

        fluidPage(
               # select_file_ui(ns("files")),
                sidebarPanel(
                        checkboxGroupInput(inputId = ns("molecules"),
                                           label = "Molecules",
                                           choices = "Waiting for server..."),
                        checkboxGroupInput(inputId = ns("samples"),
                                           label = "Samples",
                                           choices = "Waiting for server..."
                                           ),
                        checkboxInput(inputId = ns("select_all"),
                                      label = "Select all samples",
                                      value = FALSE)

                ),
                mainPanel(
                        htmlOutput(outputId = ns("x_value")),
                        titlePanel(
                                textOutput(outputId = ns("id"))
                        ),
                        plotOutput(ns("bar"),
                                   click = ns("click")),
                        checkboxInput(inputId = ns("display_raw"),
                                      label = "Display unaggregated data"
                                      ),
                        tableOutput(ns("table"))
                )
        )
}