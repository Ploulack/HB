source("helpers/ms_display/ms_display.R")

search_ui <- function(id) {
    ns <- NS(id)

    fluidPage(
        sidebarPanel(width = 3,
                     selectInput(ns("search_molecules"),
                                 label = "Select Molecule to search",
                                 choices = "Waiting from Mongo"),
                     numericInput(ns("min_concentration"),
                                  label = "Min Concentration",
                                  value = 10,
                                  min = 1),
                     fluidRow(
                         column(width = 2,
                                actionButton(ns("search_go"),label = "Search")
                        ),
                         column(width = 2, offset = 1,
                                conditionalPanel(str_interp("output['${ns('total_samples')}'] < 100"),
                                                 actionButton(ns("display_samples"),
                                                              "Display Samples"))
                         )
                     ),
                     ms_data_display_ui_sidebar(ns("search"))
        ),
        mainPanel(
            tableOutput(ns("totals")),

            tableOutput(ns("search_results")),
            ms_data_display_ui_main(ns("search"))
        )
    )
}

