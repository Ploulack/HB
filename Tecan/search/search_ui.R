search_ui <- function(id) {
    ns <- NS(id)

    fluidPage(
        sidebarPanel(
            selectInput(ns("search_molecules"),
                   label = "Select Molecule to search",
                   choices = "Waiting from Mongo"),
            numericInput(ns("min_concentration"),
                         label = "Min Concentration",
                         value = 10,
                         min = 1),
            actionButton(ns("search_go"),label = "Search")
        , width = 2),
        mainPanel(
            tableOutput(ns("totals")),
            conditionalPanel(str_interp("output['${ns('total_samples')}'] < 100"),
                             actionButton(ns("display_samples"),
                                          "Display Samples")),
            tableOutput(ns("search_results"))
        )
    )
}

