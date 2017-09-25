tecan_ui <- function(id) {
        width <- "70%"
        ns <- NS(id)
        source(file ="tecan/tecan_db_ui.R")
        fluidPage(
                sidebarPanel(width = 3,
                        textInput(ns("absorbance"),
                                "Absorbance Coef",
                                value = 0.02,
                                width = width,
                                placeholder = "in ug / (ml.cm)"),
                        textInput(ns("path"),
                                "Path Length",
                                value = .19,
                                width = width,
                                placeholder = "in cm"),
                        actionButton(ns("refresh"),
                                label = "Check for new Tecan files"),
                        selectInput(ns("file"), "Select from latest Tecan Files",
                                choices = list("Waiting from dropbox"),
                                width = width),
                        tableOutput(ns("summary"))
                ),
                mainPanel(
                        tecan_db_ui(ns("Tecan_db")),
                        titlePanel(
                                textOutput(ns("type"))
                        ),
                        plotOutput(ns("hist")),
                        dataTableOutput(ns("batch"))
                        
                )
        )
}