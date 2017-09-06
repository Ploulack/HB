tecan_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                sidebarPanel(
                        textInput(ns("absorbance"),
                                "Absorbance Coef",
                                value = 0.02,
                                placeholder = "in ug / (ml.cm)"),
                        textInput(ns("path"),
                                "Path Length",
                                value = .19,
                                placeholder = "in cm"),
                        actionButton(ns("refresh"),
                                label = "Check for new Tecan files"),
                        selectInput(ns("file"), "Select from latest Tecan Files",
                                choices = list("Waiting from dropbox")),
                        tableOutput(ns("summary"))
                ),
                mainPanel(
                        fluidRow(
                                column(12,
                                        plotOutput(ns("hist"))
                                ),
                                dataTableOutput(ns("batch"))
                        ))
        )
}