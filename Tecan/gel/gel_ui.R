library(shiny)

gel_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                titlePanel("View latest gels"),
                mainPanel(
                        #Todo: Put on two different columns so that they're aligned
                        actionButton(ns("refresh"),
                                label = "Check for new Gel Picture"),
                        selectInput(ns("file"),
                                label = "Select from latest gel captures",
                                choices = list("Waiting from dropbox")),
                        imageOutput(ns("picture"))
                )
        )
}