tecan_db_ui <- function(id) {
        ns <- NS(id)
        
        fluidPage(
                uiOutput(ns("keys")),
                textOutput(ns("test"))
        )
}