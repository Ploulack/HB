library(shiny)
ms_server <- function(input, output, session) {
        source("helpers/ui_generics/select_file_server.R")
        source("helpers/general.R")
        selected <- reactiveVal()

        ms_folder <- get_drive_url(session, "ms")

        ms_progress <- shiny::Progress$new()

        ms <- callModule(module = select_file,
                id = "files",
                progress = ms_progress,
                drive_url = ms_folder,
                selected = selected
        )
        output$id <- renderText({
                ms$id()
        })

        output$protocol <- renderTable(ms$protocols())

        observeEvent(ms$go_file(), {
                browser()
        })


}