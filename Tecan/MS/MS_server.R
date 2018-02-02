library(shiny)
ms_server <- function(input, output, session) {
        source("helpers/ui_generics/select_file_server.R")
        source("helpers/general.R")

        ms_folder <- get_drive_url(session, "ms")

        ms_progress <- shiny::Progress$new()
        callModule(module = select_file,
                id = "ms",
                progress = ms_progress,
                drive_url = ms_folder
        )
}