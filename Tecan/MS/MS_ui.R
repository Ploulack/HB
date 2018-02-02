ms_ui <- function(id) {
        ns <- NS(id)
        source(file = "helpers/delete_file_button_module.R")
        source("helpers/ui_generics/select_file_ui.R")


        fluidPage(
               select_file_ui("ms"),
                mainPanel(
                        titlePanel(
                                "WIP"
                        )
                )
        )
}