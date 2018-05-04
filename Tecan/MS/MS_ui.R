ms_ui <- function(id) {
    ns <- NS(id)
    c("helpers/delete_file_button_module.R",
        "helpers/ui_generics/select_file_ui.R",
        "helpers/ms_display/ms_display.R") %>%
        walk(source)

    fluidPage(
        sidebarPanel(width = 3,
            actionButton(ns("create_ms"),label = "Create MS instruction"),
            ms_data_display_ui_sidebar(ns("ms"))
        ),
        mainPanel(
            select_file_ui(ns("files")),
            ms_data_display_ui_main(ns("ms"))
        )
    )
}