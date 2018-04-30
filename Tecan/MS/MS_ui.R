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

            # checkboxGroupInput(inputId = ns("molecules"),
            #                    label = "Molecules",
            #                    choices = "Waiting for server..."),
            # checkboxInput(inputId = ns("select_all"),
            #               label = "Select samples with a reading",
            #               value = FALSE),
            # checkboxGroupInput(inputId = ns("samples"),
            #                    label = "Samples",
            #                    choices = "Waiting for server..."
            # )
        ),
        mainPanel(
            select_file_ui(ns("files")),
            ms_data_display_ui_main(ns("ms"))

            # htmlOutput(outputId = ns("x_value")),
            # titlePanel(
            #     textOutput(outputId = ns("file_title"))
            # ),
            # checkboxInput(ns("log_scale"),
            #     label = "Switch to log scale"),
            # plotOutput(ns("bar"),
            #     click = ns("click")
            #     # hover = hoverOpts(id = ns("hover"),
            #     #                   delayType = "debounce", delay = 300)
            # ),
            # fluidRow(
            #     column(3,checkboxInput(inputId = ns("display_raw"),
            #         label = "Display unaggregated data")
            #     ),
            #     column(3, downloadLink(outputId = ns("save_csv"),
            #         label = "Download data as csv")
            #     )
            # ),
            # tableOutput(ns("table"))
        )
    )
}