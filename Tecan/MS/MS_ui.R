ms_ui <- function(id) {
        ns <- NS(id)
        source(file = "helpers/delete_file_button_module.R")
        source("helpers/ui_generics/select_file_ui.R")

        fluidPage(
                sidebarPanel(width = 3,
                             actionButton(ns("create_ms"),label = "Create MS instruction"),
                             select_file_ui(ns("files")),
                             checkboxGroupInput(inputId = ns("molecules"),
                                                label = "Molecules",
                                                choices = "Waiting for server..."),
                             checkboxInput(inputId = ns("select_all"),
                                           label = "Select samples with a reading",
                                           value = FALSE),
                             checkboxGroupInput(inputId = ns("samples"),
                                                label = "Samples",
                                                choices = "Waiting for server..."
                             )
                ),
                mainPanel(
                        htmlOutput(outputId = ns("x_value")),
                        titlePanel(
                                textOutput(outputId = ns("file_title"))
                        ),
                        checkboxInput(ns("log_scale"),
                                      label = "Switch to log scale"),
                        plotOutput(ns("bar"),
                                   click = ns("click")
                                   # hover = hoverOpts(id = ns("hover"),
                                   #                   delayType = "debounce", delay = 300)
                        ),
                        checkboxInput(inputId = ns("display_raw"),
                                      label = "Display unaggregated data"
                        ),
                        tableOutput(ns("table"))
                )
        )
}