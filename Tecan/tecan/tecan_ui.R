tecan_ui <- function(id) {
        ns <- NS(id)
        source(file ="tecan/tecan_db_ui.R")
        source(file = "helpers/delete_file_button_module.R")
        fluidPage(
                sidebarPanel(width = 3,
                             fluidRow(
                                     column(4,textInput(ns("absorbance"),
                                                       "Absorbance",
                                                       value = 0.02,
                                                       #width = "35%",
                                                       placeholder = "in ug / (ml.cm)")),
                                     column(4, textInput(ns("path"),
                                                       "Length",
                                                       value = .19,
                                                       #width = "35%",
                                                       placeholder = "in cm"), offset = 1)
                                      ),
                        fluidRow(
                                actionButton(ns("refresh"),
                                label = "Check for new files"),
                                delete_exp_files_ui(ns("delete_button"))),
                        selectInput(ns("file"), "Latest Tecan Files",
                                choices = list(wait_msg),
                                width = "70%"),
                        tags$hr(),
                        conditionalPanel(
                                #condition = "output.type == 'NADH Detection'",
                                condition = paste0("output['",ns("type"),"'] == 'NADH Detection'" ),
                                actionButton(inputId = ns("open_calibration"),
                                             label = "Edit Concentrations"),
                                tableOutput(ns("calibration"))
                        ),
                        conditionalPanel(
                                # condition = "output.type != 'NADH Detection'",
                                condition = paste0("output['",ns("type"),"'] != 'NADH Detection'"),
                        tableOutput(ns("summary"))
                        )
                ),
                mainPanel(
                        tecan_db_ui(ns("Tecan_db")),
                        titlePanel(
                                textOutput(ns("type"))
                        ),
                        conditionalPanel(
                                # condition = "output.type != 'NADH Detection'",
                                condition = paste0("output['",ns("type"),"'] != 'NADH Detection'"),
                                plotOutput(ns("hist")),
                                tableOutput(ns("batch"))
                                ),
                        plotOutput(ns("regression_graph")),
                        tableOutput(ns("samples_predicted"))
                )
        )
}