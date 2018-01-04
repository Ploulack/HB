tecan_ui <- function(id) {
        ns <- NS(id)
        source(file = "tecan/tecan_db_ui.R")
        source(file = "helpers/delete_file_button_module.R")
        fluidPage(
                sidebarPanel(width = 3,
                             #Conditional to only display two parameter inputs for relevant file type
                             conditionalPanel(
                                     # condition = paste0("output['",ns("type"),"'] == 'DNA Quantification'" ),
                                     condition = "output.type == 'DNA Quantification'",
                                     ns = ns,
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
                                     )),
                             #Conditional not to display the refresh / delete buttons until file list received from drive
                             conditionalPanel(
                                     # condition = paste0("input['", ns("file"), "'] != '", wait_msg, "'"),
                                     condition = paste0("input.file != '", wait_msg, "'"),
                                     ns = ns,
                                     fluidRow(
                                             actionButton(ns("refresh"),
                                                          label = "Check for new files"),
                                             delete_exp_files_ui(ns("delete_button"))
                                     )),
                             selectInput(ns("protocol"),
                                         "Protocols",
                             choices = "New", width = "70%"),
                             fluidRow(
                                     column(9, selectInput(ns("file"),
                                                           "Latest Tecan Files",
                                                           choices = wait_msg)),
                                     column(1, actionButton(ns("go_file"),
                                                           "Go")
                                      )),
                             tags$hr(),
                             conditionalPanel(
                                     # condition = paste0("output['",ns("type"),"'] == 'NADH Detection'" ),
                                     condition = "output.type == 'NADH Detection'",
                                     ns = ns,
                                     actionButton(inputId = ns("open_calibration"),
                                                  label = "Edit Concentrations")
                             )
                             # ,
                             # conditionalPanel(
                             #         condition = paste0("output['",ns("type"),"'] != 'NADH Detection'"),
                             #         tableOutput(ns("summary"))
                             # )
                ),
                mainPanel(
                        titlePanel(
                                textOutput(ns("type"))
                        ),
                        fluidRow(tags$div(id = ns("widgets_bar"),
                                          tags$hr())),
                        conditionalPanel(
                                # condition = paste0("output['",ns("type"),"'] != 'NADH Detection'"),
                                condition = "output.type != 'NADH Detection'",
                                ns = ns,
                                plotOutput(ns("hist")),
                                tableOutput(ns("summary")),
                                tableOutput(ns("batch"))
                        ),
                        conditionalPanel(
                                # condition = paste0("output['",ns("type"),"'] == 'NADH Detection'" ),
                                condition = paste0("output.type == 'NADH Detection'" ),
                                ns = ns,
                                plotOutput(ns("regression_graph")),
                                tableOutput(ns("calibration")),
                                tableOutput(ns("samples_predicted"))
                        )
                )
        )
}