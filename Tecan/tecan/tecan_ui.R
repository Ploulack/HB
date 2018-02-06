tecan_ui <- function(id) {
        ns <- NS(id)
        source(file = "helpers/delete_file_button_module.R"); source("helpers/ui_generics/select_file_ui.R")

        fluidPage(
                sidebarPanel(width = 3,

                             #Conditional to only display two parameter inputs for relevant file type
                             conditionalPanel(
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
                                     )
                             ),

                             #Conditional not to display the refresh / delete buttons until file list received from drive
                             select_file_ui(ns("tecan")),
                             tags$hr(),
                             conditionalPanel(
                                     # condition = paste0("output['",ns("type"),"'] == 'NADH Detection'" ),
                                     condition = "output.type == 'NADH Detection'",
                                     ns = ns,
                                     actionButton(inputId = ns("open_calibration"),
                                                  label = "Edit Concentrations")
                             )
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