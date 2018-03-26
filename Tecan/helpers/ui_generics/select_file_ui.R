select_file_ui <- function(id) {
    ns <- NS(id)

    tagList(
        #Conditional not to display the refresh / delete buttons until file list received from drive
        conditionalPanel(
            condition = paste0("input.file != '", wait_msg, "'"),
            ns = ns,
            fluidRow(
                column(4, selectInput(ns("protocol"),
                                      "Protocols",
                                      choices = "New")),
                column(2,
                      actionButton(ns("refresh"),
                                   label = "Check for new files")
                ),
                column(2,
                       delete_exp_files_ui(ns("delete_button"))
                )
            )
        ),
        fluidRow(
            column(10,
                selectInput(ns("file"),
                            paste0("Latest ", id %>% str_extract("^.*(?=-)"), " files"),
                            choices = wait_msg,
                            width = '100%')
            ),
            column(2,
                actionButton(ns("go_file"),
                             "Open")
            )
        )
    )
}