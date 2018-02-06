select_file_ui <- function(id) {
        ns <- NS(id)

        tagList(

                     #Conditional not to display the refresh / delete buttons until file list received from drive
                     conditionalPanel(
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
                                                   paste0("Latest ", id, " files"),
                                                   choices = wait_msg)),
                             column(1, actionButton(ns("go_file"),
                                                    "Open")
                             ))
        )
}