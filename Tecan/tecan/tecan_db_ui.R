tecan_db_ui <- function(id) {
        ns <- NS(id)
        fluidRow(
                column(width = 3, offset = 0,
                        selectizeInput(
                                inputId = ns("tags"),
                                label = "Tags to search",
                                multiple = TRUE,
                                c("Choose one" = "", LETTERS)
                        )
                )
                ,
                column(width = 3, offset = 1,
                        textInput(inputId = ns("notes"),
                                label = "Free notes")
                ),
                column(width = 2, offset = 1,
                        h5("test"),
                        actionButton(inputId = ns("update"),
                                label = "Update info"))
        )
}