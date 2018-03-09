
new_ms_modal <- function(ns, required_msg = NULL) {

        # paste0("input['", ns("new_ms_protocol"),"'] == 'Unitary'") %>%
        #                print()

        showModal(
                modalDialog(title = "Specify MS plate",
                            fluidPage(
                                    selectInput(inputId = ns("new_ms_protocol"),
                                                label = "Select Experiment for your plate",
                                                choices = c("Unitary", "alt_pt", "oxydocyclase") %>% prepend("")),
                                    conditionalPanel(condition = paste0("input['", ns("new_ms_protocol"),"'] == 'Unitary'"),
                                                     new_ms_unitary(ns)
                                                     )

                            ),
                            if (!is.null(required_msg))
                                    div(tags$b(required_msg, style = "color: red;")),
                            footer = tagList(
                                    actionButton(inputId = ns("new_ms_ok"),
                                                 label = "OK")
                            ),
                            size = "l",
                            easyClose = TRUE
                )
        )
}

new_ms_unitary <- function(ns) {
        tagList(
                checkboxInput("is_48_wells_plate",
                      "48 wells plate?",
                      FALSE),
        actionButton(ns("add_sample"), label = "Add Sample"),
        conditionalPanel(condition = paste0("output['", ns("has_elements"), "'] == true"),
                         actionButton(inputId = ns("create_files"),
                                      label = "Generate"))
        )
}