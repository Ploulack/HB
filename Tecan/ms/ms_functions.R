
new_ms_modal <- function(ns, required_msg = NULL) {

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

sample_row_ui <- function(id, strains, plasmids) {
        ns <- NS(id)
        tags$div(id = ns("sample_row"),
                 fluidRow(
                         column(2, selectizeInput(inputId = ns("strain"),
                                        label = "Select Strain",
                                        choices = strains)
                                ),
                         column(2,
                                selectizeInput(inputId = ns("plasmid"),
                                        label = "Select Plasmid (optional)",
                                        choices = plasmids)
                                ),
                         column(width = 2,
                                selectInput(inputId = ns("group_id"),
                                            label = "Tag Group",
                                            choices = 1:48)
                                ),
                         column(width = 1,
                                actionButton(inputId = ns("delete_sample"),
                                                        label = "Delete")
                                )
                 ),
                 fluidRow(
                         tableOutput(outputId = ns("info")),
                         tags$hr()
                 ))
}

update_from_input <- function(ms_samples, input_react, row_nb, var_name) {
        observeEvent(input_react, {
                browser()
                samples <- ms_samples()
                samples[[var_name]][pos_row()] <- input_react
                ms_samples(samples)
        }, ignoreInit = TRUE)
}

sample_row_server <- function(input, output, session, ms_samples, sample_pos) {
        ns <- session$ns

        pos_row <- reactive({
                which(ms_samples()$pos == sample_pos)
        })

        update_from_input(ms_samples, input$strain, pos_row, "strain")
        update_from_input(ms_samples, input$plasmid, pos_row, "plasmid")
        update_from_input(ms_samples, input$group_id, pos_row, "group_id")


        #Delete a part's UI and parts() row
        observeEvent(input$delete_part, {
                removeUI(selector = paste0("#", ns("sample_row")))
                isolate(
                        ms_samples(
                                ms_samples() %>%
                                        filter(pos != sample_pos))
                )
        })
}