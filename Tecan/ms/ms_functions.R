
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
                checkboxInput(ns("is_48_wells_plate"),
                      "48 wells plate?",
                      FALSE),
        actionButton(ns("add_sample"), label = "Add Sample"),
        conditionalPanel(condition = paste0("output['", ns("has_elements"), "'] == true"),
                         actionButton(inputId = ns("create_files"),
                                      label = "Generate"))
        )
}

sample_row_ui <- function(id, strains, plasmids, positions) {
        ns <- NS(id)
        tags$div(id = ns("sample_row"),
                 fluidRow(
                         column(2, selectizeInput(inputId = ns("strain"),
                                        label = "Select Strain",
                                        choices = strains)
                                ),
                         column(2,
                                selectizeInput(inputId = ns("plasmid"),
                                        label = "Plasmid (optional)",
                                        choices = plasmids %>% prepend(NA))
                                ),
                         column(2,
                                selectizeInput(inputId = ns("pos"),
                                               label = "Plate pos",
                                               choices = positions)
                                ),
                         column(width = 2,
                                selectInput(inputId = ns("group_id"),
                                            label = "Tag Group",
                                            choices = 1:48 %>% prepend(NA))
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

update_from_input <- function(var_name, ms_samples, input, sample_label) {
        force(var_name)

        row <- reactive({
                which(ms_samples()$label == sample_label)
        })

        observeEvent(input[[var_name]], {
                samples <- ms_samples()
                samples[[var_name]][row()] <- input[[var_name]]
                ms_samples(samples)
                print(ms_samples())
        })
}

sample_row_server <- function(input, output, session, ms_samples, sample_label) {
        ns <- session$ns

        fields <- c("strain", "plasmid", "pos", "group_id")

        walk(
                fields,
                ~ update_from_input(., ms_samples, input, sample_label)
        )


        #Delete a part's UI and parts() row
        observeEvent(input$delete_sample, {
                removeUI(selector = paste0("#", ns("sample_row")))
                isolate(
                        ms_samples(
                                ms_samples() %>%
                                        filter(label != sample_label))
                )
        })
}

generate_sample_list_csv <- function(samples_tbl) {
        browser()
        samples_tbl %>%
                arrange(str_extract(pos, "[A-Z]"),
                        as.integer(str_extract(pos, "\\d+"))
                ) %>%
                mutate(ID = row_number(),
                        FILE_NAME = paste(strain,
                                         plasmid,
                                         ID,
                                         if_else(is.na(group_id), "", paste0("G-",group_id)),
                                         sep = "_"),
                       MS_FILE = "C:\\MassLynx\\UNITARY.PRO\\ACQUDB\\General_all_20180213.exp",
                       INLET_FILE = "long_hold",
                       #Nb 2 is the plate number, plate #1 is ran just before and includes standards and blanks...
                       SAMPLE_LOCATION = paste0("2:",str_sub(pos, end = 1), ",", str_sub(pos, start = 2)),
                       INJ_VOL = 3,
                       TYPE = "ANALYTE"
                       )
}