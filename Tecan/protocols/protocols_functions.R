library(purrr); library(glue)
source("protocols/protocols_values.R"); source("helpers/strings.R")
source("helpers/general.R"); source("protocols/pooling.R")

protocols_get <- function(drive_folder, prot_gsheet, session) {
        #Trigger variable to update the csv in the Google Sync folder for hami methods
        update_hami_csv <- FALSE

        protocols <- prot_gsheet %>%
                gs_read() %>%
                mutate(date_finished = lubridate::dmy(date_finished)) %>%
                filter(is.na(date_finished) | date_finished > Sys.Date()) %>%
                mutate(index = 1:n())


        #Create directory and add directory link to spreadsheet if a protocol doesn't have its own
        for (i in (1:nrow(protocols))) {
                prot_row <- protocols[i, ]
                #TODO: Add another condition like the name is not empty
                if (is.na(prot_row$folder_url)) {
                        update_hami_csv <- TRUE
                        #Create the drive directory
                        # First check if the directory doesn't already exist
                        tecan_folder_ls <- drive_ls(as_id(drive_folder), type = "folder")

                        if (!prot_row$name %in% tecan_folder_ls$name) {
                                drbl <- drive_mkdir(name = prot_row$name,
                                                    parent = as_id(drive_folder))

                                hami_drbl <- drive_mkdir(name = prot_row$name,
                                                         parent = as_id(if (is_dev_server(session)) protocols_hami_folder_dev
                                                                        else protocols_hami_folder_prod))
                        }

                        else {
                                drbl <- tecan_folder_ls %>% filter(name == prot_row$name)
                                hami_drbl <- tecan_folder_ls %>% filter(name == prot_row$name)
                        }

                        #Create the plates index
                        stopifnot(is.integer(prot_row$total_plates))
                        temp_csv <- str_interp("${prot_row$name} plates processed date.csv")
                        path_temp_csv <- paste0("temp/",temp_csv)
                        protocol_folder_ls <- drive_ls(drbl)

                        if (!temp_csv %in% (protocol_folder_ls %$% name)) {
                                tibble(processed_date = rep("unprocessed", prot_row$total_plates),
                                       tecan_file_url = rep(NA, prot_row$total_plates)) %>%
                                        write_csv(path_temp_csv, col_names = TRUE)
                                plates_processed <- drive_upload(media = path_temp_csv,
                                                                 path = drbl,
                                                                 type = "text/csv")
                        } else {
                                plates_processed <- protocol_folder_ls %>% filter(name == temp_csv)
                                #TODO: add same fail proof mechanims for the hamilton folder
                        }

                        # Get the new folder link
                        links <- list(drbl, plates_processed, hami_drbl) %>%
                                map_chr(dribble_get_link)

                        colID <- gsheet_colID_from_tibble(protocols, "folder_url")

                        # insert folder and plates tracker url in the gsheet
                        gs_edit_cells(ss = prot_gsheet,
                                      ws = 1,
                                      input = links,
                                      anchor = paste0(colID, i + 1),
                                      byrow = TRUE)

                        #Add links to protocols
                        protocols[i, "folder_url"] <- links[1]
                        protocols[i, "plates_processed_url"] <- links[2]
                }
        }
        if (update_hami_csv) {
                tmp_path <- "temp/protocols_list.csv"

                protocols %>%
                        select(index, name) %>%
                        filter(name != protocols$name[1]) %>%
                        mutate(index = index - 1) %>%
                        write.csv(tmp_path,
                                  quote = TRUE,
                                  eol = "\r\n",
                                  row.names = FALSE)

                {if (is_dev_server(session)) protocols_csv_dev
                        else protocols_csv_prod} %>%
                        as_id() %>%
                        drive_update(media = tmp_path)
        }
        return(protocols)
}

protocols_set_modal <- function(input, file_name, custom_msg, protocols,required_msg = NULL, session) {
        #TODO: gestion db...
        ns <- session$ns
        csv_non_proc_str <- "unprocessed"
        default_set_prot <- "Select protocol"
        protocols_sheet <- {if (is_dev_server(session)) protocols_sheet_dev
                else protocols_sheet_prod}

        showModal(
                modalDialog(
                        div(str_interp("Custom message for ${file_name} is ${custom_msg}.")),

                        selectInput(inputId = ns("set_protocol"),
                                    label = "Select matching protocol",
                                    choices = protocols$name[-1] %>% prepend(default_set_prot)),

                        conditionalPanel(condition = paste0("input['", ns("set_protocol"), "'] != '", default_set_prot, "'"),
                                         fluidRow(
                                                 column(2, selectInput(inputId = ns("set_plate_nb"),
                                                                       label = "Select plate nb",
                                                                       choices = "",
                                                                       multiple = FALSE
                                                 )),
                                                 column(3, numericInput(ns("tecan_sample_vol"),
                                                                        label = "Sampling uL for Tecan reading",
                                                                        value = 10,
                                                                        min = 10,
                                                                        max = 50,
                                                                        step = 5
                                                                        )),
                                                 column(3, numericInput(ns("tecan_water_vol"),
                                                                        label = "Water added for Tecan reading",
                                                                        value = 40,
                                                                        min = 30,
                                                                        max = 50,
                                                                        step = 5
                                                 ))
                                         ),
                                         fluidRow(

                                                 column(3, numericInput(ns("well_volume"),
                                                                        label = "Wells to pool current vol",
                                                                        value = 50,
                                                                        min = 40,
                                                                        max = 200,
                                                                        step = 10
                                                 )),
                                                 column(3, numericInput(ns("dw_min_conc"),
                                                                        label = "Minimum final pool conc.",
                                                                        value = 100,
                                                                        min = 0,
                                                                        max = 1000,
                                                                        step = 5
                                                 )),
                                                 column(3, numericInput(ns("min_nb_wells"),
                                                                        label = "Min wells nb to pool",
                                                                        value = 1,
                                                                        min = 1,
                                                                        max = 96))
                                         ),
                                         # numericInput(ns("target_concentration"),
                                         #              label = "Target Concentration in ng/ul",
                                         #              value = 100,
                                         #              min = 100,
                                         #              max = 500,
                                         #              step = 100)
                                         fluidRow(
                                                 column(width = 6, tableOutput(ns("pooling"))),
                                                 column(offset = 1, width = 4,
                                                        htmlOutput(ns("deep_well")))
                                         )
                        ),

                        if (!is.null(required_msg))
                                div(tags$b(required_msg, style = "color: red;")),
                        footer = tagList(
                                actionButton(inputId = ns("ok_protocol"),
                                             label = "OK"),
                                # actionButton(inputId = ns("preview_pooling"),
                                #              label = "Preview"),
                                tags$a(class = "btn btn-default",
                                       href = protocols_sheet,
                                       "Create new protocol",
                                       target = "_blank")
                        ),
                        size = "l"
                ))


}

move_drive_file <- function(protocols, prot_name, tecan, input) {
        tecan$files %>%
                filter(id == input$file) %>%
                drive_mv(path = protocols %>%
                                 filter(name == prot_name) %>%
                                 pull(folder_url) %>%
                                 as_id())
}

update_uis <- function(prot_name, tecan, file_id, session) {
        # Switch UI to the protocol where this file was moved to
        updateSelectInput(session = session,
                          inputId = "protocol",
                          selected = prot_name)

        #Set which file to select after the files menu update
        tecan$selected_file <- file_id
}

render_pooling <- function(input, output, tecan_calculated) {

        pooling_res <- reactive({
                plate_pooling(tecan_calculated,
                              well_volume = input$well_volume,
                              min_dw_conc = input$dw_min_conc,
                              min_wells_nb = input$min_nb_wells,
                              tecan_sample_vol = input$tecan_sample_vol,
                              tecan_water_vol = input$tecan_water_vol)
        })

        output$pooling <- renderTable({
                pooling_res()$plate_pooled
        })

        output$deep_well <- renderText({
                dw <- pooling_res()$deep_well

                glue("
                <p>
                        <strong>Resulting Deep Well:</strong>
                </p>
                <ul>
                        <li>Pool from {dw$n_wells_pooled} wells</li>
                        <li>Concentration {round(dw$concentration, 2)}</li>
                        <li>Volume {round(dw$volume, 2)}</li>
                </ul>")
        })
        pooling_res()
}