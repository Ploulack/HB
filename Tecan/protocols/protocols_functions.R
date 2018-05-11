library(purrr); library(glue)
library(magrittr)
source("protocols/protocols_values.R"); source("helpers/strings.R")
source("helpers/general.R"); source("protocols/pooling.R")
source("helpers/drive_helpers.R")


get_plates_tracking_drbl <- function(prot_row, drbl, tab_name) {

        #Create the plates index
        stopifnot(is.integer(prot_row$total_plates))
        temp_csv <- str_interp("${prot_row$name} plates ${tab_name} processed info.csv")
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
}

protocols_get <- function(drive_folder, prot_gsheet, session, tab_name) {
        #Trigger variable to update the csv in the Google Sync folder for hami methods
        update_hami_csv <- FALSE

        folder_url <- paste0(tab_name,"_folder_url")
        csv_folder <- paste0(tab_name, "_csv_folder_url")

        protocols <- prot_gsheet %>%
                gs_read() %>%
                mutate(date_finished = lubridate::dmy(date_finished)) %>%
                filter(is.na(date_finished) | date_finished > Sys.Date()) %>%
                mutate(index = 1:n()) #TODO: se souvenir de retirer cet index

        expected_names <- c("name", "protocol", "strain", "plasmid", "total_plates", "date_finished",
                            "description", "tecan_processed_plates","tecan_folder_url", "tecan_plates_processed_url",
                            "tecan_csv_folder_url", "ms_processed_plates", "ms_folder_url", "ms_plates_processed_url",
                            "ms_csv_folder_url", "index")

        if (!protocols %>% names() == expected_names) stop("Problem with HB Experiments column headers")


        #Create directory and add directory link to spreadsheet if a protocol doesn't have its own
        #TODO: create folders not existing
        for (i in (1:nrow(protocols))) {
                prot_row <- protocols[i, ]

                #TODO: Add a condition like the name is not empty

                if (is.na(prot_row %>% "["(folder_url))) {
                     # Get or create the folder in the TECAN or MS parent folder
                     # (depending on which tab the user has opened)
                        drbl <- drive_create_or_get_folder(parent_folder = drive_folder,
                                                   folder_name = prot_row$name)

                        # Get or create the csv that will track the plates processed and
                        # be placed inside the just created folder
                        plates_processed <- get_plates_tracking_drbl(prot_row, drbl, tab_name)
                        stopifnot(is_dribble(plates_processed))

                        # Get the new folder and csv links
                        links <- list(drbl, plates_processed) %>%
                                map_chr(dribble_get_link)

                        #Add processed plate tracking csv link to current protocols tibble
                        protocols[i, paste0(tab_name, "_plates_processed_url")] <- links[2]

                        if (is.na(prot_row[[csv_folder]])) {
                             # If we're in TECAN this creates the folder for Hamilton
                             # Where, after a tecan sampling has been run, the CSV values for the pooling will be placed
                                csv_folder_drbl <- drive_create_or_get_folder(get_drive_url(session, csv_folder),
                                                                        prot_row$name)
                                #Add csv folder link to current protocols tibble
                                protocols[i, csv_folder] <- dribble_get_link(csv_folder_drbl)
                                links[3] <- dribble_get_link(csv_folder_drbl)
                        }
                        update_hami_csv <- TRUE

                        protocols[i, folder_url] <- links[1]

                        colID <- gsheet_colID_from_tibble(protocols, folder_url)

                        # insert folder and plates tracker url in the gsheet
                        gs_edit_cells(ss = prot_gsheet,
                                      ws = 1,
                                      input = links,
                                      anchor = paste0(colID, i + 1),
                                      byrow = TRUE)
                }
        }
        #Update the list of experiments csv file used by hamilton method to pop the user experiment selection
        #for experiment dependent methods (eg: Pooling)
        if (update_hami_csv & tab_name == "tecan") {

                tmp_path <- "temp/experiments_list.csv"

                protocols %>%
                        select(index, name) %>%
                        filter(name != protocols$name[1]) %>%
                        mutate(index = index - 1) %>%
                        write.csv(tmp_path,
                                  quote = TRUE,
                                  eol = "\r\n",
                                  row.names = FALSE)

                get_drive_url(session, "experiments_csv") %>%
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
                                                 ))
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


move_drive_file <- function(tecan_n, prot_name, instrument) {
        folder_url <- paste0(instrument, "_folder_url")

        tecan_n$files() %>%
                filter(id == tecan_n$id()) %>%
                drive_mv(path = tecan_n$protocols() %>%
                                 filter(name == prot_name) %>%
                                 pull(folder_url) %>%
                                 as_id()
                         )
}

update_selected <- function(prot_name, file_id) {
        list(protocol = prot_name,
             file_id = file_id)
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