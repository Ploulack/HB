library(purrr)
source("protocols/protocols_values.R"); source("helpers/strings.R")

protocols_get <- function(drive_folder, prot_gsheet) {
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
                if (is.na(prot_row$folder_url)) {
                        update_hami_csv <- TRUE
                        #Create the drive directory
                        # First check if the directory doesn't already exist
                        tecan_folder_ls <- drive_ls(as_id(drive_folder))
                        if (!prot_row$name %in% (tecan_folder_ls %>% '[['("name"))) {
                                drbl <- drive_mkdir(name = prot_row$name, 
                                                    parent = as_id(drive_folder))
                                hami_drbl <- drive_mkdir(name = prot_row$name,
                                                         parent = as_id(protocols_hami_folder))
                        }
                                
                        else drbl <- tecan_folder_ls %>% filter(name == prot_row$name)
                        
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
                        }
                        
                        # Get the new folder link
                        links <- list(drbl, plates_processed, hami_drbl) %>%
                                map_chr(dribble_get_link)
                        
                        # insert folder and plates tracker url in the gsheet
                        gs_edit_cells(ss = prot_gsheet,
                                      ws = 1,
                                      input = links,
                                      anchor = paste0("F", i + 1),
                                      byrow = TRUE)
                        
                        #Add links to protocols
                        protocols[i, "folder_url"] <- links[1]
                        protocols[i, "plates_processed_url"] <- links[2]
                }
        }
        if (update_hami_csv) {
                tmp_path <- "temp/protocols_list.csv"
                browser()
                protocols %>%
                        select(index, name) %>%
                        filter(name != protocols$name[1]) %>%
                        mutate(index = index - 1) %>%
                        write.csv(tmp_path,
                                  quote = TRUE,
                                  eol = "\r\n",
                                  row.names = FALSE)
                protocols_csv %>%
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
        
        showModal(
                modalDialog(
                        div(str_interp("Custom message for ${file_name} is ${custom_msg}.")),

                        selectInput(inputId = ns("set_protocol"),
                                    label = "Select matching protocol",
                                    choices = protocols$name[-1] %>% prepend(default_set_prot)),

                        conditionalPanel(condition = paste0("input['", ns("set_protocol"), "'] != '", default_set_prot, "'"),
                                         selectInput(inputId = ns("set_plate_nb"),
                                                     label = "Select plate nb",
                                                     choices = "",
                                                     multiple = FALSE)),

                        if (!is.null(required_msg))
                                div(tags$b(required_msg, style = "color: red;")),
                        footer = tagList(actionButton(inputId = ns("ok_protocol"),
                                                      label = "OK"),
                                         tags$a(class = "btn btn-default",
                                                href = protocols_sheet,
                                                "Create new protocol",
                                                target = "_blank")),
                        size = "l"
                ))
        
        
}
