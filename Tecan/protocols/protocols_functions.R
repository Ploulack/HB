library(purrr)
source("protocols/protocols_values.R")

protocols_get <- function(drive_folder) {
        prot_gsheet <- gs_key(protocols_sheet)
        
        protocols <- prot_gsheet %>%
                gs_read() %>%
                mutate(date_finished = lubridate::dmy(date_finished)) %>%
                filter(is.na(date_finished) | date_finished > Sys.Date())
        
        #Create directory and add directory link to spreadsheet if a protocol doesn't have its own
        for (i in (1:nrow(protocols))) {
                prot_row <- protocols[i, ]
                if (is.na(prot_row$folder_url)) {
                        #Create the drive directory
                        # First check if the directory doesn't already exist
                        tecan_folder_ls <- drive_ls(as_id(drive_folder))
                        if (!prot_row$name %in% (tecan_folder_ls %>% '[['("name")))
                                drbl <- drive_mkdir(name = prot_row$name, 
                                                    parent = as_id(drive_folder))
                        else
                                drbl <- tecan_folder_ls %>% filter(name == prot_row$name)
                        
                        # Gett the new folder link
                        link <- drbl %>%
                                '[['("drive_resource") %>%
                                '[['(1) %>%
                                '[['("webViewLink")
                        
                        # insert folder url in the gsheet
                        gs_edit_cells(ss = prot_gsheet,
                                      ws = 1,
                                      input = link,
                                      anchor = paste0("D", i + 1))
                }
        }
        
        return(protocols)
}


