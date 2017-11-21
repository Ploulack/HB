source("hamilton/hamilton_values.R")
source("helpers/plates_helpers.R")

generate_var_files <- function(parts) {
        if (parts()$key %>% map_lgl(~ is.null(.x) || .x == "") %>% any()) return()
        #csv with part key || nb of pcr for that part
        paths <- list(parts = "temp/pcr_parts.csv",
                      counts = "temp/pcr_counts.csv") %>% unlist()
        
        write.csv(parts() %>%
                          select(key, n_pcr),
                  file = paths["parts"], quote = TRUE, eol = "\r\n", row.names = FALSE)
        
        write.csv(tibble(total_pcr = sum(parts()$n_pcr),
                         n_parts = nrow(parts())),
                  file = paths["counts"], quote = TRUE, eol = "\r\n", row.names = FALSE)
        return(paths)
}

gdrive_update_file <- function(local_path, drive_url) {
        drive_update(media = local_path,
                     file = as_id(drive_url))
}


generate_files <- function(parts) {
        generate_var_files(parts) %>%
                walk2(drive_csv_files, ~ gdrive_update_file(.x, .y))
}

generate_operator_sheets <- function(parts) {
        user_name <- drive_user()$displayName
        pcr_date <- Sys.time()
        first_parts <- str_c(parts()$key[1:min(3, nrow(parts()))], collapse = " ")
        file_name <- paste(user_name, pcr_date, first_parts, sep = "  |  ")
        desc_string <- str_interp("PCR Instruction by ${user_name} on ${pcr_date} with starting parts ${first_parts}")
        
        operator_ss_dbl <- drive_cp(file = as_id(pcr_template),
                 path = "Instructions/",
                 name = file_name)
        
        operator_ss <- gs_key(operator_ss_dbl$id)
        
        parts() %>%
                generate_plates() %>%
                #TODO: only update the part of the table that contains data...
                walk2(operator_ss$ws$ws_title, ~gs_edit_cells(ss = operator_ss,
                                        ws = .y,
                                        input = .x,
                                        anchor = "B2",
                                        col_names = FALSE,
                                        )
                )
        
        walk(operator_ss$ws$ws_title, ~ gs_edit_cells(
                ss = operator_ss,
                ws = .x,
                input = str_c(desc_string, ": ", .x, collapse = ""),
                anchor = "B11"
        ))
        
        operator_ss_dbl$drive_resource[[1]]$webViewLink
}