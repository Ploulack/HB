library(stringr)
library(lubridate)


file_date <- function(file_name, type = "tecan") {
        if (type == "tecan") {
                file_name %>%
                        str_extract("^[\\d\\s-]*") %>%
                        ymd_hms() %>% force_tz("America/Montreal")
        } else if ( type == "google_photo_app") {
                file_name %>%
                        ymd_hms(tz = "America/Montreal")
        }
}


get_ordered_filenames_from_drive <- function(drive_dir, type = "tecan", file_regex = ".xml$") {
        #TODO: Check year and folder
        
        drive_ls(path = drive_dir) %>%
                filter(name %>% str_detect(file_regex)) %>%
                mutate(exp_date = file_date(name, type)) %>%
                arrange(desc(exp_date)) %>%
                head(30)
}