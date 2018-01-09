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

#TODO: Add tests !!!
get_ordered_filenames_from_drive <- function(drive_dir,
                                             type = "tecan",
                                             filename_pattern = ".xml$",
                                             progress = NULL) {
        if (!is.null(progress)) progress$inc(.1, detail = "Getting file names")
        #TODO: Check year and folder
        if (type == "google_photo_app") {
                filename_pattern <- ".jpg$"
                }
        drive_ls(path = drive_dir, pattern = filename_pattern) %>%
                #Filter out non target files
                #filter(name %>% str_detect(file_regex)) %>%
                mutate(exp_date = file_date(name, type)) %>%
                arrange(desc(exp_date))
        # %>%
        #         head(30)
}