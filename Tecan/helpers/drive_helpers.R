library(stringr)
library(lubridate)

dribble_get_link <- function(dribble) {
        dribble %>%
                '[['("drive_resource") %>%
                '[['(1) %>%
                '[['("webViewLink")
}

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


drive_create_or_get_folder <- function(parent_folder, folder_name) {
        assert_all_are_non_missing_nor_empty_character(c(parent_folder, folder_name))

        # Get parent folder listing
        parent_folder_ls <- parent_folder %>%
                as_id() %>%
                drive_ls(type = "folder")

        # First check if the directory doesn't already exist...
        if (!folder_name %in% parent_folder_ls$name) {
                #...create it if not
                drbl <- drive_mkdir(name = folder_name,
                                    parent = as_id(parent_folder))
        } else {
                # ... get the dribble if yes
                drbl <- parent_folder_ls %>%
                        filter(name == folder_name)
        }
}
