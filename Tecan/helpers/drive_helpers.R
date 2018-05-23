library(stringr)
library(lubridate)

download_drive_file <- function(folder = "temp/", input_file) {

        #Todo: find the input_file in the dribble
        if (!dir.exists(folder)) {
                dir.create(folder)
        }
        local_xml <- paste0(folder,input_file)
        drive_download(as_id(input_file), path = local_xml, overwrite = TRUE)
        local_xml
}


dribble_get_link <- function(dribble) {
        dribble %>%
                '[['("drive_resource") %>%
                '[['(1) %>%
                '[['("webViewLink")
}

file_date <- function(file_name, type = "tecan", drive_resource = NULL) {

        switch(type,
               tecan = file_name %>%
                       str_extract("^[\\d\\s-]*") %>%
                       ymd_hms() %>% force_tz("America/Montreal"),

               google_photo_app = file_name %>%
                       ymd_hms(tz = "America/Montreal"),

               ms = drive_resource %>%
                       map("createdTime") %>%
                       pluck(1) %>%
                       ymd_hms(tz = "America/Montreal")
               )
}

#TODO: Add tests !!!
get_ordered_filenames_from_drive <- function(drive_dir,
                                             type = "tecan",
                                             progress = NULL) {
        if (!is.null(progress)) progress$inc(.1, detail = "Getting file names")
        #TODO: Check year and folder

        filename_pattern <- switch(type,
                                   tecan = ".xml$",
                                   google_photo_app = ".jpg$",
                                   ms = ".xml$")

        drive_ls(path = drive_dir, pattern = filename_pattern) %>%
             mutate(exp_date = .$drive_resource %>%
                         map_chr("createdTime") %>%
                         ymd_hms(tz = "America/Montreal")) %>%
             arrange(desc(exp_date))
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
