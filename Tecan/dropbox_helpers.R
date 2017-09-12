require(stringr)
require(lubridate)
require(rdrop2)

#Extracte Dates from names of Tecan files
file_date <- function(db_path, type = "tecan") {
        if (type == "tecan") {
                db_path %>%
                        as.character() %>%
                        basename() %>%
                        str_extract("^[\\d\\s-]*") %>%
                        ymd_hms() %>% force_tz("America/Montreal")
        } else if ( type == "db_app") {
                db_path %>%
                        basename() %>%
                        ymd_hms(tz = "America/Montreal")
        }
}

get_ordered_filenames_from_db_dir <- function(db_folder, token, type = "tecan") {
        #Todo: use the delta method to only get the new file
        #Get dropbox folder meta data
        ##Todo: add directory check
        drop_dir(db_folder, dtoken = token, n = 10) %>%
                select(path) %>% 
                mutate(exp_date = file_date(path, type),
                        path = as.character(path)) %>%
                as_tibble() %>%
                arrange(desc(exp_date)) %>%
                head(30)
}