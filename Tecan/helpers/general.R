library(stringr)

is_dev_server <- function(session) {
        host_name <- session$clientData$url_hostname
        dev_env <- c("ploulack", "127.0.0.1")
        prod_env <- c("hbio")
        if (any(str_detect(host_name, dev_env))) return(TRUE)
        else if (str_detect(host_name, prod_env)) return(FALSE)
        else stop("Unknown server environment")
}

get_drive_url <- function(session, name) {
        source("helpers/generic_values.R")
        source("ms/ms_values.R"); source("protocols/protocols_values.R")
        source("tecan/tecan_values.R")
        assert_all_are_non_missing_nor_empty_character(name)
        name <- name %>%
                tolower()

        is_dev <- is_dev_server(session)

        res <- switch(name,
               trash = {if (is_dev) trash_dev_drive_URL
                       else trash_prod_drive_URL},
               ms = ifelse(is_dev, ms_dev_drive_url, ms_prod_drive_url),
               hami = ifelse(is_dev, protocols_hami_folder_dev, protocols_hami_folder_prod),
               experiments_csv = ifelse(is_dev, protocols_csv_dev, protocols_csv_prod),
               tecan = ifelse(is_dev, tecan_dev_drive_URL, tecan_prod_drive_URL)
               )
        if (is.null(res)) {
                warning("Unkown drive URL value")
                return(NULL)}
        else return(res)
}