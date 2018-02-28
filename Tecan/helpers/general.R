library(stringr); library(assertive)

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
               #MS
               ms = ifelse(is_dev, ms_dev_drive_url, ms_prod_drive_url),
               # Parent folder for CSVs needed by the Pooling method
               hami = ifelse(is_dev, protocols_hami_folder_dev, protocols_hami_folder_prod),
               # Spreadsheet to edit & track experiments
               experiments = ifelse(is_dev, protocols_sheet_dev, protocols_sheet_prod),
               # CSV with list of ongoing experiments
               experiments_csv = ifelse(is_dev, protocols_csv_dev, protocols_csv_prod),
               # Tecan files parent folder
               tecan = ifelse(is_dev, tecan_dev_drive_URL, tecan_prod_drive_URL)
               )
        if (is.null(res)) {
                warning("Unkown drive URL value")
                return(NULL)}
        else return(res)
}

obtain_file_data <- function(go_button,
                             tab_name,
                             file_container,
                             dat_container,
                             extract_function,
                             data_saved_flag = NULL) {

        observeEvent(go_button(), {
                #Prevent re-download from Google Drive when the select files input is initialized or updated,
                # TODO: Useful to check that it's not the same file ?
                if (file_container$id() == wait_msg) return()
                else if (file_container$id() == "") {
                        showModal(
                                modalDialog(
                                title = "No File",
                                str_interp("There's no files in the ${tab_name} Drive folder."))
                        )
                } else {
                        print(paste0("Extracting file : ", file_container$file_dribble()$name))
                        file_dat <- extract_function(file_container$id(), file_container$files())
                        dat_container(file_dat)
                        # experiment$raw <- tecan_extract(input$file, tecan$files)
                        if (!is.null(data_saved_flag)) data_saved_flag(FALSE)
                }
        }, priority = 3)
}