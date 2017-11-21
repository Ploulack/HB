source("registry/registry_values.R")

get_registry <- function(token) {
        library(googlesheets)
        library(tidyverse)
        
        
        #Todo: change token to force Auth...
        #for now, using Drive read enabled token in place for order DNA decision..
        
        #Getting the Google APIs key & secret...
        #Todo: rename from 'MS' make generic
        source("MS/MS_values.R")
        gs_auth(token = token,
                key = hblab_id,
                secret = hblab_secret,
                cache = TRUE)
        
        sheet_ref <- gs_url(registry_url)
        
        sheet_ref %>%
                gs_read(ws = 1, col_names = TRUE)
}

registry_key_names <- function(registry_url, registry_sheets, file = registry_file) {
        library(readxl)
        registry_modified <- drive_get(as_id(registry_url))$drive_resource[[1]]$modifiedTime %>%
                ymd_hms(tz = "America/Montreal", quiet = TRUE)
        registry_synced <- file.mtime(file)
        outdated <- registry_modified > registry_synced
        
        if (!file.exists(file) || outdated ) 
        {drive_download(file = as_id(registry_url),
                        path = file,
                overwrite = TRUE)}
        map_dfr(
                registry_sheets,
                ~ read_xlsx(path = file,
                            sheet = .x,
                            range = cell_cols(c(1:3,5:6))
                )
        ) %>%
                mutate(Length = as.integer(Length))
}
