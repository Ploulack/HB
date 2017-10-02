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

registry_key_names <- function(registry_url, registry_sheets) {
        library(readxl)
        if (!file.exists("registry/registry.xlsx")) drive_download(file = as_id(registry_url), path = "registry/registry.xlsx")
        registry_key_names <- map_dfr(
                registry_sheets,
                ~ read_xlsx(path = "registry/registry.xlsx",
                        sheet = .x,
                        range = cell_cols(1:2)
                )
        )
}
