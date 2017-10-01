get_registry <- function(token) {
        library(googlesheets)
        library(tidyverse)
        source("registry/registry_values.R")
        
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
