library(shiny)

source("extract.R")

#Get dropbox folder meta data
db_folder <- drop_delta(path_prefix = "/HB/Tecan Data")

#Extract only files (not dirs) and tidy the date from file name
files <- db_folder$entries %>%
        filter(! as.logical(is_dir)) %>%
        select(path) %>% 
        mutate(exp_date = file_date(path),
               path = as.character(path)) %>%
        as_tibble() %>%
        arrange(desc(exp_date))

latest_exp <- tecan_extract(files$path[1], "temp/")

shinyServer(function(input, output) {
        
        output$batch <- renderDataTable({
                latest_exp$Batch_1$Measures
        })
} )