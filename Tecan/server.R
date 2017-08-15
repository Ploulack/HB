library(shiny)

source("extract.R")
source("constants.R")

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
latest_exp <- calc_values(latest_exp, beers_constants$Value[1], beers_constants$Value[2])

shinyServer(function(input, output) {
        
        output$summary <- renderTable({
                latest_exp$Results
        }, digits = 2)
        
        output$batch <- renderDataTable({
                latest_exp$Table
        })
} )