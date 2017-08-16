library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(rdrop2)


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

experiment <- reactiveValues()
#experiment$raw <- tecan_extract(files$path[1], "temp/")


shinyServer(function(session, input, output) {
        #Fill the select file input with the files's dates as names and path as values
        updateSelectInput(session, "files",
                          choices = files$path %>%
                                  set_names(files$exp_date))
        
        observeEvent(input$files, {
                #Prevent re-download from dropbox when the select files input is initialized or updated, 
                if (input$files %in% c("Waiting from dropbox")) return()
                
                experiment$raw <- tecan_extract(input$files, "temp/")
                if (experiment$raw == "File is for a 600nm test.") {
                        showModal(modalDialog(
                                title = "Ooopps that's a 600nm file",
                                "Please use another file"))
                } else { experiment$go <- Sys.time() } 
        })
        
        observeEvent(c(input$absorbance,input$path, experiment$go),{
                if (is.null(experiment$raw) ) return()
                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)
                experiment$calculated <- calc_values(experiment$raw,
                                                     absorbance,
                                                     path)
                output$summary <- renderTable({
                        experiment$calculated$Results
                }, digits = 2)
                
                output$hist <- renderPlot({
                        ggplot(experiment$calculated$Results) +
                                aes(x = factor(Sample, levels = Sample),
                                    y = Concentration,
                                    fill = Concentration > 5) +
                                geom_bar(stat = "identity") +
                                theme(legend.position= c(.9,.9))
                })
                
                output$batch <- renderDataTable({
                        experiment$calculated$Table
                })
        })
        
        
        
       
        
        
} )