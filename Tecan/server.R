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


shinyServer(function(session, input, output) {
        #Fill the select file input with the files's dates as names and path as values
        updateSelectInput(session, "file",
                          choices = files$path %>%
                                  set_names(files$exp_date))
        
        observeEvent(input$file, {
                #Prevent re-download from dropbox when the select files input is initialized or updated, 
                if (input$file %in% c("Waiting from dropbox")) return()
                
                experiment$raw <- tecan_extract(input$file, "temp/")
                # if (experiment$raw == "File is for a 600nm test.") {
                #         showModal(modalDialog(
                #                 title = "Ooopps that's a 600nm file",
                #                 "Please use another file"))
                # } else { experiment$go <- Sys.time() } 
        })
        
        observeEvent(c(input$absorbance,input$path, experiment$raw),{
                if (is.null(experiment$raw$data) ) return()
                
                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)
                
                if (!experiment$raw$kinetic) {
                        experiment$calculated <-calc_values(experiment$raw$data,
                                                             absorbance,
                                                             path)
                }
                
                output$summary <- renderTable({
                        if (!experiment$raw$kinetic) {
                                experiment$calculated$Results
                        } else {
                                experiment$raw$data$Batch_1$Measures
                        }
                }, digits = 2)
                
                output$hist <- renderPlot({
                        if (!experiment$raw$kinetic) {
                                ggplot(experiment$calculated$Results) +
                                        aes(x = factor(Sample, levels = Sample),
                                            y = Concentration,
                                            fill = Concentration > 5) +
                                        geom_bar(stat = "identity") +
                                        theme(legend.position= c(.9,.9)) +
                                        scale_x_discrete("Samples")
                        } else {
                                ggplot(experiment$raw$data$Batch_1$Measures) +
                                        aes(x = factor(Sample, levels = Sample),
                                            y = Value,
                                            fill = Value > 5) +
                                        geom_bar(stat = "identity") +
                                        theme(legend.position= c(.9,.9)) +
                                        scale_x_discrete("Samples")
                        }
                        
                })
                
                if (!experiment$raw$kinetic) {
                        output$batch <- renderDataTable({
                                experiment$calculated$Table
                        })
                }
                
        })
        
        
        
       
        
        
} )