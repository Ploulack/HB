library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(rdrop2)


source("extract.R")
source("constants.R")

dropbox_dir <- "/HB/Tecan"

#files <- get_ordered_filenames_from_db_dir(dropbox_dir)

experiment <- reactiveValues()
db_files <- reactiveValues()

shinyServer(function(session, input, output) {
        
        #Fill the select file input with the files's dates as names and path as values
        
        observeEvent(input$refresh, {
                db_files <- get_ordered_filenames_from_db_dir(dropbox_dir)
                updateSelectInput(session, "file",
                                  choices = db_files$path %>%
                                          set_names(db_files$exp_date),
                                  selected = input$file)
        })
      
          observeEvent(input$file, {
                #Prevent re-download from dropbox when the select files input is initialized or updated, 
                if (input$file %in% c("Waiting from dropbox")) {
                        db_files <- get_ordered_filenames_from_db_dir(dropbox_dir)
                        updateSelectInput(session, "file",
                                          choices = db_files$path %>%
                                                  set_names(db_files$exp_date))
                        return()}
                else if (input$file == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified dropbox folder: ",
                                       "/HB/Tecan")
                                )
                        )
                } else {
                        
                        experiment$raw <- tecan_extract(input$file)
                }
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
                                            fill = (Ratio > 1.7 & Ratio <2.0)) +
                                        geom_bar(stat = "identity") +
                                        theme(legend.position= c(.9,.9)) +
                                        scale_x_discrete("Samples") +
                                        scale_fill_discrete(limits = c('FALSE', 'TRUE'))
                        } else {
                                ggplot(experiment$raw$data$Batch_1$Measures) +
                                        aes(x = factor(Sample, levels = Sample),
                                            y = Value,
                                            fill = Value > .2) +
                                        geom_bar(stat = "identity") +
                                        theme(legend.position= c(.9,.9)) +
                                        scale_x_discrete("Samples") +
                                        scale_fill_discrete(limits = c('FALSE', 'TRUE'))
                        }
                        
                })
                
                if (!experiment$raw$kinetic) {
                        output$batch <- renderDataTable({
                                experiment$calculated$Table
                        })
                }
                
        })
        
        
        
       
        
        
} )