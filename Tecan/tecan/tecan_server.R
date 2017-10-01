tecan_server <- function(input, output, session, gtoken) {
        library(shiny)
        source("tecan/tecan_extract.R")
        #Set Drive directory
        source("tecan/tecan_values.R")
        source("drive_helpers.R")
        source("tecan/tecan_db_server.R")
        
        experiment <- reactiveValues()
        db_files <- reactiveValues()
        tecan <- reactiveValues(files = NULL)
        # cat("drive user ", unlist(googledrive::drive_user()))
        
        choiceFiles <- reactive({
                input$refresh
                #Todo: change to token from module call...
                tecan$files <- get_ordered_filenames_from_drive(as_id(drive_tecanURL))
                tecan$files$id %>%
                        set_names(tecan$files$exp_date)
        })
        
        
        observeEvent(c(choiceFiles, input$refresh), {
                choices <- choiceFiles()
                updateSelectInput(session, "file",
                        choices = choices,
                        selected = ifelse(input$refresh == 0,
                                head(choices,1),
                                input$file)
                )
                
        })
        
        tecan_file <- reactive({
                return(
                        list(
                                "file" = input$file,
                                "file_dribble" = tecan$files %>% filter(id == input$file),
                                "samples" = experiment$raw$data$Batch_1$Measures$Sample,
                                "type" = experiment$raw$kinetic ))
        })
        
        callModule(tecan_db_server,
                id = "Tecan_db",
                tecan_file = tecan_file,
                gtoken = gtoken)
        
        observeEvent(input$file, {
                #Prevent re-download from dropbox when the select files input is initialized or updated, 
                if (input$file %in% c("Waiting from dropbox")) return()
                else if (input$file == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified dropbox folder: ",
                                        "/HB/Tecan")
                        )
                        )
                } else {
                        experiment$raw <- tecan_extract(input$file, token, tecan$files)
                }
        })
        
        output$type <- renderText({
                if (is.null(experiment$raw)) {
                        return()
                }
                if_else(
                        experiment$raw$kinetic,
                        "600nm",
                        "PCR Quantification - 260nm")
        })
        
        observeEvent(c(input$absorbance,input$path, experiment$raw),{
                if (is.null(experiment$raw$data) ) return()
                
                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)
                # print(experiment$raw$data$Batch_1$Measures$Sample)
                
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
                } else {
                        output$batch <- renderDataTable({
                                NULL
                        })
                }
                
        })
}