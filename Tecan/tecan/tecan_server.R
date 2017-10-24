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
                                "is_kinetic" = experiment$raw$kinetic ))
        })
        
        #A switch to keep track of db inserts
        data_tagged_and_saved <- reactiveVal(value = FALSE)
        
        callModule(tecan_db_server,
                id = "Tecan_db",
                #tecan_file = tecan_file,
                tecan_file = isolate(tecan_file()),
                gtoken = gtoken,
                data_switch = data_tagged_and_saved)
        
        observeEvent(input$file, {
                #Prevent re-download from Google Drive when the select files input is initialized or updated, 
                if (input$file == wait_msg) return()
                else if (input$file == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified Google Drive folder: ",
                                        "/HB/Tecan")
                        )
                        )
                } else {
                        experiment$raw <- tecan_extract(input$file, token, tecan$files)
                }
        })
        
        # Tell user if it's a 260 or 600nm
        output$type <- renderText({
                if (is.null(experiment$raw)) {
                        return()
                }
                if_else(
                        experiment$raw$kinetic,
                        "600nm",
                        "PCR Quantification - 260nm")
        })
        
        # Todo: une horreur, tout reprendre clean
        observeEvent(c(input$absorbance,input$path, experiment$raw, data_tagged_and_saved()),{
                if (is.null(experiment$raw$data) ) return()
                
                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)
                
                if (!experiment$raw$kinetic) {
                        experiment$calculated <- calc_values(experiment$raw$data,
                                absorbance,
                                path)
                }
                
                
                
                output$summary <- renderTable({
                        if (!data_tagged_and_saved()) return()
                        if (!experiment$raw$kinetic) {
                                experiment$calculated$Results
                        } else {
                                experiment$raw$data$Batch_1$Measures
                        }
                }, digits = 2)
                
                output$hist <- renderPlot({
                        if (!data_tagged_and_saved()) return()
                        graph_type <- !experiment$raw$kinetic
                        if (graph_type) {
                                df <- experiment$calculated$Results
                        } else {
                                df <- experiment$raw$data$Batch_1$Measures
                        }
                                
                        ggplot(df) +
                                aes(x = factor(Sample, levels = Sample),
                                        y = if(graph_type) {Concentration}
                                        else {Value},
                                        fill = if(graph_type) {Ratio > 1.7 & Ratio <2.0}
                                        else {Value > .2}) +
                                geom_bar(stat = "identity") +
                                theme(legend.position= c(.9,.9)) +
                                scale_x_discrete("Samples") + 
                                ylab (if_else(graph_type, "Concentration", "Value")) +
                                scale_fill_discrete(limits = c('FALSE', 'TRUE')) +
                                if (graph_type) {
                                        geom_text(
                                                aes( y = Concentration + mean(Concentration) * 0.03),
                                                label = format(df$Concentration, digits = 1)
                                        )
                                } else {
                                        geom_text(
                                                aes( y = Value + mean(Value) * 0.03),
                                                label = format(df$Value, digits = 1)
                                        )
                                }
                        
                        
                        # if (!experiment$raw$kinetic && data_tagged_and_saved()) {
                        #         df <- experiment$calculated$Results
                        #         ggplot(df) +
                        #                 aes(x = factor(Sample, levels = Sample),
                        #                         y = Concentration,
                        #                         fill = (Ratio > 1.7 & Ratio <2.0)) +
                        #                 geom_bar(stat = "identity") +
                        #                 theme(legend.position= c(.9,.9)) +
                        #                 scale_x_discrete("Samples") +
                        #                 scale_fill_discrete(limits = c('FALSE', 'TRUE')) +
                        #                 geom_text(aes(y = Concentration + mean(Concentration) * 0.03),
                        #                         label = format(df$Concentration, digits = 1))
                        # } else if(data_tagged_and_saved()) {
                        #         df <- experiment$raw$data$Batch_1$Measures
                        #         ggplot(df) +
                        #                 aes(x = factor(Sample, levels = Sample),
                        #                         y = Value,
                        #                         fill = Value > .2) +
                        #                 geom_bar(stat = "identity") +
                        #                 theme(legend.position= c(.9,.9)) +
                        #                 scale_x_discrete("Samples") +
                        #                 scale_fill_discrete(limits = c('FALSE', 'TRUE')) +
                        #                 geom_text(aes(y = Concentration + mean(Concentration) * 0.03),
                        #                         label = format(df$Concentration, digits = 1))
                        # }
                })
                
                observeEvent(data_tagged_and_saved(),{
                        showNotification(ui = str_interp("data tagged: ${data_tagged_and_saved()}") ,
                                         duration = 4,
                                         type = "message")
                }, ignoreInit = TRUE)
                
                output$batch <- renderDataTable({
                        if (!data_tagged_and_saved()) return()
                        if (!experiment$raw$kinetic) {
                                        return(experiment$calculated$Table)
                        } else {
                                        NULL
                        }
                })
                
        })
}