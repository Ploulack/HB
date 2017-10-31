tecan_server <- function(input, output, session, gtoken) {
        library(shiny)
        source("tecan/tecan_extract.R")
        #Set Drive directory
        source("tecan/tecan_values.R")
        source("drive_helpers.R")
        source("tecan/tecan_db_server.R")
        source("helpers/delete_file_button_module.R")
        source("helpers/mongo_helpers.R")
        source("tecan/tecan_nadh.R")
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("mongo/db_values.R")
                db <- db_from_environment(session, collection = "lab_experiments")
        }
        
        ns <- session$ns
        
        experiment <- reactiveValues()
        db_files <- reactiveValues()
        tecan <- reactiveValues(files = get_ordered_filenames_from_drive(as_id(drive_tecanURL)))
        removed_files <- reactiveVal(NULL)
        
        choiceFiles <- eventReactive( c(input$refresh, removed_files(), tecan$files),{
                dat <- tecan$files %>%
                        filter(id != if (is.null(removed_files())) {""} else {removed_files()}) %>%
                        select(id, exp_date)
                dat$id %>% set_names(dat$exp_date)
        })
        
        
        observeEvent(c(choiceFiles(), input$refresh), {
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
                                "type" = experiment$raw$type))
        })
        
        #A switch to keep track of db inserts
        data_tagged_and_saved <- reactiveVal(value = FALSE)
        
        callModule(tecan_db_server,
                   id = "Tecan_db",
                   tecan_file = tecan_file,
                   gtoken = gtoken,
                   data_switch = data_tagged_and_saved)
        
        callModule(module = delete_exp_files,
                   id = "delete_button",
                   tecan_file = tecan_file,
                   db = db,
                   files_list = choiceFiles(),
                   removed_files)
        
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
                        experiment$raw <- tecan_extract(input$file, tecan$files)
                        if (exists("data_tagged_and_saved")) rm("data_tagged_and_saved")
                        data_tagged_and_saved <- reactiveVal(FALSE)
                }
        }, priority = 1)
        
        # Tell user if it's a 260 or 600nm
        output$type <- reactive({
                if (is.null(experiment$raw)) {
                        return()
                } else {
                        experiment$raw$type
                }
        })
        outputOptions(output, "type", suspendWhenHidden = FALSE)
        
        # Todo: une horreur, tout reprendre clean
        observeEvent(c(input$absorbance,input$path, experiment$raw, data_tagged_and_saved()),{
                if (is.null(experiment$raw$data) ) return()
                
                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)
                
                if (experiment$raw$type == "NADH Detection") {
                        nadh_detection(nadh = experiment$raw$data$Batch_1$Measures,
                                       cal_conc = calibration_concentrations,
                                       input = input,
                                       output = output,
                                       ns = ns)
                } else {
                        # removeUI(selector = paste0("#", ns("open_calibration")))
                        if (experiment$raw$type == "DNA Quantification") {
                                experiment$calculated <- calc_values(experiment$raw$data,
                                                                     absorbance,
                                                                     path)
                        }
                        
                        output$summary <- renderTable({
                                if (!data_tagged_and_saved() && experiment$raw$type == "DNA Quantification") return()
                                if (experiment$raw$type == "DNA Quantification") {
                                        experiment$calculated$Results
                                } else {
                                        experiment$raw$data$Batch_1$Measures
                                }
                        })
                        
                        output$hist <- renderPlot({
                                if (!data_tagged_and_saved() && experiment$raw$type == "DNA Quantification") return()
                                
                                is_DNAquant <- experiment$raw$type == "DNA Quantification"
                                
                                if (is_DNAquant) {
                                        df <- experiment$calculated$Results
                                } else {
                                        df <- experiment$raw$data$Batch_1$Measures
                                }
                                
                                ggplot(df) +
                                        aes(x = factor(Sample, levels = Sample),
                                            y = if (is_DNAquant) {Concentration}
                                            else {Value},
                                            fill = if (is_DNAquant) {Ratio > 1.7 & Ratio < 2.0}
                                            else {Value > .2}) +
                                        geom_bar(stat = "identity") +
                                        theme(legend.position = c(.9,.9)) +
                                        scale_x_discrete("Samples") + 
                                        ylab(if_else(is_DNAquant, "Concentration", "Value")) +
                                        scale_fill_discrete(limits = c('FALSE', 'TRUE')) +
                                        if (is_DNAquant) {
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
                        })
                        
                        output$batch <- renderTable({
                                if (!data_tagged_and_saved() && experiment$raw$type == "DNA Quantification") return()
                                if (experiment$raw$type == "DNA Quantification") {
                                        return(experiment$calculated$Table)
                                } else {
                                        NULL
                                }
                        })       
                }
                
        })
}