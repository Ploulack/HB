gel_server <- function(input, output, session) {
        source("gel/gel_values.R")
        source("dropbox_helpers.R")
        
        gel_pics <- reactiveValues()
        
        choiceFiles <- reactive({
                input$refresh
                db_files <- get_ordered_filenames_from_db_dir(
                        pics_folder,
                        token,
                        type = "db_app")
                db_files$path %>%
                        set_names(db_files$exp_date)
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
        
        observeEvent(input$file, {
                #Prevent re-download from dropbox when the select files input is initialized or updated, 
                if (input$file %in% c("Waiting from dropbox")) return()
                else if (input$file == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified dropbox folder: ",
                                        pics_folder)
                        )
                        )
                } else {
                        drop_get(path = input$file,
                               local_file = "temp/picture.jpg",
                               overwrite = TRUE)
                        
                        output$picture <-  renderImage({
                                width  <- session$clientData$output_picture_width
                                
                                list(src = "temp/picture.jpg",
                                        #Todo: Change picture width with widget
                                        width = 1200)
                                #TODO: Keep picture but delete on session Ends
                        }, deleteFile = TRUE)
                }
        })
}