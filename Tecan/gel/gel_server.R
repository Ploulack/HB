gel_server <- function(input, output, session, gtoken) {
        source("gel/gel_values.R")
        source("drive_helpers.R")
        
        gel_pics <- reactiveValues(files = NULL)
        
        choiceFiles <- reactive({
                input$refresh
                gel_pics$files <- get_ordered_filenames_from_drive(
                        as_id(pics_folder_url),
                        type = "google_photo_app"
                )
                res <- gel_pics$files$id %>%
                        set_names(gel_pics$files$exp_date)
                return(res)
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
                        drive_download(file = gel_pics$files %>% filter(id == input$file),
                                path = "temp/picture.jpg",
                                overwrite = TRUE)
                        
                        output$picture <-  renderImage({
                                width  <- session$clientData$output_picture_width
                                
                                list(src = "temp/picture.jpg",
                                        #Todo: Change picture width with widget
                                        width = 1200)
                        }, deleteFile = TRUE)
                }
        })
}