gel_server <- function(input, output, session, gtoken) {
        library(jpeg);library(grid);library(magrittr); library(magick)
        source("gel/gel_values.R")
        source("drive_helpers.R")
        ns <- session$ns
        
        #Get mongo db connector for gel pictures data
        source("helpers/mongo_helpers.R")
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("mongo/db_values.R")
                gel_db <- db_from_environment(session, collection = "gel_photos")
        }
        gel_pics <- reactiveValues(files = NULL)
        
        #function to get files list
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
        
        #Display files list choice to user
        observeEvent(c(choiceFiles, input$refresh), {
                choices <- choiceFiles()
                updateSelectInput(session, "file",
                        choices = choices,
                        selected = ifelse(input$refresh == 0,
                                head(choices,1),
                                input$file)
                )
                
        })

        #Create reactive to have only the last click
        #(to prevent return to NULL of the input)
        #Store clicks with id to tie them to index later on
        last_click <- reactiveVal(value = NULL)
        clicks <- reactiveVal(as_tibble(x = NULL, y = NULL, id = NULL))
        
        observeEvent(input$click, {
                shiny::validate(need(!is.null(input$click), message = FALSE))
                last_click(input$click)
                clicks (isolate(clicks()) %>% bind_rows(
                        list("x" = last_click()$x,
                                "y" = last_click()$y,
                                "id" = dim(clicks())[1]+1)
                )
                )
        })
        
        #Create reactive to have only the last selected area
        #Insert two crop approve / cancel buttons
        last_drag <- reactiveVal(value = NULL)
        observeEvent(input$drag_area, {
                shiny::validate(need(!is.null(input$drag_area), message = FALSE))
                last_drag(input$drag_area)
                #Remove the last click as a click is triggered on a drag
                tbl <- clicks() %>% filter(id != dim(clicks())[1])
                clicks(tbl)
                
                if (!file_record()$entry_exists) {
                        insertUI(selector = paste0("#", ns("_bar")),
                                where = "afterEnd",
                                ui = fluidRow(
                                        actionButton(inputId = ns("store_crop"),
                                                label = "Store Crop,start taging"),
                                        actionButton(inputId = ns("cancel_crop"),
                                                label = "Cancel crop")
                                )
                        ) 
                }
                
        })
        #Remove buttons when: user cancels, stores the crop, or, changes file
        observeEvent(c(input$cancel_crop, coord_stored(), input$file), {
                if (coord_stored() ||
                                (!is.null(input$cancel_crop) && input$cancel_crop>0) ||
                                is.null(last_drag())) {
                        removeUI( paste0("div:has(>#",ns("store_crop"),")"))
                        removeUI( paste0("div:has(>#",ns("cancel_crop"),")"))
                }
        })
        
        img <- reactiveVal(value = NULL)
        
        #Download image
        get_picture <- eventReactive(input$file, {
                #Prevent re-download from Drive when the select files input is initialized or updated, 
                #Todo: make common function with Tecan file
                if (input$file %in% c(wait_msg)) return()
                else if (input$file == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified Google Drive folder: ",
                                        pics_folder)
                        ))
                        return()
                } else {
                        file_name <- paste0("temp/", input$file, "_raw.jpg")
                        if (!file.exists(file_name)) {
                                drive_download(file = gel_pics$files %>% filter(id == input$file),
                                        path = file_name,
                                        overwrite = TRUE)        
                        }
                        return(file_name)
                        
                        # picture_path(file_name)
                        # return(graph())
                }
        })
        
        #Get db records attached to new file
        file_record <- eventReactive(c(input$store_crop, input$file), {
                shiny::validate(need(!is.null(input$file) && input$file != wait_msg,message = FALSE))
                mongo_file_entry(gel_db, input$file)
        })
        
       
        #reactive 'flag' to know when db insert has been a success
        coord_stored <- reactiveVal(FALSE)
        
        #Insert crop coord in db when button pressed
        observeEvent(input$store_crop, {
                if (is.null(input$store_crop) || input$store_crop ==0) return()
                
                file_name <- gel_pics$files %>%
                        filter( id == input$file) %>%
                        pull(name)
                coord_str <- jsonlite::toJSON(last_drag()[1:4]) %>%
                        str_replace_all(pattern = "\\[|\\]", replacement = "")
                str <- paste0(
                        '{"file" : "',input$file,'","name" : "',file_name,'",
                                "crop_coord" : ',coord_str ,'}'
                )
                print("new entry")
                print(jsonlite::prettify(str))
                browser()
                insert_log <- gel_db$insert(str)
                if (insert_log$nInserted == 1 && length(insert_log$writeErrors) == 0) {
                        showNotification(ui = sprintf("New entry for file %s", file_name) ,
                                duration = 3,
                                type = "message")
                        coord_stored(TRUE)
                } else return()
        })
        
        #Todo: mettre dans un helper
        # picture_path <- reactiveVal(value = NULL)
        # graph <- reactiveVal(value = NULL)
                # shiny::validate(need(!is.null(picture_path()), message = FALSE))
        init_graph <- function(path) {
                img <- readJPEG(source = path, native = TRUE)
                img_grob <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)
                graph <- ggplot() +
                        scale_x_continuous(limits = c(0,1),name = "", expand = c(0,0)) +
                        scale_y_continuous(limits = c(0,1), name = "", expand = c(0,0)) +
                        annotation_custom(grob = img_grob) +
                        theme(
                                aspect.ratio = dim(img)[1]/dim(img)[2],
                                # aspect.ratio = 1,
                                # aspect.ratio = (1/16),
                                plot.margin =unit(c(0,0,0,0),"npc"),
                                axis.line = element_blank(),
                                axis.ticks = element_blank(),
                                axis.text= element_blank()
                        )
                return(graph)
        }
        
        base_graph <- reactive({
                source_name <- paste0("temp/", input$file, "_raw.jpg")
                cropped_name <- paste0("temp/", input$file, "_cropped.jpg")
                if (is.null(last_drag()) && !file_record()$entry_exists) {
                        list("graph" = get_picture() %>% init_graph(),
                                "is_cropped" = FALSE)
                } else {
                        if (!file.exists(cropped_name)) {
                                img <-  magick::image_read(get_picture())
                                img_info <- img %>% image_info()
                                #It's important that the drag is set set after get_picture
                                #because it's in get_picture that last_drag is reset on picture change
                                if (file_record()$entry_exists) drag <- file_record()$entry$crop_coord
                                else drag <- last_drag()
                                img %>%
                                        image_crop(paste0(
                                                (drag$xmax - drag$xmin) * img_info$width,
                                                "x",
                                                (drag$ymax - drag$ymin) * img_info$height,
                                                "+",
                                                drag$xmin * img_info$width,
                                                "+",
                                                (1- drag$ymax) * img_info$height
                                        )) %>%
                                        image_write(path = cropped_name)
                        }
                        list("graph" = init_graph(cropped_name),
                        "is_cropped" = TRUE)
                        }
        })
        
        output$gel <- renderPlot({
                if (!base_graph()$is_cropped){
                        base_graph()$graph
                } else {
                        base_graph()$graph + annotate("text",
                                x = clicks()$x,
                                y = clicks()$y,
                                label = clicks()$id,
                                size = 5,
                                angle = 80,
                                hjust = 0,
                                vjust = 0)
                # isolate({
                #         if (is.null(graph())) graph(base_graph()$graph)
                # })
                #         if(!is.null(last_click())) {
                #                 isolate({
                #                         graph(graph() +
                #                                         annotate("text",
                #                                                 x = last_click()$x,
                #                                                 y = last_click()$y,
                #                                                 label = "Some text",
                #                                                 size = 5,
                #                                                 angle = 80,
                #                                                 hjust = 0,
                #                                                 vjust = 0)
                #                         )
                #                 })
                #                 
                #         }
                #         return(graph())
                }
        }
                # , height =800,
                # width =
        )
        #Reset drag area and remove cropped file on cancel
        observeEvent(input$cancel_crop, {
                shiny::validate(need(!is.null(input$cancel_crop), message = FALSE))
                last_drag(NULL)
                file.remove(
                        paste0("temp/", input$file, "_cropped.jpg")
                )
        })
        
        #On picture change:
        #Reset drag area; #Reset db insert flag
        observeEvent(input$file, {
                last_drag(NULL); coord_stored(FALSE)
                # graph(NULL)
        })
        
        
        #Remove source & cropped pictures on stop or session end
        shiny::onStop(
                fun = function() {
                        file.remove(list.files("temp/", pattern = "jpg", full.names = TRUE))
                }
        )
}