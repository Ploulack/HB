gel_server <- function(input, output, session, gtoken) {
        library(jpeg);library(grid);library(magrittr); library(magick); library(glue)
        source("gel/gel_values.R")
        source("drive_helpers.R")
        source("registry/registry_helpers.R")
        source("registry/registry_values.R")
        source("helpers/strings.R")
        source("gel/gel_functions.R")
        
        if (!exists("registry")) {
                registry <- registry_key_names(registry_url, registry_sheets)
        }
        
        
        # source("gel/parts_tagging.R")
        ns <- session$ns
        
        #Get mongo db connector for gel pictures data
        source("helpers/mongo_helpers.R")
        #Only initiate mongo connexion when needed
        if (!exists("gel_db")) {
                source("mongo/db_values.R")
                gel_db <- db_from_environment(session, collection = "gel_photos")
        }
        gel_pics <- reactiveValues(files = NULL, current_file = NULL)
        
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
        observeEvent(c(choiceFiles(), input$refresh), {
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
        last_click <- reactiveVal(value = NULL)
        #Store clicks with id to tie them to index later on
        clicks <- reactiveVal(as_tibble(colnames(c("x", "y","xmax", "ymax","sample", "key"))))
        
        #Create reactive to have only the last selected area
        #Use one for each usage type (crop image, label image)
        crop_drag <- reactiveVal(value = NULL)
        label_drag <- reactiveVal(value = NULL)
        
        observeEvent(input$drag_area, {
                if (!base_graph()$is_cropped) {
                        crop_drag(input$drag_area)
                } else {
                        label_drag(input$drag_area)
                }
                
        })
        
        #Display modal message to user that he can't start tagging if crop view not saved
        observeEvent(c(input$click, label_drag()), {
                #Todo: test to remove
                shiny::validate(need(!is.null(input$click), message = FALSE))
                last_click(input$click)
                #Only start building labeling list if the crop view is perma
                if (file_record()$entry_exists || coord_stored()) return()
                if (base_graph()$is_cropped) {
                        showModal(modalDialog(
                                title = "Save the cropped view",
                                "Can't let you place labels unless the cropped view is permanently saved.
                                        Use the button on the left",
                                footer = NULL,
                                easyClose = TRUE))
                }
        })
        

        #Remove buttons when: user cancels, stores the crop, or, changes file
        observeEvent(c(input$cancel_crop, coord_stored(), input$file), {
                if (coord_stored() ||
                                (!is.null(input$cancel_crop) && input$cancel_crop > 0) ||
                                is.null(crop_drag())) {
                        removeUI( paste0("div:has(>#",ns("store_crop"),")"))
                        removeUI( paste0("div:has(>#",ns("cancel_crop"),")"))
                }
        })
        
        #Display buttons to save the crop coordinates or cancel the crop
        observeEvent(base_graph(),{
                if (base_graph()$is_cropped &&
                                !file_record()$entry_exists
                ) {
                        output$buttons <- renderUI({
                                fluidRow(
                                        actionButton(inputId = ns("store_crop"),
                                                label = "Store Crop,start taging"),
                                        actionButton(inputId = ns("cancel_crop"),
                                                label = "Cancel crop")
                                )
                        })
                }
        }, ignoreNULL = TRUE)
        
        #Reset drag area and remove cropped file on cancel
        observeEvent(input$cancel_crop, {
                shiny::validate(need(!is.null(input$cancel_crop), message = FALSE))
                crop_drag(NULL)
                file.remove(
                        paste0("temp/", input$file, "_cropped.jpg")
                )
        })
        
        #Get db records attached to new file
        file_record <- eventReactive(c(input$store_crop, input$file), {
                shiny::validate(need(!is.null(input$file) && input$file != wait_msg,message = FALSE))
                mongo_file_entry(gel_db, input$file)
        })
        
        
        #reactive 'flag' to know when db insert has been a success
        coord_stored <- reactiveVal(FALSE)
        
        #Insert crop coord in db when button pressed
        #Todo: clean with better toJson and db update methods
        observeEvent(input$store_crop, {
                if (input$store_crop == 0) return()
                
                coord_str <- jsonlite::toJSON(crop_drag()[1:4]) %>%
                        str_replace_all(pattern = "\\[|\\]", replacement = "")
                str <- paste0(
                        '{"file" : "',input$file,'","name" : "',gel_pics$current_file,'",
                                "crop_coord" : ',coord_str ,'}'
                )
                print("new entry")
                print(jsonlite::prettify(str))
                insert_log <- gel_db$insert(str)
                if (insert_log$nInserted == 1 && length(insert_log$writeErrors) == 0) {
                        showNotification(ui = sprintf("New entry for file %s", gel_pics$current_file) ,
                                duration = 3,
                                type = "message")
                        coord_stored(TRUE)
                } else return()
        })
        
        #Download image
        ##Todo: make common function with Tecan file
        get_picture <- eventReactive(input$file, {
                #Prevent re-download from Drive when the select files input is initialized or updated, 
                
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
                }
        })

        img <- reactiveVal(value = NULL)
        
        #Rrepare the background picture then init graph
        base_graph <- reactive({
                input$picture_quality
                source_name <- paste0("temp/", input$file, "_raw.jpg")
                cropped_name <- paste0("temp/", input$file, "_cropped.jpg")
                drag <- crop_drag()
                if (is.null(drag) && !file_record()$entry_exists) {
                        list("graph" = get_picture() %>% init_graph(),
                                "is_cropped" = FALSE)
                } else {
                        if (!file.exists(cropped_name)) {
                                img <-  magick::image_read(get_picture())
                                img_info <- img %>% image_info()
                                if (file_record()$entry_exists) drag <- file_record()$entry$crop_coord
                                # else drag <- crop_drag()
                                img %>%
                                        image_crop(paste0(
                                                (drag$xmax - drag$xmin) * img_info$width,
                                                "x",
                                                (drag$ymax - drag$ymin) * img_info$height,
                                                "+",
                                                drag$xmin * img_info$width,
                                                "+",
                                                (1 - drag$ymax) * img_info$height
                                        )) %>%
                                        image_write(path = cropped_name,
                                                    quality = 25,
                                                    depth = 8)
                        }
                        list("graph" = init_graph(cropped_name),
                                "is_cropped" = TRUE)
                }
        })
        
        output$gel <- renderPlot({
                if (!base_graph()$is_cropped) {
                        base_graph()$graph
                } else {
                        rects <- clicks() %>%
                                filter(!is.na(xmax))
                        texts <- clicks() %>%
                                mutate(x = ifelse(is.na(clicks()$xmax),
                                                  clicks()$x,
                                                  clicks()$x + (clicks()$xmax - clicks()$x) / 2
                                                  ),
                                       y = ifelse(is.na(clicks()$ymax),
                                                  clicks()$y,
                                                  clicks()$y + (clicks()$ymax - clicks()$y) * .80
                                                  ))
                        base_graph()$graph +
                                annotate("rect",
                                         xmin = rects$x,
                                         xmax = rects$xmax,
                                         ymin = rects$y,
                                         ymax = rects$ymax,
                                         alpha = 0,
                                         colour = "grey",
                                         linetype = "dotted",
                                         size = 2) +
                                annotate("text",
                                         x = texts$x, y = texts$y,
                                         label = paste0(texts$sample, "\n",
                                                        texts$key,ifelse(
                                                                texts$type == "Part",
                                                                paste0(" - ", texts$length, "bp"),
                                                                "")),
                                         size = 5, angle = 80, hjust = 0, vjust = 0,
                                         color = "white", fontface = "bold"
                                ) 
                        
                }
        })
        
        #Code for the remove button logic
        source("gel/gel_sidebar_lane_remove.R")
        
        #To store the last removed label-sample
        last_del <- reactiveVal(NULL)
        
        #Diplay modal dialog for the user to tag a sample after a double click
        tag_sample_modal <- function(required_msg = NULL) {
                showModal(
                        modalDialog(
                                radioButtons(inputId = ns("sample_type"),
                                             label = "Select Sample type",
                                             choices = c("Part",non_part_types),
                                             inline = TRUE),
                                conditionalPanel(condition = paste0("input['",ns("sample_type"),"'] != 'Part'" ),
                                                 textInput(inputId = ns("non_part_key"),
                                                           label = "Enter descriptive key",
                                                           value = "",
                                                           placeholder = "Short text to appear on picture")),
                                conditionalPanel(condition = paste0("input['",ns("sample_type"),"'] == 'Part'" ),
                                                 selectizeInput(inputId = ns("part_key"),
                                                                label = "Sample's Part key",
                                                                choices =  registry$KEY %>%
                                                                        prepend(""))
                                ),
                                if (!is.null(required_msg))
                                        div(tags$b(required_msg, style = "color: red;")),
                                footer = actionButton(inputId = ns("ok"),
                                                      label = "OK")
                        )
                )
        }
        
        #Open modal dialog on double click or drag
        observeEvent(c(input$click,label_drag()), {
                if (!base_graph()$is_cropped) return()
                if (!(coord_stored() || file_record()$entry_exists)) return()
                tag_sample_modal()
        },ignoreInit = TRUE, ignoreNULL = TRUE)
        
        observeEvent(input$ok, {
                is_non_part <- input$sample_type %in% non_part_types
                
                if ((input$part_key == "" && !is_non_part ) || (input$non_part_key == "" && is_non_part)) {
                        tag_sample_modal("Need to complete")
                } else {
                        removeModal()
                        
                        sample_label <- first_unused(clicks()$sample)
                        sample_key <- if_else(is_non_part, input$non_part_key, input$part_key)
                        length <- ifelse(is_non_part,
                                         NA,
                                         registry %>%
                                                 filter(KEY == input$part_key) %>%
                                                 pull(Length))
                        
                        if (is.null(label_drag())) {
                                label_coords <- list("x" = last_click()$x,
                                                     "y" = last_click()$y,
                                                    "xmax" = NA,
                                                    "ymax" = NA)
                        } else {
                                label_coords <- list("x" = label_drag()$xmin,
                                                     "y" = label_drag()$ymin,
                                                     "xmax" = label_drag()$xmax,
                                                     "ymax" = label_drag()$ymax) 
                        }
                        new_sample <- list("x" = label_coords$x,
                                "y" = label_coords$y,
                                "xmax" = label_coords$xmax,
                                "ymax" = label_coords$ymax,
                                "sample" = sample_label,
                                "key" = sample_key,
                                "type" = input$sample_type,
                                "length" = length)
                        clicks(clicks() %>% bind_rows(new_sample))
                        
                        # Upload to gel_db the label coordinates
                        save_sample(db = gel_db,
                                    sample = new_sample,
                                    file_id = input$file,
                                    file_name = gel_pics$current_file,
                                    sample_label = sample_label)
                        
                        display_remove_button(ui_id = ns(sample_label),
                                #ui_id = ns(paste0(gel_pics$current_file,sample_label)),
                                              sample_key = sample_key,
                                              sample_label = sample_label,
                                              clicks = clicks,
                                              last_del = last_del,
                                current_file = input$file)
                                              #current_file = gel_pics$current_file)
                last_click(NULL); label_drag(NULL)
                }
        })

        observeEvent(last_del(), {
                delete_sample(db = gel_db,
                        file_id = input$file,
                        file_name = gel_pics$current_file,
                        sample_label = last_del())
        }, ignoreInit = TRUE)
        
        
        #On picture change:
        #Reset drag area, click,  db insert flag, labels / clicks tibble
        observeEvent(input$file, {
                if (is.null(input$file) || input$file == wait_msg) return()
                
                #Resetting
                crop_drag(NULL); last_click(NULL); coord_stored(FALSE); last_del(NULL)
                
                #initializing...
                gel_pics$current_file <- gel_pics$files %>%
                        filter(id == input$file) %>%
                        pull(name)
                
                if (file_record()$entry_exists &&
                   !is.null(file_record()$entry$labels[[1]]) &&
                   !length(file_record()$entry$labels[[1]]) == 0 &&
                    nrow(file_record()$entry$labels[[1]]) != 0) {
                        
                        labels_tbl <- file_record()$entry$labels[[1]] %>% as_tibble()
                        
                        #for old entries that did not have rects labels
                        if (!"xmax" %in% names(labels_tbl)) {
                                labels_tbl <- labels_tbl %>% mutate(xmax = NA, ymax = NA)
                        }
                        
                        clicks(labels_tbl)
                        
                        for (i in 1:nrow(clicks())) {
                                smpl <- clicks()[i,]
                                display_remove_button(ui_id = ns(smpl$sample),
                                                      sample_key = smpl$key,
                                                      sample_label = smpl$sample,
                                                      clicks = clicks,
                                                      last_del = last_del,
                                                      current_file = input$file)
                                
                        }
                } else {
                        clicks(as_tibble(colnames(c("x", "y","xmax","ymax","sample", "key", "type", "length"))))
                }
        }, priority = 0)
        
        #Remove source & cropped pictures on stop or session end
        shiny::onStop(
                fun = function() {
                        file.remove(list.files("temp/", pattern = "jpg", full.names = TRUE))
                }
        )
}