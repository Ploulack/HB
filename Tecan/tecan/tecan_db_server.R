tecan_db_server <- function(input, output, session, tecan_file, gtoken, data_switch = NULL) {
        library(purrr); library(stringr)
        
        source("helpers/mongo_helpers.R")
        source("registry/registry_helpers.R")
        source("registry/registry_values.R")
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("mongo/db_values.R")
                db <- db_from_environment(session, collection = "lab_experiments")
        }
        
        registry <- registry_key_names(registry_url, registry_sheets)
        
        #Get db records attached to new file
        file_record <- eventReactive(tecan_file(), {
                shiny::validate(need(!is.null(tecan_file()$type),
                                message = FALSE))
                #Display db ui only if File is not kinetic...
                if (tecan_file()$type %in% tecan_protocols_with_db) {
                        return(mongo_file_entry(db, tecan_file()$file))
                } else {
                        return(list("entry_exists" = FALSE))
                }
        })
        
        #Display message to inform him to inform and store the data
        output$test <- renderText({
                shiny::validate(need(!is.null(tecan_file()$type), message = FALSE))
                if (!data_switch()) {
                        return("To see the data & bar plot you need to fill the samples")
                } else if (!tecan_file()$type == "DNA Quantification") {
                        return(NULL)
                } else {
                        return("Displaying results...")
                }
        })
        
        samples <- reactiveVal(value = NULL)
        #Create inputs and related textOutputs
        observeEvent(tecan_file(), {
                shiny::validate(need(!is.null(registry) &&
                                             !is.null(tecan_file()$samples) &&
                                             !is.null(tecan_file()$type),
                                     message = "Something wrong with registry or tecan file"))
                ns <- session$ns
                if (tecan_file()$type == "DNA Quantification") {
                        output$keys <- renderUI({
                                fluidPage(fluidRow(tagList(
                                        map(tecan_file()$samples[-1], function(x) {
                                                column(width = 2,
                                                       selectizeInput(
                                                               inputId = ns(x),
                                                               label = sprintf("Sample %s", x ),
                                                               choices = registry$KEY  %>%
                                                                       prepend(c("", "Plasmid")),
                                                               multiple = FALSE
                                                               
                                                       )
                                                )
                                        })
                                )),
                                fluidRow(
                                        column(width = 4,
                                               textInput(inputId = ns("note"),
                                                         label = "Optional note",
                                                         placeholder = "Enter file note")
                                        ),
                                        column(width = 2,
                                               actionButton(inputId = ns("update"),
                                                            label = "Update database infos for this file"))
                                )
                                )        
                        })
                        # tecan_protocols_with_db[2] = NADH...
                } else if (tecan_file()$type == tecan_protocols_with_db[2]) {
                        removeUI(selector = str_interp("#${ns('H2O2_note')}"),multiple = TRUE)
                        
                        if (file_record()$entry_exists) {
                                samples(file_record()$entry$samples[[1]] %>% as_tibble())
                        } else {
                                # Select non-calibration samples
                                max_idx <- tecan_file()$measures$Value %>% which.max()
                                measured <- (max_idx + 1):length(tecan_file()$measures$Value)
                                # Initiate the 'samples' reactiveVal with those samples
                                samples(tecan_file()$samples[measured] %>%
                                                as_tibble() %>%
                                                mutate(Key = "") %>%
                                                rename(Sample = value))
                                
                                # Todo : create the db entry for the file
                                db_create_entry(db, tecan_file, samples)
                        }
                        #Display the note text input
                        insertUI(selector =  "#Tecan-widgets_bar",
                                 where = "beforeBegin",
                                 ui =  textInput(inputId = ns("H2O2_note"),
                                                label = "Optional note",
                                                value = file_record()$entry$note))
                        
                        # for each sample, display the widget
                        walk2(samples()$Sample,samples()$Key, ~ {
                                display_sample_widget(
                                        sample_well = .x,
                                        sample_key = .y,
                                        samples = samples,
                                        tecan_file = tecan_file,
                                        ns = ns,
                                        db = db
                                )
                        })
                        
                        #Slowed down text input updates
                        input_H2O2 <- debounce(reactive({input$H2O2_note}), 1500)
                        #Save note when changed
                        observeEvent(input_H2O2(), {
                                if (input_H2O2() == file_record()$entry$note) return()
                                str1 <- str_interp('{"file" : "${tecan_file()$file}"}')
                                str2 <- str_interp('{"$set" : {"note" : "${input_H2O2()}"}}')
                                note_upd <- db$update(str1, str2)
                                if (note_upd) {
                                        showNotification(ui = str_interp("Updated note with '${input_H2O2()}'"),
                                                         duration = 3,
                                                         type = "message")
                                }
                        }, ignoreInit = TRUE)
                        
                } else return()
                
        })
        
        #Display in input fields the db existing data
        observeEvent(tecan_file(), {
                shiny::validate(
                        need(
                                !(is.null(tecan_file()) |
                                          is.null(file_record())),
                                message = FALSE))
                if (tecan_file()$file == wait_msg) return()
                
                #When user switches tecan file, display existing entries in input fields
                if (file_record()$entry_exists) {
                        #Todo: Add missing entries (parts, samples, etc)
                        for (i in seq_along(tecan_file()$samples[-1])) {
                                updateSelectizeInput(
                                        session,
                                        # using the db entry for the input Ids
                                        # while we're seq_along the file samples (minus water well)
                                        # is an implicit matching check
                                        inputId = file_record()$entry$samples[[1]]$Sample[i],
                                        selected = file_record()$entry$samples[[1]]$Key[i]
                                )
                        }
                        updateTextInput(
                                session = session,
                                inputId = "note",
                                value = file_record()$entry$note
                        )
                }
        })
        
        #Make a reactive function of the Samples 'Key' entered by the user
        sample_keys <- reactive({
                shiny::validate(
                        need(!is.null(tecan_file()$samples), message = FALSE)
                )
                shiny::validate(need(!is.null(input[[tecan_file()$samples[-1][1]]]), message = FALSE))
                #The first Sample is water, we don't want it.
                ##TODO: study to directly remove the first well from tecan_file()$samples
                #rather than always have tecan_file()$samples
                
                map_chr(tecan_file()$samples[-1], ~input[[.x]])
        })
        
        #Display next to each sample-key input the registry part's name
        observeEvent(c(sample_keys(), tecan_file()),{
                shiny::validate(need(expr = !is.null(tecan_file()$samples[-1]), message = FALSE))
                ns <- session$ns
                walk2(tecan_file()$samples[-1], sample_keys(), function(x,y) {
                        removeUI(selector = paste0("#js_id",x))
                        #Todo: get non-parts values in a cleaner way
                        if (!y %in% c("", "Plasmid")) {
                                key <- registry %>%
                                        filter(KEY == y) %>% pull(var = 2)
                                insertUI(selector = paste0("#", ns(x)),
                                         where = "afterEnd",
                                         ui = tags$h6(paste0(key, "  "),
                                                      id = paste0("js_id",x))
                                )
                        }
                })
        })
        
        #Require all samples informed from user with a modal dialog
        are_samples_informed <- eventReactive(input$update, {
                shiny::validate(
                        need(!is.null(tecan_file()$samples), message = FALSE)
                )
                inputs <- sample_keys()
                if (any(inputs == "")) {
                        showModal(modalDialog(
                                title = "Missing wells keys",
                                "Please inform all samples before updating database",
                                easyClose = TRUE,
                                footer = NULL
                        ))
                        return(FALSE)
                } else{return(TRUE)}
        })
        
        #Reset data_switch for each file
        observeEvent(tecan_file(), {
                shiny::validate((need(!is.null(tecan_file()$type), message = FALSE)))
                if (file_record()$entry_exists || !tecan_file()$type == "DNA Quantification") {
                        data_switch(TRUE)
                } else {
                        data_switch(FALSE)
                }
        }, ignoreInit = TRUE)
        
        #Insert or Update in mongo db
        observeEvent(input$update ,{
                shiny::validate(
                        need(!is.null(tecan_file()$samples[-1]), message = FALSE)
                )
                shiny::validate(need(are_samples_informed(), message = FALSE))
                
                #Check if there's an entry for that file name
                file_id <- tecan_file()$file
                file_name <- tecan_file()$file_dribble$name
                
                #Prepare the string for the samples 'Array'
                samples_str <- jsonlite::toJSON(
                        map2(
                                tecan_file()$samples[-1],
                                sample_keys(),
                                ~list("Sample" = .x, "Key" = .y))
                ) %>% str_replace_all(pattern = "\\[|\\]", replacement = "")
                
                if (file_record()$entry_exists) {
                        str1 <- paste0('{"file" : "', file_id,'"}')
                        str2 <- paste0(
                                '{"$set" : 
                                {"note" : "',input$note,'","samples" : [',samples_str ,']}
                }')
                        print("update")
                        update_log <- db$update(str1,str2)
                        if (update_log) {
                                showNotification(ui = sprintf("Updated entry for file %s", file_name) ,
                                                 duration = 3,
                                                 type = "message")
                                data_switch(TRUE)
                        }
                        print(jsonlite::prettify(str2))
        } else {
                str <- paste0(
                        '{"file" : "',file_id,'", "name" : "', file_name,'", "type" : "tecan",
                        "note" : "', input$note, '",
                        "samples" : [',samples_str ,']
        }'
                        )
                print("new entry")
                print(jsonlite::prettify(str))
                insert_log <- db$insert(str)
                if (insert_log$nInserted == 1 && length(insert_log$writeErrors) == 0) {
                        showNotification(ui = sprintf("New entry for file %s", file_name) ,
                                         duration = 3,
                                         type = "message")
                        data_switch(TRUE)
                }
        }
})
        }