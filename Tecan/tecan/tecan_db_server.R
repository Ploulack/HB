tecan_db_server <- function(input, output, session, tecan_file, gtoken, data_switch = NULL, file_record, db) {
        library(purrr); library(stringr)
        
        source("helpers/mongo_helpers.R")
        # source("registry/registry_helpers.R")
        # source("registry/registry_values.R")
        # registry <- registry_key_names(registry_url, registry_sheets)
        
        
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
        
        #Create inputs and related textOutputs
        # observeEvent(tecan_file(), {
        #         shiny::validate(need(!is.null(registry) &&
        #                                      !is.null(tecan_file()$samples) &&
        #                                      !is.null(tecan_file()$type),
        #                              message = "Something wrong with registry or tecan file"))
        #         ns <- session$ns
        #         if (tecan_file()$type == "DNA Quantification") {
        #                 output$keys <- renderUI({
        #                         fluidPage(fluidRow(tagList(
        #                                 map(tecan_file()$samples[-1], function(x) {
        #                                         column(width = 2,
        #                                                selectizeInput(
        #                                                        inputId = ns(x),
        #                                                        label = sprintf("Sample %s", x ),
        #                                                        choices = registry$KEY  %>%
        #                                                                prepend(c("", "Plasmid")),
        #                                                        multiple = FALSE
        #                                                        
        #                                                )
        #                                         )
        #                                 })
        #                         )),
        #                         fluidRow(
        #                                 column(width = 4,
        #                                        textInput(inputId = ns("note"),
        #                                                  label = "Optional note",
        #                                                  placeholder = "Enter file note")
        #                                 ),
        #                                 column(width = 2,
        #                                        actionButton(inputId = ns("update"),
        #                                                     label = "Update database infos for this file"))
        #                         )
        #                         )        
        #                 })
        #         } else return()
        # })
        
        #Display in input fields the db existing data
        # observeEvent(tecan_file(), {
        #         shiny::validate(
        #                 need(
        #                         !(is.null(tecan_file()) |
        #                                   is.null(file_record())),
        #                         message = FALSE))
        #         if (tecan_file()$file == wait_msg) return()
        #         
        #         #When user switches tecan file, display existing entries in input fields
        #         if (file_record()$entry_exists) {
        #                 #Todo: Add missing entries (parts, samples, etc)
        #                 for (i in seq_along(tecan_file()$samples[-1])) {
        #                         updateSelectizeInput(
        #                                 session,
        #                                 # using the db entry for the input Ids
        #                                 # while we're seq_along the file samples (minus water well)
        #                                 # is an implicit matching check
        #                                 inputId = file_record()$entry$samples[[1]]$Sample[i],
        #                                 selected = file_record()$entry$samples[[1]]$Key[i]
        #                         )
        #                 }
        #                 updateTextInput(
        #                         session = session,
        #                         inputId = "note",
        #                         value = file_record()$entry$note
        #                 )
        #         }
        # })
        
        #Make a reactive function of the Samples 'Key' entered by the user
        # sample_keys <- reactive({
        #         shiny::validate(
        #                 need(!is.null(tecan_file()$samples), message = FALSE)
        #         )
        #         shiny::validate(need(!is.null(input[[tecan_file()$samples[-1][1]]]), message = FALSE))
        #         #The first Sample is water, we don't want it.
        #         ##TODO: study to directly remove the first well from tecan_file()$samples
        #         #rather than always have tecan_file()$samples
        #         
        #         map_chr(tecan_file()$samples[-1], ~input[[.x]])
        # })
        
        #Display next to each sample-key input the registry part's name
        # observeEvent(c(sample_keys(), tecan_file()),{
        #         shiny::validate(need(expr = !is.null(tecan_file()$samples[-1]), message = FALSE))
        #         ns <- session$ns
        #         walk2(tecan_file()$samples[-1], sample_keys(), function(x,y) {
        #                 removeUI(selector = paste0("#js_id",x))
        #                 #Todo: get non-parts values in a cleaner way
        #                 if (!y %in% c("", "Plasmid")) {
        #                         key <- registry %>%
        #                                 filter(KEY == y) %>% pull(var = 2)
        #                         insertUI(selector = paste0("#", ns(x)),
        #                                  where = "afterEnd",
        #                                  ui = tags$h6(paste0(key, "  "),
        #                                               id = paste0("js_id",x))
        #                         )
        #                 }
        #         })
        # })
        
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