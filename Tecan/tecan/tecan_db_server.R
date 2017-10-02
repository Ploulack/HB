tecan_db_server <- function(input, output, session, tecan_file, gtoken) {
        library(purrr); library(stringr)
        
        
        source("helpers/mongo_helpers.R")
        source("registry/registry_helpers.R")
        source("registry/registry_values.R")
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("mongo/db_values.R")
                db <- db_from_environment(session)
        }
        
        # registry <- get_registry(gtoken)
        registry <- registry_key_names(registry_url, registry_sheets)
        
        
        file_record <- eventReactive(c(input$update, tecan_file()), {
                shiny::validate(
                        need(
                                !is.null(tecan_file()$type),
                                message = FALSE)
                )
                #Display db ui only if File is not kinetic...
                if (!tecan_file()$type) {
                        return(
                                mongo_file_entry(db, tecan_file()$file)
                        )
                }
        })
        
        #Create inputs
        observeEvent(c(tecan_file()), {
                shiny::validate(need(!(is.null(registry) |
                                is.null(tecan_file()$samples) |
                                is.null(tecan_file()$type)), message = FALSE) ) 
                ns <- session$ns
                if(!tecan_file()$type) {
                        output$keys <- renderUI({
                                if (tecan_file()$type) return()
                                fluidPage(
                                        fluidRow(
                                                tagList(
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
                                                )
                                        ),
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
                }
                
        })
        
        
        #Display in input fields the db existing data
        observeEvent(tecan_file(), {
                shiny::validate(
                        need(
                                !(is.null(tecan_file()) |
                                                is.null(file_record())),
                                message = FALSE))
                if (tecan_file()$file %in% c("Waiting from dropbox")) return()
                
                #When user switches tecan file, display existing entries in input fields
                if(file_record()$entry_exists) {
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
        
        input_keys <- reactive({
                map_chr(tecan_file()$samples[-1], ~input[[.x]])
        })
        
        #Make a reactive function of the Samples 'Key' inputs
        sample_keys <- reactive({
                shiny::validate(
                        need(!is.null(tecan_file()$samples), message = FALSE)
                        )
               #The first Sample is water, we don't want it.
               ##TODO: study to directly remove the first well from tecan_file()$samples
                #rather than always have tecan_file()$samples
                
               # inputs <- map_chr(tecan_file()$samples[-1], ~input[[.x]]) 
               inputs <- input_keys()
               
               shiny::validate(need(
                       all(inputs !=""),  message = "Please link key to all samples"
               ))
               
               return(inputs)
        })
        
        observeEvent(input$update, {
                shiny::validate(
                        need(!is.null(tecan_file()$samples), message = FALSE)
                )
                inputs <- input_keys()
                if (any(inputs == "")) {
                        showModal(modalDialog(
                                title = "Missing wells keys",
                                "Please inform all samples before updating database",
                                easyClose = TRUE,
                                footer = NULL
                        ))
                }
        })
        
        
        #Insert or Update in mongo db
        observeEvent(input$update ,{
                shiny::validate(
                        need(!is.null(tecan_file()$samples[-1]), message = FALSE)
                )
                
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
                        db$update(str1,str2)
                        print(jsonlite::prettify(str2))
                } else {
                        #Todo: Add file type TECAN vs GEL
                        str <- paste0(
                                '{"file" : "',file_id,'", "name" : "', file_name,'",
                                "note" : "', input$note, '",
                                "samples" : [',samples_str ,']
                                }'
                        )
                        print("new entry")
                        print(jsonlite::prettify(str))
                        db$insert(str)
                }
        })
}