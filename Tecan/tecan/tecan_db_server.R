tecan_db_server <- function(input, output, session, tecan_file) {
        library(purrr); library(stringr)
        source("helpers/mongo_helpers.R")
        source("registry/registry_helpers.R")
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("tecan/tecan_db_values.R")
        }
        
        file_record <- eventReactive(c(input$update, tecan_file()), {
                shiny::validate(
                        need(
                                !is.null(tecan_file()$type),
                                message = FALSE)
                )
                
                if (!tecan_file()$type) {
                        return(
                                mongo_file_entry(db, tecan_file()$file)
                        )
                }
        })
        
        #TODO: pass Token to the Call module call
        token <- readRDS("hblab_token.rds")
        
        registry <- get_registry(token = token)
        
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
                                                                                multiple = FALSE,
                                                                                choices = registry$KEY
                                                                        )
                                                                )
                                                        })
                                                )
                                        ),
                                        fluidRow(
                                                column(width = 3,
                                                        textInput(inputId = ns("note"),
                                                                label = "Optional note",
                                                                placeholder = "Enter file note")
                                                ),
                                                column(width = 2,
                                                        actionButton(inputId = ns("update"),
                                                                label = "Update info"))
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
                
                print(paste0("file record ", file_record()$entry_exists))
                print(paste0(" type ", tecan_file()$type))
                
                #When user switches tecan file, display existing entries in input fields
                if(file_record()$entry_exists) {
                        #Todo: Add missing entries (parts, samples, etc)
                        #
                        for (i in seq_along(tecan_file()$samples[-1])+1) {
                                updateSelectizeInput(
                                        session,
                                        inputId = tecan_file()$samples[i],
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
        
        sample_keys <- reactive({
                if(is.null(tecan_file()$samples)) return()
                else if (!is.null(input[[tecan_file()$samples[1]]])) {
                        map_chr(tecan_file()$samples,
                                ~input[[.x]])        
                }
        })
        
        #Insert or Update in mongo db
        observeEvent(input$update ,{
                if(is.null(tecan_file()$samples)) return()
                
                #Check if there's an entry for that file name
                file_name <- tecan_file()$file
                
                #Prepare the string for the samples 'Array'
                samples_str <- toJSON(
                        map2(
                                tecan_file()$samples,
                                sample_keys(),
                                ~list("Sample" = .x, "Key" = .y))
                ) %>% str_replace_all(pattern = "\\[|\\]", replacement = "")
                
                if (file_record()$entry_exists) {
                        str1 <- paste0('{"file" : "', file_name,'"}')
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
                                '{"file" : "',file_name,'",
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