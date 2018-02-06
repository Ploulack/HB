select_file <- function(input,
                        output,
                        session,
                        progress,
                        drive_url,
                        selected) {

        #This is to dynamically get the correct column name of the protocols experiment sheet
        current_tab_col_name <- str_extract(session$ns(""), "^\\w+") %>%
                paste0("_folder_url")

        #build the protocol tibble from the spread sheet
        if (!exists("protocols")) {
                progress$inc(.1, detail = "Accessing experiments.")
                source("protocols/protocols_functions.R")
                prot_gsheet <- {if (is_dev_server(session)) gs_url(protocols_sheet_dev)
                        else gs_url(protocols_sheet_prod)}
                protocols <- reactiveVal(protocols_get(drive_url, prot_gsheet, session = session,
                                                       folder_url = current_tab_col_name))
        }

        # Update the Protocol select input
        updateSelectInput(session = session,
                          inputId = "protocol",
                          choices = "New" %>%
                                  append(protocols()$name),
                          selected = "New")

        #Container for ordered drive folder file names
        container <- reactiveValues(files = drive_url %>%
                                            as_id() %>%
                                            get_ordered_filenames_from_drive(progress = progress),
                                    selected_file = NULL,
                                    selected_protocol = NULL)

        removed_files <- reactiveVal(NULL)

        #On change of selected protocol, update file list from drive
        observeEvent(input$protocol, {

                if (input$protocol == "New")
                        url <- drive_url
                else
                        url <- protocols() %>%
                                filter(name == input$protocol) %>%
                                "[["(current_tab_col_name)

                container$files <- url %>%
                        googledrive::as_id() %>%
                        get_ordered_filenames_from_drive()
        }, ignoreInit = TRUE)

        #Generate the file selection list
        choice_files <- eventReactive(c(input$refresh, # User checks for new files
                                        removed_files(), # User has removed files
                                        container$files), # User has changed files
                                      {files <- container$files %>%
                                              filter(id != if (is.null(removed_files())) ""
                                                     else removed_files()) %>%
                                              select(id, exp_date)
                                      files$id %>% set_names(files$exp_date)})

        #Display the file selection list
        observeEvent(c(choice_files(), input$refresh), {
                choices <- choice_files()
                updateSelectInput(session = session,
                                  inputId = "file",
                                  choices = choices,
                                  selected = ifelse(input$refresh == 0,
                                                    if (is.null(container$selected_file)) head(choices,1)
                                                    else container$selected_file,
                                                    input$file))
                container$selected_file <- NULL
                if (!is.null(choices)) progress$close()
        })


        #Delete file button module
        callModule(module = delete_exp_files,
                   id = "delete_button",
                   file = list(id = input$file,
                               name = choice_files() %>%
                                       keep(~ .x == input$file) %>%
                                       names()),
                   db = db,
                   files_list = choice_files(),
                   removed_files)

        observeEvent(selected(), {
                # Switch UI to the protocol where this file was moved to
                updateSelectInput(session = session,
                                  inputId = "protocol",
                                  selected = selected()$protocol)

                #Set which file to select after the files menu update
                container$selected_file <- selected()$file_id

        })

        return(list(
                id = reactive(input$file),
                file_dribble = reactive(container$files %>% filter(id == input$file)),
                files = reactive(container$files),
                container = reactive(container),
                files_list = reactiveVal(choice_files()),
                protocol = reactive(input$protocol),
                protocols = protocols,
                protocols_gsheet = prot_gsheet,
                go_file = reactive(input$go_file)
        ))
}