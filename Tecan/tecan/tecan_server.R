tecan_server <- function(input, output, session, gtoken) {
        library(shiny); library(stringr); library(purrr); library(magrittr)
        source("tecan/tecan_extract.R"); source("tecan/tecan_values.R")
        source("drive_helpers.R"); source("helpers/delete_file_button_module.R")
        source("helpers/mongo_helpers.R"); source("tecan/tecan_nadh.R")
        source("registry/registry_helpers.R"); source("registry/registry_values.R")
        source("helpers/strings.R")
        ns <- session$ns
        
        #TODO: Try Promises on this
        registry <- registry_key_names(registry_url, registry_sheets)
        
        #Only initiate mongo connexion when needed
        if (!exists("db")) {
                source("mongo/db_values.R")
                db <- db_from_environment(session, collection = "lab_experiments")
        }
        #build the protocol tibble from the spread sheet
        if (!exists("protocols")) {
                source("protocols/protocols_functions.R")
                prot_gsheet <- gs_url(protocols_sheet)
                protocols <- reactiveVal(protocols_get(drive_tecanURL, prot_gsheet)
                )
        }
        
        # Update the Protocol select input
        updateSelectInput(session,
                          inputId = "protocol",
                          choices = "New" %>% append(protocols()$name),
                          selected = "New")

        
        #Xml extracted data temporary container.
        experiment <- reactiveValues() #TODO: Remove and replace with the tecan_file reactive values container...
        
        db_files <- reactiveValues()
        
        #Container for ordered drive folder file names
        tecan <- reactiveValues(files = drive_tecanURL %>%
                                        as_id() %>%
                                        get_ordered_filenames_from_drive(),
                                selected_file = NULL,
                                selected_protocol = NULL)
        removed_files <- reactiveVal(NULL)
        
        #Generate the file selection list
        choiceFiles <- eventReactive(c(input$refresh, # User checks for new files
                                       removed_files(), # User has removed files
                                       tecan$files), # User has changed files
                                     {dat <- tecan$files %>%
                                             filter(id != if (is.null(removed_files())) ""
                                                    else removed_files()) %>%
                                             select(id, exp_date)
                                     dat$id %>% set_names(dat$exp_date)})
        
        #Display the file selection list
        observeEvent(c(choiceFiles(), input$refresh), {
                choices <- choiceFiles()
                updateSelectInput(session, "file",
                                  choices = choices,
                                  selected = ifelse(input$refresh == 0,
                                                    if (is.null(tecan$selected_file)) head(choices,1)
                                                    else tecan$selected_file,
                                                    input$file))
                tecan$selected_file <- NULL
        })
        
        #Create and fill the tecan_file all inclusive reactive container
        tecan_file <- reactiveValues()
        observeEvent(input$go_file, {
                tecan_file$file <- input$file
                tecan_file$file_dribble <-  tecan$files %>% filter(id == input$file)
                tecan_file$samples = experiment$raw$data$Batch_1$Measures$Sample
                tecan_file$measures = experiment$raw$data$Batch_1$Measures
                tecan_file$type = experiment$raw$type
                tecan_file$user_msg = experiment$raw$user_msg
        })
        
        #A switch to keep track of db inserts
        data_tagged_and_saved <- reactiveVal(value = FALSE)
        
        #Delete file button module
        callModule(module = delete_exp_files,
                   id = "delete_button",
                   tecan_file = tecan_file,
                   db = db,
                   files_list = choiceFiles(),
                   removed_files)
        
        #On file change, extract data from the xml
        observeEvent(input$go_file, {
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
                        print(paste0("Extracting file : ", input$file))
                        experiment$raw <- tecan_extract(input$file, tecan$files)
                        if (exists("data_tagged_and_saved")) rm("data_tagged_and_saved")
                        data_tagged_and_saved <- reactiveVal(FALSE)
                }
        }, priority = 1)
        
        #Get db records attached to new file
        file_record <- eventReactive(input$go_file, {
                shiny::validate(need(!is.null(tecan_file$type),
                                     message = FALSE))
                
                #Display db ui only if File is not kinetic...
                if (tecan_file$type %in% tecan_protocols_with_db) {
                        return(mongo_file_entry(db, tecan_file$file))
                } else {
                        return(list("entry_exists" = FALSE))
                }
        })
        
        #DEBUG
        observeEvent(protocols(), {
                print(protocols()$processed_plates %>% walk(~ if (!is.na(.x)) print(.x)))
        })
        
        #PROTOCOLS
        #On first opening, move files to their appropriate folders
        observeEvent(experiment$raw, {
                move_drive_file <- function(protocols, prot_name) {
                        tecan$files %>%
                                filter(id == input$file) %>%
                                drive_mv(path = protocols %>%
                                                 filter(name == prot_name) %>%
                                                 pull(folder_url) %>%
                                                 as_id())
                }
                
                update_uis <- function(prot_name) {
                        # Switch UI to the protocol where this file was moved to
                        updateSelectInput(session = session,
                                          inputId = "protocol",
                                          selected = prot_name)
                        
                        #Set whicih file to select after the files menu update
                        tecan$selected_file <- input$file
                }
                
                if (input$protocol != "New") return()
                if (is.null(experiment$raw$user_msg) || str_length(experiment$raw$user_msg) == 0) {
                        move_drive_file(protocols(), prot_name = "Unitary")
                        update_uis("Unitary")
                } else {
                        # popup choice to the user to do the matching
                        protocols_set_modal(input = input,
                                            tecan_file$file_dribble$name,
                                            experiment$raw$user_msg,
                                            protocols = protocols(),
                                            session = session)
                        
                        #Update choices of plates in modal dialog
                        observeEvent(input$set_protocol, {
                                
                                if (!input$set_protocol %in% protocols()$name) return()
                                selected_prot <- protocols() %>%
                                        filter(name == input$set_protocol)
                                proced_plates <- selected_prot %$%
                                        processed_plates %>%
                                        str_split(", ") %>%
                                        simplify()
                                
                                total_plates <- 1:selected_prot$total_plates
                                plate_choices <- if (is.na(proced_plates)) total_plates
                                else total_plates %>%
                                        keep(!total_plates %in% proced_plates)
                                        
                                updateSelectInput(session = session,
                                          inputId = "set_plate_nb",
                                          choices = plate_choices)
                                        
                        }, ignoreInit = TRUE)
                        
                        #On modal validation, update protocols with selected plate
                        observeEvent(input$ok_protocol, {
                                removeModal()
                                
                                selected_prot <- protocols() %>%
                                        filter(name == input$set_protocol)
                                #Update protocols gsheet with new plate
                                #Add new plate to string
                                if (is.na(selected_prot$processed_plates))
                                        updated_plates <- input$set_plate_nb
                                else updated_plates <- selected_prot$processed_plates %>%
                                        str_c(", ", input$set_plate_nb) %>%
                                        str_split(", ") %>%
                                        simplify() %>%
                                        as.integer() %>%
                                        unique() %>%   #I have a feeling this observer repeats...and so by forcing unique, prevent issues
                                        sort.int() %>%
                                        as.character() %>%
                                        str_c(collapse = ", ")
                                
                                tmp_prot <- protocols()
                                tmp_prot[selected_prot$index, "processed_plates"] <- updated_plates
                                protocols(tmp_prot)
                                
                                selected_prot <- protocols() %>%
                                        filter(name == input$set_protocol)
                                
                                gs_edit_cells(ss = prot_gsheet,
                                              ws = 1,
                                              #TODO: tie the column to the procols tibble's column name...
                                              anchor = paste0("E", selected_prot$index + 1),
                                              input = str_c("'", updated_plates)) #The added quote is to force string on gsheet
                                
                                #Update CSV with new plate
                                csv_path <- "temp/modal_dialog_csv"
                                
                                #Download csv file and keep its drive dribble
                                csv_drive_id <- selected_prot$plates_processed_url %>%
                                        as_id() %>%
                                        drive_download(path = csv_path,
                                                       overwrite = TRUE)
                                
                                plates <- read_csv(csv_path)
                                
                                #Update the tibble with the current time and with the tecan xml file link
                                plates[(input$set_plate_nb %>% as.integer()), ] <- tibble(
                                        processed_date = Sys.time() %>%
                                        force_tz(tzone = "EST") %>% as.character(),
                                        tecan_file_url = tecan_file$file_dribble %>%
                                                dribble_get_link())
                                write_csv(plates, csv_path)

                                drive_update(file = csv_drive_id,
                                             media = csv_path)

                                #Move Tecan file to protocol folder
                                move_drive_file(protocols(), prot_name = input$set_protocol)
                                #Set user file selectInput on the same file
                                update_uis(prot_name = input$set_protocol)
                        }, ignoreInit = TRUE)
                }
        })
        
        #On change of selected protocol, update file list from drive
        observeEvent(input$protocol, {
                drive_url <- if (input$protocol == "New") drive_tecanURL 
                else protocols() %>%
                        filter(name == input$protocol) %$%
                        folder_url
                
                tecan$files <- drive_url %>%
                        googledrive::as_id() %>%
                        get_ordered_filenames_from_drive()
        }, ignoreInit = TRUE)
        
        
        #Container for samples
        samples <- reactiveVal(value = NULL)
        #To prevent updating empty string notes...
        note_had_characters <- reactiveVal(FALSE)
        
        #This handles the sample taggings tecan file types that require it before db writing
        observeEvent(input$go_file, {
                shiny::validate(need(!is.null(registry) &&
                                             !is.null(tecan_file$samples) &&
                                             !is.null(tecan_file$type),
                                     message = "Something wrong with registry or tecan file"))
                
                removeUI(selector = str_interp("#${ns('note_widget')}"))
                
                
                #Prepare the samples tibble
                if (file_record()$entry_exists) {
                        samples(file_record()$entry$samples[[1]] %>%
                                        as_tibble())
                        
                } else {
                        if (tecan_file$type == tecan_protocols_with_db[1]) {
                                tecan_smpls <- tecan_file$samples[-1] 
                        }
                        else if (tecan_file$type == tecan_protocols_with_db[2]) {
                                # Select non-calibration samples
                                max_idx <- tecan_file$measures$Value %>%
                                        which.max()
                                measured <- (max_idx + 1):length(tecan_file$measures$Value)
                                tecan_smpls <- tecan_file$samples[measured]
                        } else return()
                        
                        # Initiate the 'samples' reactiveVal with those samples
                        samples(tecan_smpls %>%
                                         as_tibble() %>%
                                         mutate(Key = "") %>%
                                         rename(Sample = value))
                        
                        # Todo : create the db entry for the file
                        db_create_entry(db, tecan_file, samples)
                } 
                
                #Display the note text input
                insertUI(selector =  paste0("#", ns("widgets_bar")),
                         where = "beforeBegin",
                         ui =  tags$div(id = ns("note_widget"),
                                        textInput(inputId = ns("note"),
                                                  label = "Optional note",
                                                  value = file_record()$entry$note)
                         )
                )
                
                control_samples <- reactiveValues()
                
                # for each sample, display the widget
                walk2(samples()$Sample,samples()$Key, ~ {
                        
                        insertUI(selector = paste0("#", ns("widgets_bar")),
                                 where = "beforeEnd",
                                 ui = sample_widget_ui(id = ns(.x), .x, .y, tecan_file, registry)
                        )
                        
                        control_samples[[.x]] <- callModule(module = sample_widget,
                                                id = .x,
                                                sample_well = .x,
                                                sample_key = .y,
                                                samples = samples,
                                                tecan_file,
                                                db,
                                                registry)
                })
                
                #Slowed down text input updates
                input_note <- debounce(reactive({input$note}), 1500)

                #Save note when changed
                observeEvent(input_note(), {
                        if (file_record()$entry_exists) {
                                #Don't update db when not value changed yet
                                if (input_note() == file_record()$entry$note) return()
                        } else {
                                #Don't update db with empty char except when it's a 'delete' note act
                                if (input_note() == "" && !note_had_characters()) {
                                        return()}
                        }
                        if (str_length(input_note()) > 0) note_had_characters(TRUE)
                        else note_had_characters(FALSE)
                        str1 <- str_interp('{"file" : "${tecan_file$file}"}')
                        str2 <- str_interp('{"$set" : {"note" : "${input_note()}"}}')
                        note_upd <- db$update(str1, str2)
                        if (note_upd) {
                                showNotification(ui = str_interp("Updated note with '${input_note()}'"),
                                                 duration = 3,
                                                 type = "message")
                        }
                }, ignoreInit = TRUE)
                
                #Gather the user entered keys for each sample
                key_inputs <- reactive({
                        map_chr(samples()$Sample, ~ {
                                if (is.null(control_samples[[.x]]) ||
                                    is.null(control_samples[[.x]]())
                                    ) return("")
                                else return(control_samples[[.x]]())
                                })
                })
                
                observeEvent(key_inputs(), {
                        if (any(key_inputs() == "")) data_tagged_and_saved(FALSE)
                        else data_tagged_and_saved(TRUE)
                })
                
        }, ignoreInit = TRUE)
        
        # Tell user if it's a 260 or 600nm
        output$type <- eventReactive(experiment$raw, {
                experiment$raw$type
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
                        if (experiment$raw$type == "DNA Quantification") {
                                experiment$calculated <- calc_values(experiment$raw$data,
                                                                     absorbance,
                                                                     path)
                        }
                        
                        output$summary <- renderTable({
                                if (!data_tagged_and_saved() && experiment$raw$type == "DNA Quantification")
                                        return()
                                if (experiment$raw$type == "DNA Quantification") {
                                        experiment$calculated$Results
                                } else {
                                        experiment$raw$data$Batch_1$Measures
                                }
                        })
                        
                        output$hist <- renderPlot({
                                if (!data_tagged_and_saved() && experiment$raw$type == "DNA Quantification")
                                        return()
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