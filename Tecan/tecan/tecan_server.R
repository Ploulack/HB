tecan_server <- function(input, output, session) {
        library(shiny); library(stringr); library(purrr); library(magrittr)

        source("tecan/tecan_extract.R"); source("tecan/tecan_values.R")
        source("helpers/drive_helpers.R"); source("helpers/delete_file_button_module.R")
        source("helpers/mongo_helpers.R"); source("tecan/tecan_nadh.R")
        source("registry/registry_helpers.R"); source("registry/registry_values.R")
        source("helpers/strings.R"); source("helpers/general.R")
        source("helpers/plates_helpers.R") ; source("mongo/db_values.R")
        source("helpers/ui_generics/select_file_server.R")

        ns <- session$ns
        tecan_progress <- Progress$new()

        drive_tecanURL <- get_drive_url(session, "tecan")

        #Only initiate mongo connexion when needed
        db <- db_from_environment(session, collection = "lab_experiments")

        #TODO: Try Promises on this
        tecan_progress$inc(.1, detail = "Accessing registry.")
        registry <- registry_key_names(registry_url, registry_sheets)


        selected <- reactiveVal()
        tecan_n <- callModule(module = select_file,
                              id = "tecan",
                              progress = tecan_progress,
                              drive_url = drive_tecanURL,
                              selected = selected
        )


        tecan_n$raw <- reactiveVal(NULL)
        tecan_n$experiment <- reactiveVal(NULL)
        tecan_n$calculated <- reactiveVal(NULL)

        db_files <- reactiveValues()

        #A switch to keep track of db inserts
        data_tagged_and_saved <- reactiveVal(value = FALSE)

        #On button Go pressed, extract data from the xml currently listed
        observeEvent(tecan_n$go_file(), {
                #Prevent re-download from Google Drive when the select files input is initialized or updated,
                if (tecan_n$id() == wait_msg) return()
                else if (tecan_n$id() == "") {
                        showModal(modalDialog(
                                title = "No File",
                                paste0("There's no files in specified Google Drive folder: ",
                                       "/HB/Tecan")
                        ))
                } else {
                        print(paste0("Extracting file : ", tecan_n$file_dribble()$name))
                        tecan_dat <- tecan_extract(tecan_n$id(), tecan_n$files())
                        tecan_n$raw(tecan_dat)
                        # experiment$raw <- tecan_extract(input$file, tecan$files)
                        data_tagged_and_saved(FALSE)
                }
        }, priority = 3)

        # Todo: une horreur, tout reprendre clean
        observeEvent(c(input$absorbance,input$path, tecan_n$raw(), data_tagged_and_saved()),{
                if (is.null(tecan_n$raw()$data) ) return()

                absorbance <- as.double(input$absorbance)
                path <- as.double(input$path)

                if (tecan_n$raw()$type == "NADH Detection") {
                        nadh_detection(nadh = tecan_n$raw()$data$Batch_1$Measures,
                                       cal_conc = calibration_concentrations,
                                       input = input,
                                       output = output,
                                       ns = ns)

                } else {
                        # TODO: In case of DNA quantification since results are only displayed after tagging,
                        # this could run only after tagging...OR remove the data_tagged trigger of the observer.
                        if (tecan_n$raw()$type == "DNA Quantification") {

                                #patch: if experiment has message:

                                if (is.null(tecan_n$raw()$user_msg)) {
                                        tecan_n$calculated(
                                                calc_values(tecan_n$raw()$data,
                                                            absorbance,
                                                            path)
                                        )
                                } else {
                                        tecan_n$calculated(
                                                calc_values(tecan_n$raw()$data,
                                                            water_well_pos = NULL,
                                                            water_readings = water_readings)
                                        )
                                }
                        }
                }
        }, priority = 2)

        #Get db records attached to new file
        file_record <- eventReactive(tecan_n$go_file(), {
                shiny::validate(need(!is.null(tecan_n$raw()$type),
                                     # shiny::validate(need(!is.null(experiment$raw$type),
                                     message = FALSE))

                #Display db ui only if File is not kinetic...
                if (tecan_n$raw()$type %in% tecan_protocols_with_db) {
                        return(mongo_file_entry(db, tecan_n$id()))
                } else {
                        return(list("entry_exists" = FALSE))
                }
        })

        #### PROTOCOLS ####
        #On first opening, move files to their appropriate folders
        observeEvent(tecan_n$raw(), {
                unitary_folder <- tecan_n$protocols()$name[1]
                if (tecan_n$protocol() != "New") return()
                if (is.null(tecan_n$raw()$user_msg) || str_length(tecan_n$raw()$user_msg) == 0) {
                        progress_prot_change <- Progress$new()
                        progress_prot_change$inc(.5, str_interp("Moving file to ${unitary_folder} drive folder."))
                        move_drive_file(tecan_n, prot_name = unitary_folder, "tecan")

                        progress_prot_change$inc(.5, str_interp("Displaying list of ${unitary_folder} name."))
                        selected(update_selected("Unitary", tecan_n$id()))
                        # update_uis("Unitary", tecan_n, session = session)
                        progress_prot_change$close()
                } else {
                        #If the Tecan file includes a custom msg popup choice to the user to do the matching
                        protocols_set_modal(input = input,
                                            tecan_n$file_dribble()$name,
                                            tecan_n$raw()$user_msg,
                                            protocols = tecan_n$protocols(),
                                            session = session)
                }
        })

        #Update choices of plates in modal dialog
        observeEvent(input$set_protocol, {

                if (!input$set_protocol %in% tecan_n$protocols()$name) return()
                selected_prot <- tecan_n$protocols() %>%
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
                render_pooling(input,
                               output,
                               tecan_n$calculated()$Results)

        }, ignoreInit = TRUE)


        #On modal validation, update protocols with selected plate
        observeEvent(input$ok_protocol, {
                #TODO: Add checks on the inputs...

                # Store the pooling result from the users final settings in the modal
                tecan_n$pool <- plate_pooling(tecan_n$calculated()$Results,
                                              well_volume = input$well_volume,
                                              min_dw_conc = input$dw_min_conc,
                                              min_wells_nb = input$min_nb_wells)

                # Close the dialog popup
                removeModal()

                # TODO: Create another container than tecan_n for the pooling stuff, this is dangerous:
                # on a change of file, this data is going to stick if tecan_n no re-initialized....
                tecan_n$experiment <- reactiveVal(input$set_protocol)
                tecan_n$plate <- input$set_plate_nb
                selected_prot <- tecan_n$protocols() %>%
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

                tmp_prot <- tecan_n$protocols()
                tmp_prot[selected_prot$index, "processed_plates"] <- updated_plates
                tecan_n$protocols(tmp_prot)

                selected_prot <- tecan_n$protocols() %>%
                        filter(name == input$set_protocol)

                # ColID gets the A, B, ..Z col ID for the gsheet insertion
                # More resilient to gsheet columns reordering
                colID <- gsheet_colID_from_tibble(tecan_n$protocols(), "processed_plates")

                gs_edit_cells(ss = tecan_n$protocols_gsheet,
                              ws = 1,
                              #TODO: tie the column to the procols tibble's column name...
                              anchor = paste0(colID, selected_prot$index + 1),
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
                        tecan_file_url = tecan_n$file_dribble() %>%
                                dribble_get_link())
                write_csv(plates, csv_path)

                drive_update(file = csv_drive_id,
                             media = csv_path)

                #Move Tecan file to protocol folder
                move_drive_file(tecan_n, prot_name = input$set_protocol, "tecan")

                #Set user file selectInput on the same file
                selected(update_selected(input$set_protocol, tecan_n$id()))
                # update_uis(prot_name = input$set_protocol, tecan_n, session = session)

                #Calculate water volume to normalize and generate the csv files for hamilton then upload to drive
                tmp_norm_csv <- str_interp("temp/${input$set_protocol}__plate_${input$set_plate_nb}.csv")

                tecan_n$pool$plate_pooled %>%
                        select(Sample, Aspirate_To_Pool = Pool_Volume) %>%
                        filter(!is.na(Aspirate_To_Pool)) %>%
                        mutate(Aspirate_To_Pool = round(Aspirate_To_Pool, 1)) %>%
                        plate_sort_sample() %>%
                        write.csv(file = tmp_norm_csv,
                                  quote = TRUE,
                                  eol = "\r\n",
                                  row.names = FALSE)

                drive_upload(media = tmp_norm_csv,
                             path = selected_prot$hami_folder_url %>% as_id())

        }, ignoreInit = TRUE)



        #### DB STORAGE ####
        #Container for samples
        samples <- reactiveVal(value = NULL)
        #To prevent updating empty string notes...
        #TODO: could use attribute??
        note_had_characters <- reactiveVal(FALSE)

        #This handles the sample taggings tecan file types that require it before db writing
        # tecan_file$samples <-  experiment$raw$data$Batch_1$Measures$Sample
        # tecan_file$measures <-  experiment$raw$data$Batch_1$Measures

        observeEvent(tecan_n$go_file(), {
                shiny::validate(need(!is.null(registry) &&
                                             !is.null(tecan_n$raw()$data$Batch_1$Measures$Sample ) &&
                                             !is.null(tecan_n$raw()$type),
                                     message = "Something wrong with registry or tecan file"))

                removeUI(selector = str_interp("#${ns('note_widget')}"))

                measures <- tecan_n$raw()$data$Batch_1$Measures

                #Prepare the samples tibble in case there's a db record...
                if (file_record()$entry_exists) {
                        #Initially, values were not stored, so now, when a file without stored values is opened,
                        #is detected by checking if there's a value column, we update the db.
                        #Same for the water sample.
                        samples(file_record()$entry$samples[[1]] %>%
                                        as_tibble())
                        #TODO: check if just passing samples() works downstream in subsequence db updates
                        # = check that values are correctly 'kept' with existing app udpates.
                        # TODO: eventually remove this code once all files have been processed...

                        if (!"Value" %in% (samples() %>% colnames())) {

                                temp_tbl <- samples()
                                #Check that the water samples has already been added or not
                                if (!measures$Sample[1] %in% temp_tbl$Sample) {
                                        add_row(temp_tbl, Sample = measures$Sample[1], Key = "Water", .before = 1)
                                }
                                #Add the values to the tibble
                                temp_tbl <- temp_tbl %>%
                                        left_join(measures, by = "Sample") %>%
                                        select(Sample, Value, Key)
                                samples(temp_tbl)
                                db_update_entry(db, tecan_n, samples(),
                                                notif_msg = str_interp(
                                                        "Added Values to db entry for ${tecan_n$file_dribble()$name}"))
                        }
                } else {
                        if (tecan_n$raw()$type == tecan_protocols_with_db[1]) {

                                samples(measures %>%
                                                mutate(Key = if_else(row_number() == 1 &
                                                                             (is.null(tecan_n$raw()$user_msg) ||
                                                                                      tecan_n$raw()$user_msg == ""),
                                                                     "Water",
                                                                     ""))
                                )
                        }
                        # This is for H2O2 measurement. Find the highest value, from that
                        # determine calibration values and sample values
                        else if (tecan_n$raw()$type == tecan_protocols_with_db[2]) {
                                # Select non-calibration samples
                                max_idx <- measures$Value %>%
                                        which.max()
                                measured <- (max_idx + 1):length(measures$Value)

                                samples(tecan_file$measures[measured, ] %>%
                                                mutate(Key = ""))
                        } else return()

                        # Initiate the 'samples' reactiveVal with those samples
                        db_create_entry(db, tecan_n, samples())
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

                # Don't diplay sample tagging for plates of an experiment / protocol
                # TODO: actually display information about what's in the plate
                if (is.null(tecan_n$raw()$user_msg) ||
                    tecan_n$raw()$user_msg == "")
                        # for each sample, display the widget
                        walk2(samples()$Sample,samples()$Key, ~ {
                                # removeUI(selector = paste0("#", ns(.x)))

                                control_samples[[.x]] <- callModule(module = sample_widget,
                                                                    id = paste0(input$file, "-", .x),
                                                                    sample_well = .x,
                                                                    sample_key = .y,
                                                                    samples = samples,
                                                                    tecan_n,
                                                                    db,
                                                                    registry,
                                                                    go_file = reactive(tecan_n$go_file())
                                )

                                insertUI(selector = paste0("#", ns("widgets_bar")),
                                         where = "beforeEnd",
                                         ui = sample_widget_ui(id = ns(paste0(input$file, "-", .x)),
                                                               .x,
                                                               .y,
                                                               tecan_n,
                                                               registry)
                                )
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
                        str1 <- str_interp('{"file" : "${tecan_n$id()}"}')
                        str2 <- str_interp('{"$set" : {"note" : "${input_note()}"}}')
                        note_upd <- db$update(str1, str2)
                        if (note_upd$modifiedCount == 1) {
                                showNotification(ui = str_interp("Updated note with '${input_note()}'"),
                                                 duration = 3,
                                                 type = "message")
                        }
                }, ignoreInit = TRUE)

                observeEvent(samples(), {
                        if (any(samples()$Key == "")) data_tagged_and_saved(FALSE)
                        else data_tagged_and_saved(TRUE)
                })


        }, ignoreInit = TRUE)

        # When Tecan file belongs to an experiment / protocol
        # add experiment and plate nb to the db entry
        observeEvent(tecan_n$experiment(), {
                if (is.null(tecan_n$experiment())) return()
                update_str <- str_interp('{"$set":
                                         {"experiment": {
                                         "name" : "${tecan_n$experiment()}",
                                         "plate_nb" : "${input$set_plate_nb}"}}}')
                mongo_update_file(db, tecan_n$id(), upd_str = update_str)
        }, ignoreInit = TRUE)

        output$type <- eventReactive(tecan_n$raw(), {
                tecan_n$raw()$type
        })
        outputOptions(output, "type", suspendWhenHidden = FALSE)

        #### DISPLAY ####
        # Tell user if it's a 260 or 600nm
        output$title <- eventReactive(tecan_n$raw(), {
                str_interp("${tecan_n$raw()$type} - ${file_date(tecan_n$file_dribble()$name)}")
        })

        #A centralize display switch for the plot and tables
        is_displayed <- reactive({
                if (tecan_n$raw()$type == tecan_protocols[1] %>%
                    names()) return(TRUE)
                (data_tagged_and_saved() ||
                                !(is.null(tecan_n$raw()$user_msg) ||
                                          str_length(tecan_n$raw()$user_msg) == 0)) &&
                        tecan_n$raw()$type == tecan_protocols[2] %>%
                        names()
        })

        output$summary <- renderTable({
                if (is.null(tecan_n$raw()$type)) return()
                if (!is_displayed()) return()

                if (tecan_n$raw()$type == "DNA Quantification") {
                        tecan_n$calculated()$Results
                } else {
                        tecan_n$raw()$data$Batch_1$Measures
                }
        })

        output$hist <- renderPlot({
                if (is.null(tecan_n$raw()$type)) return()
                if (!is_displayed()) return()
                is_DNAquant <- tecan_n$raw()$type == "DNA Quantification"

                if (is_DNAquant) {
                        df <- tecan_n$calculated()$Results
                } else {
                        df <- tecan_n$raw()$data$Batch_1$Measures
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
                        scale_fill_discrete(limits = c('FALSE', 'TRUE'),
                                            guide = guide_legend(title = ifelse(is_DNAquant,
                                                                                "Ratio in [1.7, 2.0]",
                                                                                "Value > 0.2"))) +
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
                if (is.null(tecan_n$raw()$type)) return()
                if (!is_displayed()) return()
                if (tecan_n$raw()$type == "DNA Quantification") {
                        return(tecan_n$calculated()$Table)
                } else {
                        NULL
                }
        })
}