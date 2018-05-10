tecan_server <- function(input, output, session) {
        library(shiny); library(stringr);
        library(purrr); library(magrittr)

        source("tecan/tecan_extract.R"); source("tecan/tecan_values.R")
        source("helpers/drive_helpers.R"); source("helpers/delete_file_button_module.R")
        source("helpers/mongo_helpers.R"); source("tecan/tecan_nadh.R")
        source("registry/registry_helpers.R"); source("registry/registry_values.R")
        source("helpers/strings.R"); source("helpers/general.R")
        source("helpers/plates_helpers.R") ; source("mongo/db_values.R")
        source("helpers/ui_generics/select_file_server.R");
        source("protocols/protocols_module.R")

        ns <- session$ns
        tecan_progress <- Progress$new()

        drive_tecanURL <- get_drive_url(session, "tecan")

        #### DEBUG ####
        observeEvent(tecan_n$id(), {
            cat("input tecan_n id ", tecan_n$id(), "\n")
        })

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


        obtain_file_data(
                tecan_n$go_file,
                "Tecan",
                file_container = tecan_n,
                dat_container = tecan_n$raw,
                extract_function = tecan_extract,
                data_saved_flag = data_tagged_and_saved
        )


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
                                                calc_values(tecan_n$raw()$data)
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


        #### PROTOCOLS ####

        tecan_p <- callModule(module = protocols_handler,
                              id = "tecan",
                              file_container = tecan_n,
                              data_container = tecan_n$raw,
                              selected = selected,
                              db = db)

        observeEvent(tecan_p$ok_protocol(), {
                pooling_modal(ns = ns)

                render_pooling(input,
                               output,
                               tecan_n$calculated()$Results)
        }, ignoreInit = TRUE)


        #On pooling modal validation, open pooling modal
        observeEvent(input$ok_pooling, {

                #TODO: Add checks on the inputs...and reopen modal with required msg if error

                # Store the pooling result from the users final settings in the modal
                tecan_n$pool <- plate_pooling(tecan_n$calculated()$Results,
                                              is_diluted = input$is_diluted,
                                              well_volume = input$well_volume,
                                              min_dw_conc = input$dw_min_conc,
                                              min_wells_nb = input$min_nb_wells)

                # Close the dialog popup
                removeModal()

                # TODO: Create another container than tecan_n for the pooling stuff, this is dangerous:
                # on a change of file, this data is going to stick if tecan_n no re-initialized....
                tecan_n$experiment <- reactiveVal(tecan_p$set_protocol())
                selected_prot <- tecan_n$protocols() %>%
                        filter(name == tecan_p$set_protocol())


                #Calculate water volume to normalize and generate the csv files for hamilton then upload to drive
                tmp_norm_csv <- str_interp("temp/${tecan_p$set_protocol()}__plate_${tecan_p$set_plate_nb()}.csv")

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
                             path = selected_prot$tecan_csv_folder_url %>% as_id())

        }, ignoreInit = TRUE)



        #### DB STORAGE ####

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

        #Container for samples
        samples <- reactiveVal(value = NULL)
        #To prevent updating empty string notes...
        #TODO: could use attribute??
        note_had_characters <- reactiveVal(FALSE)

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
                            cat("Generating module for tecan sample widget. tecan_n id ", tecan_n$id(), "\n")

                                control_samples[[.x]] <- callModule(module = sample_widget,
                                                                    id = paste0(input$file, "-", .x),
                                                                    sample_well = .x,
                                                                    sample_key = .y,
                                                                    samples = samples,
                                                                    file_id = tecan_n$id(),
                                                                    type = tecan_n$raw()$type,
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