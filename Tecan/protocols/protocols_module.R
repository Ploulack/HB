library(rlang)

protocols_handler <- function(input,
                              output,
                              session,
                              file_container,
                              data_container,
                              selected) {

        #This is to dynamically get the correct column name of the protocols experiment sheet
        tab_name <-  str_extract(session$ns(""), "^\\w+")
        processed_plates <- paste0(tab_name,"_processed_plates")

        is_unitary <- reactive({
                if (tab_name == "tecan") {
                        (is.null(data_container()$user_msg) ||
                                 str_length(data_container()$user_msg) == 0)
                }
                # if (tab_name == "ms"){}
        })


        #On first opening, move files to their appropriate folders
        observeEvent(data_container(), {
                unitary_folder <- file_container$protocols()$name[1]

                if (file_container$protocol() != "New") return()

                if (is_unitary()) {
                        progress_prot_change <- Progress$new()
                        progress_prot_change$inc(.5, str_interp("Moving file to ${unitary_folder} drive folder."))
                        move_drive_file(file_container, prot_name = unitary_folder, tab_name)

                        progress_prot_change$inc(.5, str_interp("Displaying list of ${unitary_folder} name."))
                        selected(update_selected("Unitary", file_container$id()))
                        # update_uis("Unitary", tecan_n, session = session)
                        progress_prot_change$close()
                } else {
                        #If the Tecan file includes a custom msg popup choice to the user to do the matching
                        protocols_set_modal(input = input,
                                            file_container$file_dribble()$name,
                                            data_container()$user_msg, #Once MS file struture for experiments tagging is known, this must change,
                                            protocols = file_container$protocols(),
                                            session = session)
                }
        })

        #Update choices of plates in modal dialog
        observeEvent(input$set_protocol, {

                if (!input$set_protocol %in% file_container$protocols()$name) return()

                selected_prot <- file_container$protocols() %>%
                        filter(name == input$set_protocol)

                proced_plates <- selected_prot %>%
                        "[["(processed_plates) %>%
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

                # Close the dialog popup
                removeModal()

                #TODO: Add checks on the experiment and plate nb inputs...and reopen modal with required message.

                # TODO: Create another container than tecan_n for the pooling stuff, this is dangerous:
                # on a change of file, this data is going to stick if tecan_n no re-initialized....
                file_container$experiment <- reactiveVal(input$set_protocol)
                file_container$plate <- input$set_plate_nb
                selected_prot <- file_container$protocols() %>%
                        filter(name == input$set_protocol)

                #Update protocols gsheet with new plate
                #Add new plate to string
                if (is.na(selected_prot[[processed_plates]]))
                        updated_plates <- input$set_plate_nb
                else updated_plates <- selected_prot[[processed_plates]] %>%
                        str_c(", ", input$set_plate_nb) %>%
                        str_split(", ") %>%
                        simplify() %>%
                        as.integer() %>%
                        unique() %>%   #I have a feeling this observer repeats...and so by forcing unique, prevent issues
                        sort.int() %>%
                        as.character() %>%
                        str_c(collapse = ", ")

                tmp_prot <- file_container$protocols()
                tmp_prot[selected_prot$index, processed_plates] <- updated_plates
                file_container$protocols(tmp_prot)

                selected_prot <- file_container$protocols() %>%
                        filter(name == input$set_protocol)

                # ColID gets the A, B, ..Z col ID for the gsheet insertion
                # More resilient to gsheet columns reordering
                colID <- gsheet_colID_from_tibble(file_container$protocols(), processed_plates)

                gs_edit_cells(ss = file_container$protocols_gsheet,
                              ws = 1,
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

                #Update the tibble with the current time and with the file link
                plates[(input$set_plate_nb %>% as.integer()), ] <- tibble(
                        processed_date = Sys.time() %>%
                                force_tz(tzone = "EST") %>%
                                as.character(),
                        !!paste0(tab_name,"_file_url") := file_container$file_dribble() %>%
                                dribble_get_link()
                        )
                write_csv(plates, csv_path)

                drive_update(file = csv_drive_id,
                             media = csv_path)

                #Move Tecan file to protocol folder
                move_drive_file(file_container, prot_name = input$set_protocol, tab_name)

                #Set user file selectInput on the same file
                selected(update_selected(input$set_protocol, file_container$id()))

        }, ignoreInit = TRUE, priority = 1)

        return(list(
                ok_protocol = reactive(input$ok_protocol),
                set_protocol = reactive(input$set_protocol),
                set_plate_nb = reactive(input$set_plate_nb)
        ))
}