source("tecan/tecan_values.R")

pop_calibration_values <- function(calibration, required_msg = NULL, ns) {

        showModal(
                modalDialog(
                        fluidRow(
                                map2(calibration$Sample, calibration$uM, ~ {
                                        column(width = 2,
                                               numericInput(inputId = ns(paste0(.x, "conc")),
                                                            label = .x,
                                                            value = .y)
                                        )
                                })
                        ),
                        if (!is.null(required_msg))
                                div(tags$b(required_msg, style = "color: red;")),
                        footer = tagList(actionButton(inputId = ns("ok"),
                                                       label = "OK"),
                                          modalButton("Cancel")
                        ),
                        size = "l"
                )
        )
}

nadh_detection <- function(nadh, cal_conc, input, output, ns) {
        measured_samples <- reactiveVal(NULL)
        nadh <- nadh %>%
                rename(Fluorescence = Value)

        calibration_idx <- 1:which.max(nadh$Fluorescence)
        calibr_react <- reactiveVal(value = nadh[calibration_idx, ])
        #make sure that uM variable is filled to the nth row
        cal_conc_adapted <- numeric(0)
        for (i in seq_along(calibration_idx)) {
                cal_conc_adapted[i] <- nth(cal_conc, i, default = NA)
        }
        calibr_react(calibr_react() %>%
                             mutate(uM = cal_conc_adapted)
                     )

        if (length(calibration_idx) != length(cal_conc)) {
                pop_calibration_values(calibration = calibr_react(), ns = ns)
        }

        nadh_model <- lm(uM ~ Fluorescence, data = calibr_react())

        # samples <- reactiveVal(value = nadh[-calibration_idx, ])
        # bind_cols(samples(),predict(nadh_model, samples())) %>%
        #         samples()
        measured_samples(bind_cols(nadh[-calibration_idx, ],
                                   predicted_uM = predict(nadh_model, nadh[-calibration_idx, ]))
                         )

        observeEvent(input$ok, {
                # Gather input values
                input_vals <- map_dbl(calibr_react()$Sample, ~ {
                        input[[paste0(.x, "conc")]]
                })
                # check that all values are not NA
                # and re-open modal with message if not
                if (any(is.na(input_vals))) {
                        pop_calibration_values(calibr_react(), required_msg = "Please fill all values", ns)
                        return()
                } else removeModal()

                calibr_react(calibr_react() %>%
                                     mutate(uM = input_vals))
                nadh_model <- lm(uM ~ Fluorescence, data = calibr_react())
                measured_samples(
                        bind_cols(nadh[-calibration_idx, ], predicted_uM = predict(nadh_model, nadh[-calibration_idx, ]))
                )

        })

        observeEvent(input$open_calibration, {
                pop_calibration_values(calibration = calibr_react(), ns = ns)
        })

        output$calibration <- renderTable({
                calibr_react()
        })

        output$regression_graph <- renderPlot({
                ggplot(calibr_react()) +
                        aes(y = uM, x = Fluorescence) +
                        geom_point() +
                        stat_smooth(method = "lm", se = FALSE)
        })

        output$samples_predicted <- renderTable({
                measured_samples()
        })
        #return(measured_samples())
}

sample_widget_ui <- function(id, sample_well, sample_key, tecan_file, registry) {
        ns <- NS(id)
        if (tecan_file$type == tecan_protocols_with_db[1]) {
                tags$span(id = ns("widget"),
                         column(width = 2,
                                selectizeInput(
                                        inputId = ns("well_key"),
                                        label = str_interp("Sample ${sample_well}"),
                                        choices = registry$KEY  %>%
                                                prepend(c("", "Water", "Plasmid")),
                                        selected = sample_key,
                                        multiple = FALSE)
                                )
                         )
        }
        else {
                tags$span(id = ns("widget"),
                         column(width = 2,
                                textInput(inputId = ns("well_key"),
                                          label = sample_well,
                                          value = sample_key)
                         )
                )
        }
}

sample_widget <- function(input, output, session, sample_well, sample_key, samples, tecan_file, db, registry) {
        ns <- session$ns

        # Slow text input value update
        reactive_key <- reactive({input$well_key})
        delay <- ifelse(tecan_file$type == tecan_protocols_with_db[2], 1500, 0)
        input_key <- debounce(reactive_key, delay)

        observeEvent(input_key(), {
                if (is.null(input_key()) || input_key() == "") return()

                if (input_key() != sample_key) {
                        #Update db entry
                        str1 <- str_interp('{ "file" : "${tecan_file$file}", "samples.Sample" : "${sample_well}"}')
                        str2 <- str_interp('{"$set" : {"samples.$.Key" : "${input_key()}"}}')
                        upd_check <- db$update(str1, str2)
                        if (upd_check$matchedCount == 1) {
                                disp_msg <- str_interp("Updated ${sample_well} with ${input_key()}")
                        } else {
                                disp_msg <- str_interp("Something went wrong")
                                warning("db wasn't modified for some reason. Plz investigate")
                        }
                        showNotification(ui = disp_msg,
                                         duration = 3,
                                         type = "message")
                }

                key <- registry %>%
                        filter(KEY == input_key()) %>% pull(var = 2)
                #Remove potential previous part name
                removeUI(selector = paste0("#", ns("js_id")))
                #Display part name
                insertUI(selector = paste0("#", ns("well_key")),
                                 where = "afterEnd",
                                 ui = tags$h6(paste0(key, "  "),
                                              id = ns("js_id"))
                        )
        })

        observeEvent(tecan_file$file, {
                removeUI(selector = paste0("#", ns("widget")))
                removeUI(selector = paste0("#", ns("js_id")))
        }, ignoreInit = TRUE, priority = 1)

        return(input_key)

}

db_update_entry <- function(db, tecan_file, samples, notif_msg = NULL) {
        samples_str <- jsonlite::toJSON(samples, dataframe = 'rows')
        #String to add the 280 data
        if (tecan_file$type == tecan_protocols_with_db[1]) {
                #TODO: Add check that batch_2$Wavelength == 280...
                data280 <- jsonlite::toJSON(tecan_file$data$Batch_2$Measures, dataframe = "rows")
                str280 <- str_interp(', "samples_280":${data280}')
        } else str280 <- ''

        update_str <- str_interp('{"$set": {
                                 "name" : "${tecan_file$file_dribble$name}",
                                 "type": "${tecan_file$type}",
                                 "note" : "",
                                 "samples":${samples_str}${str280}}}')

        mongo_update_file(db, tecan_file$file, update_str,
                          notif_msg = notif_msg)
        }

db_create_entry <- function(db, tecan_file, samples) {
        notif_msg <- str_interp("Created db entry for ${tecan_file$file_dribble$name}")
        db_update_entry(db, tecan_file, samples, notif_msg)
}



