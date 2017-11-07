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

sample_widget_ui <- function(id, sample_well, sample_key) {
        ns <- NS(id)
        column(width = 2,
               tags$div(id = ns("widget"),
                 textInput(inputId = ns("well_key"),
                           label = sample_well,
                           value = sample_key)
                 )
        )
}

sample_widget <- function(input, output, session, sample_well, sample_key, samples, tecan_file, db) {
        ns <- session$ns
        
        # Slow text input value update
        reactive_key <- reactive({input$well_key})
        input_key <- debounce(reactive_key, 1500)
        
        observeEvent(input_key(), {
                if (is.null(input_key())) return()
                if (input_key() == sample_key) return()
                #Todo: currently the whole 'samples' updating is unused and probably un-needed
                #the idea was taken from the work on gel, but here the number of wells is fixed...
                samples(
                        samples() %>%
                                filter(Sample != sample_well) %>%
                                bind_rows(as_tibble(list(Sample = sample_well, Key = input_key())))
                )
                
                str1 <- str_interp('{ "file" : "${tecan_file()$file}", "samples.Sample" : "${sample_well}"}')
                                   
                str2 <- str_interp('{"$set" : {"samples.$.Key" : "${input_key()}"}}')
                upd_check <- db$update(str1, str2)
                if (upd_check) {
                showNotification(ui = str_interp("Updated ${sample_well} with ${input_key()}"),
                                 duration = 3,
                                 type = "message")
                }

        })
        
        observeEvent(tecan_file()$file, {
                removeUI(selector = paste0("#", ns("widget")))
        }, ignoreInit = TRUE, priority = 1)
        
}

display_sample_widget <- function(sample_well, sample_key, samples, tecan_file, ns, db) {
        insertUI(selector = "#Tecan-widgets_bar",
                  where = "beforeEnd",
                  ui = sample_widget_ui(id = ns(sample_well), sample_well, sample_key)
                  )
                
        callModule(module = sample_widget,
                   id = sample_well,
                   sample_well = sample_well,
                   sample_key = sample_key,
                   samples = samples,
                   tecan_file,
                   db)
}

db_create_entry <- function(db, tecan_file, samples) {
        samples_str <- jsonlite::toJSON(samples(), dataframe = 'rows')
        str1 <- str_interp('{"file" : "${tecan_file()$file}" }')
        str2 <- str_interp('{"$set": {
                           "name" : "${tecan_file()$file_dribble$name}",
                           "type": "${tecan_file()$type}",
                           "note" : "",
                           "samples":${samples_str}}}')
        db$update(str1, str2, upsert = TRUE)
        showNotification(ui = str_interp("Created db entry for ${tecan_file()$file_dribble$name}"),
                         duration = 3,
                         type = "message")
}

