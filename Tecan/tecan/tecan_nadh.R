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
        samples <- bind_cols(nadh[-calibration_idx, ], pred = predict(nadh_model, nadh[-calibration_idx, ])) %>%
                reactiveVal()
        
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
                samples(
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
                        stat_smooth(method = "lm", se = FALSE, )
                
        })
        
        output$samples_predicted <- renderTable({
                samples()
        })
}