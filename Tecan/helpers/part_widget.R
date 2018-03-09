source("tecan/tecan_values.R")

part_widget_ui <- function(id, part_label, part_key, type, registry) {
        #part_label = sample_well
        #part_key = sample_key
        #args_list = tecan_file actually tecan_file, so changes will be needed,
        ns <- NS(id)
        if (type %in% c(tecan_protocols_with_db[1],"hamilton_PCR")) {
                #This is actually the general case, the other one is just for H2O2
                tags$div(id = ns("widget"),
                          column(width = 2,
                                 selectizeInput(
                                         inputId = ns("well_key"),
                                         label = str_interp("Sample ${part_label}"),
                                         choices = registry$KEY  %>%
                                                 prepend(c("")),
                                         selected = part_key,
                                         multiple = FALSE)
                          )
                )
        }
        else {
                tags$span(id = ns("widget"),
                          column(width = 2,
                                 textInput(inputId = ns("well_key"),
                                           label = part_label,
                                           value = part_key)
                          )
                )
        }
}

part_widget <- function(input, output, session, parts, part_label, part_key, type, db = NULL, registry) {
        ns <- session$ns

        # Slow text input value update
        # TODO: only for text inputs...
        reactive_key <- reactive({input$well_key})
        delay <- ifelse(type == tecan_protocols_with_db[2], 1500, 0)
        input_key <- debounce(reactive_key, delay)

        observeEvent(input_key(), {
                if (!type %in% tecan_protocols_with_db) return()
                if (is.null(input_key()) || input_key() == "") return()

                if (input_key() != sample_key) {
                        #Update db entry
                        str1 <- str_interp('{ "file" : "${tecan_file$file}", "samples.Sample" : "${sample_well}"}')

                        str2 <- str_interp('{"$set" : {"samples.$.Key" : "${input_key()}"}}')
                        upd_check <- db$update(str1, str2)
                        if (upd_check$modifiedCount == 1) {
                                showNotification(ui = str_interp("Updated ${sample_well} with ${input_key()}"),
                                                 duration = 3,
                                                 type = "message")
                        }
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

        observeEvent(c(args_list$file), { #
                removeUI(selector = paste0("#", ns("widget")))
                removeUI(selector = paste0("#", ns("js_id")))
        }, ignoreInit = TRUE, priority = 1)

        return(input_key)
}