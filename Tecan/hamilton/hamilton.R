source("hamilton/hamilton_values.R")

hami_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                sidebarPanel(
                        width = 2,
                        selectInput(inputId = ns("protocol_type"),
                                    label = "Select Hamilton Protocol",
                                    choices = protocols)
                ),
                mainPanel(
                        actionButton(inputId = ns("add_part"),
                                     label = "Add Element"),
                        conditionalPanel(condition = paste0("output['", ns("has_parts"), "'] == true"),
                                         actionButton(inputId = ns("create_files"),
                                                      label = "Generate"))        
                )
        )
}

#paste0("output['",ns("type"),"'] == 'DNA Quantification'" )

hami_server <- function(input, output, session, gtoken) {
        library(shiny);library(tidyverse);library(stringr);library(purrr)
        source("helpers/part_widget.R")
        source("registry/registry_helpers.R")
        source("helpers/strings.R")
        source("hamilton/part_row.R")
        source("hamilton/generate_files.R")
        
        ns <- session$ns
        args_list <- list(type = "hamilton_PCR")
        
        # Check if registry is loaded
        if (!exists("registry")) {
                registry <- registry_key_names(registry_url, registry_sheets)
        }
        
        # Initialize the parts storage reactive value
        parts <- reactiveVal(tibble(letter = character(),
                                    key = character(),
                                    n_pcr = integer(),
                                    l_primer = character(),
                                    r_primer = character()
                                    # ,
                                    # #For a potential later point when we combine registry plates and
                                    # #operator set plates for templates and primers
                                    # template_position_type = character(),
                                    # template_position = character(),
                                    # l_primer_position = character()
                                    ))
        
        # When user adds a part
        observeEvent(input$add_part,{
                # Set identifying label as first unused letters A-Z, AA, AZ, etc...
                current_label <- first_unused(parts()$letter)
                # Add new row to parts for the new letter
                parts(parts() %>%
                              add_row(letter = current_label
                                      # ,
                                      # template_position_type = "operator_plate"
                                      # ,
                                      # template_position = generate_96_pos()[nrow(parts()) + 1],
                                      # l_primer_position = template_position
                              ))
                insertUI(selector = paste0("#",ns("add_part")),
                         where = "beforeBegin",
                         ui = part_row_ui(ns(paste0("Part_", current_label)),
                                          part_label = current_label,
                                          part_key = "",
                                          args_list,
                                          registry)
                )
                
                callModule(module = part_row,
                           id = paste0("Part_", current_label),
                           parts = parts,
                           part_label = current_label,
                           part_key = "",
                           args_list,
                           registry)
        })
        
        #Generate a UI conditional variable used to switch the generate files button on/off
        output$has_parts <- reactive({
                length(parts()$letter) > 0 && all(
                        map_lgl(paste0("Part_", parts()$letter,"-",parts()$letter,"-well_key"),
                                ~ !is.null(input[[.x]]) && (input[[.x]] != "")))
        })
        outputOptions(output, "has_parts", suspendWhenHidden = FALSE)
       
         # user presses the generate button, and this logic generates the various files
        observeEvent(input$create_files, {
                # Generate the right_primers position column
                n <- nrow(parts())
                all_96 <- generate_96_pos()
                parts(parts() %>%
                              add_column(template_position = all_96[1:n],
                                         l_primer_positin = template_position,
                                         r_primer_position = all_96[n + 1:n])
                )
                browser()
                # Create a Progress object
                progress <- shiny::Progress$new()
                progress$set(message = "Generating files...", value = 0)
                # Create the csv files
                generate_files(parts, progress)
                
                link <- generate_operator_sheets(parts, progress)
                progress$close()
                showModal(modalDialog(
                        title = "Open Operator's Instructions",
                        tags$a(class = "btn btn-default",
                               href = link,
                               "Open instructions",
                               target = "_blank"),
                        easyClose = TRUE
                ))
        })
        
        observeEvent(input$protocol_type, {
                #IN case of PCR, remove all 'parts rows'
                walk(parts()$letter, ~ {
                        selector_str <- paste0("#", ns(paste0("Part_", .x,"-part_row")))
                        #selector_str <- paste0("#", ns(paste0("Part_", .x)),"-part_row")
                        removeUI(selector = selector_str)}
                     )
        }, ignoreInit = TRUE)
}