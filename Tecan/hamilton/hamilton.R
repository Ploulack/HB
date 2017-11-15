hami_ui <- function(id) {
        ns <- NS(id)
        fluidPage(
                actionButton(inputId = ns("add_part"),
                             label = "Add Part"),
                conditionalPanel(condition = paste0("output['", ns("has_parts"), "'] == true"),
                                 actionButton(inputId = ns("create_files"),
                                              label = "Generate"))
        )
}

#paste0("output['",ns("type"),"'] == 'DNA Quantification'" )

hami_server <- function(input, output, session) {
        library(shiny);library(tidyverse);library(stringr);library(purrr)
        source("helpers/part_widget.R")
        source("registry/registry_helpers.R")
        source("registry/registry_values.R")
        source("helpers/strings.R")
        source("hamilton/part_row.R")
        
        ns <- session$ns
        args_list <- list(type = "hamilton_PCR")
        
        
        if (!exists("registry")) {
                registry <- registry_key_names(registry_url, registry_sheets)
        }
        
        parts <- reactiveValues(letters = character(0))
        output$has_parts <- reactive({
                test <- length(parts$letters) > 0 &&
                        all(
                                map_lgl(paste0("Part_", parts$letters,"-",parts$letters,"-well_key"),
                                        ~ !is.null(input[[.x]]) && (input[[.x]] != ""))
                                )
               #if (test) browser()
                return(test)
        })
        outputOptions(output, "has_parts", suspendWhenHidden = FALSE)
        
        observeEvent(input$add_part,{
                current_label <- first_unused(parts$letters)
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
                
                parts$letters <-  append(parts$letters, current_label)
                                        
        })
}