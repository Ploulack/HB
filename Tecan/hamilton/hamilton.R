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

hami_server <- function(input, output, session, gtoken) {
        library(shiny);library(tidyverse);library(stringr);library(purrr)
        source("helpers/part_widget.R")
        source("registry/registry_helpers.R")
        source("registry/registry_values.R")
        source("helpers/strings.R")
        source("hamilton/part_row.R")
        source("hamilton/generate_files.R")
        
        ns <- session$ns
        args_list <- list(type = "hamilton_PCR")
        
        
        if (!exists("registry")) {
                registry <- registry_key_names(registry_url, registry_sheets)
        }
        
        parts <- reactiveVal(tibble(letters = character(),
                                    key = character(),
                                    n_pcr = integer(),
                                    l_primer = character(),
                                    r_primer = character()))
        
        observeEvent(input$add_part,{
                current_label <- first_unused(parts()$letters)
                parts(parts() %>% bind_rows(tibble(letters = current_label,
                                                   key = "",
                                                   n_pcr = as.integer(0),
                                                   l_primer = "",
                                                   r_primer = "")))
                
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
        
        output$has_parts <- reactive({
                length(parts()$letters) > 0 && all(
                        map_lgl(paste0("Part_", parts()$letters,"-",parts()$letters,"-well_key"),
                                ~ !is.null(input[[.x]]) && (input[[.x]] != "")))
        })
        outputOptions(output, "has_parts", suspendWhenHidden = FALSE)
        
        observeEvent(input$create_files, {
                generate_files(parts)
                link <- generate_operator_sheets(parts)
                print(link)
                showModal(modalDialog(
                        title = "Open Operator's Instructions",
                        tags$a(class = "btn btn-default",
                               href = link,
                               "Open instructions",
                               target = "_blank"),
                        easyClose = TRUE
                ))
        })
}