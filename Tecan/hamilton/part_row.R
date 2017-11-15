source("helpers/part_widget.R")

part_row_ui <- function(id, part_label, part_key, args_list, registry) {
        ns <- NS(id)
        tags$div(id = ns("part_row"),
                 fluidRow(
                 part_widget_ui(ns(part_label), part_label, part_key, args_list, registry),
                 column(width = 2, sliderInput(inputId = ns("pcr_count"),
                              label = "How many?",
                              value = 1, min = 1, max = 10, step = 1)),
                 column(width = 1, actionButton(inputId = ns("delete_part"),
                              label = "Delete"))
        ))
}

part_row <- function(input, output, session, parts, part_label, part_key, args_list, registry) {
        ns <- session$ns
        
        callModule(module = part_widget,
                   id = part_label,
                   part_label = part_label,
                   part_key = part_key,
                   args_list = args_list, 
                   registry = registry)
        
        observeEvent(input$delete_part, {
                removeUI(selector = paste0("#", ns("part_row")))
                isolate(parts$letters <- parts$letters %>% 
                        keep(~ .x != part_label))
        })
}