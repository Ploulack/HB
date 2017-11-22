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
                 ),
                 fluidRow(
                         tableOutput(outputId = ns("info")),
                         tags$hr()
                 ))
}

part_row <- function(input, output, session, parts, part_label, part_key, args_list, registry) {
        ns <- session$ns
        input_key <- reactiveValues(value = NULL)
        
        input_key$value <- callModule(module = part_widget,
                                id = part_label,
                                parts = parts,
                                part_label = part_label,
                                part_key = part_key,
                                args_list = args_list, 
                                registry = registry)
        
        part_info <- reactive({
                if (is.null(input_key$value())) return()
                if (input_key$value() == "")
                        return(tibble(
                                Name = " ",
                                Length = 0,
                                L_Primer = " ",
                                R_Primer = " "))
                else return(
                        registry %>%
                                filter(KEY == input_key$value()) %>%
                                select(2, Length, L_Primer, R_Primer))
        })
        
        observeEvent(c(input_key$value(), input$pcr_count), {
                if (any(is.null(c(input_key$value(), input$pcr_count)))) return()
                parts(parts() %>% mutate(
                        key = if_else(letter == part_label, input_key$value(), parts()$key),
                        n_pcr = if_else(letter == part_label, input$pcr_count, parts()$n_pcr),
                        l_primer = if_else(letter == part_label, part_info()$L_Primer, parts()$l_primer),
                        r_primer = if_else(letter == part_label, part_info()$R_Primer, parts()$r_primer)) 
                )
        })
        
        output$info <- renderTable({
                part_info()
        })
        
        observeEvent(input$delete_part, {
                removeUI(selector = paste0("#", ns("part_row")))
                isolate(
                        parts(
                                parts() %>% filter(letter != part_label))
                        )
        })
}