decision_server <- function(input, output, session, gtoken) {

        gs_auth(token = gtoken,
                key = hblab_id,
                secret = hblab_secret, cache = TRUE)
        
        #Get data from Google 'Assembly Cost'
        source("order_dna_decision/import_dna_decision_data.R")
        source("order_dna_decision/decision_values.R")
        
        #Getting the Google APIs key & secret...
        source("MS/MS_values.R")
        
        
        assemply_sheets <- get_assembly_costs(url) 
        step_values <- assemply_sheets$steps %>%
                #Applying failure rate
                mutate(min_time = min_time * (1 + (1 - success_rate)),
                        max_time = max_time * (1 + (1 - success_rate)),
                        researcher_time = researcher_time * (1 + (1 - success_rate)))
        hr_costs <- assemply_sheets$HR
        dna_costs <- assemply_sheets$DNA
        
        observe({
                print(step_values$`Assembly step`)
                updateCheckboxGroupInput(session, inputId = "lab_steps",
                        choices = step_values$`Assembly step`)
                
                updateSelectInput(session, inputId = "position",
                        choices = hr_costs$Position)
        })
        
        #GENERATE UI INPUT FOR PARTS
        
        #To know if Parts_Creation is selected 
        parts_selected <- reactive({
                "Parts_Creation" %in% input$lab_steps
        })
        
        #To know if the numeric input for nb of parts creation
        rendered <- reactiveVal(value = FALSE)
        
        #Add or remove numeric input for number of parts
        observeEvent(parts_selected(), {
                ns <- session$ns
                if (parts_selected() & !rendered()) {
                        rendered(TRUE)
                        insertUI(
                                selector = paste0("#", ns("lab_steps")),
                                where = "afterEnd",
                                ui = numericInput(inputId = ns("parts_nb"),
                                        label = "Nb of Parts",
                                        value = 1,
                                        min = 1,
                                        max = 20)
                        )
                } else if(!parts_selected()) {
                        rendered(FALSE)
                        removeUI(selector = paste0("div:has(> #",ns("parts_nb"),")"))
                }
                
        })
        
        output$summary <- renderTable(digits = 1,{
                shiny::validate(need(!is.null(input$lab_steps), message = "Please select one step"))
                #Get employee hourly cost
                position_values <- hr_costs[hr_costs$Position == input$position, ]
                
                if (!is.null(input$parts_nb)) {
                        step_values <- step_values %>%
                                filter(`Assembly step` == "Parts_Creation") %>%
                                mutate_if(is.numeric, funs(. *input$parts_nb)) %>%
                                bind_rows(filter(step_values,`Assembly step` != "Parts_Creation")) 
                }
                
                step_values %>%
                        filter(`Assembly step` %in% input$lab_steps) %>%
                        summarise(`Lab days avg` = (sum(min_time) + sum(max_time)) / (24*2),
                                `Researcher hours` = sum(researcher_time),
                                Cost = paste0(position_values$Hourly_Rate * `Researcher hours`,"$")
                        )
        })
        
        output$dna <- renderTable(digits = 1, {
                dna_costs %>%
                        filter(max_length ==
                                        with(dna_costs,
                                                ifelse(input$bp_length <= max(max_length),
                                                        min(max_length[input$bp_length <= max_length]),
                                                        max(max_length))
                                        )
                        ) %>%
                        mutate(exact_cost = price_per_bp * input$bp_length) %>%
                        select("Order days avg" = delay,
                                "Cost" = exact_cost,
                                "Synthesis provider" = provider) %>%
                        top_n(-1, wt = Cost)
        })
}