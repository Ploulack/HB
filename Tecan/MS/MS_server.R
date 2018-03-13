library(shiny); library(rvest)
ms_server <- function(input, output, session) {
        source("ms/ms_extract.R"); source("ms/ms_functions.R")
        source("registry/registry_helpers.R"); source("helpers/strings.R")
        source("helpers/plates_helpers.R")

        options(shiny.trace = FALSE)
        ns <- session$ns

        dics <- reactiveValues()

        #### GENERATE MS RUN ####
        observeEvent(input$create_ms, {
                new_ms_modal(ns)

                if (!exists("registry")) {
                        print("Loading Registry")
                        registry <- registry_key_names(registry_url, registry_sheets)
                }

                dics$plasmids <- registry %>%
                        select(1:3, type) %>%
                        filter(type == "Plasmids") %>%
                        pull(KEY)

                if (is.null(dics$strains)) {
                        print("Loading Strains")
                        dics$strains <- get_strains()
                }
        })

        ms_samples <- reactiveVal(
                tibble(
                        label = character(),
                        strain = character(),
                        plasmid = character(),
                        group_id = character(),
                        pos = character()
                )
        )

        available_pos <- reactive({

                if (input$is_48_wells_plate) {

                } else {
                generate_96_pos() %>%
                                keep(~!(. %in% ms_samples()$pos))
                }
        })

        observeEvent(input$add_sample, {

                #To change, for now, keep using letters
                current_label <- first_unused(ms_samples()$label)

                # Add new row for the new letter
                ms_samples(
                        ms_samples() %>%
                                add_row(label = current_label)
                )

                insertUI(selector = paste0("#",ns("add_sample")),
                         where = "beforeBegin",
                         ui = sample_row_ui(ns(paste0("sample_", current_label)),
                                          dics$strains,
                                          dics$plasmids,
                                          available_pos())
                )

                callModule(module = sample_row_server,
                           id = paste0("sample_", current_label),
                           ms_samples = ms_samples,
                           sample_label = current_label)

        })

        observeEvent(input$new_ms_ok, {
                csv_file <- generate_sample_list_csv(ms_samples())
        })

        #### INIT & FILE HANDLING ####
        source("helpers/ui_generics/select_file_server.R")
        source("helpers/general.R"); source("protocols/protocols_module.R")
        source("mongo/db_values.R"); source("helpers/mongo_helpers.R")
        tab_name <-  str_extract(session$ns(""), "^\\w+")
        selected <- reactiveVal()
        ms_folder <- get_drive_url(session, tab_name)
        ms_progress <- shiny::Progress$new()

        ms <- callModule(module = select_file,
                id = "files",
                progress = ms_progress,
                drive_url = ms_folder,
                selected = selected
        )

        ms$tbl <- reactiveVal(NULL)

        obtain_file_data(
                ms$go_file,
                "MS",
                file_container = ms,
                dat_container = ms$tbl,
                extract_function = extract_ms
        )

        #### DB storage ####
        ms_db <- db_from_environment(session, collection = "ms")

        file_record <- eventReactive(ms$go_file(), {
                record <- mongo_file_entry(ms_db, ms$id(), tab_name)

                if (!record$entry_exists) {
                        ms_dat_json <- jsonlite::toJSON(x = ms$tbl(),
                                                        dataframe = "rows",
                                                        POSIXt = "mongo",
                                                        pretty = TRUE)

                        query <- str_interp('{
                                            "_id" : "${ms$id()}",
                                            "name": "${ms$file_dribble()$name}",
                                            "data": ${ms_dat_json}}'
                        )

                        insert_log <- ms_db$insert(query)
                        cat("insert log : ", insert_log)
                }
                return(record)
        })

        #### PROTOCOLS ####
        ms_p <- callModule(module = protocols_handler,
                           id = "ms",
                           file_container = ms,
                           data_container = ms$tbl,
                           selected = selected,
                           db = ms_db)

        #### GRAPHIC LAYOUT TESTS FOR MS DATA DIPLAY ####

        observeEvent(ms$go_file(), {
                #Reset stored choices
                stored_choices(NULL)

                #Reset the select all button
                updateCheckboxInput(session, "select_all", value = FALSE)

                updateCheckboxGroupInput(session = session,
                                         inputId = "samples",
                                         choices = unique(ms$tbl()$Name),
                                         selected = unique(ms$tbl()$Name)[1]
                                         )
                updateCheckboxGroupInput(session = session,
                                         inputId = "molecules",
                                         choices = unique(ms$tbl()$Molecule),
                                         selected = unique(ms$tbl()$Molecule)
                )

        })

        stored_choices <- reactiveVal(NULL)

        observeEvent(input$select_all, {

                if (input$select_all) {
                        stored_choices(input$samples)

                        non_0_conc_choices <- ms$tbl() %>%
                                filter(Concentration > 0) %>%
                                pull(Name) %>%
                                unique()

                        updateCheckboxGroupInput(session = session,
                                                 inputId = "samples",
                                                 selected = non_0_conc_choices)

                } else if (!is.null(stored_choices())) {
                        browser()
                        updateCheckboxGroupInput(session = session,
                                                 inputId = "samples",
                                                 selected = stored_choices())
                }
        }, ignoreInit = TRUE)

        display_tbl <- reactiveVal()

        unaggregated_tbl <- reactive({
                if (any(is.null(c(input$samples, input$molecules)))) return()

                res_tbl <- ms$tbl() %>%
                        select(c(Name, Molecule, Concentration)) %>%
                        group_by(Name, Molecule) %>%
                        arrange(Name) %>%
                        filter(Name %in% input$samples) %>%
                        filter(Molecule %in% input$molecules)

                return(res_tbl)
        })

        observeEvent(unaggregated_tbl(), {
                display_tbl(unaggregated_tbl() %>%
                                    summarise(sd = sd(Concentration),
                                              Mean = mean(Concentration)) %>%
                                    ungroup() %>%
                                    mutate(cut_off = TRUE)
                            )
        }, priority = -1)

        last_click <- reactiveVal(NULL)
        observeEvent(input$click, {
                if (!is.null(input$click)) last_click(input$click)
        })

        #On file change reset the value selection from click
        observeEvent(ms$go_file(), {
                last_click(NULL)
        })

        clicked_sample <- eventReactive(last_click(), {

                if (is.null(last_click())) return(NULL)

                click_x <- last_click()$x
                n_molecules <- length(input$molecules)
                splits <- seq(1/(2 * n_molecules), 1 - 1/(2 * n_molecules), 1/n_molecules)

                sample_lvls <- display_tbl()$Name %>%
                        as_factor() %>%
                        levels()
                name <- sample_lvls[round(click_x)]

                molecule_lvls <- display_tbl()$Molecule %>%
                        as_factor() %>%
                        droplevels() %>%
                        levels()

                x <- click_x - round(click_x) + 1/2

                molecule_name <- molecule_lvls[which.min(abs(splits - x))]
                # browser()
                value <- display_tbl() %>%
                        filter(Molecule == molecule_name & Name == name) %>%
                        pull(Mean)

                list(name = name,
                     molecule = molecule_name,
                     value = value)

        }, ignoreNULL = FALSE)

        observeEvent(clicked_sample(), {

                if (is.null(clicked_sample()$value)) {
                        display_tbl(
                                display_tbl() %>%
                                        mutate(cut_off = TRUE)
                        )
                } else {

                        display_tbl(
                                display_tbl() %>%
                                        mutate(cut_off = if_else(
                                                Mean >= clicked_sample()$value,
                                                TRUE,
                                                FALSE,
                                                missing = FALSE)
                                        )
                        )
                }
        })

        output$file_title <- renderText({
                validate(
                        need(ms$tbl(), message = "no ms data")
                        )

                molecules <- ms$tbl()$Molecule %>%
                        unique() %>%
                        str_c(collapse = " ")
                date_range <- min(ms$tbl()$Time,na.rm = TRUE) %>%
                        paste(
                                max(ms$tbl()$Time,na.rm = TRUE) %>%
                                      (function(x) {paste(hour(x), minute(x), second(x), sep = ":")})
                        )

                paste0(molecules, " ~ ", date_range)
        })

        barplot_scale <- reactive({
                ifelse(input$log_scale, "log1p", "identity")
        })

        output$bar <- renderPlot({
                if (is.null(display_tbl()) || nrow(display_tbl()) == 0) return()

                g <- ggplot(display_tbl()) +
                        aes(x = Name, y = Mean, fill = Molecule) +
                        geom_bar(position = "dodge",
                                 stat = "identity",
                                 aes(alpha = cut_off %>%
                                             factor(levels = c(FALSE, TRUE))
                                     )
                                 ) +
                        geom_errorbar(position = position_dodge(.9),
                                      aes(ymax = Mean + sd,
                                          ymin = Mean - sd,
                                          width = .15)) +
                        theme(axis.text.x = element_text(angle = 60,
                                                         hjust = .8,
                                                         size = 10,
                                                         face = if_else(display_tbl()$cut_off,"bold", "plain"))) +
                        scale_y_continuous(trans = barplot_scale()) +
                        scale_fill_discrete(limits = levels(ms$tbl()$Molecule)) +
                        scale_alpha_discrete(drop = FALSE, guide = "none")

                if (!is.null(clicked_sample()$value)) {
                        g + geom_hline(yintercept = clicked_sample()$value)
                } else {
                        g
                }


        })

        output$table <- renderTable({
                if (input$display_raw) {
                        unaggregated_tbl()
                        }  else display_tbl()
        })

        # Print the name of the x value
        output$x_value <- renderText({
                if (is.null(clicked_sample())) return()
                else {
                        HTML("You've selected sample <code>", clicked_sample()$name, "</code>",
                             "<br>and molecule <code>", clicked_sample()$molecule,"</code>",
                             "<br>of value <code>", round(clicked_sample()$value,2), "</code>")
                }
        })
}