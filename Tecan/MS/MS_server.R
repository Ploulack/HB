library(shiny); library(rvest)
ms_server <- function(input, output, session) {
        source("ms/ms_extract.R");
        options(shiny.trace = FALSE)

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
                mongo_file_entry(ms_db, ms$id(), tab_name)
        })

        observeEvent(ms$go_file(), {
                if (!file_record()$entry_exists) {
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
                }
        })

        #### PROTOCOLS ####
        ms_p <- callModule(module = protocols_handler,
                           id = "ms",
                           file_container = ms,
                           data_container = ms$tbl,
                           selected = selected,
                           db = ms_db)

        #### GRAPHIC LAYOUT TESTS FOR MS DATA DIPLAY ####

        observe({
                updateCheckboxGroupInput(session = session,
                                         inputId = "samples",
                                         choices = unique(ms$tbl()$Name),
                                         selected = unique(ms$tbl()$Name)[1]
                                         )
        })

        observe({
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

                } else {
                        updateCheckboxGroupInput(session = session,
                                                 inputId = "samples",
                                                 selected = stored_choices())
                }
        }, ignoreInit = TRUE)

        unaggregated_tbl <- reactive({
                if (any(is.null(c(input$samples, input$molecules)))) return()

                ms$tbl() %>%
                        select(c(Name, Molecule, Concentration)) %>%
                        group_by(Name, Molecule) %>%
                        arrange(Name) %>%
                        filter(Name %in% input$samples) %>%
                        filter(Molecule %in% input$molecules)
        })

        display_tbl <- reactive({
                if (any(is.null(c(input$samples, input$molecules)))) return()

                unaggregated_tbl() %>%
                        summarise(sd = sd(Concentration),
                                  Mean = mean(Concentration)) %>%
                        select(Name, Molecule, Mean, sd)

        })

        clicked_sample <- reactive({
                if (is.null(input$click$x)) return()
                else {
                        click_x <- input$click$x
                        n_molecules <- length(input$molecules)
                        splits <- seq(1/(2 * n_molecules), 1 - 1/(2 * n_molecules), 1/n_molecules)

                        sample_lvls <- display_tbl()$Name %>%
                                as_factor() %>%
                                levels()
                        name <- sample_lvls[round(click_x)]

                        molecule_lvls <- display_tbl()$Molecule %>%
                                as_factor() %>%
                                levels()

                        x <- click_x - round(click_x) + 1/2

                        molecule_name <- molecule_lvls[which.min(abs(splits - x))]

                        value <- display_tbl() %>%
                                filter(Molecule == molecule_name & Name == name) %>%
                                pull(Mean)

                        list(name = name,
                             molecule = molecule_name,
                             value = value)
                }
        })

        alpha_v <- reactive({
                if (is.null(clicked_sample()$value)) {
                        res <- rep(1, nrow(display_tbl()))
                } else {
                        res <- if_else(display_tbl()$Mean >= clicked_sample()$value,
                                       1,
                                       .2,
                                       missing = .2)
                }
                print(res)
                res
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
                if (is.null(display_tbl())) return()

                # alpha_vals <- alpha_v()
                # alpha_vals <- rep(1, nrow(display_tbl()))
                # alpha_vals <- 1

                ggplot(display_tbl()) +
                        aes(x = Name, y = Mean, fill = Molecule) +
                        geom_bar(position = "dodge",
                                 stat = "identity"
                                 ) +
                        geom_errorbar(position = position_dodge(.9),
                                      aes(ymax = Mean + sd,
                                          ymin = Mean - sd,
                                          width = .15)) +
                        theme(axis.text.x = element_text(angle = 60,
                                                         hjust = .8,
                                                         size = 10,
                                                         face = "bold")) +
                        scale_y_continuous(trans = barplot_scale()) +
                        scale_fill_discrete(limits = levels(ms$tbl()$Molecule))

        })

        output$table <- renderTable({
                if (input$display_raw) {
                        unaggregated_tbl()
                        }  else display_tbl()
        })

        #### WORK ON CLICK TO FILTER ####
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