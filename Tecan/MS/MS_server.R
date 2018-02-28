library(shiny); library(rvest)
ms_server <- function(input, output, session) {
        source("ms/ms_extract.R");
        options(shiny.trace = FALSE)
        #### INIT & FILE HANDLING ####
        source("helpers/ui_generics/select_file_server.R")
        source("helpers/general.R")
        selected <- reactiveVal()
        ms_folder <- get_drive_url(session, "ms")
        ms_progress <- shiny::Progress$new()

        ms <- callModule(module = select_file,
                id = "files",
                progress = ms_progress,
                drive_url = ms_folder,
                selected = selected
        )

        output$protocol <- renderTable(ms$protocols())

        ms$tbl <- reactiveVal(NULL)

        obtain_file_data(
                ms$go_file,
                "MS",
                file_container = ms,
                dat_container = ms$tbl,
                extract_function = extract_ms
        )

        # ms$tbl() <- extract_ms("ms/msfiles/quandata_olivetolscreening_20190220.xml") %>%
        #         filter(type == "Analyte")

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


        barplot_scale <- reactive({
                ifelse(input$log_scale, "log1p", "identity")
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

        output$bar <- renderPlot({
                if (is.null(display_tbl())) return()

                ggplot(display_tbl()) +
                        aes(x = Name, y = Mean, fill = Molecule) +
                        geom_bar(position = "dodge", stat = "identity")  +
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
                if (is.null(input$click$x)) return("")
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

                        HTML("You've selected sample <code>", name, "</code>",
                             "<br><br>And molecule <code>", molecule_name,"</code>")
                }
        })
}