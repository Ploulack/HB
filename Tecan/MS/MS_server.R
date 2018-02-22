library(shiny); library(readxl)
ms_server <- function(input, output, session) {
        options(shiny.trace = FALSE)
        #### INIT & FILE HANDLING ####
        # source("helpers/ui_generics/select_file_server.R")
        # source("helpers/general.R")
        # selected <- reactiveVal()
        # ms_folder <- get_drive_url(session, "ms")
        # ms_progress <- shiny::Progress$new()
        #
        # ms <- callModule(module = select_file,
        #         id = "files",
        #         progress = ms_progress,
        #         drive_url = ms_folder,
        #         selected = selected
        # )
        # output$id <- renderText({
        #         ms$id()
        # })
        #
        # output$protocol <- renderTable(ms$protocols())


        #### GRAPHIC LAYOUT TESTS FOR MS DATA DIPLAY ####
        molecules <- c("Norcoclaurine", "Dopamine")
        ms_tbl <- map(molecules, ~ read_xlsx(sheet = .x ,
                                             path = "ms/NCS_Exp12-Selected NCS-96h.xlsx",
                                             skip = 1,) %>%
                              filter(!str_detect(Name, pattern = "blank|CAL"))) %>%
                purrr::set_names(molecules) %>%
                map_dfr(~., .id = "Molecule") %>%
                rename(Concentration = `Final Conc.`)


        observe({
                updateCheckboxGroupInput(session = session,
                                         inputId = "samples",
                                         choices = unique(ms_tbl$Name),
                                         selected = unique(ms_tbl$Name)[1]
                                         )
        })

        observe({
                updateCheckboxGroupInput(session = session,
                                         inputId = "molecules",
                                         choices = molecules,
                                         selected = molecules)
        })

        stored_choices <- reactiveVal(NULL)

        observeEvent(input$select_all, {
                if (input$select_all) {
                        stored_choices(input$samples)
                        updateCheckboxGroupInput(session = session,
                                                 inputId = "samples",
                                                 selected = unique(ms_tbl$Name))

                } else {
                        updateCheckboxGroupInput(session = session,
                                                 inputId = "samples",
                                                 selected = stored_choices())
                }
        })


        display_tbl <- reactive({
                if (any(is.null(c(input$samples, input$molecules)))) return()

                res <- ms_tbl %>%
                        select(c(Name, Concentration, Molecule)) %>%
                        group_by(Name, Molecule) %>%
                        arrange(Name) %>%
                        filter(Name %in% input$samples) %>%
                        filter(Molecule %in% input$molecules) %>%
                        summarise(sd = sd(Concentration),
                                  Mean = mean(Concentration)) %>%
                        select(Name, Molecule, Mean, sd)
                res
        })

        unaggregated_tbl <- reactive({
                if (any(is.null(c(input$samples, input$molecules)))) return()

                res <- ms_tbl %>%
                        select(c(Name, Molecule, Concentration)) %>%
                        group_by(Name, Molecule) %>%
                        arrange(Name) %>%
                        filter(Name %in% input$samples) %>%
                        filter(Molecule %in% input$molecules)

                res

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
                                                         face = "bold"))

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
                        browser()
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