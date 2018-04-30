
ms_data_display_ui_sidebar <- function(id) {
    ns <- NS(id)
    source(file = "helpers/delete_file_button_module.R")
    source("helpers/ui_generics/select_file_ui.R")

    tagList(

        checkboxGroupInput(inputId = ns("molecules"),
                           label = "Molecules",
                           choices = "Waiting for server..."),
        tags$hr(),
        checkboxInput(inputId = ns("select_all"),
                      label = "Select samples with a reading",
                      value = FALSE),
        uiOutput(ns("blanks_option")),
        actionButton(inputId = ns("unselect"),
                     label = "Unselect"),
        tags$hr(),
        checkboxGroupInput(inputId = ns("samples"),
                           label = "Samples",
                           choices = "Waiting for server..."
        )
    )
}

ms_data_display_ui_main <- function(id) {
    ns <- NS(id)
    tagList(
        htmlOutput(outputId = ns("x_value")),
        checkboxInput(ns("log_scale"),
                      label = "Switch to log scale"),
        plotOutput(ns("bar"),
                   click = ns("click")
                   # hover = hoverOpts(id = ns("hover"),
                   #                   delayType = "debounce", delay = 300)
        ),
        fluidRow(
            column(3,checkboxInput(inputId = ns("display_raw"),
                                   label = "Display unaggregated data")
            ),
            column(3, downloadLink(outputId = ns("save_csv"),
                                   label = "Download data as csv")
            )
        ),
        tableOutput(ns("table"))
    )
}

ms_data_display_server <- function(input, output, session,
                                   go_button,
                                   ms_tbl,
                                   type = c("ms", "search")) {
ns <- session$ns
    stored_choices <- reactiveVal(NULL)
    display_tbl <- reactiveVal()
    last_click <- reactiveVal(NULL)

    output$blanks_option <- renderUI({
        if (type == "ms") {
        checkboxInput(inputId = ns("show_blanks"),
                      label = "Display blanks",
                      value = FALSE)
        }
    })

    ms_data <- eventReactive(c(go_button(), input$show_blanks), {
        if (is.null(ms_tbl())) return()
browser()
        if (type == "search" || (input$show_blanks %||% FALSE)) {
            ms_tbl()
        }
        else {
            ms_tbl() %>% filter(type == "Analyte")
        }
    })

    observeEvent(c(go_button(), input$show_blanks), {

        if (is.null(ms_data())) return()
        #Reset stored choices
        stored_choices(NULL)

        #Reset the select all button
        updateCheckboxInput(session, "select_all", value = FALSE)

        updateCheckboxGroupInput(session = session,
                                 inputId = "samples",
                                 choices = unique(ms_data()$Name),
                                 selected = unique(ms_data()$Name)[1]
        )

        molecules <- unique(ms_data()$Molecule)
        updateCheckboxGroupInput(session = session,
                                 inputId = "molecules",
                                 choices = molecules,
                                 selected = molecules
        )
    })

    observeEvent(input$select_all, {

        if (input$select_all) {
            stored_choices(input$samples)

            non_0_conc_choices <- ms_data() %>%
                filter(Molecule %in% input$molecules,
                       Concentration > 0) %>%
                pull(Name) %>%
                unique()

            updateCheckboxGroupInput(session = session,
                                     inputId = "samples",
                                     selected = non_0_conc_choices)

        } else if (!is.null(stored_choices())) {
            updateCheckboxGroupInput(session = session,
                                     inputId = "samples",
                                     selected = stored_choices())
        }
    }, ignoreInit = TRUE)

    unaggregated_tbl <- reactive({
        if (any(is.null(c(input$samples, input$molecules)))) return()

        res_tbl <- ms_data() %>%
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

    observeEvent(input$click, {
        if (!is.null(input$click)) last_click(input$click)
    })

    #On file change reset the value selection from click
    observeEvent(go_button(), {
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
            scale_fill_discrete(limits = levels(ms_data()$Molecule)) +
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

    output$save_csv <- downloadHandler(
        filename = function() {
            if (type == "ms")
                paste0(ms$file_dribble()$name, ".csv")
            else
                paste0("search export ", Sys.time() %>% force_tz("America/Montreal"), ".csv")
        },
        content = function(file) {
            write_csv(
                ms_data() %>%
                    select(Name, Molecule, Concentration),
                file,
                na = "0")
        }
    )
}
