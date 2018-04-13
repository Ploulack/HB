c("mongo/tags.R",
  "mongo/db_values.R",
  "helpers/mongo_helpers.R",
  "search/search_functions.R",
  "helpers/ms_display/ms_display.R") %>%
    walk(source)

search_server <- function(input, output, session) {

    if (!exists("ms_db")) ms_db <- db_from_environment(session, collection = "ms")

    #Get unique searchable molecules from mongo
    mols_view <- db_from_environment(session, collection = "molecules")
    molecules <- mols_view$find('{}', '{"mols" : 1}')
    results <- reactiveValues(
        tbl = tibble(),
        with_samples = tibble()
    )
    rm(mols_view)

    #Fill the search molecules input
    observe({
        updateSelectInput(session = session,
                          inputId = "search_molecules",
                          choices = molecules$mols %>%
                              unlist() %>%
                              keep(~. != "") %>%
                              prepend("")
        )
    })


    observeEvent(input$search_go, {
        validate(need(input$search_molecules != "", message = "Must select at least a molecule"))

        res <- search_mol_by_min_conc(db = ms_db,
                                      molecule = input$search_molecules,
                                      min_conc = input$min_concentration)

        if (nrow(res) == 0) return(tibble(count = 0))

        results$tbl <- res %>%
            with(., tibble(
                strain = `_id`$strain,
                date_created = `_id`$date_created %||% NA,
                xml = `_id`$xml,
                count = count,
                mean = mean))
    })

    display_tbl <- reactive({
        tbl <- results$tbl
        if ( is.null(tbl)) return()
        results$tbl
    })

    output$search_results <- renderTable({
        display_tbl()
    })

    totals_tbl <- reactive({
        if (is.null(display_tbl()) || nrow(display_tbl()) == 0) return()
        display_tbl() %>%
            summarise(`Total number of samples` = sum(count),
                      `Nb of different strains` = unique(strain) %>% length())
    })

    output$totals <- renderTable({
        totals_tbl()
    })

    output$total_samples <- reactive({
        if (is.null(totals_tbl()) || nrow(totals_tbl()) == 0) return(101)
        totals_tbl() %>%
            pull(1)
    })
    outputOptions(output, "total_samples", suspendWhenHidden = FALSE)

    observeEvent(input$display_samples, {
        res <- search_mol_by_min_conc(db = ms_db,
                                      molecule = input$search_molecules,
                                      min_conc = input$min_concentration,
                                      with_samples = TRUE)
        results$with_samples <- bind_cols(
            res %>%
                select(-data),
            res$data
            ) %>%
            as_tibble()
    }, priority = 10)

    callModule(module = ms_data_display_server,
               id = "search",
               session = session,
               go_button = reactive(input$display_samples),
               ms_tbl = reactive(results$with_samples),
               type = "search")
}