c("mongo/tags.R",
    "mongo/db_values.R",
    "helpers/mongo_helpers.R",
    "search/search_functions.R",
    "helpers/ms_display/ms_display.R") %>%
    walk(source)

search_server <- function(input, output, session) {
    ns <- session$ns
    if (!exists("ms_db")) ms_db <- db_from_environment(session, collection = "ms")

    if (!exists("db_tags_view")) db_tags_view <- db_from_environment(session, "tags_view")

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

    output$tags_widget <- renderUI({
        selectizeInput(inputId = ns("tags"),
            label = "Tags",
            choices = db_tags_view$find('{}') %>%
                flatten() %>%
                flatten_chr(),
            multiple = TRUE
            # selected = if_exists_than_that(tags),

        )
    })

    observeEvent(input$search_go, {
        validate(need(input$search_molecules != "", message = "Must select at least a molecule"))

        res <- search_mol_by_min_conc(db = ms_db,
            molecule = input$search_molecules,
            min_conc = input$min_concentration,
            max_conc = input$max_concentration,
            tags = input$tags)

        if (nrow(res) == 0) {
            results$tbl <- tibble(strain = NA, date_created = NA, xml = NA, mean = NA,
                count = 0
            )
        } else {
            results$tbl <- res %>%
                with(., tibble(
                    strain = `_id`$strain %||% NA,
                    date_created = `_id`$date_created %||% NA  %>%
                        force_tz("America/Montreal") %>%
                        lubridate::date() %>%
                        as.character(),
                    xml = `_id`$xml,
                    count = count,
                    mean = mean))
        }
    })

    output$search_results <- renderTable({
        results$tbl
    })

    totals_tbl <- reactive({
        if (is.null(results$tbl) || nrow(results$tbl) == 0) return()

        results$tbl %>%
            summarise(
                `Total number of samples` = sum(count),
                `Nb of different strains` =  if (unique(strain) %>% is.na()) 0 else unique(strain) %>% length()
            )
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

        res <- search_mol_by_min_conc(
            db = ms_db,
            molecule = input$search_molecules,
            min_conc = input$min_concentration,
            max_conc = input$max_concentration,
            tags = input$tags,
            with_samples = TRUE
            )

        results$with_samples <- bind_cols(
            select(res, -data),
            res$data) %>%
            as_tibble()

    },priority = 10)

    callModule(module = ms_data_display_server,
        id = "search",
        session = session,
        go_button = reactive(input$display_samples),
        ms_tbl = reactive(results$with_samples),
        type = "search")
}