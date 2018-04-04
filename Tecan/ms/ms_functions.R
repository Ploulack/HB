library(rlang)
new_ms_modal <- function(ns, experiment_names, experiment = NULL, required_msg = NULL) {

    showModal(
        modalDialog(title = "Specify MS plate",
                    fluidPage(
                        selectInput(inputId = ns("new_ms_experiment"),
                                    label = "Experiment",
                                    choices = experiment_names %>% prepend(""),
                                    selected = if_exists_than_that(experiment)
                        ),
                        tabsetPanel(id = ns("new_ms_tabs")
                        )
                    ),
                    footer = tagList(
                        conditionalPanel(
                            paste0("input['", ns("new_ms_experiment"),"'] != ''"),
                            tagList(
                                textInput(ns("file_note"),NULL, placeholder = "Will go into csv name..."),
                                actionButton(inputId = ns("new_ms_plate"),
                                             label = "Add plate"),
                                actionButton(ns("use_plate_1"),
                                             "Use Plate 1"),
                                actionButton(inputId = ns("new_ms_ok"),
                                             label = "Generate Sample List CSV")
                            )
                        ),
                        actionButton(inputId = ns("new_ms_cancel"),
                                     label = "Cancel")
                    ),
                    size = "l",
                    easyClose = FALSE
        )
    )
}

new_ms_tab <- function(ns, current_tab, is_48 = FALSE, plate_note = NULL, url = NULL, required_msg = NULL) {

    tabPanel(title = current_tab,
             fluidRow(
                 column(3,
                        textInput(inputId = ns(paste0(current_tab, "_", "url")),
                                  label = "Add Benchling URL to CSV",
                                  value = url,
                                  placeholder = "Paste your url here...")
                 ),
                 column(3,
                        textInput(inputId = ns(paste0(current_tab, "_", "plate_note")),
                                  label = "Plate note",
                                  placeholder = "ex: Sho & Alex mix",
                                  value = plate_note,
                                  width = '100%'
                        )
                 ),
                 conditionalPanel(condition = paste0("input['", ns("new_ms_experiment"),"'] == 'Unitary' ", "&&",
                                                     " input['", ns("new_ms_tabs"), "'] != 'Plate_1'"),
                                  column(2,
                                         conditionalPanel(paste0("input['", ns(paste0(current_tab, "_","add_sample")), "'] == 0"),
                                                          checkboxInput(ns(paste0(current_tab, "_", "is_48")),
                                                                        "48 wells plate?",
                                                                        value = is_48
                                                          )
                                         )
                                  )
                 )
             ),
             conditionalPanel(condition = paste0("input['", ns("new_ms_experiment"),"'] == 'Unitary'"),
                              new_ms_unitary(ns, current_tab)
             ),
             if (!is.null(required_msg))
                 div(tags$b(required_msg, style = "color: red;"))
    )
}


new_ms_unitary <- function(ns, current_tab) {

    tagList(
        tags$hr(id = ns(paste0(current_tab, "_", "sample_bar"))),
        fluidRow(
            column(2,
                   actionButton(ns(paste0(current_tab, "_", "add_sample")),
                                "Add Sample")
            ),
            column(2,
                   numericInput(inputId = ns(paste0(current_tab, "_","n_copy")),
                                label = "Nb of Copies", value = 1, min = 1, max = 20, step = 1)
            ),
            column(2,
                   actionButton(ns(paste0(current_tab, "_","copy")),
                                paste0("Duplicate N times"))
            )
        )
    )
}

sample_row_ui <- function(id, strains, plasmids, positions, sample, pos
                          # strain = NULL, plasmid = NULL, identifier = NULL, pos = NULL, group_id = NULL
                          ) {
    ns <- NS(id)
    tags$div(id = ns("sample_row"),
             fluidRow(
                 column(2, selectizeInput(inputId = ns("strain"),
                                          label = "Strain",
                                          choices = strains,
                                          selected = if_exists_than_that(sample$strain))
                 ),
                 column(2,
                        selectizeInput(inputId = ns("plasmid"),
                                       label = "Plasmid",
                                       choices = plasmids %>% prepend(NA),
                                       selected = if_exists_than_that(sample$plasmid))
                 ),
                 column(2,
                        textInput(inputId = ns("identifier"),
                                  label = "Identifier",
                                  value = if_exists_than_that(sample$identifier))
                 ),
                 column(1,
                        selectizeInput(inputId = ns("pos"),
                                       label = "Well",
                                       choices = positions,
                                       selected = if_exists_than_that(pos))
                 ),
                 column(width = 1,
                        selectInput(inputId = ns("group_id"),
                                    label = "Group",
                                    choices = 1:48 %>% prepend(NA),
                                    selected = if_exists_than_that(sample$group_id)
                        )
                 ),
                 column(width = 3,
                        uiOutput(ns("tags_widget"))
                 ),
                 column(width = 1,
                        actionButton(inputId = ns("delete_sample"),
                                     label = "Delete")
                 )
             )
    )
}

update_from_input <- function(var_name, ms_samples, input, sample_label) {
    force(var_name)

    row <- reactive({
        which(ms_samples()$label == sample_label)
    })

    observeEvent(input[[var_name]], {
        # Concactenating the various tags into a single string
        value <- if (length(input[[var_name]]) > 1) {
            str_c(input[[var_name]], collapse = ", ")
        } else if (input[[var_name]] == "NA") {
            NA
        } else {
            input[[var_name]]
        }
        samples <- ms_samples()
        samples[[var_name]][row()] <- value
        ms_samples(samples)
    })
}

update_tbl_from_plate_info <- function(var_name, plate_name, ms_samples, input) {
    force(var_name)

    observeEvent(input[[paste0(plate_name, "_", var_name)]], {
        if (nrow(ms_samples()) == 0) return()
        samples <- ms_samples()
        value <- if_exists_than_that(input[[paste0(plate_name, "_", var_name)]], "")

        samples <- samples %>%
            mutate(!!var_name := if_else(plate == plate_name, value, samples[[var_name]]))

        ms_samples(
            samples
        )
    })
}

sample_row_server <- function(input, output, session, ms_samples, sample_label, db, tags) {
    ns <- session$ns

    fields <- c("strain", "plasmid", "identifier", "pos", "group_id", "tags")
    output$tags_widget <- renderUI({
        selectizeInput(inputId = ns("tags"),
                       label = "Tags",
                       choices = tags_retrieve(db),
                       multiple = TRUE,
                       selected = if_exists_than_that(tags),
                       options = list(create = 'true')
        )
    })

    walk(
        fields,
        ~ update_from_input(., ms_samples, input, sample_label)
    )

    observeEvent(input$tags,{
        validate(need(!is.null(input$tags), message = FALSE))

        tags <- input$tags %>%
            map_chr(~ str_interp('"${.}"'))

        tags <- str_interp('${str_c(tags, collapse = ", ")}')

        tags_add(db = db, tags = tags)
    })

    #Delete a part's UI and parts() row
    observeEvent(input$delete_sample, {
        removeUI(selector = paste0("#", ns("sample_row")))
        isolate(
            ms_samples(
                ms_samples() %>%
                    filter(label != sample_label))
        )
    })
}

insert_sample <- function(session, label,
                          dics, available_positions, ms_samples, db,
                          current_tab = NULL, sample = NULL, pos = NULL) {

    ns <- session$ns
    tab <- if_exists_than_that(current_tab, sample$plate)

    insertUI(selector = paste0("#",ns(paste0(tab, "_", "sample_bar"))),
             where = "beforeBegin",
             ui = sample_row_ui(ns(paste0(tab, "_", "sample_", label)),
                                dics$strains,
                                dics$plasmids,
                                available_positions,
                                sample,
                                if_exists_than_that(pos, sample$pos)
                                )
    )

    callModule(session = session,
               module = sample_row_server,
               id = paste0(tab, "_", "sample_", label),
               ms_samples = ms_samples,
               sample_label = label,
               db = db,
               tags = if_exists_than_that(sample$tags) %>%
                   str_split(pattern = ", ") %>%
                   unlist()
    )
}

add_sample <- function(session, input, ms_samples, dics, positions, db_tags, current_tab = NULL, n = 1, ref_sample = NULL) {
    force(ref_sample)

    current_tab <- current_tab %||% input$new_ms_experiment
    labels <- first_unused(ms_samples()$label, n)

    available_positions <- positions %>%
        keep(~!(. %in% ms_samples()$pos))


    for (i in 1:n) {
        label <- labels[i]
        # Add new row for the new letter
        ms_samples(
            ms_samples() %>%
                add_row(label = label,
                        plate = current_tab,
                        url = input[[paste0(current_tab, "_", "url")]],
                        plate_note = input[[paste0(current_tab, "_", "plate_note")]],
                        is_48 = input[[paste0(current_tab, "_", "is_48")]]
                )
        )

        insert_sample(session,
                      label = label,
                      dics = dics,
                      available_positions = positions,
                      ms_samples = ms_samples,
                      db = db_tags,
                      current_tab = current_tab,
                      sample = if_exists_than_that(ref_sample),
                      pos = available_positions[i]
        )
    }
}

add_sample_observers <- function(input_name, session, input, ms_samples, plate, ms_edit, dics, db_tags, csv_name) {

    force(input_name)
    observeEvent(input[[paste0(plate, input_name)]], {

        force(plate); force(input_name)

        if (ms_samples() %>% nrow() > 0) {
            ms_edit <- save_ms_edit_as_csv_on_drive(drive_url = get_drive_url(session, "ms_ongoing_edit"),
                                                    csv_name = csv_name(),
                                                    samples = ms_samples(),
                                                    ms_edit = ms_edit)
        }

        positions <- {
            if (input[[paste0(plate, "_", "is_48")]])
                generate_48_pos()
            else
                generate_96_pos()
        }

        if (plate == "Plate_1") positions <- positions[-(1:6)]

        if (input_name == "_add_sample") {
            add_sample(session, input, ms_samples, dics, positions, db_tags, current_tab = plate)
        } else {
            add_sample(session, input, ms_samples, dics, positions, db_tags, current_tab = plate,
                       n = input[[paste0(plate,"_n_copy")]],
                       ref_sample = ms_samples() %>%
                           filter(plate == plate) %>%
                           slice(n())
            )
        }
    },)
}

start_plate_observers <- function(session, input, ms_samples, plate, ms_edit, dics, db_tags, csv_name) {

    walk(
        c("_add_sample", "_copy"),
        ~ add_sample_observers(input_name = ., session, input, ms_samples, plate, ms_edit, dics, db_tags, csv_name)
    )

    walk(c("plate_note", "url", "is_48"),
         ~ update_tbl_from_plate_info(var_name = .,
                                      plate_name = plate,
                                      ms_samples = ms_samples,
                                      input = input
         )
    )

}

reset_ms_edit <- function(ms_samples, tab_plates) {
    ms_samples(
        ms_samples() %>%
            slice(0)
    )
    tab_plates("")
    removeModal()
}

add_plate <- function(ns, session, input, tab_plates, new_plate, ms_samples, ms_edit, dics, db_tags, csv_name) {

    tab_plates(append(tab_plates(), new_plate))

        appendTab(inputId = "new_ms_tabs",
                  tab = new_ms_tab(ns,
                                   current_tab = new_plate,
                                   is_48 = if (new_plate == "Plate_1") TRUE else FALSE,
                                   required_msg = NULL),
                  select = TRUE)

        start_plate_observers(session = session, input = input,
                              ms_samples = ms_samples, plate = new_plate,
                              ms_edit = ms_edit, dics = dics, db_tags = db_tags, csv_name = csv_name)
}
