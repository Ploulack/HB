library(rlang)
new_ms_modal <- function(ns, experiment_names, ms_edit= NULL, required_msg = NULL) {

    showModal(
        modalDialog(title = "Specify MS plate",
                    tabsetPanel(id = ns("new_ms_tabs")
                    ),
                    footer = tagList(
                        actionButton(inputId = ns("new_ms_plate"),
                                     label = "Add plate"),
                        actionButton(inputId = ns("new_ms_ok"),
                                     label = "Generate Sample List CSV"),
                        textInput(ns("file_note"),NULL, placeholder = "Will go into csv name..."),
                        actionButton(inputId = ns("new_ms_cancel"),
                                     label = "Cancel")
                    ),
                    size = "l",
                    easyClose = FALSE
        )
    )
}

new_ms_tab <- function(ns, title, experiment_names, ms_edit= NULL, required_msg = NULL) {
    tabPanel(title = title,
        fluidPage(
            fluidRow(
                column(3,
                       selectInput(inputId = ns(paste0(title, "_", "new_ms_experiment")),
                                   label = "Experiment",
                                   choices = experiment_names %>% prepend(""),
                                   selected = if_exists_than_that(ms_edit$experiment)
                       )
                ),
                column(3,
                       textInput(inputId = ns(paste0(title, "_", "url")),
                                 label = "Add Benchling URL to CSV",
                                 placeholder = "Paste your url here...")
                )
            ),
            conditionalPanel(condition = paste0("input['", ns(paste0(title, "_", "new_ms_experiment")),"'] == 'Unitary'"),
                             new_ms_unitary(ns, title, ms_edit)
            )
        ),
        if (!is.null(required_msg))
            div(tags$b(required_msg, style = "color: red;"))
    )
}


new_ms_unitary <- function(ns, current_tab, ms_edit = NULL) {

    tagList(
        fluidRow(
            column(3,
                   textInput(inputId = ns(paste0(current_tab, "_", "csv_note")),
                             label = "Plate note",
                             placeholder = "ex: Sho & Alex mix",
                             value = if_exists_than_that(ms_edit$file_note, ""),
                             width = '100%'
                   )
            ),
            column(2,
                   conditionalPanel(condition = paste0("input['", ns(paste0(current_tab, "_","add_sample")), "'] == 0"),
                                    checkboxInput(ns(paste0(current_tab, "_", "is_48_wells_plate")),
                                                  "48 wells plate?",
                                                  value = if_exists_than_that(ms_edit$is_48, FALSE))
                   )
            )
        ),
        tags$hr(id = ns(paste0(current_tab, "_", "sample_bar"))),
        fluidRow(
            column(2,
                   actionButton(ns(paste0(current_tab, "_", "add_sample")), label = "Add Sample")
            ),
            column(2,
                   numericInput(inputId = ns("n_copy"),
                                label = "Nb of Copies", value = 1, min = 1, max = 20, step = 1)
            ),
            column(2,
                   actionButton(ns("copy"),
                                paste0("Duplicate N times"))
            )
        )
    )
}

sample_row_ui <- function(id, strains, plasmids, positions,
                          strain = NULL, plasmid = NULL, identifier = NULL, pos = NULL, group_id = NULL) {
    ns <- NS(id)
    tags$div(id = ns("sample_row"),
             fluidRow(
                 column(2, selectizeInput(inputId = ns("strain"),
                                          label = "Strain",
                                          choices = strains,
                                          selected = if_exists_than_that(strain))
                 ),
                 column(2,
                        selectizeInput(inputId = ns("plasmid"),
                                       label = "Plasmid",
                                       choices = plasmids %>% prepend(NA),
                                       selected = if_exists_than_that(plasmid))
                 ),
                 column(2,
                        textInput(inputId = ns("identifier"),
                                  label = "Identifier",
                                  value = if_exists_than_that(identifier))
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
                                    selected = if_exists_than_that(group_id)
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

insert_sample <- function(session, label, dics, available_positions, ms_samples, db, current_tab, tags = NULL, ...) {

    ns <- session$ns
    insertUI(selector = paste0("#",ns(paste0(current_tab, "_", "sample_bar"))),
             where = "beforeBegin",
             ui = sample_row_ui(ns(paste0(current_tab, "_", "sample_", label)),
                                dics$strains,
                                dics$plasmids,
                                available_positions,
                                ...)
    )

    callModule(session = session,
               module = sample_row_server,
               id = paste0(current_tab, "_", "sample_", label),
               ms_samples = ms_samples,
               sample_label = label,
               db = db,
               tags = tags
    )
}

add_sample <- function(session, input, ms_samples, dics, available_pos, db_tags, current_tab, n = 1, ref_sample = NULL) {
    force(ref_sample)

    labels <- first_unused(ms_samples()$label, n)

    positions <- available_pos() %>%
        keep(~!(. %in% ms_samples()$pos))

    for (i in 1:n) {
        label <- labels[i]
        # Add new row for the new letter
        ms_samples(
            ms_samples() %>%
                add_row(label = label,
                        plate = current_tab,
                        url = input[[paste0(current_tab, "_", "url")]],
                        csv_note = input[[paste0(current_tab, "_", "csv_note")]]
                        )
        )

        insert_sample(session,
                      label = label,
                      dics = dics,
                      available_positions = available_pos(),
                      ms_samples = ms_samples,
                      db = db_tags,
                      current_tab = current_tab,
                      pos = positions[i],
                      strain = if_exists_than_that(ref_sample$strain),
                      plasmid = if_exists_than_that(ref_sample$plasmid),
                      identifier = if_exists_than_that(ref_sample$identifier),
                      group_id = if_exists_than_that(ref_sample$group_id),
                      tags = if_exists_than_that(ref_sample$tags) %>%
                          str_split(pattern = ", ") %>%
                          unlist()
        )
    }
}




