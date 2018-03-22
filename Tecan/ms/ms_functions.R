
new_ms_modal <- function(ns, experiment_names, ms_edit= NULL, required_msg = NULL) {

    showModal(
        modalDialog(title = "Specify MS plate",
                    fluidPage(
                        selectInput(inputId = ns("new_ms_experiment"),
                                    label = "Select Experiment for your plate",
                                    choices = experiment_names %>% prepend(""),
                                    selected = if_exists_than_that(ms_edit$experiment)
                        ),
                        conditionalPanel(condition = paste0("input['", ns("new_ms_experiment"),"'] == 'Unitary'"),
                                         new_ms_unitary(ns, ms_edit)
                        )

                    ),
                    if (!is.null(required_msg))
                        div(tags$b(required_msg, style = "color: red;")),
                    footer = tagList(
                        actionButton(inputId = ns("new_ms_ok"),
                                     label = "Generate Sample List CSV"),
                        actionButton(inputId = ns("new_ms_cancel"),
                                     label = "Cancel")
                    ),
                    size = "l",
                    easyClose = FALSE
        )
    )
}

new_ms_unitary <- function(ns, ms_edit = NULL) {
    tagList(
        fluidRow(
            column(3,
                   conditionalPanel(condition = paste0("input['", ns("add_sample"), "'] == 0"),
                                    checkboxInput(ns("is_48_wells_plate"),
                                                  "48 wells plate?",
                                                  value = if_exists_than_that(ms_edit$is_48, FALSE))
                   )
            ),
            column(3,
                   textInput(inputId = ns("csv_note"),
                             label = "file name note",
                             placeholder = "ex: CBG_production",
                             value = if_exists_than_that(ms_edit$file_note, "")
                   )
            )
        ),
        tags$hr(id = ns("sample_bar")),
        fluidRow(
            column(2,
                   actionButton(ns("add_sample"), label = "Add Sample")
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
                          strain = NULL, plasmid = NULL, pos = NULL, group_id = NULL) {
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
                                selectizeInput(inputId = ns("pos"),
                                               label = "Well",
                                               choices = positions,
                                               selected = if_exists_than_that(pos))
                         ),
                         column(width = 2,
                                selectInput(inputId = ns("group_id"),
                                            label = "Group",
                                            choices = 1:48 %>% prepend(NA),
                                            selected = if_exists_than_that(group_id))
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
            print(ms_samples())
        })
}


sample_row_server <- function(input, output, session, ms_samples, sample_label, db, tags) {
        ns <- session$ns

        fields <- c("strain", "plasmid", "pos", "group_id", "tags")
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

insert_sample <- function(session, label, dics, available_positions, ms_samples, db, tags = NULL, ...) {

    ns <- session$ns
    insertUI(selector = paste0("#",ns("sample_bar")),
             where = "beforeBegin",
             ui = sample_row_ui(ns(paste0("sample_", label)),
                                dics$strains,
                                dics$plasmids,
                                available_positions,
                                ...)
    )

    callModule(session = session,
               module = sample_row_server,
               id = paste0("sample_", label),
               ms_samples = ms_samples,
               sample_label = label,
               db = db,
               tags = tags
               )
}

add_sample <- function(session, ms_samples, dics, available_pos, db_tags,n = 1, ref_sample = NULL) {
    force(ref_sample)

    labels <- first_unused(ms_samples()$label, n)

    positions <- available_pos() %>%
        keep(~!(. %in% ms_samples()$pos))

    for (i in 1:n) {
        label <- labels[i]
        # Add new row for the new letter
        ms_samples(
            ms_samples() %>%
                add_row(label = label)
        )

        insert_sample(session,
                      label = label,
                      dics = dics,
                      available_positions = available_pos(),
                      ms_samples = ms_samples,
                      db = db_tags,
                      pos = positions[i],
                      strain = if_exists_than_that(ref_sample$strain),
                      plasmid = if_exists_than_that(ref_sample$plasmid),
                      group_id = if_exists_than_that(ref_sample$group_id),
                      tags = if_exists_than_that(ref_sample$tags) %>%
                          str_split(pattern = ", ") %>%
                          unlist()
        )
    }
}

generate_sample_list_csv <- function(samples_tbl) {
        #The calibration files from the MS computer is synced
        #Calibration is updated from time to time, we always use the last one
        #We filter on recent date to save time
        cal_file_name <- ms_calibration_files %>%
                as_id() %>%
                drive_ls(pattern = "ipr" , q = "modifiedTime > '2018-01-12T12:00:00'") %>%
                arrange(desc(drive_resource %>% map_chr("createdTime"))) %>%
                pull(name) %>%
                first()

       analytes <- samples_tbl %>%
                arrange(str_extract(pos, "[A-Z]"),
                        as.integer(str_extract(pos, "\\d+"))
                ) %>%
                mutate(
                       FILE_NAME = paste(strain,
                                         if_else(is.na(plasmid), "", plasmid),
                                         row_number(),
                                         if_else(is.na(group_id), "", paste0("G-",group_id)),
                                         sep = "_") %>%
                               str_remove("_$"),
                       #Nb 2 is the plate number, plate #1 is ran just before and includes standards and blanks...
                       SAMPLE_LOCATION = paste0("2:", split_pos_column_row(pos)),
                       TYPE = "ANALYTE",
                       CONC_A = "",
                       SPARE_1 = if_else(is.na(group_id), "", group_id),
                       FILE_TEXT = tags
                ) %>%
                select(-(label:pos))

       map2_dfr(c("blank1","std_1", "std_10", "std_100", "std_1000", "blank2"),
                # 96 plate for now...
                generate_48_pos()[1:6],
            ~ tibble(
                      FILE_NAME = .x,
                      SAMPLE_LOCATION = paste0("1:", split_pos_column_row(.y)),
                      TYPE = if (str_detect(.x, "blank")) "BLANK" else "STANDARD",
                      CONC_A = str_extract(.x, "(?<=_)\\d+"),
                      SPARE_1 = "",
                      FILE_TEXT = ""
                      )
       ) %>%
               bind_rows(analytes) %>%
               mutate(
                       CONC_A = if_else(is.na(CONC_A), "", CONC_A),
                       ID = row_number() %>% as.character(),
                       MS_FILE = "C:\\MassLynx\\UNITARY.PRO\\ACQUDB\\General_all_20180213.exp",
                       INLET_FILE = "long_hold",
                       INJ_VOL = 3 %>% as.integer(),
                       MS_TUNE_FILE = paste0("C:\\MassLynx\\IntelliStart\\Results\\Unit Mass Resolution\\",
                                             cal_file_name)
               ) %>%
               mutate(Index = row_number()) %>%
               select(
                       Index,
                       FILE_NAME,
                       ID,
                       MS_FILE,
                       INLET_FILE,
                       SAMPLE_LOCATION,
                       INJ_VOL,
                       TYPE,
                       CONC_A,
                       MS_TUNE_FILE,
                       SPARE_1,
                       FILE_TEXT
               )
}


check_ongoing_edit <- function(folder_url, user, ms_edit) {

        file <- folder_url %>%
                as_id() %>%
                drive_ls(pattern = user)

        if (nrow(file) == 0) {
                ms_edit$is_ongoing <- FALSE
        }
        else {
                if (nrow(file) > 1) file[-1, ] %>%
                        by_row(..f = ~ drive_trash(.x))

                path <- paste0("temp/", file[1,]$name)
                drive_download(file[1,], path, overwrite = TRUE)

                ms_edit$is_ongoing <- TRUE
                ms_edit$data  <- read_csv(path)
                ms_edit$is_48  <- str_detect(file[1,]$name, "is_48")
                #captures in file name the experiment name after 'exp_' and before the next underscore
                ms_edit$experiment <- str_extract(file[1,]$name, "(?<=exp_)[A-z]*?(?=_)")
                ms_edit$file_note <- str_extract(file[1,]$name, "(?<=_note_).*(?=\\.)")
                ms_edit$drbl <- file[1,]
        }
}


save_ms_edit_as_csv_on_drive <- function(drive_url,
                                         csv_name,
                                         samples,
                                         ms_edit,
                                         local_folder = "temp/") {

        file_name <- paste0(local_folder, csv_name)
        write_csv(samples, file_name, append = FALSE)
        if (ms_edit$is_ongoing && csv_name == ms_edit$drbl$name) {
                drive_update(file = ms_edit$drbl,
                              media = file_name)
                ms_edit
        } else {
                ms_edit$drbl <- drive_upload(media = file_name,
                                              path = drive_url %>% as_id())
                ms_edit$is_ongoing <- TRUE
                ms_edit
        }
}


split_pos_column_row <- function(pos) {
        paste0(
                str_sub(pos, end = 1), ",", str_sub(pos, start = 2)
               )
}


