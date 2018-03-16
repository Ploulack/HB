
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
                                                 label = "Generate Sample List CSV")
                            ),
                            size = "l",
                            easyClose = TRUE
                )
        )
}

new_ms_unitary <- function(ns, ms_edit = NULL) {
        tagList(
                fluidRow(
                        column(3,
                               checkboxInput(ns("is_48_wells_plate"),
                                             "48 wells plate?",
                                             value = if_exists_than_that(ms_edit$is_48, FALSE))
                        ),
                        column(3,
                               textInput(inputId = ns("csv_note"),
                                         label = "file name note",
                                         placeholder = "ex: CBG_production",
                                         value = if_exists_than_that(ms_edit$file_note, "")
                                        )
                        )
                ),
                actionButton(ns("add_sample"), label = "Add Sample"),
                conditionalPanel(condition = paste0("output['", ns("has_elements"), "'] == true"),
                                 actionButton(inputId = ns("create_files"),
                                              label = "Generate"))
        )
}

sample_row_ui <- function(id, strains, plasmids, positions,
                          strain = NULL, plasmid = NULL, pos = NULL, group_id = NULL) {
        ns <- NS(id)
        tags$div(id = ns("sample_row"),
                 fluidRow(
                         column(2, selectizeInput(inputId = ns("strain"),
                                                  label = "Select Strain",
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
                                               label = "Plate pos",
                                               choices = positions,
                                               selected = if_exists_than_that(pos))
                         ),
                         column(width = 2,
                                selectInput(inputId = ns("group_id"),
                                            label = "Tag Group",
                                            choices = 1:48 %>% prepend(NA),
                                            selected = if_exists_than_that(group_id))
                         ),
                         column(width = 1,
                                actionButton(inputId = ns("delete_sample"),
                                             label = "Delete")
                         )
                 ),
                 fluidRow(
                         tableOutput(outputId = ns("info")),
                         tags$hr()
                 ))
}

update_from_input <- function(var_name, ms_samples, input, sample_label) {
        force(var_name)

        row <- reactive({
                which(ms_samples()$label == sample_label)
        })

        observeEvent(input[[var_name]], {
                samples <- ms_samples()
                samples[[var_name]][row()] <- ifelse(input[[var_name]] == "NA", NA, input[[var_name]])
                ms_samples(samples)
                print(ms_samples())
        }, ignoreInit = TRUE)
}

sample_row_server <- function(input, output, session, ms_samples, sample_label) {
        ns <- session$ns

        fields <- c("strain", "plasmid", "pos", "group_id")

        walk(
                fields,
                ~ update_from_input(., ms_samples, input, sample_label)
        )


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

        samples_tbl %>%
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
                       ID = row_number() %>% as.character(),
                       MS_FILE = "C:\\MassLynx\\UNITARY.PRO\\ACQUDB\\General_all_20180213.exp",
                       INLET_FILE = "long_hold",
                       #Nb 2 is the plate number, plate #1 is ran just before and includes standards and blanks...
                       SAMPLE_LOCATION = paste0("2:",str_sub(pos, end = 1), ",", str_sub(pos, start = 2)),
                       INJ_VOL = 3,
                       TYPE = "ANALYTE",
                       CONC_A = "",
                       MS_TUNE_FILE = paste0("C:\\MassLynx\\IntelliStart\\Results\\Unit Mass Resolution\\",
                                             cal_file_name),
                       SPARE_1 = if_else(is.na(group_id), "", group_id)
                ) %>%
                select(-(label:pos))
}

check_ongoing_edit <- function(folder_url, user) {

        file <- folder_url %>%
                as_id() %>%
                drive_ls(pattern = user)

        if (nrow(file) == 0) list(is_ongoing = FALSE)
        else {
                if (nrow(file) > 1) file[-1, ] %>%
                        by_row(..f = ~ drive_trash(.x))

                path <- paste0("temp/", file[1,]$name)
                drive_download(file[1,], path, overwrite = TRUE)
                list(is_ongoing = TRUE,
                     data = read_csv(path),
                     is_48 = str_detect(file[1,]$name, "is_48"),
                     #captures in file name the experiment name after 'exp_' and before the next underscore
                     experiment = str_extract(file[1,]$name, "(?<=exp_)[A-z]*?(?=_)"),
                     file_note = str_extract(file[1,]$name, "(?<=_note_).*(?=\\.)"),
                     drbl = file[1,])
                }
}

add_sample <- function(ns, label, dics, available_positions, ms_samples, ...) {
        browser()
        insertUI(selector = paste0("#",ns("add_sample")),
                 where = "beforeBegin",
                 ui = sample_row_ui(ns(paste0("sample_", label)),
                                    dics$strains,
                                    dics$plasmids,
                                    available_positions,
                                    ...)
        )

        callModule(module = sample_row_server,
                   id = paste0("sample_", label),
                   ms_samples = ms_samples,
                   sample_label = label)
}

# available_positions <- function(current_position, used_positions, is_48) {
#
#         used_positions_without_current <- str_match_all(used_positions, current_position) %>%
#                 unlist()
#
#         str_match_all(used_positions, current_position) %>%
#                 unlist()
#         if (is_48) {
#         } else {
#                 generate_96_pos() %>%
#                         keep(~! (. %in% used_positions_without_current ))
#         }
# }

save_ms_edit_as_csv_on_drive <- function(drive_url,
                                         csv_name,
                                         samples,
                                         ms_edit,
                                         local_folder = "temp/") {
browser()
        file_name <- paste0(local_folder, csv_name)
        write_csv(samples, file_name, append = FALSE)
        if (ms_edit$is_ongoing && csv_name == ms_edit$drbl$name) {
                drive_update(file = ms_edit$drbl,
                              media = file_name)
        } else {
                ms_edit$drbl <<- drive_upload(media = file_name,
                                              path = drive_url %>% as_id())
                ms_edit$is_ongoing <<- TRUE
        }
}
