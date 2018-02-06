# éléments à sortir du module, examples:
input$file
tecan_file$file_dribble <-  tecan$files %>% filter(id == input$file)
experiment$raw <- tecan_extract(input$file, tecan$files) # Container...

# le selected file qui a été mis sur le tecan$ (et donc serait maintenant le 'container')

update_uis <- function(prot_name, tecan, file_id, session) {
        # Switch UI to the protocol where this file was moved to
        updateSelectInput(session = session,
                          inputId = "protocol",
                          selected = prot_name)

        #Set which file to select after the files menu update
        tecan$selected_file <- file_id
}

# protocols and the selected protocol, SUPER IMPORTANT

#Conclusion: merge tecan_file et tecan...sortir un objet générique qui contiennent les infos nécessaires
#
#Avant effacement de Tecan-server

#build the protocol tibble from the spread sheet
if (!exists("protocols")) {
        tecan_progress$inc(.1, detail = "Accessing experiments.")
        source("protocols/protocols_functions.R")
        prot_gsheet <- {if (is_dev_server(session)) gs_url(protocols_sheet_dev)
                else gs_url(protocols_sheet_prod)}
        protocols <- reactiveVal(protocols_get(drive_tecanURL, prot_gsheet, session = session))
}

# Update the Protocol select input
updateSelectInput(session,
                  inputId = "protocol",
                  choices = "New" %>% append(protocols()$name),
                  selected = "New")

#On change of selected protocol, update file list from drive
observeEvent(input$protocol, {
        if (input$protocol == "New")
                drive_url <- drive_tecanURL
        else
                drive_url <- protocols() %>%
                        filter(name == input$protocol) %$%
                        folder_url
        tecan$files <- drive_url %>%
                googledrive::as_id() %>%
                get_ordered_filenames_from_drive()
}, ignoreInit = TRUE)


#Create and fill the tecan_file all inclusive reactive container
tecan_file <- reactiveValues()
observeEvent(experiment$raw, {
        tecan_file$file <- input$file
        tecan_file$file_dribble <-  tecan$files %>% filter(id == input$file)
        tecan_file$samples <-  experiment$raw$data$Batch_1$Measures$Sample
        tecan_file$measures <-  experiment$raw$data$Batch_1$Measures
        tecan_file$type <-  experiment$raw$type
        tecan_file$user_msg <- experiment$raw$user_msg
        tecan_file$data <-  experiment$raw$data
}, priority = 1)
#Container for ordered drive folder file names
tecan <- reactiveValues(files = drive_tecanURL %>%
                                as_id() %>%
                                get_ordered_filenames_from_drive(progress = tecan_progress),
                        selected_file = NULL,
                        selected_protocol = NULL)
removed_files <- reactiveVal(NULL)

#Generate the file selection list
choiceFiles <- eventReactive(c(input$refresh, # User checks for new files
                               removed_files(), # User has removed files
                               tecan$files), # User has changed files
                             {
                                     dat <- tecan$files %>%
                                             filter(id != if (is.null(removed_files())) ""
                                                    else removed_files()) %>%
                                             select(id, exp_date)

                                     dat$id %>%
                                             set_names(dat$exp_date)
                             })

#Display the file selection list
observeEvent(c(choiceFiles(), input$refresh), {
        choices <- choiceFiles()
        updateSelectInput(session, "file",
                          choices = choices,
                          selected = ifelse(input$refresh == 0,
                                            if (is.null(tecan$selected_file)) head(choices,1)
                                            else tecan$selected_file,
                                            input$file))
        tecan$selected_file <- NULL
        if (!is.null(choices)) tecan_progress$close()
})
