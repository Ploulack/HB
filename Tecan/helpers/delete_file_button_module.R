delete_exp_files_ui <- function(id) {
        ns <- NS(id)

        actionButton(inputId = ns("remove_files_modal"),
                     label = "Remove File")
}

delete_exp_files <- function(input, output, session, tecan_file, db, files_list, removed_files) {
        if (is.null(tecan_file) || is.null(files_list)) return()

        ns <- session$ns
        observeEvent(input$remove_files_modal, {
                showModal(modalDialog(
                        "Please confirm",
                        footer = tagList(
                                modalButton("Cancel"),
                                actionButton(inputId = ns("remove_file"),
                                             label = "Remove File")
                        )
                ))
        })

        observeEvent(input$remove_file, {
                removeModal()
                # Remove file
                drive_mv(file = tecan_file$file_dribble,
                         path = as_id("https://drive.google.com/open?id=0B4_A43u3xZeRbWJiT2ZuVnpQR28"))

                #Remove db entry
                remove_log <- db$remove(paste0('{"file" : "',tecan_file$file_dribble$id,'"}'), just_one = TRUE)
                if (remove_log$modifiedCount == 1) {
                        showNotification(ui = str_interp("Removed entry for file ${tecan_file$file_dribble$name}") ,
                                         duration = 3,
                                         type = "message")
                }
                # Update choices list
                removed_files(removed_files() %>% append(tecan_file$file_dribble$id))

        })
}