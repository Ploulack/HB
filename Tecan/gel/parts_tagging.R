
observeEvent(input$click, {
        if (!base_graph()$is_cropped) return()
        showModal(
                modalDialog(
                        radioButtons(inputId = ns(lane_type),
                                label = "Select type",
                                choices = sample_types),
                        conditionalPanel(condition = paste0("input.",ns(lane_type),"!= 'Part'" ),
                                textInput(inputId = ns("non_part_label"),
                                        label = "Enter descriptive label",
                                        placeholder = "Short text to appear on picture"))
                )
        )
        
        
},ignoreInit = TRUE, ignoreNULL = TRUE)


# parts_widget <- function(input, output, session, db, registry, experiment_labels, experiment_file, file_record) {
#         #Labels: reactive with vector/list (to decide!) of labels
#         #Registry: registry tibble
#         ns <- session$ns
#         
#         output$tagging <- renderUI({
#                 wellPanel(
#                         tagList(
#                                 map(experiment_labels(), function(x) {
#                                         selectizeInput(
#                                                 inputId = ns(x),
#                                                 label = sprintf("Sample %s", x ),
#                                                 choices = registry$KEY  %>%
#                                                         prepend(c("", "Plasmid")),
#                                                 multiple = FALSE
#                                         )
#                                 })
#                         ),
#                         textInput(inputId = ns("note"),
#                                 label = "Optional note",
#                                 placeholder = "Enter file note"),
#                         actionButton(inputId = ns("update"),
#                                 label = "Update database infos for this file")
#                 )})
#         
#         #Display in input fields the db existing data
#         observeEvent(experiment_file(), {
#                 if (experiment_file == wait_msg) return()
#                 
#                 #When user switches tecan file, display existing entries in input fields
#                 if(file_record()$entry_exists) {
#                         #Todo: Add missing entries (parts, samples, etc)
#                         for (i in seq_along(tecan_file()$samples[-1])) {
#                                 updateSelectizeInput(
#                                         session,
#                                         # using the db entry for the input Ids
#                                         # while we're seq_along the file samples (minus water well)
#                                         # is an implicit matching check
#                                         inputId = file_record()$entry$samples[[1]]$Sample[i],
#                                         selected = file_record()$entry$samples[[1]]$Key[i]
#                                 )
#                         }
#                         updateTextInput(
#                                 session = session,
#                                 inputId = "note",
#                                 value = file_record()$entry$note
#                         )
#                 }
#         }, ignoreNULL = TRUE)
#         
#         
# }