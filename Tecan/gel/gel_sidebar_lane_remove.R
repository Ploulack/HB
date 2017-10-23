#sample_ui <- function(id, sample_key, sample_name) {
sample_ui <- function(id, sample_key, sample_name, current_file_name) {
        ns <- NS(id)
        tags$div(id = ns("lane_btn"),
                tags$b(str_interp("Label: ${sample_name}, Key: ${sample_key}")),
                actionButton(ns("remove_btn"), str_interp("Remove label ${sample_name}"))
        )
}

#sample_server <- function(input, output, session, clicks, sample_label, last_del, current_file) {
sample_server <- function(input, output, session, clicks, sample_label, last_del, current_file, current_file_name) {
        ns <- session$ns
       observeEvent(input$remove_btn, {
                removeUI(selector = paste0("#", ns("lane_btn")))
                clicks(clicks() %>% filter(sample != sample_label))
                #Put the last deleted sample in a reactive to get the value out of the module
                last_del(sample_label)
        },ignoreInit = TRUE)
       
       observeEvent(current_file, {
               removeUI(selector = paste0("#", ns("lane_btn")))
       }, ignoreInit = TRUE, priority = 1)
}