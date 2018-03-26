

tags_widget_server <- function(input, output, session, db) {
    ns <- session$ns
    browser()
    updateSelectizeInput(session,
                         inputId = "tags",
                         # choices = tags_retrieve(db),
                         choices = LETTERS,
                         options = list(
                             create = 'true'
                         ),
                         server = TRUE
    )

    observeEvent(input$tags,{
        validate(need(!is.null(input$tags), message = FALSE))

        tags <- input$tags %>%
            map_chr(~ str_interp('"${.}"'))

        tags <- str_interp('${str_c(tags, collapse = ", ")}')

        tags_add(db = db, tags = tags)
    })
return(reactive(input$tags))
}

tags_widget_ui <- function(id) {
    ns <- NS(id)

    selectizeInput(ns("tags"),
                   label = ns("tags"),
                   choices = NULL,
                   multiple = TRUE)
}

tags_add <- function(db, type = "ms", tags) {
    query <- str_interp('{ "_id" : "${type}"}')
    update <- str_interp('{ "$addToSet" : {"tags": {"$each" : [ ${tags} ] } } }')
    db$update(query, update)
}

tags_retrieve <- function(db, type = "ms") {
    query <- str_interp('{"_id" : "${type}" }')
    fields <- '{"tags" : 1, "_id" : 0}'
    db$find(query, fields) %>%
        pull(tags) %>%
        unlist()
}