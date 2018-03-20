


tags_add <- function(db, type = "ms", tag) {
    query <- str_interp('{ "_id" : "${type}"}')
    update <- str_interp('{ "$addToSet" : {"tags" : "${tag}"}}')
    db$update(query, update)
}


tags_retrieve <- function(db, type = "ms") {
    query <- str_interp('{"_id" : "${type}" }')
    fields <- '{"tags" : 1, "_id" : 0}'
    db$find(query, fields) %>%
        pull(tags) %>%
        unlist()
}