mongo_file_entry <- function(db, file) {
        srch <- paste0('{"file" : "', file, '"}')
        entry <- db$find(srch)
        entry_exists <- !length(entry) == 0
        
        return(list(
                "entry_exists" = entry_exists,
                "entry" = entry
        ))
}
#TODO: Add confirmation that db update worked fine
mongo_update_file <- function(db, file, upd_str, notif_msg = NULL) {
        str1 <- str_interp('{"file" : "${file}" }')
        db$update(str1, upd_str, upsert = TRUE)
        if (!is.null(notif_msg))
                showNotification(ui = notif_msg,
                                 duration = 3,
                                 type = "message")
}