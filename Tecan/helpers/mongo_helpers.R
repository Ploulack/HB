library(stringr)
mongo_file_entry <- function(db, file, type = "tecan") {
        key <- switch(type,
               tecan = "file" ,
               ms = "_id")

        srch <- str_interp('{"${key}" : "${file}"}')

        db_delay <- system.time(
                entry <- db$find(srch)
        )

        cat("Delay on checking if entry exists ", db_delay)

        entry_exists <- !length(entry) == 0

        return(list(
                "entry_exists" = entry_exists,
                "entry" = entry,
                "delay" = db_delay[3]
        ))
}
#TODO: Add confirmation that db update worked fine
mongo_update_file <- function(db, file, upd_str, type = "tecan", notif_msg = NULL) {
        key <- switch(type,
                      tecan = "file" ,
                      ms = "_id")

        query <- str_interp('{"${key}" : "${file}" }')
        update_log <- db$update(query, upd_str, upsert = TRUE)
        if (!is.null(notif_msg))
                showNotification(ui = notif_msg,
                                 duration = 3,
                                 type = "message")

        update_log
}

mongo_add_protocol <- function(db, file_id, experiment) {

        user <- drive_user()
        query <- str_interp('{"$set":
                            {"experiment" : "${experiment}",
                            "user_name": "${user$displayName}",
                            "user_email": "${user$emailAddress}"}
                        }')

        mongo_update_file(db = db,
                          file = file_id,
                          upd_str = query,
                          type = "ms"
        )
}
