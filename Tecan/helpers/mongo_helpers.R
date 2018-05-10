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

     if (entry_exists) cat(file = stderr(), "Entry found. For type ", type, " and file ", file)
     else cat(file = stderr(), " No entry found. For type ", type, " and file ", file)

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

     if (
          (type == "tecan" &&
           (update_log$upsertedId %>% is_character() && str_length(update_log$upsertedId) > 10)
           ||
           (update_log$modifiedCount == 1 && update_log$matchedCount == 1)
          )
          ||
          type == "ms"
     ) {
          if (!is.null(notif_msg))
               showNotification(ui = notif_msg,
                                duration = 3,
                                type = "message")
     } else if (update_log$modifiedCount == 0 && update_log$matchedCount == 1) {
          showNotification(ui = "DB entry found, but not modified (probably because update info already present in entry)",
                           duration = 3,
                           type = "message")
     }
     else
          {
          # cat("DB update failed. Query : ", "\n", query %>% jsonlite::prettify(), "\n", "and Update : ", upd_str %>% jsonlite::prettify())
          showNotification(ui = "DB entry failed",
                           duration = 3,
                           type = "message")
     }

     update_log
}

mongo_add_protocol <- function(db, file_id, experiment, type) {

     user <- drive_user()
     query <- str_interp('{"$set":
                            {"experiment" : "${experiment}",
                            "user_name": "${user$displayName}",
                            "user_email": "${user$emailAddress}"}
                        }')

     mongo_update_file(db = db,
                       file = file_id,
                       upd_str = query,
                       type = type
     )
}

safe_prettify <- safely(jsonlite::prettify)

#Function to pass stages of a mongo aggregate pipeline separately like
# match, project, etc.
# Uses safa_prettify to only make the db request when the json is valid
# TODO: when printed as part of a list the nice format of the prettify diagnostic is jumbled out...
aggregate_pipeline <- function(db, ...) {
     pipeline <- paste(..., sep = ",")
     pipeline <- str_interp('[${pipeline}]')

     res <- safe_prettify(pipeline)
     if (is.null(res$error)) {
          db$aggregate(pipeline)
     } else {
          stop(res$error$message)
     }
}

to_pretty_json <- function(...) {
     pipeline <- paste(..., sep = ",")
     str_interp('[${pipeline}]') %>%
          jsonlite::prettify()
}
