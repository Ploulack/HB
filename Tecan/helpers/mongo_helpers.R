mongo_file_entry <- function(db, file) {
        srch <- paste0('{"file" : "', file, '"}')
        entry <- db$find(srch)
        entry_exists <- ! length(entry) == 0
        
        return(list(
                "entry_exists" = entry_exists,
                "entry" = entry
        ))
}