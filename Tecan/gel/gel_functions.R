save_delete <- function(is_saving, db, sample = NULL, file_id, file_name, sample_label) {
        str1 <- paste0('{"file" : "', file_id,'"}')
        sample_str <-  jsonlite::toJSON(sample,
                auto_unbox = TRUE)
        
        if (is_saving) {
                str2 <- paste0(
                        '{"$addToSet" : 
                        {"labels" : ',sample_str,'}
                        }')
                update_log <- db$update(str1,str2)
                
                if (update_log) {
                        showNotification(ui = str_interp("Updated file ${file_name} with sample ${sample_label}.") ,
                                duration = 3,
                                type = "message")
                }        
        } else {
                str2 <- paste0(
                        '{"$pull" : 
                        {"labels" : {"sample" : "',sample_label,'"}}
                        }')
                update_log <- db$update(str1,str2)
                if (update_log) {
                        showNotification(ui = str_interp("Updated file ${file_name} to remove sample ${sample_label}.") ,
                                duration = 3,
                                type = "message")
                }
                
        }
}

save_sample <- function(...) {
        save_delete(is_saving = TRUE, ...)
}

delete_sample <- function(...) {
        save_delete(is_saving = FALSE, ...)
}
#TInitialize gggplot graph witih picture with correct aspect ratio
init_graph <- function(path) {
        img <- readJPEG(source = path, native = TRUE)
        img_grob <- rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE)
        graph <- ggplot() +
                scale_x_continuous(limits = c(0,1),name = "", expand = c(0,0)) +
                scale_y_continuous(limits = c(0,1), name = "", expand = c(0,0)) +
                annotation_custom(grob = img_grob) +
                theme(
                        aspect.ratio = dim(img)[1]/dim(img)[2],
                        # aspect.ratio = 1,
                        # aspect.ratio = (1/16),
                        plot.margin =unit(c(0,0,0,0),"npc"),
                        axis.line = element_blank(),
                        axis.ticks = element_blank(),
                        #text = element_text(face = "bold", color = "white"),
                        axis.text= element_blank()
                )
        return(graph)
}

display_remove_button <- function(ui_id, sample_key, sample_label, clicks, last_del, current_file) {
        #Add new sample's remove button and additional information
        insertUI(selector = "hr",
                 where = "afterEnd",
                 ui = sample_ui(id = ui_id, sample_key = sample_key, sample_label))
        cat("ui_id : ", ui_id, " ", "id :", sample_label, "\n")
        
        #Add logic attached to new sample's button
        callModule(module =  sample_server,
                   id = sample_label,
                  #id = paste0(current_file,sample_label),
                   clicks = clicks,
                   sample_label = sample_label,
                   last_del = last_del,
                   current_file = current_file)
}