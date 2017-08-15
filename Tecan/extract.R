library(rdrop2)
library(lubridate)
library(stringr)

#Extracte Dates from Dropbox list of files

file_date <- function(db_path) {
        
        db_path %>%
                as.character() %>%
                basename() %>%
        str_extract("^[\\d\\s-]*") %>%
                ymd_hms() %>% force_tz("America/Montreal")
}

tecan_extract <- function(db_file, folder) {
        require(xml2); require(dplyr)
        
        local_file <- paste0(folder,basename(file))
        drop_get(file, local_file = local_file, overwrite = TRUE)
        tecan<- read_xml(local_file)
        
        #Get the Data which is in the Elements "Section", 
        #plates <- xml_find_all(tecan, "Section") %>% xml_attr("Name")
        
        data <- map(xml_find_all(tecan, "Section"), function(x) {
                nodes <- xml_find_all(x,"Data") %>%
                        xml_find_all("Well")
                
                wavelength <- xml_find_all(x,"Parameters/Parameter") %>%
                        xml_attrs() %>%
                        keep(~.x["Name"] == "Wavelength") %>%
                        map_chr("Value")
                
                list(Measures = data_frame(Sample = nodes %>% xml_attr("Pos"),
                                           Value = nodes %>%
                                                   xml_children() %>%
                                                   xml_contents() %>%
                                                   xml_double()
                ),
                "Wavelengh" = wavelength)
        }) %>% set_names(nm = paste0("Batch_", seq_along(.)))
}
