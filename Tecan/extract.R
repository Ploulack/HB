
#Extracte Dates from Dropbox list of files

file_date <- function(db_path) {
        
        db_path %>%
                as.character() %>%
                basename() %>%
        str_extract("^[\\d\\s-]*") %>%
                ymd_hms() %>% force_tz("America/Montreal")
}

is_kinetic <- function(xml_file) {
        xml_find_all(xml_file, "Section/Parameters/Parameter") %>%
                xml_attr("Name") %>%
                as.character() %>%
                str_detect("Kinetic") %>%
                any()
}

#To do for tecan_extract: separate xml file access and create dedicated subfunction for 260nm & 600nm
tecan_extract <- function(db_file, folder) {
        require(xml2)
        
        local_ <- paste0(folder,basename(db_file))
        drop_get(db_file, local_file = local_, overwrite = TRUE)
        
        tecan<- read_xml(local_)
        
        #Test if it's a 600nm Tecan run
        if (is_kinetic(tecan) ) return("File is for a 600nm test.")
        
        
        #Get the Data which is in the Elements "Section", 
        
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
                "Wavelength" = wavelength)
        }) %>% set_names(nm = paste0("Batch_", seq_along(.)))
}

calc_values <- function(list, molar_absorbance, path_length) {
        
        list_delta <- list %>% map(~mutate(.x$Measures, Wavelength = .x$Wavelength)) %>%
                
                #Substract 1st well, which is water
                map(~mutate(.x, Delta = Value - Value[1],
                            Concentration = Delta / (molar_absorbance * path_length)))
        
        full_tbl <- list_delta %>%
                
                #Bind into single tbl
                map_dfr(~.x) %>%
                
                #Order by Sample label
                arrange( str_extract(Sample, "[A-Z]")   ,as.integer( str_extract(Sample, "\\d+")))
        
        results <- map(list_delta, pull, var = Delta) %>%
                as_tibble() %>%
                mutate(Sample =filter(full_tbl, Wavelength == 260) %>%
                               pull(Sample),
                       Ratio = Batch_1 / Batch_2,
                       Concentration = filter(full_tbl, Wavelength == 260) %>%
                               pull(var = Concentration)) %>%
                select(-starts_with("Batch"))
        
        list("Table" = full_tbl, "Results" = results)
        
}
