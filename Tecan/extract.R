require(lubridate)
require(stringr)

#Extracte Dates from names of Tecan files
file_date <- function(db_path) {
        
        db_path %>%
                as.character() %>%
                basename() %>%
        str_extract("^[\\d\\s-]*") %>%
                ymd_hms() %>% force_tz("America/Montreal")
}

is_kinetic <- function(xml_file) {
        is_kinetic <- xml_find_all(xml_file, "Section/Parameters/Parameter") %>%
                xml_attrs() %>%
                keep(~.x["Name"] == "Wavelength") %>%
                str_detect("600") %>%
                any()
        is_kinetic
}

get_ordered_filenames_from_db_dir <- function(db_folder) {
        #Todo: use the delta method to only get the new file
        #Get dropbox folder meta data
        ##Todo: add directory check
        drop_dir(db_folder) %>%
                select(path) %>% 
                mutate(exp_date = file_date(path),
                       path = as.character(path)) %>%
                as_tibble() %>%
                arrange(desc(exp_date))
}

dl_tecan_xml <- function(db_file, folder) {
        require(xml2)
        local_ <- paste0(folder,basename(db_file))
        drop_get(db_file, local_file = local_, overwrite = TRUE)
        tecan_xml <- read_xml(local_)
        file.remove(local_)
        tecan_xml
}

tecan_extract <- function(db_file) {
        folder <- "temp/"
        
        tecan <- dl_tecan_xml(db_file, folder)
        

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
        
        list("data" = data, "kinetic" = is_kinetic(tecan))
}

calc_values <- function(list, molar_absorbance, path_length) {
        
        list_delta <- list %>% map(~mutate(.x$Measures, Wavelength = .x$Wavelength)) %>%
                #Order by Sample label (should make a function)
                map(~arrange(.x,
                             str_extract(Sample, "[A-Z]"),
                             as.integer( str_extract(Sample, "\\d+")))
                ) %>%
                
        #Substract 1st well, which is water
                map(~mutate(.x, Delta = Value - Value[1],
                            Concentration = Delta / (molar_absorbance * path_length)))
        
        full_tbl <- list_delta %>%
                
                #Bind into single tbl
                map_dfr(~.x) %>%
                
                #Order by Sample label (should make a function)
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
