require(lubridate)
require(stringr)

source("drive_helpers.R")

#To extract custom field from tecan file used to tie it to workflows
tecan_custom_msg <- function(tecan) {
        custom <- tecan %>%
                xml_find_all("Script") %>%
                xml_contents() %>%
                xml_children() %>%
                xml_children() %>%
                pluck(1) %>%
                xml_children() %>%
                pluck(1) %>%
                xml_contents() %>%
                pluck(6)
        
        if (is.null(custom)) {
                print("No custom msg in file")
                custom
        } else {
                custom_msg <- custom %>%
                        xml_attrs() %>%
                        "["("description")
                print(paste0("Custom msg in file: ", custom_msg))
                custom_msg
        }
}

tecan_type <- function(xml_file) {
        type_wavelength <- xml_find_all(xml_file, "Section/Parameters/Parameter") %>%
                xml_attrs() %>%
                keep(~.x["Name"] %in% c("Wavelength", "Emission Wavelength")) %>%
                map_chr("Value") %>%
                purrr::pluck(1)
        
        if (type_wavelength %in% tecan_protocols) {
                names(tecan_protocols[tecan_protocols == type_wavelength])
        } else {
                str_interp("Unknown Wavelength: ${type_wavelength}")
        }
}


dl_tecan_xml <- function(dribble, folder) {
        library(xml2)
        if (!dir.exists(folder)) {
                dir.create(folder)
        }
        local_ <- paste0(folder,dribble$name)
        drive_download(dribble, path = local_, overwrite = TRUE)
        tecan_xml <- read_xml(local_)
        file.remove(local_)
        tecan_xml
}


tecan_extract <- function(input_file, dribble) {
        folder <- "temp/"
        #Todo: find the input_file in the dribble
        tecan <- dl_tecan_xml(dribble %>% filter(id == input_file), folder)

        #Get the Data which is in the Elements "Section", 
        
        data <- map(xml_find_all(tecan, "Section"), function(x) {
                
                nodes <- xml_find_all(x,"Data") %>%
                        xml_find_all("Well")
                
                wavelength <- xml_find_all(x,"Parameters/Parameter") %>%
                        xml_attrs() %>%
                        keep(~.x["Name"] %in% c("Wavelength", "Emission Wavelength")) %>%
                        map_chr("Value")
                
                list(Measures = data_frame(Sample = nodes %>% xml_attr("Pos"),
                                           Value = nodes %>%
                                                   xml_children() %>%
                                                   xml_contents() %>%
                                                   xml_double()
                ),
                "Wavelength" = wavelength)
        }) %>% set_names(nm = paste0("Batch_", seq_along(.)))
        
        if (nrow(data$Batch_1$Measures) == 0 || length(data$Batch_1$Wavelength) == 0) {
                showNotification(ui = "Unknown file structure",
                        type = "warning")
                return()
        }
        
        list("data" = data, "type" = tecan_type(tecan), "user_msg" = tecan_custom_msg(tecan))
}

calc_values <- function(list, molar_absorbance, path_length) {
        
        list_delta <- list %>%
                map(~mutate(.x$Measures, Wavelength = .x$Wavelength)) %>%
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
                mutate(Sample = filter(full_tbl, Wavelength == 260) %>%
                               pull(Sample),
                       Ratio = Batch_1 / Batch_2,
                       Concentration = filter(full_tbl, Wavelength == 260) %>%
                               pull(var = Concentration)) %>%
                select(-starts_with("Batch"))
        
        list("Table" = full_tbl, "Results" = results)
}


# 
# test["description"]