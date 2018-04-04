require(lubridate); library(xml2); library(rvest)
require(stringr)

source("helpers/drive_helpers.R")

#To extract custom field from tecan file used to tie it to workflows
tecan_custom_msg <- function(tecan) {
        custom_access <- function() {
                tecan %>%
                        xml_child(3) %>%
                        xml_child() %>%
                        xml_child() %>%
                        xml_child() %>%
                        xml_child() %>%
                        xml_child(6) %>%
                        xml_attr(attr = "description")
                }

        custom <- safely(custom_access)()

        if (is.null(custom$result)) {
                print("No custom msg in file")
                custom$result
        } else {
                print(paste0("Custom msg in file: ", custom$result))
                custom$result
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

tecan_data <- function(tecan) {
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
                ) %>%
                        arrange(
                                str_extract(Sample, "[A-Z]"),
                                as.integer(str_extract(Sample, "\\d+"))
                        ),
                "Wavelength" = wavelength)
       }) %>%
               set_names(nm = paste0("Batch_", seq_along(.)))

        if (nrow(data$Batch_1$Measures) == 0 || length(data$Batch_1$Wavelength) == 0) {
                showNotification(ui = "Unknown file structure",
                                 type = "warning")
                return()
        }

       return(data)
}

tecan_extract <- function(input_file, dribble) {
        folder <- "temp/"
        #Todo: find the input_file in the dribble
        if (!dir.exists(folder)) {
                dir.create(folder)
        }
        local_xml <- paste0(folder,input_file)
        drive_download(as_id(input_file), path = local_xml, overwrite = TRUE)

        tecan_read_xml(local_xml)
}

tecan_read_xml <- function(xml_file) {
        safe_read_xml <- safely(read_xml)
        tecan <- safe_read_xml(xml_file)

        if (is.null(tecan$error)) {
                list(
                        "data" = tecan_data(tecan$result),
                        "type" = tecan_type(tecan$result),
                        "user_msg" = tecan_custom_msg(tecan$result)
                )
        } else {
                list("data" = NULL,
                     "type" = tecan$error,
                     "user_msg" = NULL)
        }
}

calc_values <- function(tecan_raw_data,
                        molar_absorbance = 0.02,
                        path_length = 0.19,
                        water_well_pos = 1,
                        water_readings = NULL) {

        assert_all_are_non_negative(c(molar_absorbance, path_length))
        if (!is.null(water_well_pos)) assert_all_are_whole_numbers(water_well_pos)

        list_delta <- tecan_raw_data %>%
                map(~mutate(.x$Measures, Wavelength = .x$Wavelength)) %>%
                map(~mutate(.x,
                            Delta = Value - ifelse(!is.null(water_well_pos),
                                                   Value[water_well_pos],
                                                   water_readings[Wavelength]),
                            Concentration = Delta / (molar_absorbance * path_length)
                            )) %>%
                map(~mutate(.x,
                            Index = row_number()))

        full_tbl <- list_delta %>%
                #Bind into single tbl
                map_dfr(~.x) %>%
                #Use Index to group samples together (could use groups...)
                arrange(Index) %>%
                select(-Index)

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

#Calculate in ul the volume of water to add to normalize
tecan_calc_water_vol <- function(calc_tbl, well_volume, target_concentration, multiplier = 10) {
        calc_tbl %>%
                mutate(Normalize = ((multiplier * Concentration * well_volume) / target_concentration   - well_volume) %>%
                               round(digits = 1))
}