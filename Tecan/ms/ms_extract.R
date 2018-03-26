library(rvest); library(lubridate)


file4 <- "ms/msfiles/quandata_olivetolscreening_20190220.xml"

extract_ms <- function(input_file, dribble) {
    download_drive_file(input_file = input_file) %>%
        extract_ms_data()

}

extract_ms_data <- function(xml_file = file4) {

    samples <- read_xml(xml_file) %>%
        xml_nodes("SAMPLE")

    samples_info <- samples %>%
        map(xml_attrs) %>%
        map_df(as.list) %>%
        mutate(Time = paste0(createdate,"_",createtime) %>%
                   dmy_hms(tz = "America/Montreal")) %>%
        select(Name = name,type, sampleid = id, groupid, Time)

    samples %>%
        xml_nodes("COMPOUND") %>%
        map(xml_attrs) %>%
        map_df(as.list) %>%
        select(sampleid, Molecule = name) %>%
        add_column(
            Concentration = samples %>%
                xml_nodes("PEAK") %>%
                map(xml_attrs) %>%
                map_df(as.list) %>%
                pull(analconc)
        ) %>%
        mutate(Concentration = as.double(Concentration),
               Molecule = as.factor(Molecule)) %>%
        left_join(samples_info, by = "sampleid") %>%
        filter(type == "Analyte") %>%
        select(-groupid, -type)
}
