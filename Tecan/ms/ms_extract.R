library(rvest); library(lubridate)


extract_ms <- function(input_file, dribble) {#Are we using that dribble at all?
    download_drive_file(input_file = input_file) %>%
        read_xml() %>%
        extract_ms_data()
}

substract_blanks <- function(res) {

    n_molecules <- res %>%
        distinct(Molecule) %>%
        nrow()

    blanks_1 <- res %>%
        slice(1:n_molecules) %>%
        arrange(Molecule)

    blanks <- res %>%
        slice((n_molecules + 1):n()) %>%
        filter(str_detect(type, "(?i)Blank")) %>%
        group_by(Molecule) %>%
        summarise(Concentration = if_else(
             mean(Concentration, na.rm = TRUE) %>% is.nan(),
             0,
             mean(Concentration, na.rm = TRUE)
             )) %>%
        arrange(Molecule)

    blank <- blanks_1 %>%
        mutate(Concentration = if_else(Concentration == 0 | is.na(Concentration), blanks$Concentration, Concentration))

    map2_dfr(blank$Molecule, blank$Concentration, ~{
        res %>%
            filter(str_detect(type, "(?i)Analyte")) %>%
            filter(Molecule == .x) %>%
            mutate(Concentration = if_else( (Concentration - .y) < 0, 0, Concentration - .y))
    }) %>%
        bind_rows(res %>% filter(!str_detect(type, "(?i)Analyte"))) %>%
        arrange(as.integer(sampleid))

}

extract_ms_data <- function(xml) {
    samples <- xml %>%
        xml_nodes("SAMPLE")

    samples_info <- samples %>%
        map(xml_attrs) %>%
        map_df(as.list) %>%
        mutate(Time = paste0(createdate,"_",createtime) %>%
                   dmy_hms(tz = "America/Montreal")) %>%
        mutate(
            Tags = str_split(desc, ", "),
            Strain = str_extract(name, "(?i)^HB\\w+?(?=_)") %>% toupper(),
            Plasmid = str_extract(name, "(?i)PLAS-[\\d]+") %>% toupper(),
            Identifier = str_extract(name, "(?i)(?<=^HB\\d{1,10}_)[^_]\\w+(?=_\\d+(_G-\\d+)?$)")
        ) %>%
        select(Name = name,type, sampleid = id, Strain, Plasmid, Identifier, Tags, Time)

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
        substract_blanks() %>%
        filter(tolower(type) %in% c("analyte", "blank"))
}
