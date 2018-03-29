get_latest_calibration_file <- function() {
    ms_calibration_files %>%
        as_id() %>%
        drive_ls(pattern = "ipr" , q = "modifiedTime > '2018-01-12T12:00:00'") %>%
        arrange(desc(drive_resource %>% map_chr("createdTime"))) %>%
        pull(name) %>%
        first()
}

format_plate_pos <- function(plate, position) {
    plate_nb <- plate %>%
        str_extract("\\d$")

    well_pos <- split_pos_column_row(position)

    paste0(plate_nb, ":", well_pos)
}

generate_sample_list_csv <- function(samples_tbl, input, user) {
    #The calibration files from the MS computer is synced
    #Calibration is updated from time to time, we always use the last one
    #We filter on recent date to save time
    cal_file_name <- get_latest_calibration_file()

    analytes <- samples_tbl %>%
        arrange(plate,
                str_extract(pos, "[A-Z]"),
                as.integer(str_extract(pos, "\\d+"))
        ) %>%
        mutate(
            FILE_NAME = paste(strain,
                              if_else(is.na(plasmid), "", plasmid),
                              if_exists_than_that(identifier, ""),
                              row_number(),
                              if_else(is.na(group_id), "", paste0("G-",group_id)),
                              sep = "_") %>%
                str_remove("_$"),
            #Nb 2 is the plate number, plate #1 is ran just before and includes standards and blanks...
            SAMPLE_LOCATION = format_plate_pos(plate, pos),
            TYPE = "ANALYTE",
            CONC_A = "",
            SPARE_1 = group_id,
            SPARE_2 = plate_note,
            SPARE_5 = url,
            FILE_TEXT = tags
        ) %>%
        select(-(label:pos))

    map2_dfr(c("blank1","std_1", "std_10", "std_100", "std_1000", "blank2"),
             # 96 plate for now...
             generate_48_pos()[1:6],
             ~ tibble(
                 FILE_NAME = .x,
                 SAMPLE_LOCATION = paste0("1:", split_pos_column_row(.y)),
                 TYPE = if (str_detect(.x, "blank")) "BLANK" else "STANDARD",
                 CONC_A = str_extract(.x, "(?<=_)\\d+"),
                 SPARE_1 = "",
                 FILE_TEXT = ""
             )
    ) %>%
        bind_rows(analytes) %>%
        mutate(
            CONC_A = if_else(is.na(CONC_A), "", CONC_A),
            ID = row_number() %>% as.character(),
            MS_FILE = "C:\\MassLynx\\UNITARY.PRO\\ACQUDB\\General_all_20180213.exp",
            INLET_FILE = "long_hold",
            INJ_VOL = 3 %>% as.integer(),
            MS_TUNE_FILE = paste0("C:\\MassLynx\\IntelliStart\\Results\\Unit Mass Resolution\\",
                                  cal_file_name),
            SPARE_3 = user,
            SPARE_4 = Sys.time() %>% force_tz("America/Montreal") %>% as.character()
        ) %>%
        mutate(Index = row_number()) %>%
        select(
            Index,
            FILE_NAME,
            ID,
            MS_FILE,
            INLET_FILE,
            SAMPLE_LOCATION,
            INJ_VOL,
            TYPE,
            CONC_A,
            MS_TUNE_FILE,
            SPARE_1,
            SPARE_2,
            SPARE_3,
            SPARE_4,
            SPARE_5,
            FILE_TEXT
        )
}

check_ongoing_edit <- function(folder_url, user, ms_edit) {

    file <- folder_url %>%
        as_id() %>%
        drive_ls(pattern = user)

    if (nrow(file) == 0) {
        ms_edit$is_ongoing <- FALSE
    }
    else {
        if (nrow(file) > 1) file[-1, ] %>%
            by_row(..f = ~ drive_trash(.x))

        path <- paste0("temp/", file[1,]$name)
        drive_download(file[1,], path, overwrite = TRUE)

        ms_edit$is_ongoing <- TRUE
        ms_edit$data  <- read_csv(path)
        #captures in file name the experiment name after 'exp_' and before the next underscore
        ms_edit$experiment <- str_extract(file[1,]$name, "(?<=exp_)[A-z]*?(?=_)")
        ms_edit$file_note <- str_extract(file[1,]$name, "(?<=_note_).*(?=\\.)")
        ms_edit$drbl <- file[1,]
    }
}


save_ms_edit_as_csv_on_drive <- function(drive_url,
                                         csv_name,
                                         samples,
                                         ms_edit,
                                         local_folder = "temp/") {

    file_name <- paste0(local_folder, csv_name)
    write_csv(samples, file_name, append = FALSE)
    if (ms_edit$is_ongoing && csv_name == ms_edit$drbl$name) {
        drive_update(file = ms_edit$drbl,
                     media = file_name)
        ms_edit
    } else {
        ms_edit$drbl <- drive_upload(media = file_name,
                                     path = drive_url %>% as_id())
        ms_edit$is_ongoing <- TRUE
        ms_edit
    }
}


split_pos_column_row <- function(pos) {
    paste0(
        str_sub(pos, end = 1), ",", str_sub(pos, start = 2)
    )
}