source("registry/registry_values.R")


update_file_if_outdated <- function(url, sheets, local_file) {
        library(readxl)
        last_modified <- drive_get(as_id(url))$drive_resource[[1]]$modifiedTime %>%
                ymd_hms(tz = "America/Montreal", quiet = TRUE)
        last_synced <- file.mtime(file)
        outdated <- last_modified > last_synced

        if (!file.exists(local_file) || outdated ) {
                drive_download(file = as_id(url),
                               path = local_file,
                               overwrite = TRUE)
        }

}

registry_key_names <- function(registry_url = registry_url, registry_sheets = registry_sheets, file = registry_file) {

        update_file_if_outdated(url = registry_url, sheets = registry_sheets, local_file = file)

        map_dfr(
                registry_sheets,
                ~ read_xlsx(path = file,
                            sheet = .x,
                            range = cell_cols(c(1:3,5:6))
                )
        ) %>%
                mutate(Length = as.integer(Length))
}

get_strains <- function() {
        update_file_if_outdated(url = strains_url, sheets = strains_sheets, local_file = strains_file)
        read_xlsx(path = file,
                  sheet = strains_sheets[1],
                  range = "B3:B"
                  )
}
