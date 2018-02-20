source("registry/registry_values.R")


registry_key_names <- function(registry_url, registry_sheets, file = registry_file) {
        library(readxl)
        registry_modified <- drive_get(as_id(registry_url))$drive_resource[[1]]$modifiedTime %>%
                ymd_hms(tz = "America/Montreal", quiet = TRUE)
        registry_synced <- file.mtime(file)
        outdated <- registry_modified > registry_synced

        if (!file.exists(file) || outdated )
        {drive_download(file = as_id(registry_url),
                        path = file,
                overwrite = TRUE)}
        map_dfr(
                registry_sheets,
                ~ read_xlsx(path = file,
                            sheet = .x,
                            range = cell_cols(c(1:3,5:6))
                )
        ) %>%
                mutate(Length = as.integer(Length))
}
