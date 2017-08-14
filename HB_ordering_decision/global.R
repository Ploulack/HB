library(googlesheets)
library(dplyr)

url <- "https://docs.google.com/spreadsheets/d/1vCNA336f0qgLYP471GIF8F8ir-GuqdhQHg5FY9vw3WA/edit#gid=100984341"

sheet_ref <- gs_url(url)


step_values <- sheet_ref %>%
        gs_read(ws = 1, col_names = TRUE) %>%
        filter(!`Also when ordering?`)

hr_costs <- sheet_ref %>%
        gs_read(ws = 2, colnames = TRUE)

dna_costs <- sheet_ref %>%
        gs_read(ws = "DNA_costs", colnames = TRUE)