library(googlesheets)

url <- "https://docs.google.com/spreadsheets/d/1vCNA336f0qgLYP471GIF8F8ir-GuqdhQHg5FY9vw3WA/edit#gid=100984341"

step_values <- gs_url(url) %>%
        gs_read(ws = 1, col_names = TRUE) %>%
        filter(!`Also when ordering?`)

lab_steps <- step_values$`Assembly step`
