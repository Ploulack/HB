library(googlesheets)

url <- "https://docs.google.com/spreadsheets/d/1rJzbwZURvK2BweQCsdUpJkqjBuRosP0QxMi6-DHwsUM/pubhtml"

step_values <- gs_url(url) %>%
        gs_read(ws = 1, col_names = TRUE) %>%
        filter(!`Also when ordering?`)

lab_steps <- step_values$`Assembly step`
