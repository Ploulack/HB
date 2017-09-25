library(googlesheets)

get_assembly_costs <- function(url) {
        sheet_ref <- gs_url(url)
        
        step_values <- sheet_ref %>%
                gs_read(ws = 1, col_names = TRUE) %>%
                filter(!`Also when ordering?`)
        
        hr_costs <- sheet_ref %>%
                gs_read(ws = 2, colnames = TRUE)
        
        dna_costs <- sheet_ref %>%
                gs_read(ws = "DNA_costs", colnames = TRUE)
        
        list("steps" = step_values,
             "HR" = hr_costs,
             "DNA" = dna_costs)
}

#TODO: Complete...
get_success_rates <- function(url) {
        sheet_ref <- gs_url(url)
        
        success_rates <- sheet_ref %>%
                gs_read(ws = 1, col_names = TRUE)
        
}