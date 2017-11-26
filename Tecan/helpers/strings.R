first_unused <- function(used_labels = NULL) {
        if (is.null(used_labels)) return("A")
        quotient <- length(used_labels) %/% 26
        rest <- length(used_labels) %% 26
        
        if (rest == 0) {
                res <- NULL
                for (i in 1:(quotient + 1)) {
                        res <- paste0(res,"A")
                }
                res}
        else {
                for (i in 0:quotient) {
                        index <- used_labels %>% str_length() == i + 1 
                        if (sum(index) != 26)
                        {
                                l <- used_labels[index] %>%
                                        str_sub(start = i + 1, end = i + 1)
                                return(LETTERS[which.min(LETTERS %in% l)])
                        }
                }
                l <- used_labels %>%
                        str_sub(start = quotient + 1, end = quotient + 1) %>%
                        str_subset("[A-Z]")
                LETTERS[which.min(LETTERS %in% l)]
        }
}

