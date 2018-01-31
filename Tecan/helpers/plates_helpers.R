repeat_parts <- function(part_keys, n) {
        map2(part_keys, n, ~ rep(.x,.y)) %>%
                as_vector()
}

parts_to_vector <- function(parts) {
        repeat_parts(parts$part, parts$n_pcr)
}

primers_to_vector_repeated <- function(parts) {
        c(repeat_parts(parts$l_primer, parts$n_pcr),
          repeat_parts(parts$r_primer, parts$n_pcr))
}

build_plate <- function(elements) {
        k <- 1
        plate <- matrix(nrow = 8, ncol = 12)
        for (i in 1:12) {
                for (j in 1:8) {
                        plate[j, i] <- if (k <= length(elements)) elements[k] else ""
                        k <- k + 1
                }
        }
        return(plate %>% as_tibble())
}

generate_plates <- function(parts) {
        list(Templates = parts$key %>%
                     build_plate(),
             Primers = c(parts$l_primer, parts$r_primer) %>%
                     build_plate()
        )
}

generate_96_pos <- function() {
        map(1:12, function(x) {
                map_chr(LETTERS[1:8], ~ paste0(.x,x))
        }) %>%
                unlist()
}

#Matches A-Z, AA-AZ, etc.. to 96 plates positions
letter_to_96_pos <- function(letter) {
        l <- str_length(letter)
        pos <- which(LETTERS == str_sub(letter, l, l)) + 26 * (l - 1)
        generate_96_pos()[pos]

}

plate_sort_sample <- function(plate) {
        assert_all_are_true("Sample" %in% names(plate))
        plate %>%
                arrange(as.integer(str_extract(Sample, "\\d+")),
                                    str_extract(Sample, "[A-Z]")
                        )
}
