first_unused <- function(used_labels = NULL, n = 1) {
    if (length(used_labels) > 26^2) stop("Can't handle that many labels.")
    LETTERS_2 <- map(LETTERS, function(x) map_chr(LETTERS, ~paste0(x, .x))) %>%
        unlist() %>%
        append(x = LETTERS)

    idx <- !LETTERS_2 %in% used_labels

    LETTERS_2[idx][1:n]
}

gsheet_colID_from_tibble <- function(tbl, tbl_col) {

        stopifnot(ncol(tbl) <= 26); stopifnot(is_tibble(tbl))
        idx <- tbl_col == colnames(tbl)
        LETTERS[1:ncol(tbl)][idx]
}
