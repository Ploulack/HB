graph_bar_get_clicked_sample <- function(click_x,
                                         n_groups,
                                         name_var,
                                         group_var,
                                         data) {

        #TODO: rather than force supply of vectors, just give the variables name, unquoted, in the tibble
        #TODO: vérifier mais a priori n_groups c'est redondant et l'info est gérable avec la data

        splits <- seq(1/(2 * n_groups), 1 - 1/(2 * n_groups), 1/n_groups)

        sample_lvls <- data %>%
                as_factor() %>%
                levels()

        sample_name <- sample_lvls[round(click_x)]

        group_lvls <- sample_groups %>%
                as_factor() %>%
                droplevels() %>%
                levels()

        x <- click_x - round(click_x) + 1/2

        group_name <- group_lvls[which.min(abs(splits - x))]

        value <- display_tbl() %>%
                filter(Molecule == molecule_name & Name == name) %>%
                pull(Mean)

        list(name = name,
             molecule = molecule_name,
             value = value)
}
