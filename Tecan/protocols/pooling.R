# calculated_data <- read_xml("tests/testthat/tecan_xml_files/2018-01-15 13-01-46_plate_1.xml") %>%
#         tecan_data() %>%
#         calc_values(water_well_pos = NULL, water_readings = water_readings)
#
# plate_data <- calculated_data$Results
#

plate_pooling <- function(plate_data,
                          min_wells_nb = 1,
                          well_volume = 50, # Well volume to come from an input in the modal dialog
                          min_pipet_vol = 10,
                          min_dw_conc = 150,
                          tecan_sample_vol = min_pipet_vol,
                          tecan_water_vol = 40) {

        validate(
               need(min_wells_nb, label = "Minimum number of wells"),
               need(well_volume, label = "Volume of the PCR plate's wells from which the present Tecan plate was sampled from"),
               need(min_pipet_vol, label = "Hamilton's minimum transfer volume"),
               need(min_dw_conc, label = "Minimum final deep-well concentration"),
               need(tecan_sample_vol, label = "Volume of sample taken from the PCR plate for this Tecan reading."),
               need(tecan_water_vol, label = "Volume of water added to the sample")
        )

        dilution_to_tecan <- tecan_sample_vol / (tecan_sample_vol + tecan_water_vol)
        reverse_dilution <- 1 / dilution_to_tecan

        plate <- plate_data %>%
                mutate(Well_Volume = well_volume,
                       Pool_Volume = NA,
                       Concentration = Concentration * reverse_dilution) %>%
                arrange(desc(Concentration))

        if (is.null(min_wells_nb)) min_wells_nb <- 1

        #We initiate the deep-well condition variables as of after the transfer
        #from that first max concentration well
        plate$Pool_Volume[1] <- min_pipet_vol
        plate$Well_Volume[1] <- plate$Well_Volume[1] - min_pipet_vol

        dw_conc <- plate$Concentration[1]
        dw_vol <- min_pipet_vol

        dna_per_well <- dw_conc * min_pipet_vol
        idx <- 1

        # Then for the remaining of the wells we pool and check if any our stop conditions
        # have been met, in which case we stop at that well.
        for (i in 2:nrow(plate)) {
                # The volume we'll take from that well dna_weight set above divided
                # by its concentration
                vol_from_current_well <- dna_per_well / plate$Concentration[i]
                # We add that volume to the deep-well
                dw_vol <- vol_from_current_well + dw_vol
                # We update the deep well's concentration using the knowledge that
                # we took a similar qty of dna from each well
                dw_conc <- i * dna_per_well / dw_vol
                #Now the conditions at which we stop...
                stop_conditions <- c(
                        # if volume to draw is more than existing vol in that well
                        # less a pipetting safety margin of 5
                        insufficient_vol = (vol_from_current_well >= plate$Well_Volume[i] - 5),
                        # if the deep well is full
                        deep_well_full = (dw_vol > 2000),
                        # if the deep-well's concentration has fallen below our limit
                        dw_conc_low = (dw_conc < min_dw_conc)
                )
                if (any(stop_conditions)) {
                        cat("Reason for pool cut-off: ",names(stop_conditions[stop_conditions]), sep = "\n")
                        break
                        }
                else {
                        idx <- i
                        plate$Pool_Volume[i] <- vol_from_current_well
                        plate$Well_Volume[i] <- plate$Well_Volume[i] - vol_from_current_well
                }
        }
        validate(
                need(
                        idx >= min_wells_nb,
                        "More wells excluded than min nb of wells threshold allows.")
        )

        list(
                deep_well = tibble(n_wells_pooled = idx,
                              volume = sum(plate$Pool_Volume, na.rm = TRUE)) %>%
                        mutate(concentration = idx * dna_per_well / volume),
             plate_pooled = plate %>%
                     select(-Ratio))
}
