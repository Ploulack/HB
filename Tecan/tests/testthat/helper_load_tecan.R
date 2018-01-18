setwd("~/Documents/R/HB/Tecan/")
source("tecan/tecan_extract.R")
source("tecan/tecan_values.R")

##Prepare test objects
# tecan_1:
tecan_1 <- "tests/testthat/tecan_xml_files/2018-01-15 13-01-46_plate_1.xml" %>%
        read_xml()

# tecan_2: DNA quant, normal: water in first well, no custom message
tecan_2 <- "tests/testthat/tecan_xml_files/2017-12-20 11-31-55_plate_1.xml" %>%
        read_xml()
#Aggregate examples into a list
tecan_examples <- list(
        "quant_multi" = list(xml = tecan_1,
                             type = tecan_protocols %>% keep(~.x == "260") %>% names(),
                             desc = "DNA Quantification, No water well, with custom message with plate number"),
        "quant_normal" = list(xml = tecan_2,
                              type = tecan_protocols %>% keep(~.x == "260") %>% names(),
                              desc = "DNA Quant, water in 1st well, no custom message")
        # ,
        # "yeast_growth" = list()
)