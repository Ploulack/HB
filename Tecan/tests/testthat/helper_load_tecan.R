library(tidyverse); library(purrr)

setwd("~/Documents/R/HB/Tecan/")
source("tecan/tecan_extract.R")
source("tecan/tecan_values.R")
source("protocols/protocols_values.R")
source("protocols/pooling.R")
source("helpers/plates_helpers.R")

#Get tecan protocols names
dna_tecan <- tecan_protocols %>%
        keep(~.x == "260") %>%
        names()
yeast_tecan <- tecan_protocols %>%
        keep(~.x == "600") %>%
        names()

#Aggregate examples into a list
tecan_examples <- list(
        "quant_multi" = list(xml = read_xml("tests/testthat/tecan_xml_files/2018-01-15 13-01-46_plate_1.xml"),
                             type = dna_tecan,
                             desc = "DNA Quantification, No water well, with custom message with plate number"),

        "quant_normal" = list(xml = read_xml("tests/testthat/tecan_xml_files/2017-12-20 11-31-55_plate_1.xml"),
                              type = dna_tecan,
                              desc = "DNA Quant, water in 1st well, no custom message"),

        "yeast_growth" = list(xml = read_xml("tests/testthat/tecan_xml_files/2017-12-13 14-40-15_plate_1.xml"),
                              type = yeast_tecan,
                              desc = "Yeast Growth, all values below .2")
) %>% map(~list_modify(.x, data = tecan_data(.x$xml)))

#Examples of xml files that don't work
tecan_error_examples <- list(
        "truncated" = list(file = "tests/testthat/tecan_xml_files/2018-01-31 11-02-37_plate_1 _ truncated.xml",
                           desc = "Completely truncated.",
                           error = "Premature end of data in tag Version line 68")
)
