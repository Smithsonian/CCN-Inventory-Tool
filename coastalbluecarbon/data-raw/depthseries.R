## code to prepare `depthseries` dataset goes here

depthseries <- readr::read_csv("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/CCRCN_depthseries.csv")

usethis::use_data(depthseries, overwrite = TRUE)
