## Code to read in `big_epa_cars` dataset goes here

cores <- readr::read_csv("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/CCRCN_cores.csv")

usethis::use_data(cores, overwrite = TRUE)
