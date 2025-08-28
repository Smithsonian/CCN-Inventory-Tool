##################

# Global script for app

## Dependencies ----

# load libraries

# Try to keep the dependencies to a minimum 
# only use well-established R packages (presents less issues for the SI server)
library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(knitr)
library(sf)
library(bslib)
# library(geojsonsf)
# library(shinyBS)

# source utilities
source("utils.R")

## Data ----

# load data
app_data <- readRDS("data/app_data.rds")

# Extract components from RDS
main_table <- app_data$main_table %>% arrange(territory, habitat)
map_input <- app_data$map_input %>% filter(habitat %in% c("marsh", "mangrove", "seagrass"))
# map_polys <- app_data$map_polys
terr_bounds <- app_data$terr_bounds

## Map Input ----

# set world bounds for global map view
# sp_points <- st_as_sf(map_input, coords = c('longitude',"latitude")) # make points spatial
# st_crs(sp_points) <- 4326 # Give the points a coordinate reference system (CRS)
# world_bounds <- st_bbox(sp_points) %>% as.vector()

# Map popups?

## Links ----

link_ccn <- tags$a(
  shiny::icon("globe"), "Coastal Carbon Network", 
  href = "https://serc.si.edu/coastalcarbon",
  target = "_blank"
)

link_atlas <- tags$a(
  shiny::icon("map"), "Explore the Atlas",
  href = "https://shiny.si.edu/coastal_carbon_atlas/",
  target = "_blank"
)

link_contribute <- tags$a(
  shiny::icon("handshake"), "Contribute Data",
  href = "https://serc.si.edu/coastalcarbon/join-the-network",
  target = "_blank"
)
# other icons
# globe-pointer, leaf, file-excel, file-export

## Value Boxes ----

# https://bslib.shinyapps.io/build-a-box/

## accordion panel test
# items <- lapply(LETTERS, function(x) {
#   accordion_panel(paste("Section", x), paste("Some narrative for section", x))
# })
# 
# # First shown by default
# accordion(!!!items)
# # Nothing shown by default
# accordion(!!!items, open = FALSE)
# # Everything shown by default
# accordion(!!!items, open = TRUE)
# 
# # Show particular sections
# accordion(!!!items, open = "Section B")
# accordion(!!!items, open = c("Section A", "Section B"))

# https://rstudio.github.io/bslib/articles/tooltips-popovers/index.html

## Application state ----
# establish initial state values

## Utilities ----

## Modules ----

