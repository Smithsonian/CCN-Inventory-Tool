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
# library(rnaturalearth)
library(knitr)
library(sf)

## Data ----

# load data
app_data <- readRDS("data/app_data.rds")

# Extract components from RDS
main_table <- app_data$main_table
map_input <- app_data$map_input
map_polys <- app_data$map_polys

# configure main table
countrydata <- main_table %>% 
  dplyr::mutate(`habitat area (Ha)` = round(area_ha, 2),
                `CO2eq (TgC)` = round(compiled_EF * area_ha * 3.67 / 10^6, 2)) 

#   # upscale estimates using habitat area
#   dplyr::mutate(`habitat area (Ha)` = round(area_ha, 2),
#                 `stock avg (TgC)` = round(stock_MgHa_mean * area_ha / 10^6, 2),
#                 # stock_TgC_se = round(stock_MgHa_se * area_ha / 10^6, 2),
#                 # calculate co2 equivalent
#                 `CO2eq (TgC)` = round(stock_MgHa_mean * area_ha * 3.67 / 10^6, 2),
#                 `CO2eq SE (TgC)` = round(stock_MgHa_se * area_ha * 3.67 / 10^6, 2)) 

## Map Input ----

# set world bounds for global map view
sp_points <- st_as_sf(map_input, coords = c('longitude',"latitude")) # make points spatial
st_crs(sp_points) <- 4326 # Give the points a coordinate reference system (CRS)
world_bounds <- st_bbox(sp_points) %>% as.vector()

# Map popups?

## Application state ----
# establish initial state values

## Utilities ----

## Modules ----

