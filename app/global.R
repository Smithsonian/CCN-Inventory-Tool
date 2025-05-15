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

# source utilities
source("utils.R")

## Data ----

# load data
app_data <- readRDS("data/app_data.rds")

# Extract components from RDS
main_table <- app_data$main_table
map_input <- app_data$map_input
map_polys <- app_data$map_polys

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

