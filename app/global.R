##################

# This is the global script

## Prepare Workspace ----

# load libraries

library(shiny)
# library(shinydashboard)
library(leaflet)
library(DT)
# library(rlang)
library(plotly)
library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
# library(shinyjs)
# library(purrr)
# library(echarts4r)
# library(glue)
# library(sf)
# library(mapboxer)
# library(reactable)
# library(shinyWidgets)

# Configure ----


## Data ----

# load data
app_data <- readRDS("data/app_data.rds")

# Extract components
tier2data <- app_data$tier2data
tier1data <- app_data$tier1data
emissionsfactors <- app_data$emissionsfactors
totalstocks <- app_data$totalstocks
landuse <- app_data$landuse
map_input <- app_data$map_input
citations <- app_data$citations

# content for popups, selections, etc?
# load files to include?

## This workflow should move to the prepare app data script
# Calculate country stocks 
se <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# tier1country <- tier1data %>% 
#   # filter(country == selected_country) %>%
#   drop_na(stock_MgHa) %>% 
#   mutate(stock_MgHa_lower = case_when(habitat == "mangrove" ~ 351, 
#                                       habitat == "marsh" ~ 254, T ~ NA), 
#          stock_MgHa_upper = case_when(habitat == "mangrove" ~ 424, 
#                                       habitat == "marsh" ~ 297, T ~ NA)) %>% 
#   dplyr::rename(stock_MgHa_mean = stock_MgHa)
# 

# countrydata <- tier2data %>% 
#   tidyr::drop_na(country, habitat, stock_MgHa) %>% 
#   # dplyr::filter(country == selected_country) %>%
#   dplyr::group_by(country, tier, habitat, carbon_pool) %>%
#   dplyr::summarize(`sample size` = n(),
#                    stock_MgHa_mean = round(mean(stock_MgHa, na.rm = T), 2),
#                    stock_MgHa_se = round(se(stock_MgHa, na.rm = T), 2)) %>%
#   dplyr::mutate(stock_MgHa_lower = stock_MgHa_mean - stock_MgHa_se,
#                 stock_MgHa_upper = stock_MgHa_mean + stock_MgHa_se) %>% 
#   dplyr::ungroup() %>% 
#   
  # add tier 1 values
 # bind_rows(tier1data) %>% 
  
#^above calculations done in prepare_app_data and CCN-Data-Analytics 
#join tier1 country level and tier2 country level 
countrydata <- full_join(tier1data, tier2data) %>% 


  # # Add Activity Data and calculate inventory
  # left_join(landuse) %>% 
  # mutate(`habitat area (Ha)` = round(area_ha, 2),
  #        `stock avg (TgC)` = round(stock_MgHa_mean * area_ha / 10^6, 2),
  #        # stock_TgC_se = round(stock_MgHa_se * area_ha / 10^6, 2),
  #        # calculate co2 equivalent
  #        `CO2eq (TgC)` = round(stock_MgHa_mean * area_ha * 3.67 / 10^6, 2),
  #        `CO2eq SE (TgC)` = round(stock_MgHa_se * area_ha * 3.67 / 10^6, 2)) %>% 
  # dplyr::rename(`carbon type` = carbon_pool, 
  #               `stock (Mg/ha)` = stock_MgHa_mean,
  #               `stock SE (Mg/ha)` = stock_MgHa_se) %>% 
  # select(-c(
  #           # iso3c, 
  #           # stock_MgHa_lower, stock_MgHa_upper,
  #           # stock_MgHa_mean, stock_MgHa_se, 
  #           area_ha))

  
  
  ## Pull activity data and inventory, calculated in CCN-Data-Analytics -RC
  ## rename for app table 
  left_join(emissionsfactors) %>% #units?????
  left_join(landuse) %>% 
    mutate(`habitat area (Ha)` = round(area_ha, 2),
           `CO2eq (TgC)` = round(compiled_EF * area_ha * 3.67 / 10^6, 2))
  
  
  
  
# Mapping polygons
world_ne <- rnaturalearth::ne_countries(returnclass = "sf",
                                        # type = "countries",
                                        scale = "medium") %>% 
  dplyr::select(country = sovereignt) %>% 
  dplyr::mutate(country = recode(country, "United States of America" = "United States"))

## Application state ----

# establish initial state values


## Source utilities ----

## Source modules ----

## Test modules

