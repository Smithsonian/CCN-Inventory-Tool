##################

# This is the global script

## Prepare Workspace ----

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
library(rnaturalearth)
library(knitr)
library(sf)

# Configure ----


## Data ----

# load data
app_data <- readRDS("data/app_data.rds")

# Extract components from RDS
main_table <- app_data$main_table
map_input <- app_data$map_input
ccn_map <- app_data$ccn_map

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
#                 `CO2eq SE (TgC)` = round(stock_MgHa_se * area_ha * 3.67 / 10^6, 2)) %>%

# Map polygons

#Updating map polygons to include missing territories, downloading specified resolution (RC)
# world <- rnaturalearth::ne_download(scale = 50) 
#   
# world_ne <- world %>% 
#   dplyr::select(country = SOVEREIGNT,
#                 territory = ADMIN) %>% 
#   dplyr::mutate(country = recode(country, "United States of America" = "United States"))

## Switching natural earth map for analytics repo shp files// keeping object names the same 
world_ne <- ccn_map 

## potential issue with tibble and geometry format conflicting 5/5 ^^^




# Map popupsas_tibble()# Map popups?

## Application state ----

# establish initial state values

## Source utilities ----

## Source modules ----

## Test modules ----

## Archived Code ----

# library(rlang)
# library(shinydashboard)
# library(shinyjs)
# library(purrr)
# library(echarts4r)
# library(glue)
# library(sf)
# library(mapboxer)
# library(reactable)
# library(shinyWidgets)

# tier1data <- app_data$tier1data
# tier2data <- app_data$tier2data
# tier3data <- app_data$tier3data
# emissionsfactors <- app_data$emissionsfactors
# totalstocks <- app_data$totalstocks
# landuse <- app_data$landuse
# citations <- app_data$citations
# map_stocks <- app_data$map_cores

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
# full_join(tier1data, tier2data) %>% 
# full_join(tier3data) %>% 

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
# left_join(emissionsfactors) %>% 
# left_join(landuse) %>% 
#^above calculations done in prepare_app_data and CCN-Data-Analytics 
#join tier1 country level and tier2 country level 
# dplyr::rename(area_ha_lowerCI = hectare_LowerCI,
#               area_ha_upperCI = hectare_UpperCI) %>% 
# select(-c(area_ha))

##### Creating Table for Data Status Tab, tracking quality, quantity, and resolution of data 
#   for chosen country 

#data quantity in CCA 
# habitat_counts <- map_stocks %>% 
#   #dplyr::filter(country == input$chosen_country) %>% 
#   dplyr::filter(!is.na(country)) %>% select(c(country, habitat, core_id, site_id))
# 
# n_cores_habitat <- habitat_counts %>% 
#   dplyr::count(country, habitat) %>% dplyr::rename(n_cores = n)
# 
# n_cores_country <- habitat_counts %>% 
#   dplyr::count(country) %>% dplyr::rename(n_cores = n) %>% 
#   mutate(habitat = "total")

#create table to use in server.R -->> FIX THIS 
# datastatus_counts <- map_input %>% 
# dplyr::count(carbon_pool, habitat, country)

# full_join(n_cores_country, n_cores_habitat) %>% 
# arrange(country)
#filter(!(habitat == "total"&is.na(n_cores)))
