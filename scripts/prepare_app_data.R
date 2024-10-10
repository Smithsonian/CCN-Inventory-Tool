## Prepare Data for App Input

library(dplyr)
library(tidyr)
library(readr)

## Load Data ----

core_stocks_raw <- read_csv("data/soilstocks_1m.csv")
habitat_area <- read_csv("data/testdat_country_hab_area.csv")
biomass_stocks_raw <- read_csv("data/app_biomass_input.csv")
# any shapefiles?

# pull cores and depthseries tables from dev branch
guess_max <- nrow(read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_depthseries.csv"))
# cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_cores.csv", guess_max = guess_max)
# ds <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_depthseries.csv", guess_max = guess_max)
impacts <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_impacts.csv", 
                    guess_max = guess_max, na = "NA")

## Cleaning & Calculations ----

## Curate Tier II Data

## Soils
core_impacts <- impacts %>% 
  drop_na(site_id) %>% drop_na(core_id) %>% 
  select(-impact_notes) %>% 
  group_by(study_id, site_id, core_id) %>% 
  dplyr::summarise(impact_class = paste(unique(impact_class), collapse = ", "))

site_impacts <- impacts %>% 
  filter(is.na(core_id)) %>% select(-core_id, -impact_notes) %>% 
  group_by(study_id, site_id) %>% 
  dplyr::summarise(site_impact_class = paste(unique(impact_class), collapse = ", "))

core_stocks <- core_stocks_raw %>%  
  # dplyr::rename(soil_stock_1m_Mgha = stock_MgHa) %>% 
  drop_na(stock_MgHa) %>%
  left_join(core_impacts) %>% 
  left_join(site_impacts) %>% 
  mutate(impact_class = coalesce(impact_class, site_impact_class),
         carbon_pool = "soil") %>% 
  select(study_id, site_id, core_id, latitude, longitude, max_depth, 
         habitat, country, admin_division, impact_class, stock_MgHa, carbon_pool) 

# Vegetation
biomass_stocks <- biomass_stocks_raw %>% 
  filter(habitat != "seagrass") %>% 
  mutate(stock_MgHa = coalesce(total_C_trees, plot_biomass_carbon)) %>%
  mutate(stock_MgHa = case_when(is.na(stock_MgHa) ~ AGC_trees + BGC_trees, # include saplings?
                                T ~ stock_MgHa)) %>% 
  # select(total_C_trees, plot_biomass_carbon, stock_MgHa, everything())
  
  # filter(stock_MgHa != 0) %>% 
  mutate(country = case_when(study_id == "Cifuentes_et_al_2023_Panama" ~ "Panama",
                             study_id == "Cifuentes_et_al_2024_Nicoya" ~ "Costa Rica",
                             study_id == "Morrissette_et_al_2023" ~ "Belize", 
                             T ~ COUNTRY),
         impact_class = coalesce(land_use_class, ecosystem_health),
         core_id = case_when(study_id == "Cifuentes_et_al_2024_Nicoya" ~ plot_id,
                             study_id == "Cifuentes_et_al_2023_Panama" ~ paste(site_id, plot_id, sep = "_"),
                             T ~ core_id), 
         habitat = "mangrove",
         carbon_pool = "vegetation") %>% 
  select(study_id, site_id, plot_id, core_id, year, habitat, impact_class, latitude, longitude,
         stock_MgHa, country, carbon_pool)

## Combine country-level activity data (stocks) into one table
stocks <- bind_rows(core_stocks, biomass_stocks) %>% 
  mutate(impact_class = case_when(is.na(impact_class) ~ "unknown",
                                  impact_class == "NA" ~ "unknown",
                                  T ~ impact_class),
         tier = "country") %>% 
  select(-plot_id, -year, -impact_class, impact_class)

## Curate Tier I Data - Global Values

no_data_countries <- habitat_area %>% 
  select(country, habitat) %>% 
  filter(!country %in% unique(stocks$country))

# create placeholder table
global_values <- stocks %>% drop_na(habitat) %>% 
  distinct(country, habitat, carbon_pool) %>% 
  bind_rows(no_data_countries %>% mutate(carbon_pool = "soil")) %>% 
  bind_rows(no_data_countries %>% mutate(carbon_pool = "vegetation")) %>% 
  # placeholder numbers, need checking
  # also unsure about biomass activity data
  mutate(stock_MgHa = case_when(habitat == "mangrove" & carbon_pool == "soil" ~ 396,
                                habitat == "marsh" & carbon_pool == "soil" ~ 297, 
                                T ~ NA),
         tier = "global") %>% 
  arrange(country, habitat)  

## Map input ----

map_input <- core_stocks_raw %>% 
  drop_na(country) %>% 
  dplyr::group_by(country) %>% 
  # get min and max latitudes for each core
  dplyr::summarise(longitude_max = max(longitude, na.rm = T),
            longitude_min = min(longitude, na.rm = T),
            latitude_max = max(latitude, na.rm = T),
            latitude_min = min(latitude, na.rm = T)
  )

## Export Data for App Use ----

app_data <- list(
  tier2data = stocks,
  tier1data = global_values,
  landuse = habitat_area,
  map_input = map_input
)

# export
saveRDS(app_data, file = "app/data/app_data.rds")
