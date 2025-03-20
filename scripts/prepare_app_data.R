######## CCN Inventory Tool ########

## Prepare Data for App Input
## wolfejax@si.edu, cheneyr@si.edu

library(plyr)
library(dplyr)
library(readr)
library(tidyr)

## Load Data ####

core_stocks_raw <- read_csv("data/soilstocks_1m.csv") # need to update these tables when 1.5.0 is published?
biomass_stocks_raw <- read_csv("data/app_biomass_input.csv")

all_stocks_raw <- read_csv("app/data/all_stocks_table.csv") %>% 
  # correct some country and territory names
  mutate(country = recode(country,
                          "Russian Federation" = "Russia",
                           "Micronesia" = "Federated States of Micronesia"),
         territory = recode(territory,
                          "Russian Federation" = "Russia",
                          "Micronesia" = "Federated States of Micronesia"))

# pull cores and depthseries tables from dev branch
# guess_max <- nrow(read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_depthseries.csv"))
# cores <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_cores.csv", guess_max = guess_max)
# ds <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_depthseries.csv", guess_max = guess_max)
# impacts <- read_csv("https://raw.githubusercontent.com/Smithsonian/CCN-Data-Library/develop/data/CCN_synthesis/CCN_impacts.csv", 
#                     guess_max = guess_max, na = "NA")
  #habitat_area <- read_csv("data/testdat_country_hab_area.csv") replaced with mangrove-marsh-seagrass version
  # bibliography_raw <- read_csv("data/citations_by_country.csv")
  
## Utility Functions ####

se <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

#create country list
geokey <- all_stocks_raw %>% distinct(country, territory) %>% 
  drop_na(country) %>% 
    # assign continent and region
    mutate(region = countrycode::countrycode(sourcevar = country,
                                             origin = "country.name",
                                             destination = "region"),
           continent = countrycode::countrycode(sourcevar = country,
                                                origin = "country.name",
                                                destination = "continent")) %>% 
  select(continent, region, everything()) %>% 
  arrange(continent, region, country)
# we can add a watershed key when the time comes to include that

## Prepare Tables ####

## Curate Tier II Data/CCA Data 

# Soils
core_stocks <- core_stocks_raw %>%  
  # dplyr::rename(soil_stock_1m_Mgha = stock_MgHa) %>% 
  drop_na(stock_MgHa) %>%
  # left_join(core_impacts) %>% 
  # left_join(site_impacts) %>% 
  mutate(
    # impact_class = coalesce(impact_class, site_impact_class),
         carbon_pool = "soil") %>%
  select(carbon_pool, study_id, site_id, core_id, latitude, longitude, max_depth, 
         habitat, country, admin_division, stock_MgHa) 

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

## ... Map Input ####

## Combine country-level activity data (stocks) into one table
map_input <- bind_rows(core_stocks, biomass_stocks) %>% 
  mutate(impact_class = case_when(is.na(impact_class) ~ "unknown",
                                  impact_class == "NA" ~ "unknown",
                                  T ~ impact_class)) %>%
  #        tier = "country") %>% 
  #rename(ecosystem = habitat) %>% #standardizing column names across tables? 
  # select(-c(plot_id, year, impact_class)) %>% 
  select(country, admin_division, everything())

## ... Main Table ####

veg_smry <- biomass_stocks %>% 
  dplyr::group_by(country, habitat) %>% 
  dplyr::summarise(veg_TierII_mean = mean(stock_MgHa), 
            veg_TierII_se = se(stock_MgHa))

main_table <- all_stocks_raw %>% 
  filter(habitat != "total") %>% 
  left_join(veg_smry) %>% 
  left_join(geokey) %>% 
  dplyr::rename(area_ha = hectare,
                area_ha_lowerCI = hectare_LowerCI,
                area_ha_upperCI = hectare_UpperCI,
                n_cores = n,
                "soil_TierII_mean" = "stock_MgHa_mean",
                "soil_TierII_se" =  "stock_MgHa_se" ,
                "soil_TierII_upperCI" = "stock_MgHa_upper_CI",
                "soil_TierII_lowerCI" = "stock_MgHa_lower_CI",
                "soil_TierIII_mean" = "TierIII_mean",
                "soil_TierIII_lowerCI" = "TierIII_LowerCI",
                "soil_TierIII_upperCI" = "TierIII_UpperCI",     
                "soil_TierI_mean" = "TierI_mean",
                "soil_TierI_lowerCI" = "TierI_LowerCI",
                "soil_TierI_upperCI" = "TierI_UpperCI") %>% 
  # add biomass data
  ## Mangrove IPCC Tier I Biomass EF: Total (511) - Soil (386) = Veg (125) Mg/Ha
  mutate(veg_TierI_Mean = case_when(habitat == "mangrove" ~ 125, T ~ NA)) %>% 
  select_if(~!all(is.na(.))) %>% 
  select(-c(contains("gtlt"), contains("overlaps"), "TierIorII", "text_position")) %>% 
  select(continent, region, country, territory, everything())

# extract total stock values for total country and each habitat
# Questions: 
# seagrass not included in total country stock calcs?
# what are the units of Total_Stocks?
territory_tec <- all_stocks_raw %>%
  filter(habitat == "total") %>% # 1 total value per territory?
  select_if(~!all(is.na(.))) %>% 
  full_join(geokey) %>% 
  select(-habitat) %>% 
  select(continent, region, country, everything())

## ... Export App Data ####

app_data <- list(
  main_table = main_table,
  territory_tec = territory_tec,
  map_input = map_input
  )
# tier2data = tier2stocks,
# tier1data = tier1stocks,
# tier3data = tierIII,
# emissionsfactors = EF_table,
# totalstocks = total_stock_habitat_country,
# landuse = habitat_area,
# citations = bibliography_raw,
# map_cores = stocks

# export
saveRDS(app_data, file = "app/data/app_data.rds")
# rm(app_data) #removes from environment to run global script 


## Archived Code ---------

# Moving towards compiling app input verses splitting it into multiple tables
# Easier to keep track of everything with one or two main tables


## Curate area table- making names match
# habitat_area <- all_stocks_raw %>% 
#   select(country, territory, habitat, hectare, hectare_UpperCI, hectare_LowerCI) %>% 
#   dplyr::rename(area_ha = hectare) 


## Separate out Tier I Data - using stocks table from CCN-Data-Analytics 

# tier1stocks <- all_stocks_raw %>% 
#   filter(TierIorII == "Tier I") %>% 
#   janitor::remove_empty(which = c("rows", "cols")) %>% 
#   select(country, territory, habitat, TierI_mean, TierI_LowerCI, TierI_UpperCI) %>% 
#   mutate(tier = "TierI",
#          carbon_pool = "soil", #leave in? 
#   ) %>% 
#   dplyr::rename(stock_MgHa_mean = TierI_mean,
#                 stock_MgHa_lowerCI = TierI_LowerCI,
#                 stock_MgHa_upperCI = TierI_UpperCI)


## Curate Tier I Data - Global Values

# no_data_countries <- habitat_area %>% 
#   select(country, ecosystem) %>% 
#   filter(!country %in% unique(stocks$country))
# 
# # create placeholder table
# global_values <- stocks %>% drop_na(habitat) %>% 
#   distinct(country, habitat, carbon_pool) %>% 
#   bind_rows(no_data_countries %>% mutate(carbon_pool = "soil")) %>% 
#   bind_rows(no_data_countries %>% mutate(carbon_pool = "vegetation")) %>% 
#   # placeholder numbers, need checking
#   # also unsure about biomass activity data
#   mutate(stock_MgHa = case_when(habitat == "mangrove" & carbon_pool == "soil" ~ 396,
#                                 habitat == "marsh" & carbon_pool == "soil" ~ 297, 
#                                 T ~ NA),
#          tier = "global") %>% 
#   arrange(country, habitat)  



## Curate Tier II Data  - Country-level, Using stocks table calculated in CCN-Data-Analytics repo

# tier2stocks <- all_stocks_raw %>% 
#   filter(TierIorII == "Tier II") %>% 
#   #select(-c(hectare, hectare_UpperCI, hectare_LowerCI))
#   select(country, territory, habitat, stock_MgHa_mean,
#          stock_MgHa_se, stock_MgHa_upper_CI, stock_MgHa_lower_CI) %>% 
#   dplyr::rename(
#     stock_MgHa_upperCI = stock_MgHa_upper_CI,
#     stock_MgHa_lowerCI = stock_MgHa_lower_CI) %>% 
#   mutate(tier = "TierII",
#          carbon_pool = "soil") %>% 
#   select(country, territory, habitat, tier, carbon_pool, everything())
# 
# #list of countries with no tier II data available 
# tier2_list <- tibble(country = unique(tier2stocks$country))
# 
# no_tier2 <- anti_join(habitat_area, tier2_list) %>% #list of countries included in habitat area without a tier 2 value associated 
#   select(country) %>% unique()
# #reference for country insights


## Curate compliled EF and Total Stock for country and habitat 

# 
# total_stock_habitat <- all_stocks_raw %>% 
#   filter(!habitat == "total") %>% # isolating habitat-level totals per country 
#   select(country, territory, habitat, Total_Stocks, 
#          Total_Stockers_LowerCI, Total_Stockers_UpperCI, Total_Stocks_se) %>% 
#   dplyr::rename(total_stocks = Total_Stocks, #in MgHa?
#                 total_stocks_lower = Total_Stockers_LowerCI,
#                 total_stocks_upper = Total_Stockers_UpperCI,
#                 total_stocks_se = Total_Stocks_se)
# 
# total_stock_habitat_country <- full_join(total_stock_habitat, total_stocks_country) %>% 
#   arrange(country)
# 
# 
# #Compiled emissions factors and habitat 
# EF_table <- all_stocks_raw %>% 
#   select(country, territory, habitat, compiled_EF, compiled_LowerCI, compiled_UpperCI) %>% 
#   filter(complete.cases(compiled_EF))
# 
# 
# ## Curate Tier III Remote Sensing values
# # countries with available Tier III values, from Sanderman et al 2018 and 
# tierIII <- all_stocks_raw %>% 
#   select(c(country, territory, habitat, TierIII_mean, TierIII_LowerCI, TierIII_UpperCI, 
#            tierIII_gtlt_tier_II, tierII_overlaps_tierIII, tierIII_gtlt_tier_I, tierIII_overlaps_tierI)) %>% 
#   filter(complete.cases(TierIII_mean)) %>% 
#   dplyr::rename(stock_MgHa_mean = TierIII_mean,
#                 stock_MgHa_lowerCI = TierIII_LowerCI,
#                 stock_MgHa_upperCI = TierIII_UpperCI) %>% 
#   mutate(tier = "Tier III") 


## attempt to join impact classes...incorporate this later
# core_impacts <- impacts %>% 
#   drop_na(site_id) %>% drop_na(core_id) %>% 
#   #select(-impact_notes) %>% 
#   group_by(study_id, site_id, core_id) %>% 
#   dplyr::summarise(impact_class = paste(unique(impact_class), collapse = ", "))
# 
# site_impacts <- impacts %>% 
#   filter(is.na(core_id)) %>% select(-core_id) %>% 
#   group_by(study_id, site_id) %>% 
#   dplyr::summarise(site_impact_class = paste(unique(impact_class), collapse = ", "))


