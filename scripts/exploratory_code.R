
## Source prepare app data first...
source("scripts/prepare_app_data.R")

library(tidyverse)

selected_country <- "South Africa"

## All-inclusive figures

## country/territory? habitat? both?
## region if that's too many countries in one plot - check out the database report figs for inspo

## Estimated Habitat Area

main_table %>% 
  # filter(habitat == "marsh") %>% 
  filter(habitat == "marsh") %>%
  mutate(highlight = case_when(country == selected_country ~ T, T ~ F)) %>% 
  drop_na(area_ha) %>%
  
  ggplot(aes(area_ha, reorder(territory, area_ha), col = highlight)) + 
  geom_errorbar(aes(xmin = area_ha_lowerCI, xmax  = area_ha_upperCI), width = 0.1) +
  geom_point(size = 1, shape = 21, fill="white") +
  theme_bw()
  # facet_wrap(~habitat)
  
## Available Data Quantity

# if there is in-country data
main_table %>%
  filter(habitat == "mangrove") %>%
  drop_na(n_cores) %>% 
  mutate(norm_quantity = n_cores/area_ha*1000) %>% 

  ggplot(aes(norm_quantity, reorder(territory, norm_quantity))) +
  geom_point() +
  theme_bw()
# make it lollipop & highlight the selected country

## Tier II Stocks

# this should be generalized if we're going to display this for different habitats and data types
main_table %>% 
  # filter(habitat == "mangrove") %>%
  filter(habitat == "marsh") %>% 
  drop_na(soil_TierII_mean) %>%
  mutate(
  #   # country = recode(country, "Russian Federation" = "Russia"),
    territory = paste0(territory, ", n = ", n_cores)) %>% 
  #   territory = forcats::fct_reorder(territory, soil_TierII_mean)) %>%
  
  ggplot(aes(soil_TierII_mean, reorder(territory, soil_TierII_mean),
             xmin = soil_TierII_mean - soil_TierII_se, 
             xmax = soil_TierII_mean + soil_TierII_se)) +
  # geom_rect(aes(xmin = 351, xmax = 424, ymin = -Inf, ymax = Inf),
  #           fill = "red", alpha = 0.005) +
  # geom_vline(aes(xintercept = 386), col = "red") +
  geom_point(size = 1) +
  geom_errorbar() +
  xlab("1m Mangrove Soil Carbon Stocks (Mg/ha)") + ylab("") +
  theme_bw(base_size = 12)
