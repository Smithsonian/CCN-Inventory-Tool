
## Source prepare app data first...
source("scripts/prepare_app_data.R")

library(tidyverse)

selected_country <- "Belize"

## Country-specific Figures ####

## Data Metrics

map_input %>% filter(country == selected_country) %>% 
  dplyr::count(carbon_pool, habitat, country) %>% 
  
  ggplot2::ggplot() + 
  geom_col(aes(habitat, n, fill = carbon_pool)) +
  # geom_errorbar(aes(x= habitat, ymin = cores, ymax = hectare_UpperCI, y= area_ha), width = 0.1) +
  coord_flip() +
  ylab("Number of Samples") + theme_bw(base_size = 20) +
  theme(legend.position = "bottom")

## Emissions Factors (Carbon Stocks)

main_table %>% filter(country == selected_country) %>% 
  select(-c(contains("gtlt"), contains("overlaps"), "TierIorII", "text_position")) %>%
  select(habitat, contains("stock"), contains("Tier")) %>% 
  select(-contains("Total")) %>% 
  pivot_longer(cols = -habitat, names_to = "tier", values_to = "stock") %>% 
  separate(tier, into = c("carbon_pool", "tier", "stat"), sep = "_") %>% 
  pivot_wider(id_cols = c("habitat", "carbon_pool", "tier"), names_from = stat, values_from = stock) %>% 
  
  ggplot2::ggplot(aes(mean, habitat, col = tier)) +
  # geom_boxplot(aes(stock_MgHa, habitat, col = `carbon pool`)) +
  geom_errorbar(aes(xmin = lowerCI, xmax  = upperCI), width = 0.1) +
  geom_point(size = 2, shape = 21, fill="white") +
  theme_bw() +
  facet_wrap(~`carbon_pool`, 
             # scales = "free", 
             dir = "v")

## Activity Data (Landuse/Habitat Area)

main_table %>% filter(country == selected_country) %>% 
  ggplot2::ggplot() + 
  geom_col(aes(habitat, area_ha, fill = habitat)) +
  geom_errorbar(aes(x= habitat, ymin = area_ha_lowerCI, ymax = area_ha_upperCI, y= area_ha), width = 0.1) +
  coord_flip() +
  ylab("Area (hectares)") + theme_bw(base_size = 20) +
  theme(legend.position = "bottom")

## Regional/Global Figures ####

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
  filter(habitat == "mangrove") %>%
  # filter(habitat == "marsh") %>% 
  drop_na(soil_TierII_mean) %>%
  mutate(
  #   # country = recode(country, "Russian Federation" = "Russia"),
    territory = paste0(territory, ", n = ", n_cores)) %>% 
  #   territory = forcats::fct_reorder(territory, soil_TierII_mean)) %>%
  
  ggplot(aes(soil_TierII_mean, reorder(territory, soil_TierII_mean),
             xmin = soil_TierII_mean - soil_TierII_se, 
             xmax = soil_TierII_mean + soil_TierII_se)) +
  geom_rect(aes(xmin = 351, xmax = 424, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.005) +
  geom_vline(aes(xintercept = 386), col = "red") +
  geom_point(size = 1) +
  geom_errorbar() +
  xlab("1m Mangrove Soil Carbon Stocks (Mg/ha)") + ylab("") +
  theme_bw(base_size = 12)
