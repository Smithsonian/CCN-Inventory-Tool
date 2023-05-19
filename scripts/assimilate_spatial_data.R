## Spatial Data Assimilation

# Goal: summarize spatial data to tables of land use cover by country

library(tidyverse)

## creating lookup table for habitat area per country

library(countrycode)

mangrove_world_area <- readxl::read_xls("ccn_inventory_tool/data/mangroveWorld_Area.xls") %>% 
  mutate(habitat = "mangrove",
         area_ha = SUM_ST_AREA_SH/10000) %>% 
  rename(country = COUNTRY) %>% # same column used to assign geography to cores
  select(-c(Rowid, FID, FREQUENCY, SUM_ST_AREA_SH)) %>% 
  drop_na(country) # 13.9km^2 were not assigned geography

marsh_world_area <- read_csv("ccn_inventory_tool/data/testdata_country_marsh_area.csv") %>% 
  mutate(country = strsplit(x = country, split = ",")) %>% 
  unnest(country) %>% 
  mutate(country = trimws(country),
         area_ha = as.numeric(area_ha)
  )

combo_hab_area <- bind_rows(mangrove_world_area, marsh_world_area) %>% 
  # correct some country names so they pull the right ISO code
  mutate(country = case_when(country == "Bonaire" ~ "Caribbean Netherlands",
                             country == "Galapagos" ~ "Ecuador",
                             country == "Hawaii" ~ "United States",
                             country == "Saint Martin" ~ "Sint Maarten",
                             country == "Micronesia" ~ "Federated States of Micronesia",
                             country == "Phoenix Group" ~ "Kiribati",
                             country == "Andaman and Nicobar" ~ "India",
                             TRUE ~ country)) %>% 
  mutate(iso3c = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>% 
  # resolve some duplication
  group_by(iso3c, habitat) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  ungroup() %>% 
  # standardize country names using the country code
  mutate(country = countrycode(iso3c, origin = 'iso3c', destination = "country.name")) %>% 
  arrange(country) %>% select(country, everything())

write_csv(combo_hab_area, "ccn_inventory_tool/app_input/testdat_country_hab_area.csv")


# library(sf)
# library(leaflet)
# 
# world_marsh <- sf::st_read("ccn_inventory_tool/data/shapefiles/WCMC027_Saltmarsh_v6_1/01_Data/WCMC027_Saltmarshes_Py_v6_1.shp")
# world_seagrass <- sf::st_read("ccn_inventory_tool/data/shapefiles/014_001_WCMC013-014_SeagrassPtPy2021_v7_1/01_Data/WCMC013014-Seagrasses-Py-v7_1.shp")
# 
# names(world_marsh)
# 
# ggplot() +
#   geom_sf(data = world_marsh[1,1],
#           linewidth = 0.5)
# 
# # world_marsh[1,1] %>% 
# # st_transform(4326)
# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(world_marsh[1,1])
