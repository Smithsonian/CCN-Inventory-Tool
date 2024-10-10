
selected_country <- "United States"

world_ne <- rnaturalearth::ne_countries(returnclass = "sf",
                         # type = "countries",
                         scale = "medium") %>% 
  dplyr::rename(country = sovereignt) %>% 
  dplyr::mutate(country = recode(country, "United States of America" = "United States"))


leaflet() %>% 
  # addTiles() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB, group = "CartoDB") %>%
  
  addPolygons(data = world_ne %>% filter(country == selected_country), 
              weight = 2, 
              # fill = FALSE, 
              group = "Outline") %>% 
  
  addCircleMarkers(data = tier2data %>% drop_na(latitude) %>% filter(country == selected_country), 
                   lng = ~longitude, lat = ~latitude, radius = 2,
                   group = "Cores") %>%
  
  addLayersControl(
    baseGroups = c("OSM (default)", "CartoDB"),
    overlayGroups = c("Cores", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )
