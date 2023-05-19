## Script to test visualizations for the Inventorying tool

library(tidyverse)
library(plotly)
library(leaflet)

core_stocks <- read_csv("ccn_inventory_tool/app_input/core_stocks.csv")
worldwetlands <- read_csv("data/testdat_country_hab_area.csv")

country_smry <- core_stocks %>%
  drop_na(country) %>%
  group_by(country, iso3c) %>%
  summarize(core_count = n(),
            habitat_types = paste(unique(habitat), collapse = ", ")
            )

## fun with plotly

# read in habitat area
# mangrove_world_area <- readxl::read_xls("ccn_inventory_tool/data/mangroveWorld_Area.xls") %>%
#   mutate(habitat = "mangrove",
#          habitat_area_ha = SUM_ST_AREA_SH/10000) %>%
#   rename(country = COUNTRY) %>% # same column used to assign geography to cores
#   select(-c(Rowid, FID, FREQUENCY, SUM_ST_AREA_SH)) %>%
#   drop_na(country) # 13.9km^2 were not assigned geography

countrydata <- country_smry %>%
  full_join(worldwetlands %>% distinct(country, iso3c)) %>%
  # filter(country != "United States") %>%
  mutate(tier = ifelse(!is.na(core_count), 2, 1),
         data_availability = ifelse(is.na(core_count), "No", "Yes"),
         core_count = ifelse(is.na(core_count), 0, core_count),
         hover = ifelse(core_count != 0,
                        paste(country, '<br>',
                       "Cores: ", core_count,
                       '<br>', "Tier: ", tier,
                       '<br>', "Habitats: ", habitat_types),
                       paste(country, '<br>', "No available data")))

plot_ly(countrydata) %>%
  add_trace(
    type='choropleth',
    locations = ~iso3c,
    z = ~core_count,
    text= ~hover,
    color = ~core_count,
    colorscale = "Blues",
    reversescale = T
    )
  # colorbar(title = "Available Cores") %>%
  # layout(
  #   title = 'Number of Available Cores per Country<br>(Hover for breakdown)',
  #   geo = list(scope = 'world')
  # )

# try a discrete plot
plot_ly(countrydata,
        type = "choropleth",
        locations = ~iso3c,
        # z = ~data_availability,
        text= ~hover,
        color = ~data_availability,
        marker = list(colorscale = 'Accent'))
  # add_trace(
  #   type = "choropleth",
  # )
#

# Ehh lets do it in leaflet lol

# Create a color palette for the map:
mypalette <- colorNumeric(palette="viridis", domain = countrydata$core_count, na.color="transparent")
# mypalette(c(45,43))

# Basic choropleth with leaflet?
leaflet(countrydata) %>%
  addTiles()  %>%
  # setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = ~mypalette(core_count), stroke=FALSE )


labels <- sprintf(
  "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
  states$name, states$density
) %>% lapply(htmltools::HTML)

m <-

m <- m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

