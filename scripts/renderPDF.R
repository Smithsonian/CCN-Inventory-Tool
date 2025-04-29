## this script renders all country-level pdfs for use in app
#cheneyr@si.edu

library(tidyverse)
library(rmarkdown)
library(RefManageR)
library(readr)
library(webshot)
library(mapview)
tinytex::tlmgr_install("pdfcrop")


## .. 1. Run functions in environment 

#write bibs 
writeBibs <- function(chosen_geography){
  
  citations_geog <- territory_citations %>% filter(territory == chosen_geography) %>% 
    left_join(geographies_list) %>% 
    select(-territory) %>% 
    mutate(bibliography_id = str_replace_all(bibliography_id, "\\.", ""), #fixing misc formatting errors 
           bibliography_id = str_replace_all(bibliography_id, "\\ ", "_")) %>% 
    mutate(uniqueID = if_else(is.na(study_id), bibliography_id, paste(bibliography_id, study_id, ecosystem, sep = "_")))
  
  citations_geog <- as.BibEntry(citations_geog %>% 
                                  column_to_rownames("uniqueID"))
  
  WriteBib(citations_geog, file = paste0(chosen_geography, ".bib"))
}

#render maps 

#map 1 -> when there is no CCA data
renderMap1 <- function(chosen_geog){
  select_map_territory <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium") %>% 
    dplyr::select(territory = admin) %>% 
    dplyr::mutate(territory = recode(territory, "United States of America" = "United States")) %>% 
    filter(territory == chosen_geog)
  
  geographies_list <- allstocks %>% select(country, territory) %>% 
    distinct()
  chosen_country <- geographies_list$country
  
  map <- leaflet() %>% 
    # basemap options
    addTiles(group = "OSM (default)") %>% #not interactive, do not need multiple layers 
    
    #add polygon layer for selected territory border 
    addPolygons(data = select_map_territory, weight = 2)
  
  #save as png to use in md pdf - does not allow leaflet (html widgets)
  mapshot(map, file = paste0("app/data/reports/", chosen_geog, ".png"))
  
}

#map 2 -> when there is CCA data 
renderMap2 <- function(chosen_geog){
  select_map_territory <- rnaturalearth::ne_countries(returnclass = "sf", scale = "medium") %>% 
    dplyr::select(territory = admin) %>% 
    dplyr::mutate(territory = recode(territory, "United States of America" = "United States")) %>% 
    filter(territory == chosen_geog)
  
  geographies_list <- allstocks %>% select(country, territory) %>% distinct()
  
  chosen_country <- geographies_list %>% filter(territory == chosen_geog) %>% select(country)
  chosen_country <- as.character(chosen_country)
  
  chosen_map_input <- map_input %>% filter(country == chosen_country)
  
  map <- leaflet() %>% 
    # basemap options
    addTiles(group = "OSM (default)") %>% #not interactive, do not need multiple layers 
    
    #add polygon layer for selected territory border 
    #addPolygons(data = select_map_territory, weight = 2) %>% 
    addCircleMarkers(data = chosen_map_input, lat = ~latitude, lng = ~longitude)
  
  #save as png to use in md pdf - does not allow leaflet (html widgets)
  mapshot(map, file = paste0("app/data/reports/", chosen_geog, "2.png"))
  
}


#when data inclues tier II
writeInsightsDat <- function(chosen_geog, bib){
  
  bib <-paste0(chosen_geog, ".bib")
  # generate country pdf
  rmarkdown::render(input = "app/data/insights_tierII.Rmd",
                    output_file = paste0(chosen_geog, "_Report.html"),
                    output_dir = "app/www/reports/",
                    params = list(
                      country = chosen_geog
                    ))
}

#only tier I 
writeInsightsNOdat <- function(chosen_geog, bib){
  
  bib <-paste0(chosen_geog, ".bib")
  # generate country pdf
  rmarkdown::render(input = "app/data/insights_NO_tierII.Rmd",
                    output_file = paste0(chosen_geog,"_Report.html"),
                    output_dir = "app/www/reports/",
                    params = list(
                      country = chosen_geog
                    ))
}


## 2. create list of territories included ####
app_data <- read_rds("app/data/app_data.rds")
allstocks <- app_data$main_table
map_input <- app_data$map_input
rm(app_data)

#list of all countries with tier II data in at least 1 habitat 
#update to unique geographies? 
any_tierII_data <- allstocks %>% 
  filter(!is.na(soil_TierII_mean)) %>% 
  select(territory) %>% distinct()
  #select(country) %>% distinct()

geographies_with_NO_tierII_data <- allstocks %>% 
  filter(is.na(soil_TierII_mean)) %>% 
  select(territory) %>% distinct()
#^^ this still contains overlap

tier3geographies <- allstocks %>% 
  filter(!is.na(soil_TierIII_mean)) %>% 
  select(territory) %>% distinct()

#if case exists of tier II data, remove from NO data list 
onlytierI <- anti_join(geographies_with_NO_tierII_data, any_tierII_data)

IIIoverlapI <- semi_join(tier3geographies, onlytierI) #no overlap

## ....create lists to loop over #####

#distinct territories 
tierIgeographies <- onlytierI  %>% distinct() 
tierIIgeographies <- any_tierII_data %>% distinct()

#make sure there is no territory overlap 
check_tier <- semi_join(tierIIgeographies, tierIgeographies) # value should be 0

#create vectors 
tierIgeog_list <- as.vector(tierIgeographies) %>% unlist(use.names = FALSE)
tierIIgeog_list <- as.vector(tierIIgeographies) %>% unlist(use.names = FALSE)

#create tier 2 test list 
tierIITEST <- as.vector(c("Palau", "Ireland", "Vietnam", "Belize", "Australia", "Costa Rica", "South Africa", "El Salvador"))
tierITEST <- as.vector(c("Cabo Verde", "Peru", "Chile", "Nigeria", "Fiji"))

#### Spot fix citations

#all_citations
citations <- read_csv("data/citations_by_country.csv")
    #citations are at country level 

#list of geographies // country, territory 
geographies_list <- allstocks %>% select(country, territory) %>% distinct()

territory_citations <- citations %>% 
  mutate(country = case_when(country == "Micronesia"~ "Federated States of Micronesia",
                             grepl("Russia", country) ~ "Russia",
                             TRUE ~ country)) %>% 
  left_join(geographies_list) %>% 
  select(country, territory, everything())


##RUN DEMO LOOPS ####

# 1. write bibs

for (i in seq_along(tierIITEST)) {

  writeBibs(tierIITEST[i])
}

for (i in seq_along(tierITEST)) {
  
  writeBibs(tierITEST[i])
}

# 2. render maps 

for (i in seq_along(tierIITEST)) {
  
  {renderMap1(tierIITEST[i])}
}

# 
# for (i in seq_along(tierIITEST)) {
#   
#   {renderMap2(tierIITEST[i])}
# }

# 3. render reports

for (i in seq_along(tierITEST)) {
  
  {writeInsightsNOdat(tierITEST[i])}
}

for (i in seq_along(tierIITEST)) {
  
  {writeInsightsDat(tierIITEST[i])}
}



writeInsightsNOdat("Peru")



