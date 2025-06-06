## Materials and Methodology 

This page details the data sources and methodology used to develop and estimate habitat areas and sediment carbon stocks, including data sources and term definitions. 

### Disclaimer

Country and territory names used in this map and dashboard have been aligned with current ESRI guidelines.The boundaries and names shown and the designations used on this map do not imply the expression of any opinion whatsoever on the part of the Coastal Carbon Network or the Smithsonian Institution concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. 

### Data Sources 

Multiple data sources were compiled in order to synthesize data for this tool. Worthington et al 2021 was used to estimate salt marsh area data, Bunting et al 2018 was used to estimate mangrove area data, and McKenzie et al 2021 was used to estimate seagrass area data, when available. Soil and vegetation data available in the Coastal Carbon Atlas, hosted by the Coastal Carbon Network, was used to estimate mean country-level sediment carbon stocks. This data was filtered to exclude cores shorter than 1 meter, in order to align with IPCC global carbon stock value standards. 

Modeled carbon stock values, estimated Tier III sediment carbon stocks, were estimated from Maxwell et al 2021 and Sanderman et al 2018 for all applicable territories. These estimated values were derived from remote sensing data present in these two syntheses, Maxwell et al 2021 for mangrove habitats, and Sanderman et al 2018 for tidal salt marsh habitats. 

When neither Tier II or Tier III values were available for a habitat in a geographic territory, the global IPCC sediment carbon stock value for that habitat is applied. 

### Mapping 

The interactive map featured in this tool was created using the leaflet package in R. OSM (default) and CartoDB provider tiles were used to create a global background layer. 

EEZ data source and "Countries" data source.
Spatial interpolation of unmonitored territories/watersheds. 

### Estimating Sediment Carbon Stocks

To estimate territory-level sediment carbon stocks, data from the Coastal Carbon Atlas and Data Library was used. 

### Definitions: Total Stocks 

Habitat Area (ha) : In hectares, area of specified habitat in selected territory.

Mean Stock (Mg/ha) : Mean soil carbon stock value in milligrams per hectare

Mean Stock Upper CI (Mg/ha) : Upper confidence interval for mean soil carbon stock value

Mean Stock Lower CI (Mg/ha): Lower confidence interval for mean soil carbon stock value 

CO2eq (TgC): (column name will be changed?) 

Reporting Tier: Level of reporting resolution for carbon stocks. Tiers listed include IPCC global values, country-level stock calculations, and modeled values. Tier I stocks correspond to IPCC global values, Tier II stocks correspond to country-level stock calculations, Tier III stocks correspond to modeled values. The reporting tier listed is the highest available resolution for a given territory and habitat. 

Reporting Insight: Specifies whether country-specific or modeled carbon stock averages overlap with IPCC global values

### Visualizations 


