#
# This is the server logic of a Shiny web application. 

# Define server logic 
function(input, output, session) {

  # Initialize app state when nothing is selected yet
  # app_state <- reactiveValues({
  #   geography = chosen_country
  # })
  
  # compile these in to one reactiveValues object?
  
  ## creating popup when app opens// can this connect to the selectize input dropdown?
  observe({
    showModal(
      modalDialog(
        title = "Welcome to the Coastal Carbon Inventory Tool!",
        easyClose = T,
        "Please select your country or territory in the dropdown menu to see country-level carbon stocks estimates, data visualizations and more."
      )
    )
  }) 
  
  geo_bounds <- reactive({
    # map_polys %>% dplyr::filter(territory == input$chosen_geography)
    terr_bounds %>% dplyr::filter(territory == input$chosen_geography)
  })
  
  geography_subset <- reactive({
    if(nchar(input$chosen_geography)==0){
      stocktable <- main_table
    } else {
      stocktable <- main_table %>% dplyr::filter(territory == input$chosen_geography)
    } 
    return(stocktable)
  })

  points_subset <- reactive({
    map_input %>% dplyr::filter(territory == input$chosen_geography)
      # mutate(highlight = ifelse(territory == input$chosen_geography, T, F))
  })

  observeEvent(input$chosen_geography, {
    if(nchar(input$chosen_geography)!=0){
      updateTabsetPanel(session, "inTabset", selected = "stockpanel")
    }
  })
  
  # when resent map button is pushed
  # return to global stocks panel and reset geography selection input
  observeEvent(input$reset, {
    updateTabsetPanel(session, "inTabset", selected = "globalpanel")
    updateSelectInput(session, "chosen_geography", selected = c("Choose" =""))
  })
  
  ## Map -------------------

  # Feature: Zoom options for globe or particular country
  # initial map state --> world map with all cca samples   
  output$map <-
    
    renderLeaflet({

      leaflet() %>% 
        # basemap options
        addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
        addTiles(group = "OSM (default)") %>%
        
        # add data points (global cca samples)
        addCircleMarkers(data = map_input %>% filter(carbon_pool == "soil"),
                         lng = ~longitude, lat = ~latitude, radius = 2,
                         label = ~paste(habitat, carbon_pool, "data", sep = " "),
                         group = "Soil Samples", 
                         # eventually have veg be plotted separately for color coding
                         color = "#7570b3") %>% 
        addCircleMarkers(data = map_input %>% filter(carbon_pool == "vegetation"),
                         lng = ~longitude, lat = ~latitude, radius = 2,
                         label = ~paste(habitat, carbon_pool, "data", sep = " "),
                         group = "Plant Surveys", 
                         # eventually have veg be plotted separately for color coding
                         color = "#1b9e77") %>% 
        #add layer options 
        addLayersControl(
          baseGroups = c("OSM (default)", "CartoDB"),
          overlayGroups = c("Soil Samples", "Plant Surveys"), # "Border"
          options = layersControlOptions(collapsed = FALSE)
        )
    })
  
  # map_input <- map_input %>% mutate(highlight = ifelse(territory == input$chosen_geography, T, F))
  # pal <- colorFactor(palette = "Dark2", domain = dat()$highlight)
  
  ## An observe statement to update the map, filtering to chosen territory 
  observeEvent(input$chosen_geography, {
    
    bounds <- geo_bounds() %>% dplyr::select(-territory) %>% as.numeric() # define bounding box for polygon
    # json_geo <- sf_geojson(r(), atomise = F) # convert to GEOjson for plotting
    
    # using leafletProxy to call in original map 
    leafletProxy(mapId = "map") %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      # reset map layers
      # removeMarker(group = "highlight") %>%
      # clearControls() %>% 
      # clearShapes() %>% 
      # clearGeoJSON() %>% 

      # # add polygon layer
      # addPolygons(data = r(), weight = 2, fill = T, opacity = 0.1,
      #             group = "Border") %>%
      # addGeoJSON(json_geo, weight = 1, group = "Border") %>% 
      # addCircleMarkers(data = dat(), lng = ~longitude, lat = ~latitude, radius = 2, color = "red") %>%
      # # #auto zoom to selection
      # # fitBounds is more straightforward, but people love a smooth transition
  })
  
  
#reset to original world map, linked to action button   
 observeEvent(input$reset, {
    
   leafletProxy(mapId = "map") %>% 
     # clearGeoJSON() %>% 
     # clearShapes() %>% 
     # clearControls() %>% 
     fitBounds(world_bounds[1], world_bounds[2], world_bounds[3], world_bounds[4])
 })
  
 ## Plots ----------------
  
 ## Global Stock Plot
 # output$worldstockplot <- renderPlot({
 #   req(input$chosen_habitat)
 #   
 #   globalStocks(main_table, input$chosen_habitat)
 # 
 # })
 
 output$worldstockplot <- renderImage({ 
     list(src = globalStocksFig(input$chosen_habitat), height = "100%") 
   }, 
   deleteFile = FALSE 
 ) 
 
 
 ## Data Status
 ## quantity, quality, representation 
 # use value boxes instead to express: # soil cores; # habitats represented; # plots surveyed
 # the plot should be that of the area-normalized sampling effort
 output$datastatus <- renderPlotly({
   req(input$chosen_geography)
   
   #case where there are cores available in CCA 
   #if(input$chosen_country %in% datastatus_counts$country){
   dat() %>% 
     dplyr::count(carbon_pool, habitat, territory) %>% 
     plot_ly(x = ~habitat, y = ~n, type = "bar",
             color = ~carbon_pool) %>% 
     layout(xaxis = list(title = "Habitat Type"),
            yaxis = list(title = "Number of Samples"))
   # include legend even when theres just one type of sample
   # specify what a sample unit is: soils = core; vegetation = plot
   
   # dat() %>% 
   #   dplyr::count(carbon_pool, habitat, country) %>% 
   #   
   #   ggplot2::ggplot() + 
   #   geom_col(aes(habitat, n, fill = carbon_pool)) +
   #   # geom_errorbar(aes(x= habitat, ymin = cores, ymax = hectare_UpperCI, y= area_ha), width = 0.1) +
   #   coord_flip() +
   #   ylab("Number of Samples") + theme_bw(base_size = 20) +
   #   theme(legend.position = "bottom")
   
   # } else {
   #   table_other <- tibble(country = input$chosen_country,
   #                         availability = "There are currently no values for this country available in the Coastal Carbon Atlas. Go sample!",
   #                         data_tier_available = "Tier I")
   # }
 }) #%>% bindEvent(input$go)
  
 ## Emission Factor plot
 # what about veg?
 output$efplot <- renderPlotly({
   req(input$chosen_geography)
   
   plot_ly(geography_subset(), x = ~habitat, y = ~soil_TierI_mean, type = "bar", 
           error_y = ~list(array = soil_TierI_upperCI - soil_TierI_mean, 
                           arrayminus = soil_TierI_mean - soil_TierI_lowerCI, color = "black"),
           name = "IPCC global value") %>% 
     add_trace(y = ~soil_TierII_mean, 
               error_y = ~list(array = soil_TierII_upperCI - soil_TierII_mean, 
                               arrayminus = soil_TierII_mean - soil_TierII_lowerCI, color = "black"),
               name = "Country-specific value") %>% 
     add_trace(y = ~soil_TierIII_mean, 
               error_y = ~list(array = soil_TierIII_upperCI - soil_TierIII_mean, 
                               arrayminus = soil_TierIII_mean - soil_TierIII_lowerCI, color = "black"),
               name = "Modeled value") %>% 
     layout(
       # title = "",
       xaxis = list(title = "Habitat Type"),
       yaxis = list(title = "Soil Carbon Stock (Mg/ha)"))
   
 }) 
  
  ## Activity Data
  output$activityplot <- renderPlotly({
    req(input$chosen_geography)
    
    geography_subset() %>% 
      plot_ly(x = ~habitat, y = ~area_ha, type = "bar",
              # color = ~habitat, colors = "Set1",
              error_y = ~list(array = area_ha_upperCI - area_ha, 
                              arrayminus = area_ha - area_ha_lowerCI, color = "black")) %>% 
      layout(xaxis = list(title = "Habitat Type"),
             yaxis = list(title = "Area (ha)"))
    # custom formatting options
    # layout(xaxis = list(title = 'Habitat',
    #                     zerolinecolor = '#ffff',
    #                     zerolinewidth = 2,
    #                     gridcolor = 'ffff'),
    #        yaxis = list(title = 'Area (Ha)',
    #                     zerolinecolor = '#ffff',
    #                     zerolinewidth = 2,
    #                     gridcolor = 'ffff'),
    #        plot_bgcolor='#e5ecf6')
    
    # make it an option to change plotting units? (ex. km^2)
    
  }) #%>% bindEvent(input$go)

  ## Tables --------------
    
  output$tec <- renderDataTable({
    # req(input$chosen_geography)
 
    # need to format the names of the table columns to be more user friendly
    DT::datatable(geography_subset() %>% 
                    mutate(across(c(area_ha, compiled_EF, compiled_UpperCI, compiled_LowerCI, veg_TierII_mean, veg_TierII_se), 
                                  ~round(.x, 2))) %>% # some numbers aren't rounding well..still have non-terminating decimals
                    mutate(compiled_EF = case_when(!is.na(compiled_EF) ~ paste0(compiled_EF, " (+", 
                                                                                compiled_UpperCI - compiled_EF, "/-", 
                                                                                compiled_EF - compiled_LowerCI, ")"),
                                                   T ~ NA_character_),
                           veg_TierII_mean = case_when(!is.na(veg_TierII_mean) ~ paste0(veg_TierII_mean, " (+/-", 
                                                                                        veg_TierII_se, ")"),
                                                       T ~ NA_character_),
                           `Reporting Tier` = case_when(TierIorII == "Tier II" ~ "Country-specific value", 
                                                        TierIorII == "Tier I" ~ "IPCC global value", T ~ TierIorII)) %>% 
                    select(territory, habitat, area_ha, compiled_EF, veg_TierII_mean, `CO2 equivalent (Tg)`, `Reporting Tier`) %>% 
                    rename(Geography = territory, 
                           Habitat = habitat, 
                           `Mean Soil Stock (MgC/ha)` = compiled_EF,
                           `Mean Biomass Stock (MgC/ha)` = veg_TierII_mean,
                           `Habitat Area (ha)` = area_ha),
                  
                  caption = "Carbon stocks estimated for tidal wetland ecosystems.",
                  options = list(searching = TRUE,
                                 paging = FALSE,
                                 info = FALSE,
                                 scrollY = 300,
                                 # scrollX = 300,
                                 scrollCollapse = TRUE),
                  rownames = FALSE)
  }) 
    
  ## Report Download ---------------
  
  output$downloadReport <- downloadHandler(
    # name of exported file
    filename = function(){
      paste0(input$chosen_geography, "_Inventory_Report_", Sys.Date(), ".html")
    },
    # copy PDF file from the folder containing the pre-generated reports
    content = function(file) {
      file.copy(paste0("www/reports/", input$chosen_geography, "_Report.html"), file)
      
      # Potential add: Informational popup or handling for when a country name doesn't exist (ideally this wouldn't happen though)
      # Potential add: option to download PDF or HTML version
    }
  )
  
  
 ## Main Table Download ------------------
  
  # output$downloadTable <- downloadHandler(
  #   filename = function() {
  #     paste0(input$chosen_geography, "_Stocks_Table_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     write.csv(stockplot(), file, row.names = FALSE)
  #   }
  # )
  # 

  ################################################
  
  
## Create output to source html reports -----------------
  
  #shiny::addResourcePath()
  
  # output$report <- renderUI({
  #   req(input$chosen_geography) #require territory input
  # 
  #   filename <- paste0("www/reports/", input$chosen_geography, "_Report.html")
  # 
  #   tags$iframe(
  #     seamless = "seamless",
  #     src = filename,
  #     height = 1000,
  #     width = 1000
  #   )
  # })

    
}
 
