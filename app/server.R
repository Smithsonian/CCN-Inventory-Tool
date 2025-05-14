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
  
  r <- reactive(
    # which(map_input$country == input$chosen_country)
    map_polys %>% filter(territory == input$chosen_geography)
  )
  
  geography_subset <- reactive({
    countrydata %>% filter(territory == input$chosen_geography)
  })

  dat <- reactive({
    map_input %>% filter(territory == input$chosen_geography)
  })

  ## Map -------------------

  # Feature: Zoom options for globe or particular country
#initial map state --> world map with all cca samples   
  output$map <-
    
    renderLeaflet({

      leaflet() %>% 
        # basemap options
        addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
        addTiles(group = "OSM (default)") %>%
        
        # add data points (global cca samples)
        addCircleMarkers(data = map_input,
                         lng = ~longitude, lat = ~latitude, radius = 2,
                         label = ~paste(habitat, carbon_pool, "data", sep = " "),
                         group = "Samples", 
                         # eventually have veg be plotted separately for color coding
                         color = "green") 
    })
  
  
  ## An observe statement to update the map, filtering to chosen territory 
  observeEvent(input$chosen_geography, {
    
    bounds <- st_bbox(r()) %>% as.vector()

    # using leafletProxy to call in original map 
    leafletProxy(mapId = "map") %>%
      # reset map layers
      # clearMarkers() %>%
      # clearControls() %>% 
      clearShapes() %>% 

      # add polygon layer
      addPolygons(data = r(), weight = 2, fill = T, opacity = 0.1,
                  group = "Border") %>%

      # #auto zoom to selection
      # fitBounds is more straightforward, but people love a smooth transition
      flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      
      #add layer options 
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB"),
        overlayGroups = c("Samples", "Border"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
  })
  
  
#reset to original world map, linked to action button   
 observeEvent(input$reset, {
    
   leafletProxy(mapId = "map") %>% 
     clearShapes() %>% clearControls() %>% 
     flyToBounds(world_bounds[1], world_bounds[2], world_bounds[3], world_bounds[4])
 })
  
 ## Plots ----------------
  
 ## Global Stock Plot
 output$worldstockplot <- renderPlot({
   req(input$chosen_habitat)
   
   globalStocks(main_table, input$chosen_habitat)

 })
 
 ## Data Status
 ## quantity, quality, representation 
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
   
 }) #%>% bindEvent(input$go)
  
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
    
  output$tec <- renderDT({
    req(input$chosen_geography)
    
    # need to format the names of the table columns to be more user friendly
    DT::datatable(geography_subset() %>% 
                    mutate(`Reporting Tier` = case_when(TierIorII == "Tier II" ~ "Country-specific value", 
                                                        TierIorII == "Tier I" ~ "IPCC global value", T ~ TierIorII)) %>% 
                    select(territory, habitat, area_ha, contains("compiled"), `CO2eq (TgC)`, 
                           `Reporting Tier`, tier_II_overlaps_TierI) %>% 
                    mutate(across(area_ha:compiled_LowerCI, ~round(.x, 2))) %>% 
                    rename(`Country or Territory` = territory, 
                           Habitat = habitat, 
                           `Reporting Insight` = tier_II_overlaps_TierI,
                           `Mean Stock Upper CI (Mg/ha)` = compiled_UpperCI,
                           `Mean Stock Lower CI (Mg/ha)` = compiled_LowerCI,
                           `Mean Stock (Mg/ha)` = compiled_EF,
                           `Habitat Area (ha)` = area_ha),
                  
                  caption = paste("Carbon stocks estimated for tidal wetland ecosystems in ", input$chosen_geography),
                  options = list(searching = FALSE,
                                 paging = FALSE,
                                 info = FALSE,
                                 # scrollY = 300,
                                 # scrollX = 300,
                                 scrollCollapse = TRUE),
                  rownames = FALSE)
  }) #%>% bindEvent(input$go)
    
  ## Report Download ---------------
  
  output$downloadReport <- downloadHandler(
    # name of exported file
    filename = function(){
      paste0(input$chosen_geography, "_Inventory_Report_", Sys.Date(), ".pdf")
    },
    # copy PDF file from the folder containing the pre-generated reports
    content = function(file) {
      file.copy(paste0("www/reports/", input$chosen_geography, "_Detailed_Insights.pdf"), file)
      
      # Potential add: Informational popup or handling for when a country name doesn't exist (ideally this wouldn't happen though)
      # Potential add: option to download PDF or HTML version
    }
  )
  
  ################################################
  
  ## Conditional Insight
  
  # output$datainsight <- renderText({
  #   
  #   # Case: no in country data 
  #   if(!input$chosen_country %in% unique(tier2data$country)){
  #     "No data for this country."
  #   } else{
  #     paste("Congratulations, you have data for this country! There are", 
  #           length(unique(geography_subset()$habitat)), "habitats represented.",
  #           sep = " ",
  #           "This tab includes country-specific insights and more detailed analysis, including carbon stocks, emissions factors, and ecosystem wetland area for mangrove, marsh, and seagrass ecosystems.")
  #   }
  #   
  # }) %>% bindEvent(input$go)
  # 
  
  # Call modules
    
}
 
