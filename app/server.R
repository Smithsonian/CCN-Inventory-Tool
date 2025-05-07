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
        "Please select your country or territory in the dropdown menu to see country-level carbon stocks estimates, data visualizations and more"
      )
    )
  }) 
  
  
  r <- reactive(
    # which(map_input$country == input$chosen_country)
    world_ne %>% filter(territory == input$chosen_geography)
  )
  
  geography_subset <- reactive({
    countrydata %>% filter(territory == input$chosen_geography)
  })

  dat <- reactive({
    map_input %>% filter(country == input$chosen_geography)
  })

  ## Map -------------------
  
  # output$map <- renderLeaflet({
  #   leaflet(options = leafletOptions()) %>%
  #     addTiles() %>% 
  #     fitBounds(-135, -50, 145, 60) # initial conditions
  # })
  
  

#initial map state --> world map with all cca samples   
  output$map <-
    
    renderLeaflet({

      leaflet() %>% 
        # basemap options
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
        
        # add data points (global cca samples)
        addCircleMarkers(data = map_input,
                         lng = ~longitude, lat = ~latitude, radius = 2,
                         label = ~paste(habitat, carbon_pool, "data", sep = " "), # or have veg be plotted separately for color coding?
                         group = "Samples") %>% 
        
        addLayersControl(
          baseGroups = c("OSM (default)", "CartoDB"),
          overlayGroups = c("Samples"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    })
  
  
  ## An observe statement to update the map, filtering to chosen territory 
  observeEvent(input$chosen_geography, {
    
    bounds <- st_bbox(r()) %>% as.vector()

    # using leafletProxy to call in original map 
    leafletProxy(mapId = "map") %>%
      # reset map layers
      clearMarkers() %>% clearControls() %>% 
      
      # basemap options
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 

      # add polygon layer
      addPolygons(data = r(), weight = 2,
                  group = "Border") %>%

      # add new markers
      addCircleMarkers(data = dat(),
                       lng = ~longitude, lat = ~latitude, radius = 2,
                       label = ~paste(habitat, carbon_pool, "data", sep = " "), # or have veg be plotted separately for color coding?
                       group = "Samples", 
                       color = "purple") %>% 
      
      # #auto zoom to selection
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>% 
      
      #add layer options 
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB"),
        overlayGroups = c("Samples", "Border"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
  })
  
  
#reset to original world map, linked to action button   
 observeEvent(input$reset, {
    
    output$map <- renderLeaflet({
      
      leaflet() %>% 
        # basemap options
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
        # add data points 
        addCircleMarkers(data = map_input,
                         lng = ~longitude, lat = ~latitude, radius = 2,
                         label = ~paste(habitat, carbon_pool, "data", sep = " "), # or have veg be plotted separately for color coding?
                         group = "Samples") %>% 
        
        addLayersControl(
          baseGroups = c("OSM (default)", "CartoDB"),
          overlayGroups = c("Samples"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    })
 })

 #trying to connect reset button for map to dropdown menu, clearing the map and country selection 
 # observeEvent(input$reset, {
 #   
 #   updateSelectizeInput(
 #     session, "chosen_geography", 
 #     choices = unique(main_table$territory) %>% sort(),
 #     selected = NULL,
 #     multiple = T
 #   )
 #   
 # })
 
  # output$map <- renderLeaflet({
  #   # leaflet(r(), options = leafletOptions()) %>%
  #   #   addTiles() %>% 
  #   #   addPolygons(weight = 2)
  #     # setView(lng = -88, lat = 17, zoom = 6) %>% 
  #     # fitBounds(lng1 = map_input$longitude_max[r()], lng2 = map_input$longitude_min[r()],
  #               # lat1 = map_input$latitude_max[r()], lat2 = map_input$latitude_min[r()])
  #   
  #   leaflet() %>% 
  #     # basemap options
  #     addTiles(group = "OSM (default)") %>%
  #     addProviderTiles(providers$CartoDB, group = "CartoDB") %>% 
  #     
  #     # add polygon layer
  #     addPolygons(data = r(), weight = 2, 
  #                 # fill = FALSE, 
  #                 group = "Border") %>% 
  #     
  #     # add data points 
  #     # for cores
  #     addCircleMarkers(data = dat(),
  #                      # data = map_input %>% 
  #                      #                  drop_na(latitude) %>% 
  #                      #                  filter(country == input$chosen_country) %>% 
  #                      #                  filter(carbon_pool == "soil"), 
  #                      lng = ~longitude, lat = ~latitude, radius = 2,
  #                      label = ~paste(habitat, carbon_pool, "data", sep = " "), # or have veg be plotted separately for color coding?
  #                      group = "Samples") %>% 
  #     
  #     addLayersControl(
  #       baseGroups = c("OSM (default)", "CartoDB"),
  #       overlayGroups = c("Samples", "Border"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     )
  #   
  #}) #%>% bindEvent(input$go)
  
  
  ## Plots ----------------
  
  ## Data Status
  ## quantity, quality, representation 
  output$datastatus <- renderPlot({
    
    #case where there are cores available in CCA 
    #if(input$chosen_country %in% datastatus_counts$country){
    
    # quantity_table <- datastatus_counts %>% 
    #   dplyr::filter(country == input$chosen_country) %>% 
    #   mutate(data_tier_available = ifelse(!is.na(n_cores), "Tier II", "Tier I"))
    
    dat() %>% 
      dplyr::count(carbon_pool, habitat, country) %>% 
      
      ggplot2::ggplot() + 
      geom_col(aes(habitat, n, fill = carbon_pool)) +
      # geom_errorbar(aes(x= habitat, ymin = cores, ymax = hectare_UpperCI, y= area_ha), width = 0.1) +
      coord_flip() +
      ylab("Number of Samples") + theme_bw(base_size = 20) +
      theme(legend.position = "bottom")
    
    # } else {
    #   table_other <- tibble(country = input$chosen_country,
    #                         availability = "There are currently no values for this country available in the Coastal Carbon Atlas. Go sample!",
    #                         data_tier_available = "Tier I")
    # }
  }) #%>% bindEvent(input$go)
  
  ## Emission Factor plot
  output$efplot <- renderPlotly({
    ggplotly(
      
      geography_subset() %>% 
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
        # theme(legend.position = "bottom")
        
    )
  }) #%>% bindEvent(input$go)
  
  ## Activity Data
  output$activityplot <- renderPlot({
    
    # countrydata %>% 
      #dplyr::filter(complete.cases(area_ha)) %>% 
      # dplyr::filter(country == input$chosen_country) %>% 
      ggplot2::ggplot(geography_subset()) + 
      geom_col(aes(habitat, area_ha, fill = habitat)) +
      geom_errorbar(aes(x= habitat, ymin = area_ha_lowerCI, ymax = area_ha_upperCI, y= area_ha), width = 0.1) +
      coord_flip() +
      ylab("Area (hectares)") + theme_bw(base_size = 20) +
      theme(legend.position = "bottom")
    # make it an option to change plotting units (ex. km^2)
    
  }) #%>% bindEvent(input$go)

  
  ## Tables --------------
    
  output$tec <- renderDT({
    
    # need to format the names of the table columns to be more user friendly
    
    DT::datatable(geography_subset(), 
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
      file.copy(paste0("www/reports", input$chosen_geography, "_Report.pdf"), file) 
      
      # Potential add: Informational popup or handling for when a country name doesn't exist (ideally this wouldn't happen though)
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
 
