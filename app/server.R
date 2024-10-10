#
# This is the server logic of a Shiny web application. 

# Define server logic required to draw a histogram
function(input, output, session) {

  # Initialize app state when nothing is selected yet
  # app_state <- reactiveValues({
  #   geography = chosen_country
  # })
  
  r <- reactive(
    # which(map_input$country == input$chosen_country)
    world_ne %>% filter(country == input$chosen_country)
  )
  
  country_subset <- reactive(
    countrydata %>% filter(country == input$chosen_country)
  )
  # 
  # tier1subset <- reactive(
  #   tier1data %>% filter(country == input$chosen_country)
  # )
    # bindCache(input$chosen_country) %>%
    # bindEvent(input$go)
  

  ## Map -------------------
  
  # output$map <- renderLeaflet({
  #   leaflet(options = leafletOptions()) %>%
  #     addTiles() %>% 
  #     fitBounds(-135, -50, 145, 60) # initial conditions
  # })
  
  output$map <- renderLeaflet({
    # leaflet(r(), options = leafletOptions()) %>%
    #   addTiles() %>% 
    #   addPolygons(weight = 2)
      # setView(lng = -88, lat = 17, zoom = 6) %>% 
      # fitBounds(lng1 = map_input$longitude_max[r()], lng2 = map_input$longitude_min[r()],
                # lat1 = map_input$latitude_max[r()], lat2 = map_input$latitude_min[r()])
    
    leaflet() %>% 
      # basemap options
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$CartoDB, group = "CartoDB") %>%
      
      # add polygon layer
      addPolygons(data = r(), weight = 2, 
                  # fill = FALSE, 
                  group = "Border") %>% 
      # add data points
      addCircleMarkers(data = tier2data %>% drop_na(latitude) %>% filter(country == input$chosen_country), 
                       lng = ~longitude, lat = ~latitude, radius = 2,
                       group = "Cores") %>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "CartoDB"),
        overlayGroups = c("Cores", "Border"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  }) %>% 
    bindEvent(input$go)
  
  
  ## Plots ----------------
  
  
  ## Emission Factor plot
  output$efplot <- renderPlotly({
    ggplotly(
      ggplot(country_subset(), aes(`stock (Mg/ha)`, habitat, col = tier)) +
        # geom_boxplot(aes(stock_MgHa, habitat, col = `carbon pool`)) +
        geom_errorbar(aes(xmin = stock_MgHa_lower, xmax  = stock_MgHa_upper), width = 0.1) +
        geom_point(size = 2, shape = 21, fill="white") +
        theme_bw() +
        facet_wrap(~`carbon type`, 
                   # scales = "free", 
                   dir = "v") +
        theme(legend.position = "bottom") 
    )
  }) %>% bindEvent(input$go)
  
  ## Activity Data
  output$activityplot <- renderPlot({
    
    landuse %>% 
      dplyr::filter(country == input$chosen_country) %>% 
      ggplot2::ggplot() + 
      geom_col(aes(habitat, area_ha, fill = habitat)) +
      coord_flip() +
      ylab("Area (hectares)") + theme_bw(base_size = 20) +
      theme(legend.position = "bottom")
    # make it an option to change plotting units (ex. km^2)
    
  }) %>% bindEvent(input$go)

  
  ## Tables --------------
  output$maintable <- renderDT({
      
    country_subset() %>% 
      select(-c(stock_MgHa_lower, stock_MgHa_upper)) %>% 
      DT::datatable(caption = paste("Carbon stocks estimated for tidal wetland ecosystems in ", input$chosen_country),
                    options = list(searching = FALSE,
                                   paging = FALSE,
                                   info = FALSE,
                                   # scrollY = 300,
                                   # scrollX = 300,
                                   scrollCollapse = TRUE),
                    rownames = FALSE)
  }) %>% 
    bindEvent(input$go)
  
  
  output$tec <- renderDT({
    
    # Case: there is in-country data
    if(input$chosen_country %in% tier2data$country){
      tec_table <- tier2data %>%
        tidyr::drop_na(country, habitat, stock_MgHa) %>%
        dplyr::filter(country == input$chosen_country) %>%
        
        # add tier I data
        dplyr::bind_rows(tier1data %>% dplyr::filter(country == input$chosen_country)) %>% 
        
        dplyr::group_by(country, habitat, carbon_pool, tier) %>%
        dplyr::summarize(`sample size` = n(),
                         stock_MgHa_mean = mean(stock_MgHa, na.rm = T),
                         stock_MgHa_se = sd(stock_MgHa, na.rm = T)) %>%
        dplyr::ungroup() %>%
        
        # join country land use/habitat area data
        dplyr::left_join(landuse %>% dplyr::filter(country == input$chosen_country)) %>% 
        
        # upscale estimates using habitat area
        dplyr::mutate(`habitat area (Ha)` = round(area_ha, 2),
                      `stock avg (TgC)` = round(stock_MgHa_mean * area_ha / 10^6, 2),
                      # stock_TgC_se = round(stock_MgHa_se * area_ha / 10^6, 2),
                      # calculate co2 equivalent
                      `CO2eq (TgC)` = round(stock_MgHa_mean * area_ha * 3.67 / 10^6, 2),
                      `CO2eq SE (TgC)` = round(stock_MgHa_se * area_ha * 3.67 / 10^6, 2)) %>%
        # select(-c(iso3c, stock_MgHa_mean, stock_MgHa_se, area_ha)) %>% 
        
        # summarize TEC
        dplyr::group_by(country, tier) %>% 
        dplyr::summarise(`TEC (TgC)` = sum(`stock avg (TgC)`, na.rm = T),
                         `TEC CO2eq (TgC)` = sum(`CO2eq (TgC)`, na.rm = T))
      
      # Case: no in-country data
    } else {
     tec_table <- tier1data %>% dplyr::filter(country == input$chosen_country) %>% 
        
        dplyr::group_by(country, habitat, carbon_pool, tier) %>%
        dplyr::summarize(`sample size` = n(),
                         stock_MgHa_mean = mean(stock_MgHa, na.rm = T),
                         stock_MgHa_se = sd(stock_MgHa, na.rm = T)) %>%
        dplyr::ungroup() %>%
        
        # join country land use/habitat area data
        dplyr::left_join(landuse %>% dplyr::filter(country == input$chosen_country)) %>% 
        
        # upscale estimates using habitat area
        dplyr::mutate(`habitat area (Ha)` = round(area_ha, 2),
                      `stock avg (TgC)` = round(stock_MgHa_mean * area_ha / 10^6, 2),
                      # stock_TgC_se = round(stock_MgHa_se * area_ha / 10^6, 2),
                      # calculate co2 equivalent
                      `CO2eq (TgC)` = round(stock_MgHa_mean * area_ha * 3.67 / 10^6, 2),
                      `CO2eq SE (TgC)` = round(stock_MgHa_se * area_ha * 3.67 / 10^6, 2)) %>%
        # select(-c(iso3c, stock_MgHa_mean, stock_MgHa_se, area_ha)) %>% 
        
        # summarize TEC
        dplyr::group_by(country, tier) %>% 
        dplyr::summarise(`TEC (TgC)` = sum(`stock avg (TgC)`, na.rm = T),
                         `TEC CO2eq (TgC)` = sum(`CO2eq (TgC)`, na.rm = T))
    }
    
    DT::datatable(tec_table, 
                  caption = paste("Activity data for tidal wetland ecosystems in ", input$chosen_country),
                  options = list(searching = FALSE,
                                 paging = FALSE,
                                 info = FALSE,
                                 # scrollY = 300,
                                 # scrollX = 300,
                                 scrollCollapse = TRUE),
                  rownames = FALSE)
  }) %>% bindEvent(input$go)
    
  # ggplot(tec, aes(tier, `TEC CO2eq (TgC)`)) + geom_point() + coord_flip() + theme_bw()

  ################################################
  
  ## Conditional Insight
  
  output$datainsight <- renderText({
    
    # Case: no in country data 
    if(!input$chosen_country %in% unique(tier2data$country)){
      "No data for this country. Go sample."
    } else{
      paste("Congratulations, you have data for this country! There are", 
            length(unique(country_subset()$habitat)), "habitats.",
            sep = " ")
    }
    
  }) %>% bindEvent(input$go)
  
  
  ## Value Boxes
  ## Won't work without Shiny dashboard
  # output$num_cores <- renderValueBox({
  #   valueBox(
  #     nrow(tier2subset() %>% filter(carbon_pool == "soil")), 
  #     "Number of cores", icon = icon("vials"),
  #     color = "aqua"
  #   )
  # })
  # 
  # output$num_habitats <- renderValueBox({
  #   valueBox(
  #     nrow(tier2subset() %>%
  #            dplyr::filter(!is.na(habitat)) %>%
  #            dplyr::count(habitat)), 
  #     "Number of habitats", icon = icon("tree"),
  #     color = "green"
  #   )
  # })
  # 
  # output$num_veg <- renderValueBox({
  #   valueBox(
  #     nrow(tier2subset() %>% filter(carbon_pool == "vegetation")),
  #     "Number of vegetation surveys", icon = icon("ruler-vertical"),
  #     color = "light-blue"
  #   )
  # })
  
  
  # Call modules
    
}

