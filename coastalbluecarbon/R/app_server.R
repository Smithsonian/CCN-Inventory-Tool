#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr filter left_join select count full_join ungroup across
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes coord_flip geom_boxplot theme
#' @importFrom readr read_csv
#' @importFrom tidyr drop_na
#' @importFrom plotly plot_ly add_trace plot_geo layout renderPlotly colorbar
#' @importFrom leaflet leaflet addTiles colorNumeric addPolygons
#' @importFrom DT renderDT datatable
#' @noRd
app_server <- function(input, output, session) {
  # rv <- reactiveValues()

  # Your application server logic

  ## Side Panel ----
  # render text
  output$sidetext <- renderText({
    "Hello user, enter your input here:"
  })

  ## Main Panel ----

  # get data
  coredata <- reactive({
    readr::read_csv("data/core_stocks.csv")
  })

  habdata <- reactive({
    readr::read_csv("data/testdat_country_hab_area.csv")
  })

  # Map ----
  # map available data
  output$datamap <- renderPlotly({

    country_smry <- coredata() %>%
      drop_na(country) %>%
      group_by(country, iso3c) %>%
      summarize(core_count = n(),
                habitat_types = paste(unique(habitat), collapse = ", ")
      )

    wetland_smry <- habdata() %>%
      group_by(country, iso3c) %>%
      summarize(area_ha = sum(area_ha))
      # distinct(country, iso3c)

    countrydata <- country_smry %>%
      full_join(wetland_smry) %>%
      # filter(country != "United States") %>%
      mutate(tier = ifelse(!is.na(core_count), 2, 1),
             data_availability = ifelse(is.na(core_count), "No", "Yes"),
             core_count = ifelse(is.na(core_count), 0, core_count),
             norm_core_count = core_count / area_ha * 1000,
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
        z = ~norm_core_count,
        text= ~hover,
        color = ~norm_core_count,
        colorscale = "Blues",
        reversescale = T
      ) %>%
      colorbar(title = "Available Cores per 1000ha<br>Coastal Wetland") %>%
      layout(title = 'Coastal Carbon Data Availability (Hover for breakdown)')
  })

  # Plot ----
  # render plot
  output$mainplot <- renderPlot({

    coredata() %>%
      drop_na(country, stock_MgHa) %>%
      filter(country == input$chosen_country) %>%
      ggplot(aes(x = habitat, y = stock_MgHa
                 # col = habitat
                 )) +
      geom_boxplot() +
      coord_flip() +
      theme_bw(base_size = 15) +
      theme(legend.position = "bottom") +
      ggtitle(paste0("1m C Stocks by Habitat for ", input$chosen_country))
  })

  # print output based on user selected input
  output$feedback <- renderText({
    paste("Here are the carbon stocks for ", input$chosen_country,
          " calculated from real data!")
  })

  # Table ----

  output$maintable <- renderDT({
    country_smry <- coredata() %>%
      drop_na(country, stock_MgHa) %>%
      filter(country == input$chosen_country) %>%
      group_by(country, habitat) %>%
      summarize(stock_MgHa_mean = mean(stock_MgHa, na.rm = T),
                stock_MgHa_se = sd(stock_MgHa, na.rm = T)) %>%
      ungroup() %>%
      # join country habitat area data
      left_join(habdata()) %>%
      # upscale estimates using habitat area
      mutate(carbon_type = "soil",
             stock_TgC = stock_MgHa_mean * area_ha / 10^6,
             stock_TgC_se = stock_MgHa_se * area_ha / 10^6,
             # calculate co2 equivalent
             co2equiv_TgC = stock_MgHa_mean * area_ha * 3.67 / 10^6,
             co2equiv_TgC_se = stock_MgHa_se * area_ha * 3.67 / 10^6) %>%
      # mutate(across(c("stock_TgCha", "stock_TgCha_se", "co2equiv_TgCha", "co2equiv_TgCha_se"),
      #               round(., digits = 3))) %>%
      select(-c(iso3c, stock_MgHa_mean, stock_MgHa_se, area_ha))
    # next add tier 1 scaled values to compare

    # summary table of min/max flux for each site
    datatable(country_smry,
              caption = paste("Carbon stocks estimated for tidal wetland ecosystems in ", input$chosen_country),
              options = list(searching = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             # scrollY = 300,
                             # scrollX = 300,
                             scrollCollapse = TRUE),
              rownames = FALSE)
  })


  # mod_stocks_server("stocks_1")
  # mod_choice_server("choice_1", r = r)
  # mod_map_server("map_1")
  # mod_plot_stocks_server("plot_stocks_1", r = r)
  # mod_first_module_server("first_module_1")
  # mod_second_module_server("second_module_1")
  # mod_third_module_server("third_module_1")
}


# to try out various shiny features
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "015-layout-navbar")
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "149-onRender")
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "149-onRender")
