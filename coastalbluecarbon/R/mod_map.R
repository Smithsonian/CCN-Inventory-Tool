#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leaflet addTiles setView leafletOutput renderLeaflet addCircleMarkers
#' @importFrom readr read_csv

# cores <- readr::read_csv("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/CCRCN_cores.csv")

mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), width="100%", height="100%"),
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      leaflet(readr::read_csv("https://raw.githubusercontent.com/Smithsonian/CCRCN-Data-Library/main/data/CCRCN_synthesis/CCRCN_cores.csv")) %>%
        addTiles() %>%
        addCircleMarkers(radius = 2)
        # setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
