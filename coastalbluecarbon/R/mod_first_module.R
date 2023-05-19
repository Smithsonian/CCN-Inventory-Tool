#' first_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_first_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Blue Carbon Stocks"),
    selectInput(
      inputId = "chosen_country",
      label = "Select a country",
      selected = "United States",
      choices = readr::read_csv("data/core_stocks.csv") %>%
        distinct(country) %>% pull(country)
    ),
    tableOutput(ns("table"))
  )
}

#' first_module Server Functions
#'
#' @noRd
mod_first_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tbl_subset <- reactive({
      readr::read_csv("data/core_stocks.csv") %>%
        drop_na(country, core_stock_MgHa) %>%
        filter(!!sym("country") == input$chosen_country)
      # mtcars %>%
      #   filter(!!sym("cyl") == input$filter_cyl)
    })

    output$table <- renderTable({tbl_subset()})

      # output$stockplot <- renderPlot({
      #
      #   readr::read_csv("data/core_stocks.csv") %>%
      #     drop_na(country, core_stock_MgHa) %>%
      #     filter(country == input$chosen_country) %>%
      #     ggplot(aes(x = habitat, y = core_stock_MgHa)) +
      #     geom_boxplot() +
      #     coord_flip() +
      #     ggtitle(paste0("1m C Stocks by Habitat for ", input$chosen_country))
      # })
  })
}

## To be copied in the UI
# mod_first_module_ui("first_module_1")

## To be copied in the server
# mod_first_module_server("first_module_1")
