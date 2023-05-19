#' plot_stocks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_stocks_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Carbon Stocks"),
    plotOutput(ns("stockplot"))
  )
}

#' plot_stocks Server Functions
#'
#' @noRd
mod_plot_stocks_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$stockplot <- renderPlot({

      readr::read_csv("data/core_stocks.csv") %>%
        drop_na(country, core_stock_MgHa) %>%
        filter(country == r$chosen_country) %>%
        ggplot(aes(x = habitat, y = core_stock_MgHa)) +
        geom_boxplot() +
        coord_flip() +
        ggtitle(paste0("1m C Stocks by Habitat for ", r$chosen_country))
    })
  })
}

## To be copied in the UI
# mod_plot_stocks_ui("plot_stocks_1")

## To be copied in the server
# mod_plot_stocks_server("plot_stocks_1")
