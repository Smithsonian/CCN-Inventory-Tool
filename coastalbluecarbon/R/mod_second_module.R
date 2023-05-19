#' second_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot geom_point geom_errorbar xlab ggtitle theme_bw aes
#' @importFrom forcats fct_reorder
#' @importFrom dplyr ungroup
mod_second_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Another plot"),
    plotOutput(ns("plot"))
  )
}

#' second_module Server Functions
#'
#' @noRd
mod_second_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$plot <- renderPlot({
      coreStocks() %>% ungroup() %>%
        filter(habitat == "mangrove") %>%
        mutate(country = paste0(country, ", n = ", n),
               country = fct_reorder(country, stock_mean)) %>%
        ggplot(aes(stock_mean, country,
                   xmin = stock_mean - stock_se, xmax = stock_mean + stock_se)) +
        geom_point() +
        geom_errorbar() +
        xlab("Carbon Stocks (MgC ha-1)") +
        ggtitle("Mangrove 1m Core C Stocks by Country") +
        theme_bw()
    })
  })
}

## To be copied in the UI
# mod_second_module_ui("second_module_1")

## To be copied in the server
# mod_second_module_server("second_module_1")
