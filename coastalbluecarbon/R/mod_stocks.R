#' stocks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom rlang sym
#' @importFrom dplyr distinct pull
mod_stocks_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("filter_country"),
      label = "Select a country",
      selected = "United States",
      choices = coreStocks() %>%
        distinct(!!sym("country")) %>%
        pull()
    ),
    tableOutput(ns("tbl_stocks"))
  )
}

#' stocks Server Functions
#'
#' @noRd
mod_stocks_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    all_stocks <- reactive({
      coreStocks() %>%
        filter(!!sym("country") == input$filter_country)
    })
    output$tbl_stocks <- renderTable({all_stocks()})
  })
}

## To be copied in the UI
# mod_stocks_ui("stocks_1")

## To be copied in the server
# mod_stocks_server("stocks_1")
