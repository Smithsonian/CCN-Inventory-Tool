#' third_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_third_module_ui <- function(id){
  ns <- NS(id)
  tagList(
      selectInput(
        inputId = ns("filter_cyl"),
        label = "Select a number of cylinders",
        selected = 4,
        choices = mtcars %>%
          distinct(!!sym("cyl")) %>%
          pull()
      ),
      tableOutput(ns("tbl_mtcars"))
  )
}

#' third_module Server Functions
#'
#' @noRd
mod_third_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    my_mtcars <- reactive({
      mtcars %>%
        filter(!!sym("cyl") == input$filter_cyl)
    })
    output$tbl_mtcars <- renderTable({my_mtcars()})
  })
}

## To be copied in the UI
# mod_third_module_ui("third_module_1")

## To be copied in the server
# mod_third_module_server("third_module_1")
