#' choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    # select a country
    selectInput(inputId = ns("choice"),
                label = "Select a country",
                choices = c("United States", "Brazil", "Mexico", "Australia", "Costa Rica")
    ),
    actionButton(
      # We need to ns() all ids
      inputId = ns("go"),
      label = "Render Plot"
    )
  )
}



#' choice Server Functions
#'
#' @noRd
mod_choice_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$go, {
      r$chosen_country <- input$choice
    })
  })
}


## To be copied in the UI
# mod_choice_ui("choice_1")

## To be copied in the server
# mod_choice_server("choice_1")
