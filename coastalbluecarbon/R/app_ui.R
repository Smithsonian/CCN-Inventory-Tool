#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr distinct pull
#' @importFrom rlang sym
#' @importFrom readr read_csv
#' @importFrom shinythemes shinytheme
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @noRd
app_ui <- function(request) {
  # Application UI logic
  # navbarPage("Coastal Carbon Inventory",
  #            # you can use input$id in your server logic to determine which of the current tabs is active.
  #            # The value will correspond to the value argument that is passed to tabPanel().
  #            tabPanel("Page 1"),
  #            tabPanel("Page 2"),
  #            tabPanel("About"),
  #            # navbarMenu("More",
  #            #            tabPanel("Sub-Component A"),
  #            #            tabPanel("Sub-Component B")),
  #            inverse = TRUE,
  #            )

  tagList(
    #   # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      # set dashboard theme
      theme = shinytheme("cerulean"),
      # theme opts: https://rstudio.github.io/shinythemes/

      # App Title ----
      titlePanel("Coastal Carbon Inventory"),

      # Sidebar ----
      sidebarLayout(

        # sidebar panel for inputs
        sidebarPanel(

          textOutput("sidetext"),
          br(), # vertical spacing

          # mod_choice_ui("choice_1"), # modular version
          # mod_first_module_ui("first_module_1")
          # select a country
          selectInput(inputId = "chosen_country",
                      label = "Select a country",
                      choices = readr::read_csv("data/core_stocks.csv") %>%
                        # drop_na, arrange
                        distinct(country) %>% pull(country),
                      selected = "United States"
          ),
          br(),

          selectInput(inputId = "chosen_habitat",
                      label = "Select an ecosystem type",
                      choices = readr::read_csv("data/core_stocks.csv") %>%
                        # drop_na, arrange
                        distinct(habitat) %>% pull(habitat)
          ),
          br(),

          # select the tier
          radioButtons(inputId = "chosen_type",
                       label = "Select a carbon stock type",
                       choices = c("Soil", "Biomass"),
                       selected = "Soil"),
          br(),

          actionButton(
            inputId = "go",
            label = "Download Data",
            class = "btn-info"),
          br(),

          helpText("Please direct any questions or suggestions to CoastalCarbon@si.edu"),

        ),

        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Map", plotlyOutput("datamap")),
                      tabPanel("Plot", plotOutput("mainplot")),
                      tabPanel("Table", DTOutput("maintable")),
                      tabPanel("About", includeMarkdown("about.md"))
          )

          # plotlyOutput("datamap"),
          # plotOutput("mainplot"),
          # textOutput("feedback"),
          # tableOutput("maintable")
        )
      )
    )
  )
}
#   # fluidPage(
#   #   h1("Coastal Carbon Inventory"),
#   #   mod_map_ui("map_1")
#   #   # mod_stocks_ui("stocks_1"),
#   #   # mod_first_module_ui("first_module_1"),
#   #   # mod_second_module_ui("second_module_1"),
#   #   # mod_third_module_ui("third_module_1"),
#   #   # mod_choice_ui("choice_1"),
#   #   # mod_choice_ui("choice_2")
#   # )

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "coastalbluecarbon"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
