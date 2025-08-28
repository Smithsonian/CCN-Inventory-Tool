## User Interface for the Inventory Tool
# Designed with bslib

page_navbar(
  theme = bs_theme(bootswatch = "cosmo"),
  title = "Coastal Carbon Inventory",
  # sidebar = "Dashboard",
  # navbar_options()
  
  # Define pages
  nav_panel("Dashboard", 
            layout_columns(
              # A negative number in widths creates white space
              col_widths = c(6,6,6,6),
              row_heights = c(1,1),
              
              card(full_screen = TRUE,
                   layout_sidebar(sidebar = sidebar(
                     p("Welcome to the Coastal Carbon Inventory Tool!"),
                     # select a country
                     selectInput(inputId = "chosen_geography",
                                 label = "Select a geography",
                                 choices = c(Choose='', unique(main_table$territory) %>% sort()),
                                 selectize = F
                     ),
                     tags$b("Reset map to world view"), 
                     actionButton(
                       inputId = "reset",
                       label = "Reset",
                       class = "btn-primary"
                     ), 
                     # helpText("Please direct any questions or suggestions to CoastalCarbon@si.edu"),
                     img(src = "SI_logo_no_text.png", height = "50px",
                         style="display: block; margin-left: auto; margin-right: auto;")
                   ),
                   # Map
                   leafletOutput("map"))
              ),
              
              navset_card_tab(full_screen = TRUE,
                              # title = "",
                              # sidebar = sidebar(""),
                              nav_panel("Habitat Area", plotlyOutput("activityplot")),
                              nav_panel("Available Data", plotlyOutput("datastatus")),
                              nav_panel("Carbon Stocks", uiOutput("dynamic_output", fill = T)), # plotlyOutput("efplot")
                              nav_panel("Summary Table", dataTableOutput("tec"))
                              ),
              
              card(full_screen = TRUE,
                   # class = "d-flex justify-content-between",
                   card_header("Global Trends"),
                   
                   card_body(
                     # class = "align-items-center",
                     span(
                       "Soil carbon stocks by country. The red line represents the global value for the each habitat.",
                       tooltip(
                         shiny::icon("circle-info"),
                         "Global values are defined by the IPCC",
                         placement = "bottom"
                       )
                     ),
                     radioButtons(inputId = "chosen_habitat",
                                  label = "Select a habitat:",
                                  choices = c("mangrove", "marsh", "seagrass"),
                                  inline = T),
                     imageOutput("worldstockplot") # plotOutput scrunches everything
                   )
              ),
              
              card(full_screen = TRUE, 
                   # scrollable by default (bless)
                   card_header("Summary Report"),
                   # uiOutput('markdown', fill = T)
                   # includeMarkdown("test_report.md")
                   # lorem::ipsum(paragraphs = 3, sentences = 5),
                   
                   layout_column_wrap(width = 1/3, 
                                      # !!!vbs
                                      # list(
                                      value_box(
                                        title = "Countries",
                                        value = textOutput("country_n"),
                                        # showcase = shiny::icon("earth-americas"),
                                        # showcase_layout = "bottom",
                                        theme = value_box_theme(bg = "#00923f", fg = "#FFFFFF")
                                        # max_height = "100px"
                                        # p("The 1st detail")
                                      ),
                                      value_box(
                                        title = "Habitats",
                                        value = textOutput("habitat_n"),
                                        # showcase = shiny::icon("leaf"),
                                        # showcase_layout = "left center",
                                        theme = value_box_theme(bg = "#835c31", fg = "#FFFFFF")
                                        # max_height = "100px"
                                        # p("The 2nd detail")
                                      ),
                                      value_box(
                                        title = "Samples",
                                        value = textOutput("obs_n"),
                                        # showcase = shiny::icon("chart-simple"),
                                        # showcase_layout = "top right",
                                        theme = value_box_theme(bg = "#2c409f", fg = "#FFFFFF")
                                        # max_height = "100px"
                                        # p("The 3th detail")
                                        # )
                                      )
                   ),
                   
                   tags$b("Generate detailed insight about the carbon data available for the selected geography:"),
                   p("Once a geography is selected, click the button below to compile and download country-specific insights and more detailed breakdown of ecosystem carbon stock across represented wetland habitats. This report will download as an HTML file to preserve the interactivity of the figures."),
                   downloadButton("downloadReport",
                                  label = "Download Report",
                                  class = "btn-primary")
                   )
            )),
  
  nav_panel(title = "About",
            includeMarkdown("about.md")
  ),
  
  nav_spacer(),
  nav_item(title = "Contact Us", 
           align = "right"),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_ccn),
    nav_item(link_atlas),
    nav_item(link_contribute)
    # github? or how to contact us
  )
)