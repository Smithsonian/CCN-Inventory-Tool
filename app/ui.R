################################################

# This is the user-interface definition of a Shiny web application.

# rmdfiles <- c("country_insights.Rmd")
# sapply(rmdfiles, knit, quiet = T) #transform .Rmd to .md to render below 

# Application UI logic
fluidPage(
  navbarPage("Coastal Carbon Inventory",
             # you can use input$id in your server logic to determine which of the current tabs is active.
             # The value will correspond to the value argument that is passed to tabPanel().
             
             ################################################
             tabPanel("Dashboard",
                      
                      # Sidebar ----
                      sidebarLayout(
                        
                        # sidebar panel for inputs
                        sidebarPanel(
                          
                          # textOutput("sidetext"),
                          p("Welcome to the Coastal Carbon Inventory Tool!"),
                          
                          br(), # vertical spacing

                          # select a country
                          selectInput(inputId = "chosen_geography",
                                      label = "Select a geography",
                                      choices = c(Choose='', unique(main_table$territory) %>% sort()),
                                      selectize = F
                          ),
                          
                          br(),
                          
                          tags$b("Reset map to world view"), 
                          actionButton(
                            inputId = "reset",
                            label = "Reset",
                            class = "btn-info"
                          ), 
                          
                          div(style="margin-bottom:30px"), 
                          
                          helpText("Please direct any questions or suggestions to CoastalCarbon@si.edu"),
                          
                          img(src = "SI_logo_no_text.png", height = "50px",
                              style="display: block; margin-left: auto; margin-right: auto;"),
                          # img(src = "ccrcn_logo.png", height = "50px"),
                                   # style="display: block; margin-left: auto; margin-right: auto;"), # center the logo
                          width = 2
                        ),
                        
                        mainPanel(
                          # fluidRow(
                          #   div(id = "value_box_row",
                          #       valueBoxOutput("num_cores", width = 3),
                          #       valueBoxOutput("num_veg", width = 3),
                          #       valueBoxOutput("num_habitats", width = 4)
                          #   )
                          # ),
                          verticalLayout(
                            
                            fluidRow( # or use splitLayout()?...doesn't seem to facilitate text wrapping
                            
                              column(leafletOutput("map"), width = 6),
                              
                              column(
                                 # p("This section will contain information about the available data for the selected country."),
                                     
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Global Stocks", 
                                                          radioButtons(inputId = "chosen_habitat",
                                                                       label = "Select a habitat", 
                                                                       choices = c("mangrove", "marsh", "seagrass"), 
                                                                       inline = T),
                                                          plotOutput("worldstockplot")),
                                                 tabPanel("Data Status", plotlyOutput("datastatus")),
                                                 tabPanel("Emissions Factors", plotlyOutput("efplot")),
                                                 tabPanel("Activity Data", plotlyOutput("activityplot")),
                                     ),
                                     width = 6)
                            ),
                            
                            fluidRow(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Stocks Table", DTOutput("tec")),
                                          tabPanel("Download Report", 
                                                   tags$br(), 
                                                   
                                                   tags$b("Insight about data available for selected territory. Please navigate to the `Reports` tab to view report in browser."),
                                                   
                                                   tags$br(), tags$br(), 
                                                   
                                                   #includeHTML("paste0(input$chosen_geography, `_Report.html`"),
                                                   
                                                   downloadButton("downloadReport", 
                                                                  label = "Download Report", 
                                                                  class = "btn-primary"),
                                                   tags$br(), tags$br(),
                                                   
                                                   downloadButton("downloadTable",
                                                                  label = "Download Stocks Table",
                                                                  class = "btn-primary")), 
                                          #tabPanel("Detailed Insight", includeMarkdown("country_insights.md")), #this renders .md
                                          # tabPanel("TEC", p("TEC plot"))
                              ),
                              width = 10)
                          ),
                          width = 10
                        )
                      )
             ),
             
             ################################################
             tabPanel(
               "About", 
               includeMarkdown("about.md")),
             tabPanel(
               "Methods",
               includeMarkdown("methods.md")
             ),
             tabPanel(
               "Reports",
               htmlOutput("report") #include report html as reports tab 
             ),
             # navbarMenu("More",
             #            tabPanel("Sub-Component A"),
             #            tabPanel("Sub-Component B")),
             inverse = TRUE
  )
)

#     # set dashboard theme
#     # theme = shinytheme("cerulean"),
#     # theme opts: https://rstudio.github.io/shinythemes/

#         # plotlyOutput("datamap"),
#         # plotOutput("mainplot"),
#         # textOutput("feedback"),
#         # tableOutput("maintable")

