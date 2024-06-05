# Green Infrastructure
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(rgeos)

# Data Source: https://data.cityofnewyork.us/Environment/DEP-Green-Infrastructure/spjh-pz7h
greenInf.load <- st_read("http://data.cityofnewyork.us/api/geospatial/spjh-pz7h?method=export&format=GeoJSON")
boros.load <- st_read("http://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON")

icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)

# Define UI for application
ui <- navbarPage("NYC Green Infrastructure",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Select Sewer Type
                              selectInput("sewerSelect",
                                          "Sewer Type",
                                          choices = unique(sort(greenInf.load$sewer_type)),
                                          selected = c("MS4", "Non-combined"),
                                          selectize = T,
                                          multiple = T),
                              # Select NYC Borough
                              radioButtons("boroSelect",
                                           "Borough Filter:",
                                           choices = unique(sort(greenInf.load$borough)),
                                           selected = "Bronx")
                            ),
                            # Map Panel
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Page
                              leafletOutput("leaflet")
                              )
                            )
                          ),
                 # Data Table Pannel
                 tabPanel("Data",
                          fluidPage(
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic required to create a map
server <- function(input, output) {
   # Basic Map
   output$leaflet <- renderLeaflet({
     leaflet() %>%
       addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
       addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
       setView(-74.0060, 40.7128, 9) %>%
       addLayersControl(baseGroups = c("Google", "Wiki"))
   })
   # Green Infrastructure Filtered data
   greenInfInputs <- reactive({
     greenInf <- greenInf.load 
     
     # Boros
     greenInf <- subset(greenInf, borough == input$boroSelect)
     # Sewer type
     if (length(input$sewerSelect) > 0) {
       greenInf <- subset(greenInf, sewer_type %in% input$sewerSelect)
     }
      
     return(greenInf)
   })
   # Replace layer with filtered greenInfrastructure
   observe({
     greenInf <- greenInfInputs()
     # Data is greenInf
     leafletProxy("leaflet", data = greenInf) %>%
       # In this case either lines 89 or 90 will work
       clearMarkers() %>%
       clearGroup(group = "greenInf") %>%
       addAwesomeMarkers(icon = ~icons[sewer_type], popup = ~paste0("<b>", project_na, "</b>: ", sewer_type), group = "greenInf", layerId = ~asset_id)
   })
   # Data Table
   output$table <- DT::renderDataTable(greenInfInputs(), options = list(scrollX = T))
   # Print Inputs
   observe({
     print(reactiveValuesToList(input))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

