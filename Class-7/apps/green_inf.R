# Green Infrastructure
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)

# Data Source: https://data.cityofnewyork.us/Environment/DEP-Green-Infrastructure/spjh-pz7h
greenInf.load <- st_read("https://data.cityofnewyork.us/api/geospatial/spjh-pz7h?method=export&format=GeoJSON")

icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)

# Define UI for application
ui <- navbarPage("NYC Green Infrastructure",
                 theme = shinytheme("united"),
                 # Map Panel
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
                            # Map Page
                            mainPanel(
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
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
   
   output$leaflet <- renderLeaflet({
     # Load green infrastructure filtered data
     greenInf <- greenInfInputs()
     
     # Build Map
     leaflet() %>%
       addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google") %>%
     addAwesomeMarkers(data = greenInf, icon = ~icons[sewer_type], popup = ~paste0("<b>", project_na, "</b>: ", sewer_type, group = "greenInf")) %>%
       addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
       addLayersControl(baseGroups = c("Google", "Wiki"))
   })
    
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
   output$table <- DT::renderDataTable(greenInfInputs(), options = list(scrollX = T))
}

# Run the application 
shinyApp(ui = ui, server = server)

