library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Load and clean data ----------------------------------------------
starwars.load <- starwars %>%
  mutate(films = as.character(films),
         vehicles = as.character(vehicles),
         starships = as.character(starships),
         name = as.factor(name))

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Star Wars Dashboard")

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    
    # Inputs: select variables to plot ----------------------------------------------
    selectInput("worldSelect",
                "Homeworld:",
                choices = sort(unique(starwars.load$homeworld)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("Naboo", "Tatooine")),
    
    # Birth year Selection ----------------------------------------------
    sliderInput("birthSelect",
                "Birth Year:",
                min = min(starwars.load$birth_year, na.rm = T),
                max = max(starwars.load$birth_year, na.rm = T),
                value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
                step = 1)
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
          ),
          
          fluidRow(
            
            # tabBox Plot Location ----------------------------------------------
            
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    starwars <- starwars.load %>%
      
      # Slider Filter ----------------------------------------------
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    
    # Homeworld Filter ----------------------------------------------
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(starwars)
  })
  
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  
  # A plot showing the mass of characters -----------------------------
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "mass")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  
  # A plot showing the height of characters -----------------------------------
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  
  # Mass mean info box ----------------------------------------------
  output$mass <- renderInfoBox({
    sw <- swInput()
  })
  
  # Height mean value box ----------------------------------------------
  output$height <- renderValueBox({
    sw <- swInput()
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)