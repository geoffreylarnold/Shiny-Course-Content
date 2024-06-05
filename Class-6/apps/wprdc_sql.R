# Class 13 SQL Query Test App

# Load required packages
library(shiny)
library(httr)
library(shinythemes)
library(dplyr)
library(jsonlite)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("readable"),
    # Set text area to width of page
    tags$style("#query {width: calc(100vw - 30px)}"),

    # Application title
    titlePanel("WPRDC SQL Query Test App"),
    
    # Query Area Input 
    fluidRow(style  = "padding: 15px",
             # Action Button to run query
             actionButton("run", "Run Query", icon = icon("play")),
             # Area to create and write query
             textAreaInput("query", "SQL Query", height = "25vh")
    ),
    # SQL Results
    fluidRow(style  = "padding: 15px",
        DT::dataTableOutput("results")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    getResults <- eventReactive(input$run, {
        # URL Encode the query
        q <- paste(input$query, "LIMIT 500")
        formatQuery <- URLencode(q, repeated = TRUE)
        # Build URL for GET request
        url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=", formatQuery)
        # Run Get Request
        g <- GET(url)
        # Check if there's an error
        if (g$status_code != 200) {
            # Send error to table
            results <- as_tibble(content(g)$error$info)
        } else {
            # Retrieve and display the results if successful
            results <- fromJSON(content(g, "text"))$result$records
        }
        
        return(results)
    })
    # Display results in Table
    output$results <- DT::renderDataTable(
        getResults(),
        extensions = 'Buttons',
        options = list(scrollX = TRUE,
                       dom = 'Bplfrti',
                       buttons = c('copy', 'csv', 'excel')))
}

# Run the application 
shinyApp(ui = ui, server = server)
