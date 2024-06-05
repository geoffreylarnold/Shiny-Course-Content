# DBI Example
# THIS IS AN EXAMPLE APPLICATION IT WILL NOT RUN

library(shiny)
library(DBI)
library(pool)

# You should always store credentials and keys in a file or enviornmental variable and have it ignored in your Git repository (if a file)
# json 
creds <- jsonlite::fromJSON(".creds.json")
un <- creds$un
pw <- creds$pw
# .env example
dotenv::load_dot_env()
un1 <- Sys.getenv("un")
pw1 <- Sys.getenv("pw")

# Create the DB Connection
pool <- dbPool(odbc::odbc(), driver = "FreeTDS", server = "IP_or_HOST_ADDRESS", port = 1433, database = "DBName", uid = un , pwd = pw, TDS_Version = "8.0")

# Get some stuff for UI
conn <- poolCheckout(pool)
selections <- sort(dbGetQuery(conn, "SELECT DISTINCT(TYPE) FROM Database.dbo.Things")$TYPE)
poolReturn(conn)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Database Dashboard"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        # Date selection
         sdateRangeInput("dates",
                         "Select Dates",
                         start = Sys.Date()-30,
                         end = Sys.Date()),
         # Something selction
         selectInput("select",
                     "Selections",
                     choices = selections,
                     selected = "Some Type")
      ),
      
      # Show plot
      mainPanel(
         plotOutput("examplePlot")
      )
   )
)

# Define server logic
server <- function(input, output, session) {
   dataLoad <- reactivePoll(60000, session, 
                            checkFunc = function(){
                              print("Entered Check")
                              Sys.time()
                              print(Sys.time())
                              # gets max date from database table to determine if data has been updated
                              conn <- poolCheckout(pool)
                              max_date <- dbGetQuery(con, "SELECT UNIX_TIMESTAMP(DATE_COLUMN) FROM SOME_TABLE;")
                              data <- dbGetQuery(conn, sql)
                              poolReturn(conn)
                              return(max_date)
                            },
                            valueFunc = function(){
                              # Generate SQL Statement which handles all filtering
                              sql <- paste0("SELECT * FROM SOME_TABLE WHERE TYPE = ", input$select)
                              # Run SQL Statement
                              conn <- poolCheckout(pool)
                              data <- dbGetQuery(conn, sql)
                              poolReturn(conn)
                              return(data)
                            }
   )
   dataFilter <- 
   output$examplePlot <- renderPlot({
     data <- dataFilter()
     
     req(input$dates)
     table<- filter(DATE >= input$dates[1] & DATE <= input$dates[2])
       
     ggplot(table, aes(x = STATUS, fill = STATUS)) +
       geom_bar()
   })
   onStop(
     poolClose(pool)
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

