library(shiny)
library(DT)
library(tidyverse)
#
library(DBI)
db <- "farms"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "sa2143"
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = db,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_pass
)
#farms <- dbGetQuery(conn, "select * from farms limit 10")
farms <- dbGetQuery(conn, "select * from farms")
farmssummary <- dbGetQuery(conn, "select min(id::int) as id, farm_type, min(animals::int) as min, max(animals::int) as max, count(animals::int) as farms_quantity, CAST(AVG(animals::int) AS int) as average, sum(animals::int) as sum from farms group by farm_type")



# Define UI for application
ui <- fluidPage(
  
  # CSS
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: 1100px; padding: 20px; }
      .topimg { width: 120px; display: block; margin: 0px auto 20px auto; }
      .title { text-align: center; }
      .toprow { margin: 20px 0px; padding: 30px; background-color: #fae8bb; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 20px; margin-top: 20px; }
      .leaflet-top { z-index:999 !important; }
      "))
    ),
   
  # Top image
  img(class = "topimg", src = "https://brand.ncsu.edu/img/logo/brick2x2.jpg"),
  
   # Application title
   h1("NCSU - Farms Database", class = "title"),
   
   
   fluidRow (
     column(6, class = "bar",
       plotOutput("farm_maxBar")
     )
   ),

   fluidRow (
     column(6, class = "bar",
       plotOutput("farm_typeBar")
     )
   ),
 

   fluidRow(class = "toprow",
     fluidRow (class = "filters",
               
       column(6,
         # farm_type Menu
         selectInput("type", "Table content filter", c("All",
                                         "sow",
                                         "finisher",
                                         "nursery"))
       )
     )
   ),

   
   fluidRow (class = "table",
     # Table
     dataTableOutput("table")
   ),
   
   
   h3("SUMMARY"),
   fluidRow (class = "tablesummary",
     # Table
     dataTableOutput("tablesummary")
   ),
   hr(),
      p("Copyright (c) NCSU - Farms Database - Interview Challage",align="center")

   
   
   
)



# Define server logic
server <- function(input, output) {

# Create bar chart of max
  output$farm_typeBar <- renderPlot( {
    types <- group_by(farms, farm_type) %>% 
	  summarise(avgRating =  max(animals)) %>% 
      arrange(desc(avgRating)) %>% 
      top_n(3)
    
    ggplot(types, aes(reorder(farm_type, avgRating))) +
      geom_bar(aes(weight = avgRating), fill = "tomato3") + 
      coord_flip() +
      ggtitle("Types") +
      xlab("farm_type") +
      ylab("Max. Animals") +
      theme_bw(base_size = 16)
    
  })
  
  
# Create bar chart of min
  output$farm_maxBar <- renderPlot( {
    types <- group_by(farms, farm_type) %>% 
	  summarise(avgRating =  min(animals)) %>% 
      arrange(desc(avgRating)) %>% 
      top_n(3)
    
    ggplot(types, aes(reorder(farm_type, avgRating))) +
      geom_bar(aes(weight = avgRating), fill = "tomato3") + 
      coord_flip() +
      ggtitle("Types") +
      xlab("farm_type") +
      ylab("Min. Animals") +
      theme_bw(base_size = 16)
    
  })
  
  
  # Create data table
  output$table <- renderDataTable({
    
    # Filter data based on selected Style
    if (input$type != "All") {
      farms <- filter(farms, farm_type == input$type)
    }
    
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(farms) > 0, "")
    )
    farms[,2:4]
  })
  
 
  # Create sumary data table
  output$tablesummary <- renderDataTable({
    
    
    # Hide table when user has filtered out all data
    validate (
      need(nrow(farmssummary) > 0, "")
    )
    farmssummary[,2:7]
  }
  
  ,
	options = list(dom='t')
  )
 
}

# Run the application 
shinyApp(ui = ui, server = server)

