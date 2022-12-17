library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(DT)

stops <- read.csv("stops.txt")

Oct_22 <- read.csv("stop_times 22Oct.txt")
Sep_22 <- read.csv("stop_times 22Sep.txt")
Aug_22 <- read.csv("stop_times 22Aug.txt")
Jul_22 <- read.csv("stop_times 22Jul.txt")
Jun_22 <- read.csv("stop_times 22Jun.txt")
May_22 <- read.csv("stop_times 22May.txt")
Apr_22 <- read.csv("stop_times 22Apr.txt")
Mar_22 <- read.csv("stop_times 22Mar.txt")
Feb_22 <- read.csv("stop_times 22Feb.txt")
Jan_22 <- read.csv("stop_times 22Jan.txt")
Dec_21 <- read.csv("stop_times 21Dec.txt")
Nov_21 <- read.csv("stop_times 21Nov.txt")

stop_time_data <- rbind(Nov_21, Dec_21, Jan_22, Feb_22, Mar_22, Apr_22, May_22, Jun_22, Jul_22, Aug_22,
                        Sep_22, Oct_22)

df <- stops %>% inner_join(stop_time_data,by=c('stop_id'))

df_mbta <- df %>% 
  filter(stop_name == "Boston University Central" | stop_name == "Hynes Convention Center" |
           stop_name == "Bowdoin" | stop_name == "Boardway" | stop_name == "Kendall/MIT" |
           stop_name == "Aquarium" | stop_name == "Logan Airport Ferry Terminal"| stop_name == "Long Wharf (North)" | 
           stop_name == "Long Wharf (South)" | stop_name == "Charlestown" | stop_name == "Georges Island" | 
           stop_name == "Hingham" | stop_name == "Newburyport" | stop_name == "Endicott"| stop_name == "Mansfield" |
           stop_name == "Providence" | stop_name == "TF Green Airport" | stop_name == "South Acton" | 
           stop_name == "Commonwealth Ave @ Babcock St" | stop_name == "Commonwealth Ave @ Silber Way"| stop_name ==
           "Tremont St @ Washington St" | stop_name == "Washington St @ Market St" | stop_name == "A St @ Mt Washington
         Ave" | stop_name == "B St @ Silver St") 

df_mbta <- df_mbta %>%
  separate(stop_desc, c("name", "line", "decs"), sep = "-") %>%
  drop_na(line)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    selectInput("line", 
                label = "choose a line",
                choices = unique(df_mbta$line),
                selected = "Green Line",
                multiple = FALSE)
  ),
  dashboardBody(
    box(width = 12,
        leafletOutput(outputId = "map")
    ),
    verbatimTextOutput("map_marker_click"),
    DT::dataTableOutput("table")
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(df_mbta) %>% addTiles() %>%
      addCircleMarkers(lng = ~stop_lon, lat = ~stop_lat, 
                       popup = ~stop_name,
                       radius = 5,
                       color = "forestgreen")})
  
  output$table <- DT::renderDataTable(datatable(
    df_new <- df_mbta[,c(3,5,8,22,23)] %>%
      filter(line==input$line)
  ))
    
  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click  
    print(p)
  })
  
}

shinyApp(ui, server)















