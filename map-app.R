library(shiny)
library(leaflet)
library(RCurl)

towns_url <- getURL("https://raw.githubusercontent.com/RWParsons/shiny-demo/main/input/qld_towns.csv")
towns <- read.csv(text = towns_url)


ui <- fluidPage(
  selectInput('town_name', 'Town Name',
              choices = c('None', sort(towns$location)),
              selected = "None"),
  leafletOutput('map')
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        lng=towns$x,
        lat=towns$y,
        popup=glue::glue("<b>Location:</b> {towns$location}"),
        radius=2, 
        fillOpacity=0,
        group="Towns"
      )
  })
    
  observe({
    if(input$town_name!="None") {
      town_row <- towns[towns$location==input$town_name, ]
      leafletProxy("map") %>%
        flyTo(lng=town_row$x, lat=town_row$y, zoom=10)
    } else {
      leafletProxy("map") %>%
        flyToBounds(
          lng1 = 137.725724, lat1 = -28.903687,
          lng2 = 151.677076, lat2 = -10.772608
        )
    }
  })
    
}

shinyApp(ui, server)