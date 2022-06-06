
library(shiny)
library(leaflet)

towns <- read.csv("input/QLD_locations_with_RSQ_times_20220518.csv")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      div.full-page {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        overflow: hidden;
        padding: 0;
      }
      "
    ))
  ),
  div(
      class="full-page",
      leafletOutput('map', height="100%", width="100%"),
      absolutePanel(
        top=75, left=10,
        class = "panel panel-default",
        selectInput('town_name', 'Town Name',
                    choices = c('None', sort(towns$location)),
                    selected = "None")
      )
  )
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
