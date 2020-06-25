library(shiny)
library(leaflet)
library(rgdal)

sf <- readOGR(dsn="../shapefiles/sa20025WGSfilcth")
leaflet(sf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
  addPolygons(color="#000", opacity = 1, weight=1,
                            popup = sf@data$SA22018__1) %>%
  setView(174, -41, 5)

# Define UI
ui <- fluidPage(
)

# Define server logic
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
