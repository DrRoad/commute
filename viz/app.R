library(shiny)
library(leaflet)
library(rgdal)
library(readr)
library(dplyr)

work_travel <- read_csv("../travel-work.csv")
shpf <- readOGR(dsn="../shapefiles/sa20025WGSfilcth")

# Define UI
ui <- fluidPage(
  leafletOutput("map")
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
      addPolygons(color="#000", opacity = 1, weight=1,
                                popup = shpf@data$SA22018__1) %>%
      setView(174, -41, 5)
  })
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click
    print(p)
    pdat <- data.frame(Longitude = p$lng,
                      Latitude =p$lat)
    # Assignment modified according
    coordinates(pdat) <- ~ Longitude + Latitude
    # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
    proj4string(pdat) <- proj4string(shpf)
    ppoly <- over(pdat, shpf)
    print(ppoly)
    print(work_travel[work_travel$SA2_code_usual_residence_address == 
                        ppoly[1,"SA22018_V1"],])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
