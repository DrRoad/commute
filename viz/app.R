library(shiny)
library(leaflet)
library(rgdal)

sf <- readOGR(dsn="../shapefiles/sa20025WGSfil")
crs <- leafletCRS(proj4def = sf@proj4string,
                  bounds=sf@bbox)
leaflet(sf) %>% addPolygons()

# Define UI for application that draws a histogram
ui <- fluidPage(
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
