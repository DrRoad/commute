library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(dplyr)

# work_travel <- read_csv("../travel-work.csv")
load(file="datasets.RData")
shpf <- readOGR(dsn="../shapefiles/sa20025WGSfilcth")
sa.in.dest <- shpf@data$SA22018_V1 %in% work_to$work_code
sa.in.home <- shpf@data$SA22018_V1 %in% work_from$res_code
transport.t <- c("Work at home", "Private car", "Company car", 
                "Carpool", "Bus", "Train", "Bicycle", "Walk",
                "Ferry", "Other", "None")

codelist <- shpf@data %>% 
  mutate(sa2_code = as.numeric(as.character(SA22018_V1))) %>% 
  select(sa2_code)

startcols <- codelist %>% left_join(work_from, by = c("sa2_code" = "res_code"))
startcols <- tencols[startcols$MAX]
startcols <- ifelse(is.na(startcols), "#808080", startcols)


# Define UI
ui <- fluidPage(
  useShinyjs(),
  tags$style(type = "text/css", 
  "html, body {
    width:100%;
    height:100%
  }
  #map {
    height: 100% !important;
    position: absolute !important;
    top: 0;
    left: 0;
  }
  #loading {
    cursor: progress !important;
  }
  #loading p {
    border-radius: 25px;
    background: #FFFFFF;
    padding: 10px;
    border: 2px solid #000000;
    font-size: 1.5em;
    font-weight: bold;
  }"),
  leafletOutput("map"),
  absolutePanel(bottom = 30, left = 30, id="loading",
                p("Loading..."))
)

# Define server logic
server <- function(input, output) {
  sel.SA2.code <- 0
  p.layers <- c("polya", "polyb")
  output$map <- renderLeaflet({
    leaf <- leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
      addPolygons(group = p.layers[1] ,color="#000", opacity = 1, weight=1,
                                fillColor = startcols, 
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1) %>%
      setView(174, -41, 5) %>%
      addLegend(position = "topleft",
                colors = c(tencols, "#808080"),
                labels = transport.t, opacity = 1)
    shinyjs::hideElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide", time=7)
    leaf
  })
  observeEvent(input$map_shape_click, {
    shinyjs::showElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide")
    p <- input$map_shape_click
    print(p)
    pdat <- data.frame(Longitude = p$lng,
                      Latitude =p$lat)
    # Assignment modified according
    coordinates(pdat) <- ~ Longitude + Latitude
    # Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
    proj4string(pdat) <- proj4string(shpf)
    ppoly <- over(pdat, shpf)
    codetmp <- as.numeric(as.character(ppoly[1,"SA22018_V1"]))
    print(sel.SA2.code)
    sel.SA2.code <<- ifelse(sel.SA2.code == codetmp, 0, codetmp)
    print(ppoly)
    #print(work_simp[work_simp$res_code == 
    #                    sel.SA2.code,])
    print(sel.SA2.code)
    print(codetmp)
    fcols <- startcols
    if (sel.SA2.code != 0) {
      print(head(work_simp))
      codvs <- work_simp %>% filter(work_code == sel.SA2.code)
      print(head(codvs))
      codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
      print(head(codvs))
      print(table(codvs$MAX))
      codvs <- tencols[codvs$MAX]
      print(table(codvs))
      fcols <- ifelse(is.na(codvs), "#808080", codvs)
      print(table(fcols))
    }
    print(table(fcols))
    leafletProxy("map", data = shpf) %>%
      addPolygons(group = p.layers[2] ,color="#000", opacity = 1, weight=1,
                                fillColor = fcols,
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1) %>%
      clearGroup(p.layers[1])
    p.layers <<- rev(p.layers)
    shinyjs::hideElement(selector="#loading p", asis=TRUE, 
                         anim=TRUE, animType = "slide", time = 7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
