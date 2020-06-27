library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(dplyr)
library(leaflet.extras)

source("leafletfunctions.R")

# work_travel <- read_csv("../travel-work.csv")
load(file="datasets.RData")
shpf <- readOGR(dsn="../shapefiles/sa20025WGSfilcth")
sa.in.dest <- shpf@data$SA22018_V1 %in% work_to$work_code
sa.in.home <- shpf@data$SA22018_V1 %in% work_from$res_code
transport.t <- c("Work at home", "Private car", "Company car", 
                "Carpool", "Bus", "Train", "Bicycle", "Walk",
                "Ferry", "Other", "None")
cols.labs <- c(transport.t[1:10], "Total")

codelist <- shpf@data %>% 
  mutate(sa2_code = as.numeric(as.character(SA22018_V1))) %>% 
  select(sa2_code)

startcols <- codelist %>% left_join(work_from, by = c("sa2_code" = "res_code"))
startcols <- tencols[startcols$MAX]
startcols <- ifelse(is.na(startcols), "#808080", startcols)


# Define UI
ui <- fluidPage(
  useShinyjs(),
  leafletjs,
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
    border-radius: 5px;
    background-color: rgba(255, 255, 255, 0.8);
    padding: 6px 8px;
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    font-size: 1.5em;
    font-weight: bold;
  }
  #mapcontrol {
    background-color: rgba(255, 255, 255, 0.8);
    border-radius: 5px;
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    padding: 6px 8px;
    font: 14px/16px Arial, Helvetica, sans-serif;
  }
  #lochtml ul {
    padding-left: 15px;
  }"),
  leafletOutput("map"),
  absolutePanel(top = 10, right = 10, id="mapcontrol",
                radioButtons("radioinout", label=NULL,
                             choices = c(
                               "Work in" = "work",
                               "Live in" = "res"
                               ),
                             inline = TRUE),
                div(id="locinfo",
                    htmlOutput("lochtml"))),
  absolutePanel(bottom = 30, left = 30, id="loading",
                p("Loading..."))
)

# Define server logic
server <- function(input, output) {
  sel.SA2.code <- reactiveVal(0)
  p.layers <- c("polya", "polyb")
  output$map <- renderLeaflet({
    leaf <- leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
      addPolygons(color="#000", opacity = 1, weight=1,
                                fillColor = startcols, 
                  layerId = ~SA22018_V1,
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1) %>%
      setView(174, -41, 5) %>%
      addResetMapButton() %>%
      addLegend(position = "topleft",
                colors = c(tencols, "#808080"),
                labels = transport.t, opacity = 1)
    shinyjs::hideElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide", time=7)
    leaf
  })
  updateMap <- function() {
    selcode <- sel.SA2.code()
    selcode <- ifelse(is.na(selcode), 0, selcode)
    shinyjs::showElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide")
    fcols <- startcols
    
    if (selcode != 0) {
      codvs <- work_simp %>% filter(work_code == selcode)
      codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
      codvs <- tencols[codvs$MAX]
      fcols <- ifelse(is.na(codvs), "#808080", codvs)
    }
    lp <- leafletProxy("map", data = shpf) %>%
      setShapeStyle(layerId = ~SA22018_V1, fillColor = fcols) %>%
      clearControls() %>%
      addLegend(position = "topleft",
                colors = c(tencols, "#808080"),
                labels = transport.t, opacity = 1) %>%
      clearGroup("hpoly")
    if (selcode %in% shpf@data$SA22018_V1) {
      lp %>% addPolygons(group = "hpoly",
                          weight = 4,
                          data = shpf[which(shpf@data$SA22018_V1 == selcode),],
                          color = "#000000",
                         fill = FALSE, opacity = 1)
    }
    shinyjs::hideElement(selector="#loading p", asis=TRUE, 
                         anim=TRUE, animType = "slide",
                         time = 1)
  }
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click
    pdat <- data.frame(Longitude = p$lng,
                      Latitude =p$lat)
    coordinates(pdat) <- ~ Longitude + Latitude
    proj4string(pdat) <- proj4string(shpf)
    ppoly <- over(pdat, shpf)
    codetmp <- as.numeric(as.character(ppoly[1,"SA22018_V1"]))
    codetmp <- ifelse(is.na(codetmp), 0, codetmp)
    sel.SA2.code(ifelse(sel.SA2.code() == codetmp, 0, codetmp))
    updateMap()
  })
  output$lochtml <- renderUI({
    seled <- sel.SA2.code()
    seled <- ifelse(is.na(seled), 0, seled)
    if (seled == 0) {
      HTML("")
    } else {
      hrstr <- "<hr style='border-top: 1px solid #000;'/>"
      str <- sprintf("<p><b>%s</b></p>", 
                     shpf@data$SA22018__1[shpf@data$SA22018_V1 == seled])
      if (input$radioinout == "work") {
        str <- paste0("<p>People who work in</p>", str)
        vals <- as.numeric(work_to[work_to$work_code == seled, 5:15])
        vals <- ifelse(is.na(vals), 0, vals)
        vals <- ifelse(vals < 0, "~0", as.character(vals))
        listi <- paste0(sprintf("<li>%s: %s</li>", cols.labs, 
                vals),
                collapse="")
        str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
      } else {
        str <- paste0("<p>People who live in</p>", str)
        vals <- as.numeric(work_from[work_from$res_code == seled, 5:15])
        vals <- ifelse(is.na(vals), 0, vals)
        vals <- ifelse(vals < 0, "~0", as.character(vals))
        listi <- paste0(sprintf("<li>%s: %s</li>", cols.labs, 
                vals),
                collapse="")
        str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
      }
      HTML(str)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
