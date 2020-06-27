library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(dplyr)
library(leaflet.extras)
library(shinyWidgets)

source("leafletfunctions.R")

# work_travel <- read_csv("../travel-work.csv")
load(file="datasets.RData")
shpf <- readOGR(dsn="sa20025WGSfilcth")
sa.in.dest <- shpf@data$SA22018_V1 %in% work_to$work_code
sa.in.home <- shpf@data$SA22018_V1 %in% work_from$res_code
transport.t <- c("Work at home", "Private car", "Company car", 
                "Carpool", "Bus", "Train", "Bicycle", "Walk",
                "Ferry", "Other", "None")
cols.labs <- c(transport.t[1:10], "Total")

codelist <- shpf@data %>% 
  mutate(sa2_code = as.numeric(as.character(SA22018_V1))) %>% 
  select(sa2_code)

startcols.res <- codelist %>% left_join(work_from, by = c("sa2_code" = "res_code"))
startcols.res <- tencols[startcols.res$MAX]
startcols.res <- ifelse(is.na(startcols.res), "#808080", startcols.res)
startcols.work <- codelist %>% left_join(work_to, by = c("sa2_code" = "work_code"))
startcols.work <- tencols[startcols.work$MAX]
startcols.work <- ifelse(is.na(startcols.work), "#808080", startcols.work)

hrstr <- "<hr style='border-top: 1px solid #000;'/>"

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
  }
  .radio label span p {
    margin-top: 3px;
    margin-bottom: 0px;
  }
  .leaflet-container {
    background-color: #84e1e1;
  }"),
  leafletOutput("map"),
  absolutePanel(top = 10, right = 10, id="mapcontrol",
                radioButtons("radioinout", label="Show commuters who",
                             choiceNames = list(
                               HTML("<p>Commute <b>from</b> selected area</p>"),
                               HTML("<p>Commute <b>to</b> selected area</p>")),
                             choiceValues = list(
                               "res",
                               "work"
                               ),
                             inline = FALSE),
                radioButtons("radiocolour",
                             label = "Colour by",
                             choiceNames = list(
                               HTML("<p>Most common commute method</p>"),
                               HTML("<p>Number of commuters</p>")
                             ),
                             choiceValues = list(
                               "type",
                               "number"
                             ),
                             inline = FALSE),
                div(id="locinfo",
                    htmlOutput("lochtml"))),
  absolutePanel(top = 25, right = 10, id="control2",
                materialSwitch("controlswitch", value=TRUE, right=TRUE,
                               inline=TRUE, status="info")),
  absolutePanel(bottom = 30, left = 10, id="loading",
                p("Loading..."))
)

# Define server logic
server <- function(input, output) {
  sel.SA2.code <- reactiveVal(0)
  output$map <- renderLeaflet({
    leaf <- leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
      addPolygons(color="#000", opacity = 1, weight=1,
                                fillColor = startcols.res, 
                  layerId = ~SA22018_V1,
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1) %>%
      setView(174, -41, 5) %>%
      addResetMapButton() %>%
      addLegend(position = "topleft",
                colors = c(tencols, "#808080"),
                labels = transport.t, opacity = 1,
                title = "Commute method")
    shinyjs::hideElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide", time=10)
    leaf
  })
  updateMap <- function() {
    shinyjs::showElement(selector="#loading p", asis = TRUE, 
                         anim=TRUE, animType = "slide")
    selcode <- sel.SA2.code()
    selcode <- ifelse(is.na(selcode), 0, selcode)
    psel <- selcode %in% shpf@data$SA22018_V1
    if (input$radiocolour == "type") {
      if (input$radioinout == "work") {
        fcols <- startcols.work
        if (psel) {
          codvs <- work_simp %>% filter(work_code == selcode)
          codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
          codvs <- tencols[codvs$MAX]
          fcols <- ifelse(is.na(codvs), "#808080", codvs)
        }
      } else {
      fcols <- startcols.res
        if (psel) {
          codvs <- work_simp %>% filter(res_code == selcode)
          codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "work_code"))
          codvs <- tencols[codvs$MAX]
          fcols <- ifelse(is.na(codvs), "#808080", codvs)
        }
        
      }
      lp <- leafletProxy("map", data = shpf) %>%
        setShapeStyle(layerId = ~SA22018_V1, fillColor = fcols) %>%
        clearControls() %>%
        addLegend(position = "topleft",
                  colors = c(tencols, "#808080"),
                  labels = transport.t, opacity = 1,
                  title = "Commute method"
                  ) %>%
        clearGroup("hpoly")
    } else {
      if (input$radioinout == "work") {
        if (psel) {
          codvs <- work_simp %>% filter(work_code == selcode)
          codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
          cvals <- ifelse(codvs$total == 0, NA, codvs$total)
          
        } else {
          codvs <- codelist %>% 
            left_join(work_to, by = c("sa2_code" = "work_code"))
          cvals <- ifelse(codvs$total == 0, NA, codvs$total)
        }
      } else {
        if (psel) {
          codvs <- work_simp %>% filter(res_code == selcode)
          codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "work_code"))
          cvals <- ifelse(codvs$total == 0, NA, codvs$total)
        } else {
          codvs <- codelist %>% 
            left_join(work_from, by = c("sa2_code" = "res_code"))
          cvals <- ifelse(codvs$total == 0, NA, codvs$total)
        }
      }
      cvr <- range(cvals, na.rm = TRUE)
      binner <- colorBin(c("white", "red"), cvr, bins = 7, pretty = TRUE)
      lp <- leafletProxy("map", data = shpf) %>%
        setShapeStyle(layerId = ~SA22018_V1, fillColor = binner(cvals)) %>%
        clearControls() %>%
        addLegend(position = "topleft",
                  pal = binner,
                  values = cvals, opacity = 1,
                  na.label = "None",
                  title = "Number of commuters") %>%
        clearGroup("hpoly")
    }
    if (psel) {
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
  observeEvent(input$map_click, ignoreInit = TRUE, {
    p <- input$map_click
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
  observeEvent(input$map_shape_mouseover, once=TRUE,{
    shinyjs::html(selector=".leaflet-control-attribution.leaflet-control",
                  html = '
<a href="http://leafletjs.com" 
title="A JS library for interactive maps">Leaflet</a> | <a 
href="https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/"
title="Source data">
StatsNZ</a> | <a href="https://petras.space/page/cv/" title="Hire me!">
Petra Lamborn</a> | Numbers subject to <a
href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/
confidentiality-of-info-supplied-to-snz/safeguarding-confidentiality.aspx"
title="A method of preserving confidentiality and anonymity">
random rounding</a>
                  '
                )
               })
  observeEvent(input$radioinout, ignoreInit = TRUE, {
    updateMap()
  })
  observeEvent(input$radiocolour, ignoreInit = TRUE, {
    updateMap()
  })
  observeEvent(input$controlswitch, ignoreInit = TRUE, {
    shinyjs::toggleElement("mapcontrol", anim=TRUE,
                           time = 0.5)
  })
  output$lochtml <- renderUI({
    seled <- sel.SA2.code()
    seled <- ifelse(is.na(seled), 0, seled)
    if (!(seled %in% shpf@data$SA22018_V1)) {
      HTML(paste0(hrstr, 
                  "<p><em>No area selected</em></p>"))
    } else {
      namesel <- shpf@data$SA22018__1[shpf@data$SA22018_V1 == seled]
      if (input$radiocolour == "type") {
        str <- sprintf("<b>%s</b>", namesel)
        if (input$radioinout == "work") {
          str <- sprintf("<p>Commuting method of people who <b>work</b> in</p>
                         <p><b><u>%s</u></b></p>", str)
          vals <- as.numeric(work_to[work_to$work_code == seled, 5:15])
          vals <- ifelse(is.na(vals), 0, vals)
          vals <- ifelse(vals < 0, "~0", as.character(vals))
          listi <- paste0(sprintf("<li>%s: %s</li>", cols.labs, 
                  vals),
                  collapse="")
          str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
        } else {
          str <- sprintf("<p>Commuting method of people who <b>live</b> in</p>
                         <p><u>%s</u></p>", str)
          vals <- as.numeric(work_from[work_from$res_code == seled, 5:15])
          vals <- ifelse(is.na(vals), 0, vals)
          vals <- ifelse(vals < 0, "~0", as.character(vals))
          listi <- paste0(sprintf("<li>%s: %s</li>", cols.labs, 
                  vals),
                  collapse="")
          str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
        }
        HTML(str)
      } else {
        str <- hrstr
        if (input$radioinout == "work") {
          val <- as.numeric(work_to[work_to$work_code == seled, 15])
          val <- ifelse(is.na(val), 0, ifelse(val < 0, 0, val))
          str <- sprintf("%s<p>%d people commute <b>to</b></p>
                          <p><b><u>%s</u></b></p>", str, val, namesel)
          if (val > 0) {
            subs <- work_simp %>% filter(work_code == seled) %>%
              arrange(desc(total)) %>% head(10)
            listi <- paste0(sprintf("<li>%s: %s</li>", subs$res_name, 
                  subs$total),
                  collapse="")
            str <- sprintf("%s<p>Top areas to commute from<p>
                           <ul>%s</ul>", str, listi)
          }
        } else {
          val <- as.numeric(work_from[work_from$res_code == seled, 15])
          val <- ifelse(is.na(val), 0, ifelse(val < 0, 0, val))
          str <- sprintf("%s<p>%d people commute <b>from</b></p>
                          <p><b><u>%s</u></b></p>", str, val, namesel)
          if (val > 0) {
            subs <- work_simp %>% filter(res_code == seled) %>%
              arrange(desc(total)) %>% head(10)
            listi <- paste0(sprintf("<li>%s: %s</li>", subs$work_name, 
                  subs$total),
                  collapse="")
            str <- sprintf("%s<p>Top areas to commute to<p>
                           <ul>%s</ul>", str, listi)
          }
          
        }
        HTML(str)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
