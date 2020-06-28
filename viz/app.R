library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(dplyr)
library(leaflet.extras)
library(shinyWidgets)

source("leafletfunctions.R")
source("extras.R")

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
  tags$style(type = "text/css", extracss),
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
                p("Loading...")),
  absolutePanel(bottom=26, right=10, left=10, top=10, id="infopanel",
                p("Test")),
  absolutePanel(bottom=10, right=10, id="infobuttoncontainer",
    prettyToggle("mapinfobutton", label_on = "Info",
                 label_off = "Info", icon_on=icon("times"),
                 icon_off = icon("info"),
                 animation = "pulse",
                 inline = TRUE,
                 status_on = "danger",
                 status_off = "info")
  )
)

# Define server logic
server <- function(input, output) {
  sel.SA2.code <- reactiveVal(0)
  attribupdate <- FALSE
  output$map <- renderLeaflet({
    leaf <- leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13)) %>% 
      addPolygons(color="#000", opacity = 1, weight=1,
                                fillColor = startcols.res, 
                  layerId = ~SA22018_V1,
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1) %>%
      setView(174, -41, 6) %>%
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
  observeEvent(input$map_zoom, once=TRUE, {
    if (!attribupdate) {
      print(attribupdate)
      shinyjs::html(selector=".leaflet-control-attribution.leaflet-control",
                    html = attribhtml)
      attribupdate <<- TRUE
    }
  })
  observeEvent(input$map_shape_mouseover, once=TRUE,{
    # Backup
    if (!attribupdate) {
      print(attribupdate)
      shinyjs::html(selector=".leaflet-control-attribution.leaflet-control",
                    html = attribhtml)
      attribupdate <<- TRUE
    }
  })
  observeEvent(input$radioinout, ignoreInit = TRUE, {
    updateMap()
  })
  observeEvent(input$infobutton, {
    print(input$infobutton)
  })
  observeEvent(input$radiocolour, ignoreInit = TRUE, {
    updateMap()
  })
  observeEvent(input$controlswitch, ignoreInit = TRUE, {
    shinyjs::toggleElement("mapcontrol", anim=TRUE,
                           time = 0.5)
  })
  observeEvent(input$mapinfobutton, ignoreInit = TRUE, {
    shinyjs::toggleElement("infopanel", anim=TRUE,
                           time = 1)
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
