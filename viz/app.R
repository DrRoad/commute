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
shpf <- readOGR(dsn="sa20025WGSfil")
sa.in.dest <- shpf@data$SA22018_V1 %in% work_to$work_code
sa.in.home <- shpf@data$SA22018_V1 %in% work_from$res_code
transport.t <- c("Private car", "Passenger in car",
                 "Walk", "Bicycle", "Company car", "Bus", "Train",
                 "Ferry", "Work at home", "Other", "None/Unknown")
edu.t <- c("Drive self", "Passenger in car", "Walk", "Bicycle",
           "School bus", "Public bus", "Train", "Ferry", "Study at home",
           "Other", "None/Unknown")
cols.labs <- c(transport.t[1:10], "Total")
cols.edu.labs <- c(edu.t[1:10], "Total")

codelist <- shpf@data %>% 
  mutate(sa2_code = as.numeric(as.character(SA22018_V1))) %>% 
  select(sa2_code)

startcols.res <- codelist %>% left_join(work_from, by = c("sa2_code" = "res_code"))
startcols.res <- tencols[startcols.res$MAX]
startcols.res <- ifelse(is.na(startcols.res), "#808080", startcols.res)
startcols.work <- codelist %>% left_join(work_to, by = c("sa2_code" = "work_code"))
startcols.work <- tencols[startcols.work$MAX]
startcols.work <- ifelse(is.na(startcols.work), "#808080", startcols.work)
startcols.edures <- codelist %>% left_join(edu_from, 
                                           by = c("sa2_code" = "res_code"))
startcols.edures <- tencols[startcols.edures$MAX]
startcols.edures <- ifelse(is.na(startcols.edures), "#808080", startcols.edures)
startcols.edu <- codelist %>% left_join(edu_to, by = c("sa2_code" = "edu_code"))
startcols.edu <- tencols[startcols.edu$MAX]
startcols.edu <- ifelse(is.na(startcols.edu), "#808080", startcols.edu)

hrstr <- "<hr/>"

# Define UI
ui <- fluidPage(
  useShinyjs(),
  leafletjs,
  keyboardjs,
  tags$style(type = "text/css", extracss),
  
  leafletOutput("map"),
  absolutePanel(top = 10, right = 10, id="mapcontrol",
                div(
                radioButtons("radioeduemp", 
                             label = "Commuters (age 15+) travelling to",
                             choiceNames = list(
                               HTML("Em<span class='shortcut'>p</span>loyment"),
                               HTML("E<span class='shortcut'>d</span>ucation")
                             ),
                             choiceValues = list(
                               "Employment", "Education"
                             ),
                             inline = TRUE),
                radioButtons("radioinout", label="Show commuters who",
                             choiceNames = list(
                           HTML("Commute <span class='shortcut'>f</span>rom selected area"),
                           HTML("Commute <span class='shortcut'>t</span>o selected area")),
                             choiceValues = list(
                               "res",
                               "work"
                               ),
                             inline = FALSE),
                radioButtons("radiocolour",
                             label = "Colour by",
                             choiceNames = list(
                           HTML("M<span class='shortcut'>o</span>st common commute method"),
                           HTML("N<span class='shortcut'>u</span>mber of commuters")
                             ),
                             choiceValues = list(
                               "type",
                               "number"
                             ),
                             inline = FALSE),
                div(class="locinfo",
                    htmlOutput("lochtml")),
                div(id="loc2"),
                    htmlOutput("secondarylochtml"))
                ),
  absolutePanel(top = 25, right = 10, id="control2",
                materialSwitch("controlswitch", value=TRUE, right=TRUE,
                               inline=TRUE, status="info")),
  absolutePanel(bottom = 26, right = 10, id="loading",
                p("Loading...")),
  absolutePanel(bottom=26, right=10, left=10, top=10, id="infopanel",
                infotext),
  absolutePanel(bottom=10, left=10, id="infobuttoncontainer",
    prettyToggle("mapinfobutton", 
                 label_on = HTML("<span class='shortcut'>I</span>nfo"),
                 label_off = HTML("<span class='shortcut'>I</span>nfo"),
                 icon_on=icon("times"),
                 icon_off = icon("info"),
                 animation = "pulse",
                 inline = TRUE,
                 status_on = "danger",
                 status_off = "info",
                 value = TRUE)
  )
)

# Define server logic
server <- function(input, output) {
  sel.SA2.code <- reactiveVal(0)
  attribupdate <- FALSE
  mouseover <- reactive({
    lastover <- input$map_shape_mouseover$id
    lastover <- ifelse(is.null(lastover), 0, lastover)
    lastout <- input$map_shape_mouseout$id
    lastout <- ifelse(is.null(lastout), 0, lastout)
    ifelse(lastout == lastover, 0, lastover)
  })
  output$map <- renderLeaflet({
    leaf <- leaflet(shpf, options = leafletOptions(minZoom = 3, maxZoom = 13,
                                                   crs = NULL)) %>% 
      addPolygons(color="#000", opacity = 1, weight=1,
                                fillColor = startcols.res, 
                  layerId = ~SA22018_V1,
                  label = shpf@data$SA22018__1,
                  fillOpacity = 1, group = "polys") %>%
      setView(174, -41, 6) %>%
      addResetMapButton() %>%
      addSearchFeatures("polys", 
                        options = searchFeaturesOptions(
                          hideMarkerOnCollapse = TRUE,
                          autoCollapse = FALSE,
                          openPopup = FALSE,
                          zoom=11,
                          position="topleft")) %>%
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
    if (input$radioeduemp == "Employment") {
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
    } else {
      # Education code
      if (input$radiocolour == "type") {
        if (input$radioinout == "work") {
          fcols <- startcols.edu
          if (psel) {
            codvs <- edu_simp %>% filter(edu_code == selcode)
            codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
            codvs <- tencols[codvs$MAX]
            fcols <- ifelse(is.na(codvs), "#808080", codvs)
          }
        } else {
          fcols <- startcols.edures
          if (psel) {
            codvs <- edu_simp %>% filter(res_code == selcode)
            codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "edu_code"))
            codvs <- tencols[codvs$MAX]
            fcols <- ifelse(is.na(codvs), "#808080", codvs)
          }
          
        }
        lp <- leafletProxy("map", data = shpf) %>%
          setShapeStyle(layerId = ~SA22018_V1, fillColor = fcols) %>%
          clearControls() %>%
          addLegend(position = "topleft",
                    colors = c(tencols, "#808080"),
                    labels = edu.t, opacity = 1,
                    title = "Commute method"
                    ) %>%
          clearGroup("hpoly")
      } else {
        if (input$radioinout == "work") {
          if (psel) {
            codvs <- edu_simp %>% filter(edu_code == selcode)
            codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "res_code"))
            cvals <- ifelse(codvs$total == 0, NA, codvs$total)
            
          } else {
            codvs <- codelist %>% 
              left_join(edu_to, by = c("sa2_code" = "edu_code"))
            cvals <- ifelse(codvs$total == 0, NA, codvs$total)
          }
        } else {
          if (psel) {
            codvs <- edu_simp %>% filter(res_code == selcode)
            codvs <- codelist %>% left_join(codvs, by=c("sa2_code" = "edu_code"))
            cvals <- ifelse(codvs$total == 0, NA, codvs$total)
          } else {
            codvs <- codelist %>% 
              left_join(edu_from, by = c("sa2_code" = "res_code"))
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
    cursel <- sel.SA2.code()
    p <- input$map_click
    pdat <- data.frame(Longitude = p$lng,
                      Latitude =p$lat)
    coordinates(pdat) <- ~ Longitude + Latitude
    proj4string(pdat) <- proj4string(shpf)
    ppoly <- over(pdat, shpf)
    codetmp <- as.numeric(as.character(ppoly[1,"SA22018_V1"]))
    codetmp <- ifelse(is.na(codetmp), 0, codetmp)
    newsl <- ifelse(sel.SA2.code() == codetmp, 0, codetmp)
    if (newsl != cursel) {
      sel.SA2.code(newsl)
      updateMap()
    }
  })
  observeEvent(input$map_zoom, once=TRUE, {
    if (!attribupdate) {
      shinyjs::html(selector=".leaflet-control-attribution.leaflet-control",
                    html = attribhtml)
      attribupdate <<- TRUE
    }
  })
  observeEvent(input$map_shape_mouseover, once=TRUE,{
    # Backup
    if (!attribupdate) {
      shinyjs::html(selector=".leaflet-control-attribution.leaflet-control",
                    html = attribhtml)
      attribupdate <<- TRUE
    }
  })
  observeEvent(input$radioeduemp, ignoreInit = TRUE, {
    updateMap()
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
  observeEvent(input$mapinfobutton, ignoreInit = TRUE, {
    if (input$mapinfobutton) {
      shinyjs::showElement("infopanel", anim=TRUE,
                           time = 0.5)
    } else {
      shinyjs::hideElement("infopanel", anim=TRUE,
                           time = 0.5)
      shinyjs::runjs("document.getElementById('map').focus()")
    }
  })
  output$lochtml <- renderUI({
    seled <- sel.SA2.code()
    seled <- ifelse(is.na(seled), 0, seled)
    if (!(seled %in% shpf@data$SA22018_V1)) {
      HTML("")
    } else {
      namesel <- shpf@data$SA22018__1[shpf@data$SA22018_V1 == seled]
      if (input$radioeduemp == "Employment") {
        if (input$radiocolour == "type") {
          str <- sprintf("<b>%s</b>", namesel)
          if (input$radioinout == "work") {
            str <- sprintf("<p>Commuting method of people who <b>work</b> in 
                           <u>%s</u></p>", str)
            vals <- as.numeric(work_to[work_to$work_code == seled, 5:15])
            vals <- ifelse(is.na(vals), 0, vals)
            vals <- ifelse(vals < 0, "~0", as.character(vals))
            listi <- paste0(sprintf("<li>%s: %s</li>", cols.labs, 
                    vals),
                    collapse="")
            str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
          } else {
            str <- sprintf("<p>Commuting method of people who <b>live</b> in 
                           <u>%s</u></p>", str)
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
            str <- sprintf("%s<p>%d people commute <b>to</b> employment in 
                            <b><u>%s</u></b></p>", str, val, namesel)
            if (val > 0) {
              subs <- work_simp %>% filter(work_code == seled) %>%
                arrange(desc(total)) %>% head(10)
              listi <- paste0(sprintf("<li>%s: %s</li>", subs$res_name, 
                    subs$total),
                    collapse="")
              str <- sprintf("%s<p>Top areas to commute from:<p>
                             <ul>%s</ul>", str, listi)
            }
          } else {
            val <- as.numeric(work_from[work_from$res_code == seled, 15])
            val <- ifelse(is.na(val), 0, ifelse(val < 0, 0, val))
            str <- sprintf("%s<p>%d people commute to employment <b>from</b> 
                            <b><u>%s</u></b></p>", str, val, namesel)
            if (val > 0) {
              subs <- work_simp %>% filter(res_code == seled) %>%
                arrange(desc(total)) %>% head(10)
              listi <- paste0(sprintf("<li>%s: %s</li>", subs$work_name, 
                    subs$total),
                    collapse="")
              str <- sprintf("%s<p>Top areas to commute to:<p>
                             <ul>%s</ul>", str, listi)
            }
            
          }
              HTML(str)
        }
      } else {
        if (input$radiocolour == "type") {
          str <- sprintf("<b>%s</b>", namesel)
          if (input$radioinout == "work") {
            str <- sprintf("<p>Commuting method of people who commute to 
                           <b>education</b> in 
                           <u>%s</u></p>", str)
            vals <- as.numeric(edu_to[edu_to$edu_code == seled, 5:15])
            vals <- ifelse(is.na(vals), 0, vals)
            vals <- ifelse(vals < 0, "~0", as.character(vals))
            listi <- paste0(sprintf("<li>%s: %s</li>", cols.edu.labs, 
                    vals),
                    collapse="")
            str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
          } else {
            str <- sprintf("<p>Commuting method to education
                           of people who <b>live</b> in 
                           <u>%s</u></p>", str)
            vals <- as.numeric(edu_from[edu_from$res_code == seled, 5:15])
            vals <- ifelse(is.na(vals), 0, vals)
            vals <- ifelse(vals < 0, "~0", as.character(vals))
            listi <- paste0(sprintf("<li>%s: %s</li>", cols.edu.labs, 
                    vals),
                    collapse="")
            str <- paste0(hrstr, str, "<ul>", listi, "</ul>")
            
          }
        } else {
          str <- hrstr
          if (input$radioinout == "work") {
            val <- as.numeric(edu_to[edu_to$edu_code == seled, 15])
            val <- ifelse(is.na(val), 0, ifelse(val < 0, 0, val))
            str <- sprintf("%s<p>%d people commute <b>to</b> education in 
                            <b><u>%s</u></b></p>", str, val, namesel)
            if (val > 0) {
              subs <- edu_simp %>% filter(edu_code == seled) %>%
                arrange(desc(total)) %>% head(10)
              listi <- paste0(sprintf("<li>%s: %s</li>", subs$res_name, 
                    subs$total),
                    collapse="")
              str <- sprintf("%s<p>Top areas to commute from:<p>
                             <ul>%s</ul>", str, listi)
            }
          } else {
            val <- as.numeric(edu_from[edu_from$res_code == seled, 15])
            val <- ifelse(is.na(val), 0, ifelse(val < 0, 0, val))
            str <- sprintf("%s<p>%d people commute to education <b>from</b> 
                            <b><u>%s</u></b></p>", str, val, namesel)
            if (val > 0) {
              subs <- edu_simp %>% filter(res_code == seled) %>%
                arrange(desc(total)) %>% head(10)
              listi <- paste0(sprintf("<li>%s: %s</li>", subs$edu_name, 
                    subs$total),
                    collapse="")
              str <- sprintf("%s<p>Top areas to commute to:<p>
                             <ul>%s</ul>", str, listi)
            }
              
          }
        }
          HTML(str)
      }
    }
  })
  
  output$secondarylochtml <- renderUI({
    curshp <- mouseover()
    cursel <- sel.SA2.code()
    if (curshp == 0) {
      if (cursel == 0) {
        HTML(paste0(hrstr, 
                  "<p><em>No area selected. Click on 
                  an area for more information.</em></p>"))
      } else {
        HTML("")
      }
    } else {
      shpname <- shpf@data$SA22018__1[curshp == shpf@data$SA22018_V1]
      if (cursel == 0) {
        if (input$radioeduemp == "Employment") {
          if (input$radioinout == "res") {
            fdf <- work_from %>% filter(res_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             transport.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to employment from 
                         %s. %s</em></p>", hrstr, tot, shpname,
                         pmp))
          } else {
            fdf <- work_to %>% filter(work_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             transport.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to employment in 
                         %s. %s</em></p>", hrstr, tot, shpname,
                         pmp))
            
          }
        } else {
          if (input$radioinout == "res") {
            fdf <- edu_from %>% filter(res_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             edu.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to education from 
                         %s. %s</em></p>", hrstr, tot, shpname,
                         pmp))
          } else {
            fdf <- edu_to %>% filter(edu_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             edu.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to education in 
                         %s. %s</em></p>", hrstr, tot, shpname,
                         pmp))
            
          }
          
        }
      } else {
        shpname.0 <- shpf@data$SA22018__1[cursel == shpf@data$SA22018_V1]
        if (input$radioeduemp == "Employment") {
          if (input$radioinout == "res") {
            fdf <- work_simp %>% filter(res_code == cursel,
                                        work_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             transport.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to employment 
                          in %s from %s. %s</em></p>", hrstr, tot, shpname,
                         shpname.0, pmp))
          } else {
            fdf <- work_simp %>% filter(work_code == cursel,
                                        res_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             transport.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to employment 
                          in %s from %s. %s</em></p>", hrstr, tot, shpname.0,
                         shpname, pmp))
          }
        } else {
          if (input$radioinout == "res") {
            fdf <- edu_simp %>% filter(res_code == cursel,
                                        edu_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             edu.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to education 
                          in %s from %s. %s</em></p>", hrstr, tot, shpname,
                         shpname.0, pmp))
          } else {
            fdf <- edu_simp %>% filter(edu_code == cursel,
                                        res_code == curshp)
            tot <- ifelse(nrow(fdf) == 0, 0, fdf$total)
            ttype <- ifelse(is.na(fdf$MAX) || nrow(fdf) == 0, 0, fdf$MAX)
            pmp <- ""
            if (ttype != 0) {
              pmp <- sprintf("Most common mode of transport: %s", 
                             edu.t[ttype])
            }
            HTML(sprintf("%s<p><em>%d people commute to education 
                          in %s from %s. %s</em></p>", hrstr, tot, shpname.0,
                         shpname, pmp))
          }
        }
        
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
