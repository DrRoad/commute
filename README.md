# How did Kiwis commute in 2018?

This is an R Shiny and Leaflet data visualisation of the [commuter data](https://datafinder.stats.govt.nz/data/category/census/2018/commuter-view/) from the 2018 NZ Census of Populations and Dwellings; you can see it in action [here](https://shiny.petras.space/commute/). It serves as my entry for the 2020 [There and Back Again](https://www.stats.govt.nz/2018-census/there-and-back-again-data-visualisation-competition) data viz competition. The code for the visualisation itself is in the `viz/` folder, however note that it requires the code in `process.R` to be run first to produce the `.RData` file, along with the separate download of a [SA2 shapefile](https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/) (in WGS projection, and possibly simplified by the code in `maps.R`).

## Issues

Some problems with the code, for future personal reference.

### The Chatham Islands

Unfortunately I had to remove the Chatham Islands from the map, despite it being in the dataset! This was because their position on the far side of the 180° meridian causes issues with leaflet maps on WGS projections, while I was [unable to get an alternative projection to work](https://github.com/rstudio/leaflet/issues/693).

### Hovering

If you hover the cursor over a region, and then over the sea or a lake, the shiny code is capable of registering that you're not over that same region still. *However* if you then return to the same region it doesn't recognise that. This seems to be a limitation of how the shiny events stuff works. There might be something I can do with resetting the `input$`, but I'll look into it later.