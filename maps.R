library(rgdal)
library(rmapshaper)

sf <- readOGR(dsn = "shapefiles/statsnzstatistical-area-2-2018-generalised-SHP/")

plot(sf, xlim=c(1750000, 1760000), ylim=c(5430000, 5440000))
# insf <- ms_innerlines(sf)

ssf1 <- ms_simplify(sf)
plot(ssf1, xlim=c(1750000, 1760000), ylim=c(5430000, 5440000))

ssf2.5 <- ms_simplify(sf, 0.025)
plot(ssf2.5, xlim=c(1750000, 1760000), ylim=c(5430000, 5440000))

ssf2 <- ms_simplify(sf, 0.01)
plot(ssf2, xlim=c(1750000, 1760000), ylim=c(5430000, 5440000))

ssf3 <- ms_simplify(sf, 0.002)
plot(ssf3, xlim=c(1750000, 1760000), ylim=c(5430000, 5440000))

writeOGR(ssf2, dsn = "shapefiles/sa2001/", layer = "SA2", driver="ESRI Shapefile")
