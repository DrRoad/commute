library(rgdal)
library(rmapshaper)

# sf <- readOGR(dsn = "shapefiles/statsnzstatistical-area-2-2018-generalised-SHP/")
sf <- readOGR(dsn = "shapefiles/SA2-2018-WGS/")
nrow(sf@data)

plot(sf, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
# insf <- ms_innerlines(sf)

ssf1 <- ms_simplify(sf, keep_shapes=TRUE)
plot(ssf1, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
nrow(ssf1@data)

ssf2.5 <- ms_simplify(sf, 0.025, keep_shapes=TRUE)
plot(ssf2.5, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
nrow(ssf2.5@data)

ssf2 <- ms_simplify(sf, 0.01, keep_shapes=TRUE)
plot(ssf2, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
nrow(ssf2@data)

ssf3 <- ms_simplify(sf, 0.002, keep_shapes=TRUE)
plot(ssf3, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
nrow(ssf3@data)

writeOGR(ssf2.5, dsn = "shapefiles/sa20025WGS/", layer = "SA2", driver="ESRI Shapefile")

ssf3b <- ms_simplify(sf, 0.002, keep_shapes=FALSE)
plot(ssf3b, xlim=c(1480000, 1510000), ylim=c(5150000, 5180000))
nrow(ssf3b@data)

ci <- sf@data$SA22018__1 != "Chatham Islands"
head(ssf2.5@data)
laper <- (sf@data$LAND_AREA_ / sf@data$AREA_SQ_KM)
sf@data$SA22018__1[laper > 0.1 & laper < 0.4]
plot(ssf2.5[(laper > 0.1) & ci,])
writeOGR(ssf2.5[(laper > 0.1) & ci,], dsn = "shapefiles/sa20025WGSfil/", layer = "SA2", driver="ESRI Shapefile")
