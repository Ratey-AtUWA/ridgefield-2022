library(sp)
library(rgdal)
LongLat <- CRS("+proj=longlat +ellps=WGS84 
               +datum=WGS84 +no_defs")
UTM50S <- CRS("+proj=utm +zone=50 +south")

nim2 <- na.omit(nimb); row.names(nim2) <- NULL

NimBrk <- SpatialPointsDataFrame(coords = nim2[,c("Longitude","Latitude")], 
                 data=data.frame(Elevation=nim2[,c("Elevation")]), # seem to need a variable
                 proj4string = LongLat, match.ID = FALSE)

writeOGR(obj=NimBrk["Elevation"], # this is where variable is needed
         dsn = "NimBrk.kml", 
         layer = "Elevation", 
         driver = "KML",
         overwrite_layer = TRUE)
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

tempdata <- na.omit(afw21[,c("Easting","Northing","Ca","Fe","K","Na")])
afw21utm <- SpatialPointsDataFrame(coords = tempdata[,c("Easting","Northing")], 
                                   data=tempdata[,c("Ca","Fe","K","Na")], 
                                   proj4string = UTM50)
afw21LL <- spTransform(afw21utm, CRSobj = LongLat)

writeOGR(obj = afw21LL["Na"], 
         dsn = "afw21.kml", 
         layer = "Na", 
         driver = "KML",
         overwrite_layer = TRUE)
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

cores_LL <- SpatialPointsDataFrame(coords = core_locs[,c("Longitude","Latitude")],
                                    data = data.frame(CoreNum=seq(1:13)),
                                    proj4string = LongLat)
writeOGR(obj = cores_LL["CoreNum"], 
         dsn = "DBCAcore.kml", 
         layer = "CoreNum", 
         driver = "KML",
         overwrite_layer = TRUE)
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

newE2022_LL <- SpatialPointsDataFrame(coords = newE2022[,c("Longitude","Latitude")],
                                   data = data.frame(CoreNum=seq(1:8)),
                                   proj4string = LongLat)
writeOGR(obj = newE2022_LL["CoreNum"], 
         dsn = "newE22.kml", 
         layer = "CoreNum", 
         driver = "KML",
         overwrite_layer = TRUE)
