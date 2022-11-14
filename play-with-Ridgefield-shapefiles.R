library(sf)
TEMP.utm <- st_as_sf(x = rf_boundary,
                            coords = c("Easting", "Northing"), crs = UTM50S)
summary(st_coordinates(TEMP.utm), digits=8)
TEMP.LL <- st_transform(TEMP.utm, crs = LongLat)
print(summary(st_coordinates(TEMP.LL)), digits=8)
#
rf_boundary$Longitude <- signif(st_coordinates(TEMP.LL)[,1],9)
rf_boundary$Latitude <- signif(st_coordinates(TEMP.LL)[,2],9)
write.csv(rf_boundary, file="rf_boundary_utm_LL.csv", row.names=FALSE)

with(rf_boundary, plot(Longitude, Latitude, asp=1, type="l"))
image(elev.TEMP[,1],elev.TEMP[,2],elev.TEMP[,3])

rf_contours <- read_sf(paste0(getwd(),"/Contours/ContourLines_gda94_utm50_EPSG28350.shp"))
plot(rf_contours[21], max.plot=24);axis(1);axis(2)
head(st_coordinates(rf_contours))

rf_hydrology <- read_sf(paste0(getwd(),"/Hydrology/RiverSystems_gda94_utm50_EPSG28350.shp"))
str(rf_hydrology)
plot(rf_hydrology[3], max.plot=24);axis(1);axis(2)
head(st_coordinates(rf_hydrology))
unique(rf_hydrology$DOMAIN_V1)

rf_landforms <- read_sf(paste0(getwd(),"/Landforms/Landforms_gda94_utm50_EPSG28350.shp"))
str(rf_landforms)
par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(rf_landforms[7], max.plot=24, key.pos=4);axis(1);axis(2)
head(st_coordinates(rf_landforms))
unique(rf_landforms$MU_Soil)

list.files(paste0(getwd(),"/Vegetation"))
rf_veg <- read_sf(paste0(getwd(),"/Vegetation/Vegetation_gda94_utm50_EPSG28350.shp"))
str(rf_veg)
par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(st_geometry(rf_veg))
plot(st_geometry(rf_hydrology),col="blue",add=T)
plot(st_geometry(rf_contours),col="gold",add=T)
plot(rf_veg[9], max.plot=24, key.pos=4);axis(1);axis(2)
head(st_coordinates(rf_veg))
unique(rf_veg$MU_VEG)

list.files(paste0(getwd(),"/Roads"))
rf_roads <- read_sf(paste0(getwd(),"/Roads/Roads_gda94_utm50_EPSG28350.shp"))
str(rf_roads)
par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(st_geometry(rf_roads))
plot(rf_roads[1], max.plot=24, xlim=c(499210,500280),
     ylim=c(6403066,6407050));axis(1);axis(2)
plot(st_coordinates(rf_veg)[,1:2],asp=1,type="l")
unique(rf_veg$MU_VEG)

require(ggmap)
rf_gg <- get_googlemap(c(mean(lons0),mean(lats0)), maptype="satellite",zoom=13)
ggmap(rf_gg)+
  geom_path(mapping = aes(x=Longitude,y=Latitude),
            data = rf_boundary,col="white") +
  # geom_sf(data = rf_contours) +
  coord_sf(crs=st_crs(4326))

ggplot() +
  geom_sf(data = rf_veg, aes(fill=MAPPING_UN)) +
  geom_sf(data = rf_hydrology, aes(col=SUB_TYPE)) +
  theme_bw()
