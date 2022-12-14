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

rf_contours <-
  read_sf(paste0(getwd(),"/Contours/ContourLines_gda94_utm50_EPSG28350.shp"))
plot(st_convex_hull(rf_contours[2]), max.plot=24);axis(1);axis(2)
head(st_coordinates(rf_contours))

rf_hydrology <-
  read_sf(paste0(getwd(),"/Hydrology/RiverSystems_gda94_utm50_EPSG28350.shp"))
str(rf_hydrology)
plot(rf_hydrology[3], max.plot=24);axis(1);axis(2)
head(st_coordinates(rf_hydrology))
unique(rf_hydrology$DOMAIN_V1)

rf_landforms <-
  read_sf(paste0(getwd(),"/Landforms/Landforms_gda94_utm50_EPSG28350.shp"))
str(rf_landforms)
par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(rf_landforms[7], max.plot=24, key.pos=4);axis(1);axis(2)
head(st_coordinates(rf_landforms))
unique(rf_landforms$MU_Soil)

list.files(paste0(getwd(),"/Vegetation"))
rf_veg <-
  read_sf(paste0(getwd(),"/Vegetation/Vegetation_gda94_utm50_EPSG28350.shp"))
str(rf_veg)

list.files(paste0(getwd(),"/Paddocks"))
rf_paddocks <-
  read_sf(paste0(getwd(),"/Paddocks/Paddocks_gda94_utm50_EPSG28350.shp"))
str(rf_paddocks)

par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(st_geometry(rf_veg),col=c(2,3,4,5,6,7,8))
plot(st_geometry(rf_hydrology),col="blue",add=T)
plot(st_geometry(rf_contours),col="chocolate",add=T)
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

list.files(paste0(getwd(),"/Elevation")) # not point elevations for Ridgefield!
rf_elevation_point <- read_sf(paste0(getwd(),"/Elevation/elevation_point.shp"))
str(rf_elevation_point)
par(oma=c(4,4,2,22),mar=c(0,0,0,0))
plot(st_geometry(rf_elevation_point))
plot(rf_elevation_point[22], max.plot=24)
plot(st_coordinates(elevation_point)[,1:2],asp=1,type="l")
unique(rf_veg$MU_VEG)

# GSWA shapefiles ####

list.files(paste0(getwd(),"/50k_shp_INDEX"))
index_50k <-
  read_sf(paste0(getwd(),"/50k_shp_INDEX/index50.shp"))
str(index_50k)
index_50k_GDA94 <- st_transform(index_50k, crs=st_crs(28350))
index_crop <- st_crop(index_50k, xmin=115.5, ymin=-33, xmax=117.6, ymax=-31.8)
index_crop_GDA94 <- st_transform(index_crop, crs=st_crs(28350))
plot(st_geometry(index_crop_GDA94))
plot(rf_paddocks[2], add=TRUE, col="red2")
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="PERTH"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="PINJARRA"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="MANDURAH"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="ROCKINGHAM"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="FREMANTLE"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="MUNDARING"), add=T)
plot(subset(index_50k_GDA94, index_50k_GDA94$SHEET_NAME=="PENCELL"), add=T)
plot(rf_hydrology, add=TRUE, col="red2")

list.files(paste0(getwd(),"/Pinjarra_geology"))
pinj_geol <-
  read_sf(paste0(getwd(),"/Pinjarra_geology/geology_surface_20321.shp"))
str(pinj_geol)
pinj_geol_GDA94 <- st_transform(pinj_geol, crs=st_crs(28350))
st_bbox(pinj_geol_GDA94)
st_bbox(rf_hydrology)

require(ggmap)
rf_gg <- get_googlemap(c(mean(lons0),mean(lats0)), maptype="satellite",zoom=13)
ggmap(rf_gg)+
  geom_path(mapping = aes(x=Longitude,y=Latitude),
            data = rf_boundary,col="white") +
  # geom_sf(data = rf_contours) +
  coord_sf(crs=st_crs(4326))

str(rf_veg)
unique(rf_veg$MU_LFORM)
plot(rf_veg, max.plot=18)
palette(colorRampPalette(c("#600000","#F0F0F0"))(6))
ggplot() +
  geom_sf(data = rf_veg, aes(fill=MU_LFORM)) +
  geom_sf(data = rf_hydrology, aes(col=SUB_TYPE)) +
  scale_fill_manual(values=palette()[c(2,4,1,3,5,6)]) +
  scale_color_manual(values = c("#0000B0", "darkorchid")) +
  # geom_path(mapping = aes(x=Longitude,y=Latitude),
  #           data = rf_boundary[,3:4],col="#B0B0B080",size=2) +
  xlim(116.945,117.007) +
  ylim(-32.51,-32.468) +
  coord_sf(crs=st_crs(4326)) +
  theme_bw()

palette(rainbow(length(rf_veg$geometry),v=0.8))
plot(st_coordinates(rf_veg$geometry[[1]]), xlim=c(495060,500570),
     ylim=c(6403060,6407750),col="#00000000")
for(i in 1:length(rf_veg$geometry)){
  polygon(st_coordinates(rf_veg$geometry[[i]])[,1:2],border=i,lwd=1)
}

# 2 metre contours from SLIP WA ####
# https://catalogue.data.wa.gov.au/dataset/dpird-2-metre-contours

list.files(paste0(getwd(),"/WA_2m_contours"))
contours2m <-
  read_sf(paste0(getwd(),"/WA_2m_contours/Contours_2_m_DPIRD_072.shp"))
str(contours2m)
paddocksGDA94 <- st_transform(rf_paddocks, crs=st_crs(4283))
contours_RF <- st_crop(contours2m,st_bbox(paddocksGDA94))
contours_RF2m <- st_intersection(contours_RF,paddocksGDA94)
rm(contours2m)
save.image()
plot(st_geometry(contours_RF2m))
