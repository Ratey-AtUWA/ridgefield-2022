# create matrix of test elevations in Excel using summed exponential functions &
# import into R data frame testElev, check with plot:

# testElev <- read.table("clipboard", sep="\t", header=TRUE) # no accidental run
image(seq(498000,499950,50), seq(6405700,6407650,50),
      as.matrix(testElev[,2:41]))
contour(seq(498000,499950,50), seq(6405700,6407650,50),
        as.matrix(testElev[,2:41]), add=TRUE, labcex=1.)
# make into a 3-column dataframe (x,y,elev)
elevDF <- data.frame(x=rep(seq(498000,499950,50),40),
                     y=rep(seq(6405700,6407650,50),each=40),
                     elev=c(as.matrix(testElev[,2:41])))
# summary(elevDF)
testRast <- rasterFromXYZ(elevDF)
crs(testRast) <- "+proj=utm +zone=50 +south"
test_slope <- terrain(testRast, opt = "slope", unit = "degrees")

# this section based on
# https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/

# first load packages and some other setup stuff
library(raster) # slope parameters from raster::terrain() function
library(rgdal)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fgeo.analyze)
GDA94 <- CRS("+proj=geocent +ellps=GRS80 +units=m +no_defs +type=crs")

# read geoTIFF into R rasterLayer
GDALinfo("Ridgefield_DEM_gda94_utm50_epsg28350.tif") # just file info
RF_elev <- raster("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
  # make raster subset with no NA
  NWrect <- extent(498000,500500,6405700,6407000)
  NW_elev <- raster::crop(RF_elev,NWrect)
  RF_elevDF <- as.data.frame(RF_elev, xy=TRUE)
  colnames(RF_elevDF)[3] <- "elev"

# plot the data frame made from elevation raster
elevMapGG <- ggplot() +
  geom_raster(data = RF_elevDF , aes(x = x, y = y, fill = elev)) +
  geom_sf(data = rf_hydrology, col="steelblue2") + # add the streams
  scale_fill_viridis_c(na.value = "#FFFFFF00", name="Elevation (m)") +
  labs(x = "Longitude", y="Latitude") +
  coord_sf(crs = st_crs(28350)) +
  annotate(geom="text", x=495000, y=6407500,
           label=paste0("Elevations based on DEM with 13m cells",
                        "\nProjection: Long-Lat using GDA94"),
           size=6, hjust = 0) +
  theme_bw() +
  theme(axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
elevMapGG

# for terrain() function see
# https://arc2r.github.io/book/Surface.html

# working with slope ####
# extract slope from elevation data
RF_slope <- terrain(RF_elev, opt = "slope", unit = "degrees", neighbors=4)
# make a data frame version of slope
RF_slopeDF <- as.data.frame(RF_slope_MGA, xy=TRUE);head(RF_slopeDF,10)

# plot the raw slope data
ggplot() +
  geom_raster(data = RF_slopeDF , aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c(na.value = "#00000000") +
  labs(x = "Easting", y="Northing") +
  coord_fixed() +
  theme_bw()

# categorise the slopes into range classes
# nice breaks between classes found by trial and error!
RF_slopeDF$SlopeCat <- cut(RF_slopeDF$slope,
                       breaks = c(0,1,2,3,4,6,10,15,20,25,99),
                       labels=c("0-1","1-2","2-3","3-4", "4-6","6-10","12-15",
                                "15-20","20-25",">25"))
# summarise the slope categories
(pctslopes <-
  round(100*table(RF_slopeDF$SlopeCat)/sum(table(RF_slopeDF$SlopeCat)),2))

# plot the categorised slope map
slopeCatMapGG <- ggplot() +
  geom_raster(data = RF_slopeDF , aes(x = x, y = y, fill = SlopeCat)) +
  geom_sf(data = rf_hydrology, col="steelblue2") + # add streams
  scale_fill_viridis_d(na.value = "#FFFFFF00", name="Slope range (\u00B0)") +
  labs(x = "Longitude", y="Latitude") +
  coord_sf(crs=st_crs(28350)) +
  annotate(geom="text", x=495000, y=6407500,
          label=paste0("Slope categories based on DEM with 13m cells",
                        "\nProjection: Long-Lat using GDA94"),
           size=6, hjust = 0) +
  theme_bw() +
  theme(axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
slopeCatMapGG

# make elevation + slope image file
png(filename = "RF_elev_slope.png",width=1920, height=800)
ggarrange(elevMapGG,slopeCatMapGG,ncol=2)
dev.off()

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# working with aspect ####
# extract aspect from elevation data
RF_aspect <- terrain(RF_elev, opt = "aspect", unit = "degrees")
# make data frame version of aspect data
RF_aspectDF <- as.data.frame(RF_aspect, xy=TRUE)

# aspect with ggplot
RF_aspectGG <- ggplot() +
  geom_raster(data = RF_aspectDF, aes(x = x, y = y, fill = aspect)) +
  # make interpretable colour gradient
  scale_fill_gradientn(na.value = "#00000000",
                       colors = c("cornsilk","gold","royalblue2","blue4",
                                  "blue4","royalblue2","gold","cornsilk"),
                       name="Aspect\ndirection (\u00B0)") +
  geom_sf(data = rf_hydrology, col="coral", size=0.8) +
  labs(x = "Longitude", y="Latitude") +
  annotate(geom="text", x=495000, y=6407500,
           label=paste0("Slope aspect directions based on DEM with 13m cells",
                        "\n(yellow shades ~ North, blue shades ~ South)",
                        "\nProjection: Long-Lat using GDA94"),
           size=6, hjust = 0) +
  coord_sf(crs=st_crs(28350)) +
  theme_bw() +
  theme(axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
RF_aspectGG

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# terrain water flow direction ####
# extract flow direction from elevation data
RF_flowdir <- terrain(RF_elev, opt = "flowdir")
# make data frame version of flow directions
RF_flowdirDF <- as.data.frame(RF_flowdir, xy=TRUE)

# flow direction map with ggplot
RF_flowdirDF$flowQ <- cut(RF_flowdirDF$flowdir, breaks=c(0,2^seq(0,7)+0.5),
                          labels=c("E","SE","S","SW","W","NW","N","NE"))
RF_flowGG <- ggplot() +
  geom_raster(data = RF_flowdirDF, aes(x = x, y = y, fill = flowQ)) +
  scale_fill_viridis_d(na.value = "#00000000",
                       option = "rocket",
                       name="Flow\ndirection") +
  geom_sf(data = rf_hydrology, col="coral", size=0.8) +
  labs(x = "Longitude", y="Latitude") +
  annotate(geom="text", x=495000, y=6407500,
           label=paste0("Water flow direction classes based on DEM with 13m cells",
                        "\nProjection: Long-Lat using GDA94"),
           size=6, hjust = 0) +
  coord_sf(crs=st_crs(28350)) +
  theme_bw() +
  theme(axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
RF_flowGG

# make aspect + water flow image file
png(filename = "RF_aspect_flow.png",width=1920, height=800)
ggarrange(RF_aspectGG, RF_flowGG, ncol=2)
dev.off()

# Writing data back to geoTIFF ####
writeRaster(RF_slope, filename="Ridgefield_Slope_EPSG28350.tif", format="GTiff")
writeRaster(RF_aspect, filename="Ridgefield_Aspect_EPSG28350.tif", format="GTiff")
writeRaster(RF_flowdir, filename="Ridgefield_flowdir_EPSG28350.tif", format="GTiff")

GDALinfo("Ridgefield_Slope_EPSG28350.tif") # just file info
GDALinfo("Ridgefield_Aspect_EPSG28350.tif") # just file info
GDALinfo("Ridgefield_flowdir_EPSG28350.tif") # just file info


# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.
# not sure if this works!
# trying on the basis that slope curvature ~ second derivative of elevation???
RF_curvature <- terrain(RF_slope, opt = "slope", unit="degrees")
plot(RF_curvature, col=cividis(128))

# try fgeo.analyze:: R package for curvature ####

# try the test data first (made near top of this file)
elevTopoTib <- fgeo_topography(elevDF,gridsize=4,xdim=40,ydim=40, edgecorrect = F)
elevTopo <- as.data.frame(elevTopoTib)
image(seq(1,40), seq(1,40), as.matrix(testElev[,2:41]),xlab="x",ylab="y")
contour(seq(0,36,4), seq(0,36,4), matrix(elevTopo$convex,ncol=10),add=T,
        labcex=2)
# -=-=-=- OR -=-=-=-
image(seq(0,36,4), seq(0,36,4), matrix(elevTopo$convex,ncol=10),
      xlim=c(0,40), ylim=c(0,40))
elevTopo$curv <- cut(elevTopo$convex, breaks=c(-999999,0,999999),
                     labels=c("convex","concave"))
ggplot() +
  geom_raster(data = elevTopo, aes(x = gx, y = gy, fill = curv)) +
  geom_contour(data = elevDF, aes(x=x, y=y, z=elev)) +
  scale_fill_manual(values = c("steelblue","wheat"),na.value = "#FFFFFF00") +
  coord_equal() +
  theme_bw()

test_slopeDF <- as.data.frame(test_slope, xy=T)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# fgeo_topography() function still not working on Ridgefield data :( ####
# even when using a raster subset with no NA values
# NW_elevDF <- as.data.frame(NW_elev, xy=TRUE)
# names(NW_elevDF)[3] <- "elev"
# summary(NW_elevDF)

# > NW_topo_pars <- fgeo_topography(NW_elevDF, gridsize=20, xdim=425, ydim = 360)
## Error: cannot allocate vector of size 59.7 Gb

# > NW_topo_pars <- fgeo_topography(NW_elevDF, gridsize=40, xdim=425, ydim = 360)
## Error in if (theta1 <= theta2) { : missing value where TRUE/FALSE needed

# NW_topo_pars <- fgeo_topography(NW_elevDF, gridsize=40, xdim=425, ydim = 360,
#                                 edgecorrect = F)
## Error in if (theta1 <= theta2) { : missing value where TRUE/FALSE needed
