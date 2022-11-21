# create matrix of test elevations in Excel using saddle quadratic function &
# import into R data frame testElev, check with plot:
# testElev <- read.table("clipboard", sep="\t",header = TRUE)
image(seq(498000,499950,50), seq(6405700,6407650,50),
      as.matrix(testElev[,2:41]))
contour(seq(1,40), seq(1,40), as.matrix(testElev[,2:41]),add=T,
        labcex=1.)
# make into a 3-column dataframe (x,y,elev)
elevDF <- data.frame(x=rep(seq(498000,499950,50),40),
                     y=rep(seq(6405700,6407650,50),each=40),
                     elev=c(as.matrix(testElev[,2:41])))
# summary(elevDF)
testRast <- rasterFromXYZ(elevDF)
crs(testRast) <- "+proj=utm +zone=50 +south"
test_slope <- terrain(testRast, opt = "slope", unit = "degrees")

# this section from
# https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/
library(raster) # slope parameters from raster::terrain() function
library(rgdal)
library(ggplot2)
library(ggpubr)
library(viridis)
library(fgeo.analyze)
GDA94 <- CRS("+proj=geocent +ellps=GRS80 +units=m +no_defs +type=crs")

GDALinfo("Ridgefield_DEM_gda94_utm50_epsg28350.tif") # just file info
RF_raster <- raster("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
NWrect <- extent(498000,500500,6405700,6407000)
NW_elev <- raster::crop(RF_raster,NWrect)
RF_elevDF <- as.data.frame(RF_raster, xy=TRUE)
colnames(RF_elevDF)[3] <- "elev"

elevMapGG <- ggplot() +
  geom_raster(data = RF_elevDF , aes(x = x, y = y, fill = elev)) +
  geom_sf(data = rf_hydrology, col="steelblue2") +
  scale_fill_viridis_c(na.value = "#FFFFFF00", name="Elevation (m)") +
  labs(x = "Longitude", y="Latitude") +
  coord_sf(crs=GDA94) +
  annotate(geom="text", x=495000, y=6407200,
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
RF_slope <- terrain(RF_raster, opt = "slope", unit = "degrees", neighbors=4)
plot(RF_slope, col=viridis(30,begin=0))
RF_slope_MGA <- projectRaster(from=RF_slope, crs="+init=epsg:28350")
RF_slopeDF <- as.data.frame(RF_slope_MGA, xy=TRUE);head(RF_slopeDF,10)
slopeMapGG <- ggplot() +
  geom_raster(data = RF_slopeDF , aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c(na.value = "#00000000") +
  labs(x = "Easting", y="Northing") +
  coord_fixed() +
  theme_bw()

RF_slopeDF$SlopeCat <- cut(RF_slopeDF$slope,
                       breaks = c(0,1,2,3,4,6,10,15,20,25,99),
                       labels=c("0-1","1-2","2-3","3-4", "4-6","6-10","12-15",
                                "15-20","20-25",">25"))
(pctslopes <-
  round(100*table(RF_slopeDF$SlopeCat)/sum(table(RF_slopeDF$SlopeCat)),2))
slopeCatMapGG <- ggplot() +
  geom_raster(data = RF_slopeDF , aes(x = x, y = y, fill = SlopeCat)) +
  geom_sf(data = rf_hydrology, col="steelblue2") +
  scale_fill_viridis_d(na.value = "#FFFFFF00", name="Slope range (\u00B0)") +
  labs(x = "Longitude", y="Latitude") +
  coord_sf(crs=st_crs(28350)) +
  annotate(geom="text", x=495000, y=6407200,
          label=paste0("Slope categories based on DEM with 13m cells",
                        "\nProjection: Long-Lat using GDA94"),
           size=6, hjust = 0) +
  geom_rect(aes(xmin = 498000, xmax = 500500, ymin = 6405700, ymax = 6407000),
            alpha = 0, color = "red") +
  theme_bw() +
  theme(axis.title = element_text(size=16, face="bold"),
        axis.text = element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        legend.text = element_text(size=14))
slopeCatMapGG

png(filename = "RF_elev_slope.png",width=1920, height=800)
ggarrange(elevMapGG,slopeCatMapGG,ncol=2)
dev.off()

# working with aspect ####
RF_aspect <- terrain(RF_raster, opt = "aspect", unit = "degrees")
plot(RF_aspect, col=cividis(30))
RF_aspectDF <- as.data.frame(RF_aspect, xy=TRUE)
ggplot() +
  geom_raster(data = RF_aspectDF, aes(x = x, y = y, fill = aspect)) +
  scale_fill_viridis_c(na.value = "#00000000", option = "inferno") +
  labs(x = "Easting", y="Northing") +
  coord_sf() +
  theme_bw()

# not sure if this works!
RF_curvature <- terrain(RF_slope, opt = "slope", unit="degrees")
plot(RF_curvature, col=cividis(128))

RF_flowdir <- terrain(RF_raster, opt = "flowdir")
plot(log(RF_flowdir,base=2), col=rocket(8,begin=0.1),
     xlab="Easting", ylab="Northing")
log(2^seq(1,8), base=2)

# use fgeo.analyze:: R package for curvature

# try the test data first
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
# fgeo_topography() function still not working ####
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
