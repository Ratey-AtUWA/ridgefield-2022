# create matrix of test elevations in Excel using saddle quadratic function &
# import into R data frame testElev, check with plot:
image(seq(1,10), seq(1,10), as.matrix(testElev[,2:11]))
contour(seq(1,10), seq(1,10), as.matrix(testElev[,2:11]),add=T,
        labcex=2, levels=seq(0,100,2))

# this section from
# https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/
library(raster) # slope parameters from raster::terrain() function
library(rgdal)
library(ggplot2)
librray(viridis)

GDALinfo("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
RF_raster <- raster("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
RF_rast2df <- as.data.frame(RF_raster, xy=TRUE)
colnames(RF_rast2df)[3] <- "Elevation"

ggplot() +
  geom_raster(data = RF_rast2df , aes(x = x, y = y, fill = Elevation)) +
  scale_fill_viridis_c(na.value = "#00000000") +
  coord_fixed() +
  theme_bw()

# for terrain() function see
# https://arc2r.github.io/book/Surface.html
RF_slope <- terrain(RF_raster, opt = "slope", unit = "degrees", neighbors=4)
plot(RF_slope, col=viridis(30,begin=0))
RF_slopeDF <- as.data.frame(RF_slope, xy=TRUE)
ggplot() +
  geom_raster(data = RF_slopeDF , aes(x = x, y = y, fill = slope)) +
  scale_fill_viridis_c(na.value = "#00000000") +
  coord_fixed() +
  theme_bw()


RF_aspect <- terrain(RF_raster, opt = "aspect", unit = "degrees")
plot(RF_aspect, col=cividis(30))
RF_aspectDF <- as.data.frame(RF_aspect, xy=TRUE)
ggplot() +
  geom_raster(data = RF_aspectDF, aes(x = x, y = y, fill = aspect)) +
  scale_fill_viridis_c(na.value = "#00000000", option = "inferno") +
  coord_fixed() +
  theme_bw()

# not sure if this works!
RF_curvature <- terrain(RF_slope, opt = "slope", unit="degrees")
plot(RF_curvature, col=cividis(128))

RF_flowdir <- terrain(RF_raster, opt = "flowdir")
plot(log(RF_flowdir), col=rocket(8,begin=0))
log(2^seq(1,7))
